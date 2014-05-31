/**
 * 
 */
package io.sarl.eclipse.wizards;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileInfo;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceStatus;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.corext.util.Messages;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.dialogs.StatusInfo;
import org.eclipse.jdt.internal.ui.util.CoreUtility;
import org.eclipse.jdt.internal.ui.util.ExceptionHandler;
import org.eclipse.jdt.internal.ui.wizards.ClassPathDetector;
import org.eclipse.jdt.internal.ui.wizards.NewWizardMessages;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jdt.ui.wizards.JavaCapabilityConfigurationPage;
import org.eclipse.jdt.ui.wizards.NewJavaProjectWizardPageTwo;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.ui.actions.WorkspaceModifyDelegatingOperation;

/**
 * The second page of the SARL new project wizard.
 * Most part of the code of this class is copy/paste from {@link NewJavaProjectWizardPageTwo}
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("restriction")
public class NewSARLProjectWizardPageTwo extends JavaCapabilityConfigurationPage {

	private static final String FILENAME_PROJECT = ".project"; //$NON-NLS-1$
	private static final String FILENAME_CLASSPATH = ".classpath"; //$NON-NLS-1$

	private final NewSARLProjectWizardPageOne fFirstPage;

	private URI fCurrProjectLocation; // null if location is platform location
	private IProject fCurrProject;

	private boolean fKeepContent;

	private File fDotProjectBackup;
	private File fDotClasspathBackup;
	private Boolean fIsAutobuild;
	private HashSet<IFileStore> fOrginalFolders;

	/**
	 * Constructor for the {@link NewJavaProjectWizardPageTwo}.
	 * 
	 * @param mainPage the first page of the wizard
	 */
	public NewSARLProjectWizardPageTwo(NewSARLProjectWizardPageOne mainPage) {
		this.fFirstPage = mainPage;
		this.fCurrProjectLocation = null;
		this.fCurrProject = null;
		this.fKeepContent = false;

		this.fDotProjectBackup = null;
		this.fDotClasspathBackup = null;
		this.fIsAutobuild = null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jdt.ui.wizards.JavaCapabilityConfigurationPage#useNewSourcePage()
	 */
	@Override
	protected final boolean useNewSourcePage() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.IDialogPage#setVisible(boolean)
	 */
	@Override
	public void setVisible(boolean visible) {
		boolean isShownFirstTime = visible && this.fCurrProject == null;
		if (visible) {
			if (isShownFirstTime) { // entering from the first page
				createProvisonalProject();
			}
		} else {
			if (getContainer().getCurrentPage() == this.fFirstPage) { // leaving back to the first page
				removeProvisonalProject();
			}
		}
		super.setVisible(visible);
		if (isShownFirstTime) {
			setFocus();
		}
	}

	private boolean hasExistingContent(URI realLocation) throws CoreException {
		IFileStore file = EFS.getStore(realLocation);
		return file.fetchInfo().exists();
	}

	private IStatus changeToNewProject() {
		class UpdateRunnable implements IRunnableWithProgress {
			public IStatus infoStatus = Status.OK_STATUS;

			public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
				try {
					if (NewSARLProjectWizardPageTwo.this.fIsAutobuild == null) {
						NewSARLProjectWizardPageTwo.this.fIsAutobuild = Boolean.valueOf(CoreUtility.setAutoBuilding(false));
					}
					this.infoStatus = updateProject(monitor);
				} catch (CoreException e) {
					throw new InvocationTargetException(e);
				} catch (OperationCanceledException e) {
					throw new InterruptedException();
				} finally {
					monitor.done();
				}
			}
		}
		UpdateRunnable op = new UpdateRunnable();
		try {
			getContainer().run(true, false, new WorkspaceModifyDelegatingOperation(op));
			return op.infoStatus;
		} catch (InvocationTargetException e) {
			final String title = NewWizardMessages.NewJavaProjectWizardPageTwo_error_title;
			final String message = NewWizardMessages.NewJavaProjectWizardPageTwo_error_message;
			ExceptionHandler.handle(e, getShell(), title, message);
		} catch (InterruptedException e) {
			// cancel pressed
		}
		return null;
	}

	private static URI getRealLocation(String projectName, URI location) {
		if (location == null) { // inside workspace
			try {
				URI rootLocation = ResourcesPlugin.getWorkspace().getRoot().getLocationURI();

				location = new URI(rootLocation.getScheme(), null, Path.fromPortableString(rootLocation.getPath()).append(projectName).toString(), null);
			} catch (URISyntaxException e) {
				Assert.isTrue(false, "Can't happen"); //$NON-NLS-1$
			}
		}
		return location;
	}

	private final IStatus updateProject(IProgressMonitor monitor) throws CoreException, InterruptedException {
		IStatus result = StatusInfo.OK_STATUS;
		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}
		try {
			monitor.beginTask(NewWizardMessages.NewJavaProjectWizardPageTwo_operation_initialize, 7);
			if (monitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			String projectName = this.fFirstPage.getProjectName();

			this.fCurrProject = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
			this.fCurrProjectLocation = this.fFirstPage.getProjectLocationURI();

			URI realLocation = getRealLocation(projectName, this.fCurrProjectLocation);
			this.fKeepContent = hasExistingContent(realLocation);

			if (monitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			if (this.fKeepContent) {
				rememberExistingFiles(realLocation);
				rememberExisitingFolders(realLocation);
			}

			if (monitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			try {
				createProject(this.fCurrProject, this.fCurrProjectLocation, new SubProgressMonitor(monitor, 2));
			} catch (CoreException e) {
				if (e.getStatus().getCode() == IResourceStatus.FAILED_READ_METADATA) {
					result = new StatusInfo(IStatus.INFO, Messages.format(NewWizardMessages.NewJavaProjectWizardPageTwo_DeleteCorruptProjectFile_message, e.getLocalizedMessage()));

					deleteProjectFile(realLocation);
					if (this.fCurrProject.exists())
						this.fCurrProject.delete(true, null);

					createProject(this.fCurrProject, this.fCurrProjectLocation, null);
				} else {
					throw e;
				}
			}

			if (monitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			initializeBuildPath(JavaCore.create(this.fCurrProject), new SubProgressMonitor(monitor, 2));
			configureJavaProject(new SubProgressMonitor(monitor, 3)); // create the Java project to allow the use of the new source folder page
		} finally {
			monitor.done();
		}
		return result;
	}

	/**
	 * Evaluates the new build path and output folder according to the settings on the first page. The resulting build path is set by calling {@link #init(IJavaProject, IPath, IClasspathEntry[], boolean)}. Clients can override this method.
	 * 
	 * @param javaProject the new project which is already created when this method is called.
	 * @param monitor the progress monitor
	 * @throws CoreException thrown when initializing the build path failed
	 */
	protected void initializeBuildPath(IJavaProject javaProject, IProgressMonitor monitor) throws CoreException {
		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}
		monitor.beginTask(NewWizardMessages.NewJavaProjectWizardPageTwo_monitor_init_build_path, 2);

		try {
			IClasspathEntry[] entries = null;
			IPath outputLocation = null;
			IProject project = javaProject.getProject();

			if (this.fKeepContent) {
				if (!project.getFile(FILENAME_CLASSPATH).exists()) {
					final ClassPathDetector detector = new ClassPathDetector(this.fCurrProject, new SubProgressMonitor(monitor, 2));
					entries = detector.getClasspath();
					outputLocation = detector.getOutputLocation();
					if (entries.length == 0)
						entries = null;
				} else {
					monitor.worked(2);
				}
			} else {
				List<IClasspathEntry> cpEntries = new ArrayList<IClasspathEntry>();
				IWorkspaceRoot root = project.getWorkspace().getRoot();

				IClasspathEntry[] sourceClasspathEntries = this.fFirstPage.getSourceClasspathEntries();
				for (int i = 0; i < sourceClasspathEntries.length; i++) {
					IPath path = sourceClasspathEntries[i].getPath();
					if (path.segmentCount() > 1) {
						IFolder folder = root.getFolder(path);
						CoreUtility.createFolder(folder, true, true, new SubProgressMonitor(monitor, 1));
					}
					cpEntries.add(sourceClasspathEntries[i]);
				}

				cpEntries.addAll(Arrays.asList(this.fFirstPage.getDefaultClasspathEntries()));

				entries = cpEntries.toArray(new IClasspathEntry[cpEntries.size()]);

				outputLocation = this.fFirstPage.getOutputLocation();
				if (outputLocation.segmentCount() > 1) {
					IFolder folder = root.getFolder(outputLocation);
					CoreUtility.createDerivedFolder(folder, true, true, new SubProgressMonitor(monitor, 1));
				}
			}
			if (monitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			init(javaProject, outputLocation, entries, false);
		} finally {
			monitor.done();
		}
	}

	private void deleteProjectFile(URI projectLocation) throws CoreException {
		IFileStore file = EFS.getStore(projectLocation);
		if (file.fetchInfo().exists()) {
			IFileStore projectFile = file.getChild(FILENAME_PROJECT);
			if (projectFile.fetchInfo().exists()) {
				projectFile.delete(EFS.NONE, null);
			}
		}
	}

	private void rememberExisitingFolders(URI projectLocation) {
		this.fOrginalFolders = new HashSet<IFileStore>();

		try {
			IFileStore[] children = EFS.getStore(projectLocation).childStores(EFS.NONE, null);
			for (int i = 0; i < children.length; i++) {
				IFileStore child = children[i];
				IFileInfo info = child.fetchInfo();
				if (info.isDirectory() && info.exists() && !this.fOrginalFolders.contains(child.getName())) {
					this.fOrginalFolders.add(child);
				}
			}
		} catch (CoreException e) {
			JavaPlugin.log(e);
		}
	}

	private void restoreExistingFolders(URI projectLocation) {
		HashSet<IFileStore> foldersToKeep = new HashSet<IFileStore>(this.fOrginalFolders);
		// workaround for bug 319054: Eclipse deletes all files when I cancel a project creation (symlink in project location path)
		for (IFileStore originalFileStore : this.fOrginalFolders) {
			try {
				File localFile = originalFileStore.toLocalFile(EFS.NONE, null);
				if (localFile != null) {
					File canonicalFile = localFile.getCanonicalFile();
					IFileStore canonicalFileStore = originalFileStore.getFileSystem().fromLocalFile(canonicalFile);
					if (!originalFileStore.equals(canonicalFileStore)) {
						foldersToKeep.add(canonicalFileStore);
					}
				}
			} catch (IOException e) {
			} catch (CoreException e) {
			}
		}

		try {
			IFileStore[] children = EFS.getStore(projectLocation).childStores(EFS.NONE, null);
			for (int i = 0; i < children.length; i++) {
				IFileStore child = children[i];
				IFileInfo info = child.fetchInfo();
				if (info.isDirectory() && info.exists() && !foldersToKeep.contains(child)) {
					child.delete(EFS.NONE, null);
					this.fOrginalFolders.remove(child);
				}
			}

			for (Iterator<IFileStore> iterator = this.fOrginalFolders.iterator(); iterator.hasNext();) {
				IFileStore deleted = iterator.next();
				deleted.mkdir(EFS.NONE, null);
			}
		} catch (CoreException e) {
			JavaPlugin.log(e);
		}
	}

	private void rememberExistingFiles(URI projectLocation) throws CoreException {
		this.fDotProjectBackup = null;
		this.fDotClasspathBackup = null;

		IFileStore file = EFS.getStore(projectLocation);
		if (file.fetchInfo().exists()) {
			IFileStore projectFile = file.getChild(FILENAME_PROJECT);
			if (projectFile.fetchInfo().exists()) {
				this.fDotProjectBackup = createBackup(projectFile, "project-desc"); //$NON-NLS-1$
			}
			IFileStore classpathFile = file.getChild(FILENAME_CLASSPATH);
			if (classpathFile.fetchInfo().exists()) {
				this.fDotClasspathBackup = createBackup(classpathFile, "classpath-desc"); //$NON-NLS-1$
			}
		}
	}

	private void restoreExistingFiles(URI projectLocation, IProgressMonitor monitor) throws CoreException {
		int ticks = ((this.fDotProjectBackup != null ? 1 : 0) + (this.fDotClasspathBackup != null ? 1 : 0)) * 2;
		monitor.beginTask("", ticks); //$NON-NLS-1$
		try {
			IFileStore projectFile = EFS.getStore(projectLocation).getChild(FILENAME_PROJECT);
			projectFile.delete(EFS.NONE, new SubProgressMonitor(monitor, 1));
			if (this.fDotProjectBackup != null) {
				copyFile(this.fDotProjectBackup, projectFile, new SubProgressMonitor(monitor, 1));
			}
		} catch (IOException e) {
			IStatus status = new Status(IStatus.ERROR, JavaUI.ID_PLUGIN, IStatus.ERROR, NewWizardMessages.NewJavaProjectWizardPageTwo_problem_restore_project, e);
			throw new CoreException(status);
		}
		try {
			IFileStore classpathFile = EFS.getStore(projectLocation).getChild(FILENAME_CLASSPATH);
			classpathFile.delete(EFS.NONE, new SubProgressMonitor(monitor, 1));
			if (this.fDotClasspathBackup != null) {
				copyFile(this.fDotClasspathBackup, classpathFile, new SubProgressMonitor(monitor, 1));
			}
		} catch (IOException e) {
			IStatus status = new Status(IStatus.ERROR, JavaUI.ID_PLUGIN, IStatus.ERROR, NewWizardMessages.NewJavaProjectWizardPageTwo_problem_restore_classpath, e);
			throw new CoreException(status);
		}
	}

	private File createBackup(IFileStore source, String name) throws CoreException {
		try {
			File bak = File.createTempFile("eclipse-" + name, ".bak"); //$NON-NLS-1$//$NON-NLS-2$
			copyFile(source, bak);
			return bak;
		} catch (IOException e) {
			IStatus status = new Status(IStatus.ERROR, JavaUI.ID_PLUGIN, IStatus.ERROR, Messages.format(NewWizardMessages.NewJavaProjectWizardPageTwo_problem_backup, name), e);
			throw new CoreException(status);
		}
	}

	private void copyFile(IFileStore source, File target) throws IOException, CoreException {
		InputStream is = source.openInputStream(EFS.NONE, null);
		FileOutputStream os = new FileOutputStream(target);
		copyFile(is, os);
	}

	private void copyFile(File source, IFileStore target, IProgressMonitor monitor) throws IOException, CoreException {
		FileInputStream is = new FileInputStream(source);
		OutputStream os = target.openOutputStream(EFS.NONE, monitor);
		copyFile(is, os);
	}

	private void copyFile(InputStream is, OutputStream os) throws IOException {
		try {
			byte[] buffer = new byte[8192];
			while (true) {
				int bytesRead = is.read(buffer);
				if (bytesRead == -1)
					break;

				os.write(buffer, 0, bytesRead);
			}
		} finally {
			try {
				is.close();
			} finally {
				os.close();
			}
		}
	}

	/**
	 * Called from the wizard on finish.
	 * 
	 * @param monitor the progress monitor
	 * @throws CoreException thrown when the project creation or configuration failed
	 * @throws InterruptedException thrown when the user cancelled the project creation
	 */
	public void performFinish(IProgressMonitor monitor) throws CoreException, InterruptedException {
		try {
			monitor.beginTask(NewWizardMessages.NewJavaProjectWizardPageTwo_operation_create, 3);
			if (this.fCurrProject == null) {
				updateProject(new SubProgressMonitor(monitor, 1));
			}
			String newProjectCompliance = this.fKeepContent ? null : this.fFirstPage.getCompilerCompliance();
			configureJavaProject(newProjectCompliance, new SubProgressMonitor(monitor, 2));

		} finally {
			monitor.done();
			this.fCurrProject = null;
			if (this.fIsAutobuild != null) {
				CoreUtility.setAutoBuilding(this.fIsAutobuild.booleanValue());
				this.fIsAutobuild = null;
			}
		}
	}

	/**
	 * Creates the provisional project on which the wizard is working on. The provisional project is typically created when the page is entered the first time. The early project creation is required to configure linked folders.
	 * 
	 * @return the provisional project
	 */
	protected IProject createProvisonalProject() {
		IStatus status = changeToNewProject();
		if (status != null && !status.isOK()) {
			ErrorDialog.openError(getShell(), NewWizardMessages.NewJavaProjectWizardPageTwo_error_title, null, status);
		}
		return this.fCurrProject;
	}

	/**
	 * Removes the provisional project. The provisional project is typically removed when the user cancels the wizard or goes back to the first page.
	 */
	protected void removeProvisonalProject() {
		if (!this.fCurrProject.exists()) {
			this.fCurrProject = null;
			return;
		}

		IRunnableWithProgress op = new IRunnableWithProgress() {
			public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
				doRemoveProject(monitor);
			}
		};

		try {
			getContainer().run(true, true, new WorkspaceModifyDelegatingOperation(op));
		} catch (InvocationTargetException e) {
			final String title = NewWizardMessages.NewJavaProjectWizardPageTwo_error_remove_title;
			final String message = NewWizardMessages.NewJavaProjectWizardPageTwo_error_remove_message;
			ExceptionHandler.handle(e, getShell(), title, message);
		} catch (InterruptedException e) {
			// cancel pressed
		}
	}

	private final void doRemoveProject(IProgressMonitor monitor) throws InvocationTargetException {
		final boolean noProgressMonitor = (this.fCurrProjectLocation == null); // inside workspace
		if (monitor == null || noProgressMonitor) {
			monitor = new NullProgressMonitor();
		}
		monitor.beginTask(NewWizardMessages.NewJavaProjectWizardPageTwo_operation_remove, 3);
		try {
			try {
				URI projLoc = this.fCurrProject.getLocationURI();

				boolean removeContent = !this.fKeepContent && this.fCurrProject.isSynchronized(IResource.DEPTH_INFINITE);
				if (!removeContent) {
					restoreExistingFolders(projLoc);
				}
				this.fCurrProject.delete(removeContent, false, new SubProgressMonitor(monitor, 2));

				restoreExistingFiles(projLoc, new SubProgressMonitor(monitor, 1));
			} finally {
				CoreUtility.setAutoBuilding(this.fIsAutobuild.booleanValue()); // fIsAutobuild must be set
				this.fIsAutobuild = null;
			}
		} catch (CoreException e) {
			throw new InvocationTargetException(e);
		} finally {
			monitor.done();
			this.fCurrProject = null;
			this.fKeepContent = false;
		}
	}

	/**
	 * Called from the wizard on cancel.
	 */
	public void performCancel() {
		if (this.fCurrProject != null) {
			removeProvisonalProject();
		}
	}
}
