/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.eclipse.wizards.newproject;

import io.sarl.eclipse.util.PluginUtil;
import io.sarl.lang.ui.internal.SARLActivator;

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
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.actions.WorkspaceModifyDelegatingOperation;
import org.eclipse.xtext.builder.EclipseOutputConfigurationProvider;
import org.eclipse.xtext.builder.preferences.BuilderPreferenceAccess;
import org.eclipse.xtext.generator.OutputConfiguration;
import org.eclipse.xtext.ui.editor.preferences.PreferenceStoreAccessImpl;
import org.eclipse.xtext.ui.preferences.OptionsConfigurationBlock;
import org.eclipse.xtext.xbase.lib.Pair;

import com.google.inject.Injector;

/**
 * The second page of the SARL new project wizard.
 * Most part of the code of this class is copy/paste from {@link NewJavaProjectWizardPageTwo}.
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NewSARLProjectWizardPageTwo extends JavaCapabilityConfigurationPage {

	private static final String FILENAME_PROJECT = ".project"; //$NON-NLS-1$
	private static final String FILENAME_CLASSPATH = ".classpath"; //$NON-NLS-1$
	private static final int FILE_COPY_BLOCK_SIZE = 8192;
	private static final int UPDATE_PROJECT_MONITORED_STEPS = 7;

	private final NewSARLProjectWizardPageOne fFirstPage;

	/** Location of the current project.
	 * It is <code>null</code> if the location is a platform location.
	 */
	private URI fCurrProjectLocation;
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
		setPageComplete(false);

		this.fFirstPage = mainPage;
		this.fCurrProjectLocation = null;
		this.fCurrProject = null;
		this.fKeepContent = false;

		this.fDotProjectBackup = null;
		this.fDotClasspathBackup = null;
		this.fIsAutobuild = null;

		setTitle(SARLProjectNewWizardMessages.SARLProjectNewWizard_WIZARD_PAGE_NAME);
		setDescription(SARLProjectNewWizardMessages.SARLProjectNewWizard_WIZARD_PAGE_2_DESCRIPTION);
		setImageDescriptor(PluginUtil.getImageDescriptor(
				PluginUtil.NEW_PROJECT_WIZARD_DIALOG_IMAGE));
	}

	@Override
	protected final boolean useNewSourcePage() {
		return true;
	}

	@Override
	public void setVisible(boolean visible) {
		boolean isShownFirstTime = visible && this.fCurrProject == null;
		if (visible) {
			// Entering from the first page
			if (isShownFirstTime) {
				createProvisonalProject();
			}
		} else {
			// Leaving back to the first page
			if (getContainer().getCurrentPage() == this.fFirstPage) {
				removeProvisonalProject();
			}
		}
		super.setVisible(visible);
		if (isShownFirstTime) {
			setFocus();
		}
	}

	private static boolean hasExistingContent(URI realLocation) throws CoreException {
		IFileStore file = EFS.getStore(realLocation);
		return file.fetchInfo().exists();
	}

	private IStatus changeToNewProject() {
		UpdateRunnable op = new UpdateRunnable();
		try {
			getContainer().run(true, false, new WorkspaceModifyDelegatingOperation(op));
			return op.getInfoStatus();
		} catch (InvocationTargetException e) {
			final String title = NewWizardMessages.NewJavaProjectWizardPageTwo_error_title;
			final String message = NewWizardMessages.NewJavaProjectWizardPageTwo_error_message;
			updateStatus(e);
			ExceptionHandler.handle(e, getShell(), title, message);
		} catch (InterruptedException e) {
			// cancel pressed
		}
		return null;
	}

	/** Update the status of this page according to the given exception.
	 *
	 * @param e - the exception.
	 */
	private void updateStatus(Throwable e) {
		Throwable cause = e;
		while (cause != null
				&& (!(cause instanceof CoreException))
				&& cause.getCause() != null
				&& cause.getCause() != cause) {
			cause = cause.getCause();
		}
		if (cause instanceof CoreException) {
			updateStatus(((CoreException) cause).getStatus());
		} else {
			String message;
			if (cause != null) {
				message = cause.getLocalizedMessage();
			} else {
				message = e.getLocalizedMessage();
			}
			IStatus status = new StatusInfo(IStatus.ERROR, message);
			updateStatus(status);
		}
	}

	private static URI getRealLocation(String projectName, URI location) {
		URI theLocation = location;
		// Test if the project is inside workspace
		if (theLocation == null) {
			try {
				URI rootLocation = ResourcesPlugin.getWorkspace().getRoot().getLocationURI();
				theLocation = new URI(rootLocation.getScheme(), null,
						Path.fromPortableString(
								rootLocation.getPath()).append(projectName).toString(),
								null);
			} catch (URISyntaxException e) {
				Assert.isTrue(false, "Can't happen"); //$NON-NLS-1$
			}
		}
		return theLocation;
	}

	private IStatus updateProject(IProgressMonitor monitor) throws CoreException, InterruptedException {
		IProgressMonitor theMonitor = monitor;
		IStatus result = StatusInfo.OK_STATUS;
		if (theMonitor == null) {
			theMonitor = new NullProgressMonitor();
		}
		try {
			theMonitor.beginTask(
					NewWizardMessages.NewJavaProjectWizardPageTwo_operation_initialize,
					UPDATE_PROJECT_MONITORED_STEPS);
			if (theMonitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			String projectName = this.fFirstPage.getProjectName();

			this.fCurrProject = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
			this.fCurrProjectLocation = this.fFirstPage.getProjectLocationURI();

			URI realLocation = getRealLocation(projectName, this.fCurrProjectLocation);
			this.fKeepContent = hasExistingContent(realLocation);

			if (theMonitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			if (this.fKeepContent) {
				rememberExistingFiles(realLocation);
				rememberExisitingFolders(realLocation);
			}

			if (theMonitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			try {
				createProject(this.fCurrProject, this.fCurrProjectLocation,
						new SubProgressMonitor(theMonitor, 2));
			} catch (CoreException e) {
				if (e.getStatus().getCode() == IResourceStatus.FAILED_READ_METADATA) {
					result = new StatusInfo(
							IStatus.INFO, Messages.format(
									NewWizardMessages.NewJavaProjectWizardPageTwo_DeleteCorruptProjectFile_message,
									e.getLocalizedMessage()));

					deleteProjectFile(realLocation);
					if (this.fCurrProject.exists()) {
						this.fCurrProject.delete(true, null);
					}

					createProject(this.fCurrProject, this.fCurrProjectLocation, null);
				} else {
					throw e;
				}
			}

			if (theMonitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			initializeBuildPath(JavaCore.create(this.fCurrProject), new SubProgressMonitor(theMonitor, 2));
			// Create the Java project to allow the use of the new source folder page
			configureJavaProject(new SubProgressMonitor(theMonitor, 3));
		} finally {
			theMonitor.done();
		}
		return result;
	}

	private Pair<IClasspathEntry[], IPath> keepExistingBuildPath(IProject project, IProgressMonitor monitor)
			throws CoreException {
		IClasspathEntry[] entries = null;
		IPath outputLocation = null;
		if (!project.getFile(FILENAME_CLASSPATH).exists()) {
			final ClassPathDetector detector = new ClassPathDetector(
					this.fCurrProject, new SubProgressMonitor(monitor, 2));
			entries = detector.getClasspath();
			outputLocation = detector.getOutputLocation();
			if (entries.length == 0) {
				entries = null;
			}
		} else {
			monitor.worked(2);
		}
		return Pair.of(entries, outputLocation);
	}

	private Pair<IClasspathEntry[], IPath> buildNewBuildPath(IProject project, IProgressMonitor monitor) throws CoreException {
		List<IClasspathEntry> cpEntries = new ArrayList<>();
		IWorkspaceRoot root = project.getWorkspace().getRoot();

		for (IClasspathEntry sourceClasspathEntry : this.fFirstPage.getSourceClasspathEntries()) {
			IPath path = sourceClasspathEntry.getPath();
			if (path.segmentCount() > 1) {
				IFolder folder = root.getFolder(path);
				CoreUtility.createFolder(
						folder,
						true, true,
						new SubProgressMonitor(monitor, 1));
			}
			cpEntries.add(sourceClasspathEntry);
		}

		this.fFirstPage.putDefaultClasspathEntriesIn(cpEntries);

		IClasspathEntry[] entries = cpEntries.toArray(new IClasspathEntry[cpEntries.size()]);

		IPath outputLocation = this.fFirstPage.getOutputLocation();
		if (outputLocation.segmentCount() > 1) {
			IFolder folder = root.getFolder(outputLocation);
			CoreUtility.createDerivedFolder(
					folder,
					true, true,
					new SubProgressMonitor(monitor, 1));
		}

		return Pair.of(entries, outputLocation);
	}

	/**
	 * Evaluates the new build path and output folder according to the settings on the first page.
	 * The resulting build path is set by calling
	 * {@link #init(IJavaProject, IPath, IClasspathEntry[], boolean)}.
	 * Clients can override this method.
	 *
	 * @param javaProject the new project which is already created when this method is called.
	 * @param monitor the progress monitor
	 * @throws CoreException thrown when initializing the build path failed
	 */
	protected void initializeBuildPath(IJavaProject javaProject, IProgressMonitor monitor) throws CoreException {
		IProgressMonitor theMonitor = monitor;
		if (theMonitor == null) {
			theMonitor = new NullProgressMonitor();
		}
		theMonitor.beginTask(NewWizardMessages.NewJavaProjectWizardPageTwo_monitor_init_build_path, 2);

		try {
			IClasspathEntry[] entries = null;
			IPath outputLocation = null;
			IProject project = javaProject.getProject();

			if (this.fKeepContent) {
				Pair<IClasspathEntry[], IPath> pair = keepExistingBuildPath(project, theMonitor);
				entries = pair.getKey();
				outputLocation = pair.getValue();
			} else {
				Pair<IClasspathEntry[], IPath> pair = buildNewBuildPath(project, theMonitor);
				entries = pair.getKey();
				outputLocation = pair.getValue();
			}
			if (theMonitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			init(javaProject, outputLocation, entries, false);
		} catch (CoreException e) {
			throw e;
		} catch (Throwable e) {
			if (e.getCause() instanceof CoreException) {
				throw (CoreException) e.getCause();
			}
			Throwable ee;
			if (e.getCause() != null && e.getCause() != e) {
				ee = e.getCause();
			} else {
				ee = e;
			}
			throw new CoreException(PluginUtil.createStatus(IStatus.ERROR, ee));
		} finally {
			theMonitor.done();
		}
	}

	private static void deleteProjectFile(URI projectLocation) throws CoreException {
		IFileStore file = EFS.getStore(projectLocation);
		if (file.fetchInfo().exists()) {
			IFileStore projectFile = file.getChild(FILENAME_PROJECT);
			if (projectFile.fetchInfo().exists()) {
				projectFile.delete(EFS.NONE, null);
			}
		}
	}

	private void rememberExisitingFolders(URI projectLocation) {
		this.fOrginalFolders = new HashSet<>();

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
			PluginUtil.log(e);
		}
	}

	private void restoreExistingFolders(URI projectLocation) {
		HashSet<IFileStore> foldersToKeep = new HashSet<>(this.fOrginalFolders);
		// workaround for bug 319054: Eclipse deletes all files when I cancel
		// a project creation (symlink in project location path)
		for (IFileStore originalFileStore : this.fOrginalFolders) {
			try {
				File localFile = originalFileStore.toLocalFile(EFS.NONE, null);
				if (localFile != null) {
					File canonicalFile = localFile.getCanonicalFile();
					IFileStore canonicalFileStore =
							originalFileStore.getFileSystem().fromLocalFile(canonicalFile);
					if (!originalFileStore.equals(canonicalFileStore)) {
						foldersToKeep.add(canonicalFileStore);
					}
				}
			} catch (IOException e) {
				//
			} catch (CoreException e) {
				//
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
			PluginUtil.log(e);
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
			IStatus status = new Status(
					IStatus.ERROR,
					JavaUI.ID_PLUGIN,
					IStatus.ERROR,
					NewWizardMessages.NewJavaProjectWizardPageTwo_problem_restore_project,
					e);
			throw new CoreException(status);
		}
		try {
			IFileStore classpathFile = EFS.getStore(projectLocation).getChild(FILENAME_CLASSPATH);
			classpathFile.delete(EFS.NONE, new SubProgressMonitor(monitor, 1));
			if (this.fDotClasspathBackup != null) {
				copyFile(this.fDotClasspathBackup, classpathFile, new SubProgressMonitor(monitor, 1));
			}
		} catch (IOException e) {
			IStatus status = new Status(
					IStatus.ERROR,
					JavaUI.ID_PLUGIN,
					IStatus.ERROR,
					NewWizardMessages.NewJavaProjectWizardPageTwo_problem_restore_classpath,
					e);
			throw new CoreException(status);
		}
	}

	private static File createBackup(IFileStore source, String name) throws CoreException {
		try {
			File bak = File.createTempFile("eclipse-" + name, ".bak"); //$NON-NLS-1$//$NON-NLS-2$
			copyFile(source, bak);
			return bak;
		} catch (IOException e) {
			IStatus status = new Status(
					IStatus.ERROR,
					JavaUI.ID_PLUGIN,
					IStatus.ERROR,
					Messages.format(
							NewWizardMessages.NewJavaProjectWizardPageTwo_problem_backup,
							name),
							e);
			throw new CoreException(status);
		}
	}

	private static void copyFile(IFileStore source, File target) throws IOException, CoreException {
		try (InputStream is = source.openInputStream(EFS.NONE, null)) {
			try (FileOutputStream os = new FileOutputStream(target)) {
				unsecureCopyFile(is, os);
			}
		}
	}

	private static void copyFile(File source, IFileStore target, IProgressMonitor monitor) throws IOException, CoreException {
		try (FileInputStream is = new FileInputStream(source)) {
			try (OutputStream os = target.openOutputStream(EFS.NONE, monitor)) {
				unsecureCopyFile(is, os);
			}
		}
	}

	private static void unsecureCopyFile(InputStream is, OutputStream os) throws IOException {
		byte[] buffer = new byte[FILE_COPY_BLOCK_SIZE];
		int bytesRead = is.read(buffer);
		while (bytesRead > 0) {
			os.write(buffer, 0, bytesRead);
			bytesRead = is.read(buffer);
		}
		os.flush();
	}

	private String findGenerationSourcePath() {
		IPath projectPath = this.fCurrProject.getFullPath();
		IPath p;
		String s;
		for (IClasspathEntry entry : getRawClassPath()) {
			p = entry.getPath();
			p = p.removeFirstSegments(p.matchingFirstSegments(projectPath));
			s = p.toOSString();
			if (s.endsWith(Config.DEFAULT_GENERATED_SOURCE_FOLDER)) {
				return s;
			}
			if (s.contains("src-gen") || s.contains("generated")) { //$NON-NLS-1$//$NON-NLS-2$
				return s;
			}
		}
		return null;
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

			String generationFolder = findGenerationSourcePath();
			if (generationFolder != null) {

				// Retreive the preference page for the project
				Injector injector = SARLActivator.getInstance().getInjector(SARLActivator.IO_SARL_LANG_SARL);

				PreferenceStoreAccessImpl preferenceStoreAccessImpl = injector.getInstance(
						PreferenceStoreAccessImpl.class);
				IPreferenceStore preferenceStore = preferenceStoreAccessImpl.getWritablePreferenceStore(
						this.fCurrProject);

				// Force to use a specific configuration.
				preferenceStore.setValue(OptionsConfigurationBlock.IS_PROJECT_SPECIFIC, true);

				// Initialize the project configurations
				EclipseOutputConfigurationProvider configurationProvider =
						injector.getInstance(EclipseOutputConfigurationProvider.class);
				for (OutputConfiguration projectConfiguration
						: configurationProvider.getOutputConfigurations(this.fCurrProject)) {
					String directoryKey = BuilderPreferenceAccess.getKey(
							projectConfiguration,
							EclipseOutputConfigurationProvider.OUTPUT_DIRECTORY);
					preferenceStore.putValue(directoryKey, generationFolder);
				}
			}

			String newProjectCompliance = this.fKeepContent ? null : this.fFirstPage.getCompilerCompliance();
			configureJavaProject(newProjectCompliance, new SubProgressMonitor(monitor, 2));

		} catch (Throwable e) {
			if (this.fCurrProject != null) {
				removeProvisonalProject();
			}
			throw e;
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
	 * Creates the provisional project on which the wizard is working on.
	 * The provisional project is typically created when the page is entered
	 * the first time. The early project creation is required to configure
	 * linked folders.
	 *
	 * @return the provisional project
	 */
	protected IProject createProvisonalProject() {
		IStatus status = changeToNewProject();
		if (status != null) {
			updateStatus(status);
			if (!status.isOK()) {
				ErrorDialog.openError(
						getShell(),
						NewWizardMessages.NewJavaProjectWizardPageTwo_error_title,
						null,
						status);
			}
		}
		return this.fCurrProject;
	}

	/**
	 * Removes the provisional project. The provisional project is typically
	 * removed when the user cancels the wizard or goes back to the first page.
	 */
	protected void removeProvisonalProject() {
		if (!this.fCurrProject.exists()) {
			this.fCurrProject = null;
			return;
		}

		IRunnableWithProgress op = new IRunnableWithProgress() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
				doRemoveProject(monitor);
			}
		};

		try {
			getContainer().run(true, true, new WorkspaceModifyDelegatingOperation(op));
		} catch (InvocationTargetException e) {
			String title = NewWizardMessages.NewJavaProjectWizardPageTwo_error_remove_title;
			String message = NewWizardMessages.NewJavaProjectWizardPageTwo_error_remove_message;
			ExceptionHandler.handle(e, getShell(), title, message);
		} catch (InterruptedException e) {
			// cancel pressed
		}
	}

	private void doRemoveProject(IProgressMonitor monitor) throws InvocationTargetException {
		IProgressMonitor theMonitor = monitor;
		// Test if the project is inside the workspace
		final boolean noProgressMonitor = (this.fCurrProjectLocation == null);
		if (theMonitor == null || noProgressMonitor) {
			theMonitor = new NullProgressMonitor();
		}
		theMonitor.beginTask(NewWizardMessages.NewJavaProjectWizardPageTwo_operation_remove, 3);
		try {
			try {
				URI projLoc = this.fCurrProject.getLocationURI();

				boolean removeContent = !this.fKeepContent
						&& this.fCurrProject.isSynchronized(IResource.DEPTH_INFINITE);
				if (!removeContent) {
					restoreExistingFolders(projLoc);
				}
				this.fCurrProject.delete(removeContent, false, new SubProgressMonitor(theMonitor, 2));

				restoreExistingFiles(projLoc, new SubProgressMonitor(theMonitor, 1));
			} finally {
				// fIsAutobuild must be set
				CoreUtility.setAutoBuilding(this.fIsAutobuild.booleanValue());
				this.fIsAutobuild = null;
			}
		} catch (CoreException e) {
			throw new InvocationTargetException(e);
		} finally {
			theMonitor.done();
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

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class UpdateRunnable implements IRunnableWithProgress {
		private IStatus infoStatus = Status.OK_STATUS;

		public UpdateRunnable() {
			//
		}

		public IStatus getInfoStatus() {
			return this.infoStatus;
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
			try {
				if (NewSARLProjectWizardPageTwo.this.fIsAutobuild == null) {
					NewSARLProjectWizardPageTwo.this.fIsAutobuild =
							Boolean.valueOf(CoreUtility.setAutoBuilding(false));
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

}
