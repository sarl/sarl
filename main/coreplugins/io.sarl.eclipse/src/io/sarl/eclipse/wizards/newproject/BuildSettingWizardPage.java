/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

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
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.ui.dialogs.StatusInfo;
import org.eclipse.jdt.internal.ui.util.CoreUtility;
import org.eclipse.jdt.internal.ui.util.ExceptionHandler;
import org.eclipse.jdt.internal.ui.wizards.NewWizardMessages;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jdt.ui.wizards.JavaCapabilityConfigurationPage;
import org.eclipse.jdt.ui.wizards.NewJavaProjectWizardPageTwo;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.ui.actions.WorkspaceModifyDelegatingOperation;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.natures.SARLProjectConfigurator;
import io.sarl.eclipse.util.classpath.SarlClassPathDetector;
import io.sarl.lang.util.OutParameter;

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
@SuppressWarnings("checkstyle:classdataabstractioncoupling")
public class BuildSettingWizardPage extends JavaCapabilityConfigurationPage {

	private static final String FILENAME_PROJECT = ".project"; //$NON-NLS-1$

	private static final String FILENAME_CLASSPATH = ".classpath"; //$NON-NLS-1$

	private static final int FILE_COPY_BLOCK_SIZE = 8192;

	private static final int UPDATE_PROJECT_MONITORED_STEPS = 7;

	private final MainProjectWizardPage firstPage;

	/** Location of the current project.
	 * It is {@code null} if the location is a platform location.
	 */
	private URI currProjectLocation;

	private IProject currProject;

	private boolean keepContent;

	private File dotProjectBackup;

	private File dotClasspathBackup;

	private Boolean isAutobuild;

	private Map<String, IFileStore> orginalFolders;

	/**
	 * Constructor for the {@link NewJavaProjectWizardPageTwo}.
	 *
	 * @param mainPage the first page of the wizard
	 */
	public BuildSettingWizardPage(MainProjectWizardPage mainPage) {
		setPageComplete(false);

		this.firstPage = mainPage;
		this.currProjectLocation = null;
		this.currProject = null;
		this.keepContent = false;

		this.dotProjectBackup = null;
		this.dotClasspathBackup = null;
		this.isAutobuild = null;

		setTitle(Messages.SARLProjectNewWizard_3);
		setDescription(Messages.SARLProjectNewWizard_2);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(
				SARLEclipseConfig.NEW_PROJECT_WIZARD_DIALOG_IMAGE));
	}

	@Override
	protected final boolean useNewSourcePage() {
		return true;
	}

	@Override
	public void setVisible(boolean visible) {
		final boolean isShownFirstTime = visible && this.currProject == null;
		if (visible) {
			// Entering from the first page
			if (isShownFirstTime) {
				createProvisonalProject();
			}
		} else {
			// Leaving back to the first page
			if (getContainer().getCurrentPage() == this.firstPage) {
				removeProvisonalProject();
			}
		}
		super.setVisible(visible);
		if (isShownFirstTime) {
			setFocus();
		}
	}

	private static boolean hasExistingContent(URI realLocation) throws CoreException {
		final IFileStore file = EFS.getStore(realLocation);
		return file.fetchInfo().exists();
	}

	private IStatus changeToNewProject() {
		final UpdateRunnable op = new UpdateRunnable();
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
	 * @param event the exception.
	 */
	private void updateStatus(Throwable event) {
		Throwable cause = event;
		while (cause != null
				&& (!(cause instanceof CoreException))
				&& cause.getCause() != null
				&& cause.getCause() != cause) {
			cause = cause.getCause();
		}
		if (cause instanceof CoreException) {
			updateStatus(((CoreException) cause).getStatus());
		} else {
			final String message;
			if (cause != null) {
				message = cause.getLocalizedMessage();
			} else {
				message = event.getLocalizedMessage();
			}
			final IStatus status = new StatusInfo(IStatus.ERROR, message);
			updateStatus(status);
		}
	}

	private static URI getRealLocation(String projectName, URI location) {
		URI theLocation = location;
		// Test if the project is inside workspace
		if (theLocation == null) {
			try {
				final URI rootLocation = ResourcesPlugin.getWorkspace().getRoot().getLocationURI();
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

	@SuppressWarnings("checkstyle:npathcomplexity")
	private IStatus updateProject(IProgressMonitor monitor) throws CoreException, InterruptedException {
		final SubMonitor subMonitor = SubMonitor.convert(monitor, 7);
		IStatus result = StatusInfo.OK_STATUS;
		try {
			subMonitor.beginTask(
					NewWizardMessages.NewJavaProjectWizardPageTwo_operation_initialize,
					UPDATE_PROJECT_MONITORED_STEPS);
			if (subMonitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			final String projectName = this.firstPage.getProjectName();

			this.currProject = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
			this.currProjectLocation = this.firstPage.getProjectLocationURI();

			final URI realLocation = getRealLocation(projectName, this.currProjectLocation);
			this.keepContent = hasExistingContent(realLocation);

			if (subMonitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			if (this.keepContent) {
				rememberExistingFiles(realLocation);
				rememberExisitingFolders(realLocation);
			}

			if (subMonitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			try {
				createProject(this.currProject, this.currProjectLocation,
						subMonitor.newChild(2));
			} catch (CoreException e) {
				if (e.getStatus().getCode() == IResourceStatus.FAILED_READ_METADATA) {
					result = new StatusInfo(
							IStatus.INFO, MessageFormat.format(
									NewWizardMessages.NewJavaProjectWizardPageTwo_DeleteCorruptProjectFile_message,
									e.getLocalizedMessage()));

					deleteProjectFile(realLocation);
					if (this.currProject.exists()) {
						this.currProject.delete(true, null);
					}

					createProject(this.currProject, this.currProjectLocation, null);
				} else {
					throw e;
				}
			}

			if (subMonitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			initializeBuildPath(JavaCore.create(this.currProject), subMonitor.newChild(2));
			// Create the Java project to allow the use of the new source folder page
			configureJavaProject(subMonitor.newChild(3));
		} finally {
			subMonitor.done();
		}
		return result;
	}

	private void keepExistingBuildPath(IProject project, OutParameter<IClasspathEntry[]> classpath,
			OutParameter<IPath> outputLocation, IProgressMonitor monitor)
			throws CoreException {
		final SubMonitor subMonitor = SubMonitor.convert(monitor, 3);
		IClasspathEntry[] entries = null;
		IPath outLocation = null;
		if (!project.getFile(FILENAME_CLASSPATH).exists()) {
			// Determine the default values
			if (classpath != null) {
				final List<IClasspathEntry> cpEntries = new ArrayList<>();
				final Collection<IClasspathEntry> originalEntries = SARLProjectConfigurator.getDefaultSourceClassPathEntries(
								new Path(this.firstPage.getProjectName()).makeAbsolute());
				cpEntries.addAll(originalEntries);
				this.firstPage.putDefaultClasspathEntriesIn(cpEntries);
				if (!cpEntries.isEmpty()) {
					classpath.set(cpEntries.toArray(new IClasspathEntry[cpEntries.size()]));
				}
			}
			if (outputLocation != null) {
				outputLocation.set(this.firstPage.getOutputLocation());
			}
			// Override with the existing configuration
			final SarlClassPathDetector detector = new SarlClassPathDetector(
					this.currProject, this.firstPage, subMonitor.newChild(1));
			entries = detector.getClasspath();
			outLocation = detector.getOutputLocation();
			if (entries.length == 0) {
				entries = null;
			}
		}
		subMonitor.worked(2);
		if (classpath != null && entries != null) {
			classpath.set(entries);
		}
		if (outputLocation != null && outLocation != null) {
			outputLocation.set(outLocation);
		}
		subMonitor.done();
	}

	private void buildNewBuildPath(IProject project, OutParameter<IClasspathEntry[]> classpath,
			OutParameter<IPath> outputLocation, IProgressMonitor monitor) throws CoreException {
		final List<IClasspathEntry> cpEntries = new ArrayList<>();
		final IWorkspaceRoot root = project.getWorkspace().getRoot();

		final Collection<IClasspathEntry> originalEntries = SARLProjectConfigurator.getDefaultSourceClassPathEntries(
						new Path(this.firstPage.getProjectName()).makeAbsolute());
		final SubMonitor subMonitor = SubMonitor.convert(monitor, originalEntries.size() + 1);
		for (final IClasspathEntry sourceClasspathEntry : originalEntries) {
			final IPath path = sourceClasspathEntry.getPath();
			if (path.segmentCount() > 1) {
				final IFolder folder = root.getFolder(path);
				CoreUtility.createFolder(
						folder,
						true, true,
						subMonitor.newChild(1));
			}
			cpEntries.add(sourceClasspathEntry);
		}

		this.firstPage.putDefaultClasspathEntriesIn(cpEntries);

		final IClasspathEntry[] entries = cpEntries.toArray(new IClasspathEntry[cpEntries.size()]);

		final IPath outLocation = this.firstPage.getOutputLocation();
		if (outLocation.segmentCount() > 1) {
			final IFolder folder = root.getFolder(outLocation);
			CoreUtility.createDerivedFolder(
					folder,
					true, true,
					subMonitor.newChild(1));
		}

		if (classpath != null) {
			classpath.set(entries);
		}
		if (outputLocation != null) {
			outputLocation.set(outLocation);
		}
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
			final OutParameter<IClasspathEntry[]> entries = new OutParameter<>();
			final OutParameter<IPath> outputLocation = new OutParameter<>();
			final IProject project = javaProject.getProject();

			if (this.keepContent) {
				keepExistingBuildPath(project, entries, outputLocation, theMonitor);
			} else {
				buildNewBuildPath(project, entries, outputLocation, theMonitor);
			}
			if (theMonitor.isCanceled()) {
				throw new OperationCanceledException();
			}

			init(javaProject, outputLocation.get(), entries.get(), false);
		} catch (CoreException e) {
			throw e;
		} catch (Throwable e) {
			if (e.getCause() instanceof CoreException) {
				throw (CoreException) e.getCause();
			}
			final Throwable ee;
			if (e.getCause() != null && e.getCause() != e) {
				ee = e.getCause();
			} else {
				ee = e;
			}
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, ee));
		} finally {
			theMonitor.done();
		}
	}

	private static void deleteProjectFile(URI projectLocation) throws CoreException {
		final IFileStore file = EFS.getStore(projectLocation);
		if (file.fetchInfo().exists()) {
			final IFileStore projectFile = file.getChild(FILENAME_PROJECT);
			if (projectFile.fetchInfo().exists()) {
				projectFile.delete(EFS.NONE, null);
			}
		}
	}

	private void rememberExisitingFolders(URI projectLocation) {
		this.orginalFolders = new TreeMap<>();

		try {
			final IFileStore[] children = EFS.getStore(projectLocation).childStores(EFS.NONE, null);
			for (int i = 0; i < children.length; i++) {
				final IFileStore child = children[i];
				final IFileInfo info = child.fetchInfo();
				if (info.isDirectory() && info.exists() && !this.orginalFolders.containsKey(info.getName())) {
					this.orginalFolders.put(info.getName(), child);
				}
			}
		} catch (CoreException e) {
			SARLEclipsePlugin.getDefault().log(e);
		}
	}

	private void restoreExistingFolders(URI projectLocation) {
		final Map<String, IFileStore> foldersToKeep = new TreeMap<>(this.orginalFolders);
		// workaround for bug 319054: Eclipse deletes all files when I cancel
		// a project creation (symlink in project location path)
		for (final IFileStore originalFileStore : this.orginalFolders.values()) {
			try {
				final File localFile = originalFileStore.toLocalFile(EFS.NONE, null);
				if (localFile != null) {
					final File canonicalFile = localFile.getCanonicalFile();
					final IFileStore canonicalFileStore =
							originalFileStore.getFileSystem().fromLocalFile(canonicalFile);
					if (!originalFileStore.equals(canonicalFileStore)) {
						foldersToKeep.put(canonicalFileStore.fetchInfo().getName(), canonicalFileStore);
					}
				}
			} catch (IOException e) {
				//
			} catch (CoreException e) {
				//
			}
		}

		try {
			final IFileStore[] children = EFS.getStore(projectLocation).childStores(EFS.NONE, null);
			for (int i = 0; i < children.length; i++) {
				final IFileStore child = children[i];
				final IFileInfo info = child.fetchInfo();
				if (info.isDirectory() && info.exists() && !foldersToKeep.containsKey(info.getName())) {
					child.delete(EFS.NONE, null);
					this.orginalFolders.remove(info.getName());
				}
			}

			for (Iterator<IFileStore> iterator = this.orginalFolders.values().iterator(); iterator.hasNext();) {
				final IFileStore deleted = iterator.next();
				deleted.mkdir(EFS.NONE, null);
			}
		} catch (CoreException e) {
			SARLEclipsePlugin.getDefault().log(e);
		}
	}

	private void rememberExistingFiles(URI projectLocation) throws CoreException {
		this.dotProjectBackup = null;
		this.dotClasspathBackup = null;

		final IFileStore file = EFS.getStore(projectLocation);
		if (file.fetchInfo().exists()) {
			final IFileStore projectFile = file.getChild(FILENAME_PROJECT);
			if (projectFile.fetchInfo().exists()) {
				this.dotProjectBackup = createBackup(projectFile, "project-desc"); //$NON-NLS-1$
			}
			final IFileStore classpathFile = file.getChild(FILENAME_CLASSPATH);
			if (classpathFile.fetchInfo().exists()) {
				this.dotClasspathBackup = createBackup(classpathFile, "classpath-desc"); //$NON-NLS-1$
			}
		}
	}

	private void restoreExistingFiles(URI projectLocation, IProgressMonitor monitor) throws CoreException {
		final SubMonitor subMonitor = SubMonitor.convert(monitor, 4);
		final int ticks = ((this.dotProjectBackup != null ? 1 : 0) + (this.dotClasspathBackup != null ? 1 : 0)) * 2;
		monitor.beginTask("", ticks); //$NON-NLS-1$
		try {
			final IFileStore projectFile = EFS.getStore(projectLocation).getChild(FILENAME_PROJECT);
			projectFile.delete(EFS.NONE, subMonitor.newChild(1));
			if (this.dotProjectBackup != null) {
				copyFile(this.dotProjectBackup, projectFile, subMonitor.newChild(1));
			}
		} catch (IOException e) {
			final IStatus status = new Status(
					IStatus.ERROR,
					JavaUI.ID_PLUGIN,
					IStatus.ERROR,
					NewWizardMessages.NewJavaProjectWizardPageTwo_problem_restore_project,
					e);
			throw new CoreException(status);
		}
		try {
			final IFileStore classpathFile = EFS.getStore(projectLocation).getChild(FILENAME_CLASSPATH);
			classpathFile.delete(EFS.NONE, subMonitor.newChild(1));
			if (this.dotClasspathBackup != null) {
				copyFile(this.dotClasspathBackup, classpathFile, subMonitor.newChild(1));
			}
		} catch (IOException e) {
			final IStatus status = new Status(
					IStatus.ERROR,
					JavaUI.ID_PLUGIN,
					IStatus.ERROR,
					NewWizardMessages.NewJavaProjectWizardPageTwo_problem_restore_classpath,
					e);
			throw new CoreException(status);
		}
		subMonitor.done();
	}

	private static File createBackup(IFileStore source, String name) throws CoreException {
		try {
			final File bak = File.createTempFile("eclipse-" + name, ".bak"); //$NON-NLS-1$//$NON-NLS-2$
			copyFile(source, bak);
			return bak;
		} catch (IOException e) {
			final IStatus status = new Status(
					IStatus.ERROR,
					JavaUI.ID_PLUGIN,
					IStatus.ERROR,
					MessageFormat.format(
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
		final byte[] buffer = new byte[FILE_COPY_BLOCK_SIZE];
		int bytesRead = is.read(buffer);
		while (bytesRead > 0) {
			os.write(buffer, 0, bytesRead);
			bytesRead = is.read(buffer);
		}
		os.flush();
	}

	/**
	 * Called from the wizard on finish.
	 *
	 * @param monitor the progress monitor
	 * @throws CoreException thrown when the project creation or configuration failed
	 * @throws InterruptedException thrown when the user canceled the project creation
	 */
	public void performFinish(IProgressMonitor monitor) throws CoreException, InterruptedException {
		final SubMonitor subMonitor = SubMonitor.convert(monitor, 4);
		try {
			monitor.beginTask(NewWizardMessages.NewJavaProjectWizardPageTwo_operation_create, 3);
			if (this.currProject == null) {
				updateProject(subMonitor.newChild(1));
			}

			final String newProjectCompliance = this.keepContent ? null : this.firstPage.getCompilerCompliance();
			configureJavaProject(newProjectCompliance, subMonitor.newChild(1));
		} catch (Throwable e) {
			if (this.currProject != null) {
				removeProvisonalProject();
			}
			throw e;
		} finally {
			subMonitor.done();
			this.currProject = null;
			if (this.isAutobuild != null) {
				CoreUtility.setAutoBuilding(this.isAutobuild.booleanValue());
				this.isAutobuild = null;
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
		final IStatus status = changeToNewProject();
		if (status != null) {
			updateStatus(status);
			if (!status.isOK()) {
				ErrorDialog.openError(
						getShell(),
						NewWizardMessages.NewJavaProjectWizardPageTwo_error_title,
						NewWizardMessages.NewJavaProjectWizardPageTwo_error_title,
						status);
			}
		}
		return this.currProject;
	}

	/**
	 * Removes the provisional project. The provisional project is typically
	 * removed when the user cancels the wizard or goes back to the first page.
	 */
	protected void removeProvisonalProject() {
		if (!this.currProject.exists()) {
			this.currProject = null;
			return;
		}

		final IRunnableWithProgress op = new IRunnableWithProgress() {
			@SuppressWarnings("synthetic-access")
			@Override
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

	private void doRemoveProject(IProgressMonitor monitor) throws InvocationTargetException {
		// Test if the project is inside the workspace
		final boolean noProgressMonitor = this.currProjectLocation == null;
		final SubMonitor subMonitor = SubMonitor.convert(noProgressMonitor ? null : monitor, 3);
		subMonitor.beginTask(NewWizardMessages.NewJavaProjectWizardPageTwo_operation_remove, 3);
		try {
			try {
				final URI projLoc = this.currProject.getLocationURI();

				final boolean removeContent = !this.keepContent
						&& this.currProject.isSynchronized(IResource.DEPTH_INFINITE);
				if (!removeContent) {
					restoreExistingFolders(projLoc);
				}
				this.currProject.delete(removeContent, false, subMonitor.newChild(2));

				restoreExistingFiles(projLoc, subMonitor.newChild(1));
			} finally {
				// fIsAutobuild must be set
				CoreUtility.setAutoBuilding(this.isAutobuild.booleanValue());
				this.isAutobuild = null;
			}
		} catch (CoreException e) {
			throw new InvocationTargetException(e);
		} finally {
			subMonitor.done();
			this.currProject = null;
			this.keepContent = false;
		}
	}

	/**
	 * Called from the wizard on cancel.
	 */
	public void performCancel() {
		if (this.currProject != null) {
			removeProvisonalProject();
		}
	}

	/** Task for updating the project.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class UpdateRunnable implements IRunnableWithProgress {
		private IStatus infoStatus = Status.OK_STATUS;

		UpdateRunnable() {
			//
		}

		public IStatus getInfoStatus() {
			return this.infoStatus;
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
			try {
				if (BuildSettingWizardPage.this.isAutobuild == null) {
					BuildSettingWizardPage.this.isAutobuild =
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
