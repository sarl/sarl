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
package io.sarl.tests.api;

import static com.google.common.collect.Sets.newHashSet;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.jar.Manifest;
import java.util.logging.Logger;

import com.google.common.base.Strings;
import com.google.common.collect.ImmutableList;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Singleton;
import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.internal.ErrorEditorPart;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.resource.FileExtensionProvider;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.XtextEditorInfo;
import org.eclipse.xtext.ui.editor.utils.EditorUtils;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.ui.util.PluginProjectFactory;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.xbase.lib.Functions;
import org.osgi.framework.Bundle;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.ui.preferences.SARLPreferences;

/** Provides tools for setting up and managing the Eclipse workspace.
 * This class was adapted from the Xtend test suite.
 *
 * @author $Author: sgalland$
 * @author $Author: ngaud$
 * @author Jan Koehnlein - Xtend contributor
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
@SuppressWarnings("static-method")
public class WorkbenchTestHelper {

	/** Logger.
	 */
	public static final Logger log = Logger.getLogger(WorkbenchTestHelper.class.getName());

	/** Name of the project that is generated during unit tests.
	 */
	public static final String TESTPROJECT_NAME = "test.project"; //$NON-NLS-1$

	/**
	 * ID of this nature.
	 */
	public static final String NATURE_ID = "io.sarl.eclipse.SARLProjectNature"; //$NON-NLS-1$

	/** List of the bundles that are required for created a testing project.
	 */
	public static final ImmutableList<String> DEFAULT_REQ_BUNDLES = ImmutableList.of(
			"com.google.inject", //$NON-NLS-1$
			"org.eclipse.xtend.lib", //$NON-NLS-1$
			"org.eclipse.xtext.xbase.lib", //$NON-NLS-1$
			"javax.inject", //$NON-NLS-1$
			"io.sarl.lang", //$NON-NLS-1$
			"io.sarl.lang.core", //$NON-NLS-1$
			"io.sarl.lang.ui", //$NON-NLS-1$
			"io.sarl.tests.testdata", //$NON-NLS-1$
			"org.junit"); //$NON-NLS-1$

	private Set<IFile> files = newHashSet();

	@Inject
	private XtextEditorInfo editorInfo;

	@Inject
	private FileExtensionProvider fileExtensionProvider;

	@Inject
	private IWorkbench workbench;

	@Inject
	private IWorkspace workspace;

	@Inject
	private IResourceSetProvider resourceSetProvider;
	
	@Inject
	private ProjectCreator projectCreator;

	private boolean isLazyCreatedProject = false;
	
	private String lastProjectName = null;

	/** Init the helper for the unit test.
	 * 
	 * @throws Exception
	 */
	public void setUp() throws Exception {
		//
	}
	
	/** Dispose the helper for the unit test.
	 * 
	 * @throws Exception
	 */
	public void tearDown() throws Exception {
		this.lastProjectName = null;
		// Close the editors safely.
		if (this.workbench != null) {
			try {
				final IWorkbenchWindow window = this.workbench.getActiveWorkbenchWindow();
				if (window != null) {
					window.getActivePage().closeAllEditors(false);
				}
			} catch (Throwable e) {
				//
			}
		}
		new WorkspaceModifyOperation() {

			@SuppressWarnings("synthetic-access")
			@Override
			protected void execute(IProgressMonitor monitor) throws CoreException,
			InvocationTargetException, InterruptedException {
				for (IFile file : getFiles()) {
					try {
						file.delete(true, null);
					} catch (Exception exc) {
						// Be silent because it is outside the scope of the tests
					}
				}
				getFiles().clear();
				IFolder binFolder = getProject(false).getFolder("bin"); //$NON-NLS-1$
				if (binFolder.exists()) {
					for (IResource binMember : binFolder.members()) {
						try {
							binMember.delete(true, null);
						} catch (Exception exc) {
							// Be silent because it is outside the scope of the tests
						}
					}
				}
				if (WorkbenchTestHelper.this.isLazyCreatedProject) {
					deleteProject(getProject(false));
					WorkbenchTestHelper.this.isLazyCreatedProject = false;
				}
			}

		}.run(null);
		awaitAutoBuild();
	}

	private static void deleteProjects(IProject[] projects) throws CoreException {
		for (IProject iProject : projects) {
			if (iProject.exists()) {
				iProject.delete(true,true, new NullProgressMonitor());
			}
		}
	}
	
	/** Clear the workspace.
	 */
	public void clearWorkspace() {
		try {
			IProject[] visibleProjects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
			deleteProjects(visibleProjects);
			IProject[] hiddenProjects = ResourcesPlugin.getWorkspace().getRoot().getProjects(org.eclipse.core.resources.IContainer.INCLUDE_HIDDEN);
			deleteProjects(hiddenProjects);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
	
	/** Replies the ccreated files.
	 *
	 * @return the created files.
	 */
	public Set<IFile> getFiles() {
		return this.files;
	}

	/** Replies the current project.
	 * 
	 * @return the project.
	 */
	public IProject getProject() {
		return getProject(true);
	}

	/** Replies the current Java project.
	 * 
	 * @return the Java project.
	 */
	public IJavaProject getJavaProject() {
		return JavaCore.create(getProject());
	}

	/** Replies the Java project for the given resource.
	 *
	 * @param resource the resource to search for.
	 * @return the Java project.
	 */
	public IJavaProject getJavaProject(Resource resource) {
		return JavaCore.create(getProject(resource, true));
	}

	/** Replies the current project.
	 * 
	 * @param createOnDemand create the project if it does not exist yet.
	 * @return the project.
	 */
	public IProject getProject(boolean createOnDemand) {
		return getProject(this.lastProjectName, createOnDemand);
	}

	/** Replies the project with the given name.
	 * 
	 * @param projectName the name of the project.
	 * @param createOnDemand create the project if it does not exist yet.
	 * @return the project.
	 */
	public IProject getProject(String projectName, boolean createOnDemand) {
		String prjName = projectName;
		if (Strings.isNullOrEmpty(prjName)) {
			prjName = TESTPROJECT_NAME;
		}
		IProject project = this.workspace.getRoot().getProject(prjName);
		if (createOnDemand && !project.exists()) {
			try {
				this.isLazyCreatedProject = true;
				project = createPluginProject(prjName, this.projectCreator);
			} catch (CoreException e) {
				throw new RuntimeException(e);
			}
		}
		return project;
	}

	/** Replies the project for the given resource.
	 * 
	 * @param resource the resource to search for.
	 * @param createOnDemand create the project if it does not exist yet.
	 * @return the project.
	 */
	public IProject getProject(Resource resource, boolean createOnDemand) {
		if (resource != null) {
			final URI uri = resource.getURI();
			final String platformString = uri.toPlatformString(true);
			final IPath resourcePath = new Path(platformString);
			final IFile file = this.workspace.getRoot().getFile(resourcePath);
			if (file != null) {
				final IProject project = file.getProject();
				if (project != null && project.exists()) {
					return project;
				}
			}
		}
		return getProject(createOnDemand);
	}

	/** Open the Xtext editor.
	 * 
	 * @param fileName the name of the file to open.
	 * @param content the content of the file.
	 * @return the editor.
	 * @throws Exception
	 */
	public XtextEditor openEditor(String fileName, String content) throws Exception {
		int cursor = content.indexOf('|');
		IFile file = createFile(fileName, content.replace("|", ""));  //$NON-NLS-1$//$NON-NLS-2$
		XtextEditor editor = openEditor(file);
		editor.getInternalSourceViewer().setSelectedRange(cursor, 0);
		editor.getInternalSourceViewer().getTextWidget().setFocus();
		return editor;
	}

	/** Create a file in the source folder.
	 * 
	 * @param fileName the name of the file to create.
	 * @param content the content of the file.
	 * @return the file.
	 * @throws Exception
	 * see {@link #getFullFileNameInSources(String)}
	 */
	public IFile createFile(String fileName, String content) throws Exception {
		String fullFileName = getFullFileNameInSources(fileName);
		return createFileImpl(fullFileName, content);
	}

	private static IProject createProject(IProject project) throws CoreException {
		if (!project.exists())
			project.create(new NullProgressMonitor());
		project.open(new NullProgressMonitor());
		return project;
	}

	/** Create the container tree.
	 *
	 * @param container the container
	 * @throws InvocationTargetException in case of error.
	 * @throws InterruptedException in case of errir.
	 */
	public static void create(org.eclipse.core.resources.IContainer container)
			throws InvocationTargetException, InterruptedException {
		new WorkspaceModifyOperation() {

			@SuppressWarnings("synthetic-access")
			@Override
			protected void execute(IProgressMonitor monitor)
					throws CoreException, InvocationTargetException,
					InterruptedException {
				if (!container.exists()) {
					create(container.getParent());
					if (container instanceof IFolder) {
						((IFolder) container).create(true, true, new NullProgressMonitor());
					} else {
						IProject iProject = (IProject) container;
						createProject(iProject);
					}
				}
			}
		}.run(new NullProgressMonitor());
	}

	/** Create a file.
	 * 
	 * @param fullFileName the name of the file to create.
	 * @param content the content of the file.
	 * @return the file.
	 * @throws Exception
	 */
	public IFile createFileImpl(String fullFileName, String content) throws Exception {
		final IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(fullFileName));
		new WorkspaceModifyOperation() {
			@Override
			protected void execute(IProgressMonitor monitor)
					throws CoreException, InvocationTargetException,
					InterruptedException {
				create(file.getParent());
				file.delete(true, new NullProgressMonitor());
				try (final InputStream is = new StringInputStream(content)) {
					file.create(is, true, new NullProgressMonitor());
				} catch (Exception exception) {
					//
				}
			}

		}.run(new NullProgressMonitor());
		getFiles().add(file);
		return file;
	}

	/** Create a file.
	 * 
	 * @param file the name of the file to create.
	 * @param content the content of the file.
	 * @return the file.
	 * @throws Exception
	 */
	public IFile createFileImpl(IFile file, InputStream content) throws Exception {
		new WorkspaceModifyOperation() {
			@Override
			protected void execute(IProgressMonitor monitor)
					throws CoreException, InvocationTargetException,
					InterruptedException {
				create(file.getParent());
				file.delete(true, new NullProgressMonitor());
				file.create(content, true, new NullProgressMonitor());
			}

		}.run(new NullProgressMonitor());
		getFiles().add(file);
		return file;
	}

	/** Replies the file with the given name.
	 * 
	 * @param fileName the name without the file extension.
	 * @return the file.
	 */
	public IFile getFile(String fileName) {
		return this.workspace.getRoot().getFile(new Path(getFullFileNameInSources(fileName)));
	}

	/** Compute the full filename.
	 * 
	 * @param fileName the name with the file extension.
	 * @return the full filename.
	 * @see #getFullFileNameInProject(String)
	 */
	public String getFullFileNameInSources(String fileName) {
		final String extension = (fileName.indexOf(".") != -1) ? "" : "." + getFileExtension(); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
		final String fullFileName = getProject().getName() + "/src/" + fileName + extension; //$NON-NLS-1$
		return fullFileName;
	}

	/** Replies the file with the given name.
	 * 
	 * @param fileName the name.
	 * @return the file.
	 */
	public IFile getFileInProject(String fileName) {
		return getFileInRoot(getProject().getName(), fileName);
	}

	/** Replies the file with the given name.
	 * 
	 * @param projectName the name of the project
	 * @param fileName the name.
	 * @return the file.
	 */
	public IFile getFileInRoot(String projectName, String fileName) {
		String fullFileName = projectName + "/" + fileName; //$NON-NLS-1$
		return getFile(new Path(fullFileName));
	}

	/** Replies the file with the given name.
	 * 
	 * @param path the name of the file.
	 * @return the file.
	 */
	public IFile getFile(IPath path) {
		return this.workspace.getRoot().getFile(path);
	}

	/** Replies the folder with the given name.
	 * 
	 * @param path the name of the folder.
	 * @return the folder.
	 */
	public IFolder getFolder(IPath path) {
		return this.workspace.getRoot().getFolder(path);
	}

	/** Compute the full filename inside the proejct root directory.
	 * 
	 * @param fileName the name without the file extension.
	 * @return the full filename.
	 * @see #getFullFileNameInSources(String)
	 */
	public String getFullFileNameInProject(String fileName) {
		String extension = (fileName.indexOf(".") != -1) ? "" : "." + getFileExtension(); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
		String fullFileName = getProject().getName() + "/" + fileName + extension; //$NON-NLS-1$
		return fullFileName;
	}

	/** Replies the file extension.
	 *
	 * @return the file extension.
	 */
	public String getFileExtension() {
		return this.fileExtensionProvider.getFileExtensions().iterator().next();
	}

	/** Compute the URI of the given file.
	 *
	 * @param file the file.
	 * @return the uri.
	 */
	public URI uri(IFile file) {
		return URI.createPlatformResourceURI(file.getFullPath().toString(), true);
	}

	/** Create a SARL file.
	 *
	 * @param fileName the filename.
	 * @param content the content.
	 * @return the SARL script.
	 * @throws Exception
	 */
	public SarlScript sarlScript(String fileName, String content) throws Exception {
		return sarlScript(fileName, content, true);
	}

	/** Create a SARL file.
	 *
	 * @param fileName the filename.
	 * @param content the content.
	 * @param assertNoError indicates if this function asserts no error in the content.
	 * If <code>true</code> this function may fail. Otherwise, it never fails on syntax errors.
	 * @return the SARL script.
	 * @throws Exception
	 */
	public SarlScript sarlScript(String fileName, String content, boolean assertNoError) throws Exception {
		IFile file = createFile(fileName, content);
		Resource resource = getResourceSet().createResource(uri(file));
		try (InputStream is = new StringInputStream(content)) {
			resource.load(is, null);
			if (assertNoError) {
				assertEquals(resource.getErrors().toString(), 0, resource.getErrors().size());
			}
			SarlScript xtendFile = (SarlScript) resource.getContents().get(0);
			return xtendFile;
		}
	}

	/** Create a SARL file.
	 *
	 * @param file the file.
	 * @return the SARL script.
	 * @throws Exception
	 */
	public SarlScript sarlScript(IFile file) throws Exception {
		Resource resource = getResourceSet().createResource(uri(file));
		resource.load(null);
		return (SarlScript) resource.getContents().get(0);
	}

	/** Create a SARL file.
	 *
	 * @param project the project.
	 * @param fileName the filename in the project.
	 * @param content the content.
	 * @return the SARL script.
	 * @throws Exception
	 */
	public SarlScript sarlScript(IProject project, String fileName, String content) throws Exception {
		IFile file = createFileImpl(project.getName() + "/src/" + fileName, content); //$NON-NLS-1$
		Resource resource = this.resourceSetProvider.get(project).createResource(uri(file));
		try (InputStream is = new StringInputStream(content)) {
			resource.load(is, null);
			assertEquals(resource.getErrors().toString(), 0, resource.getErrors().size());
			SarlScript xtendFile = (SarlScript) resource.getContents().get(0);
			return xtendFile;
		}
	}
	
	/** Replies the resource associated to the given file in the given project.
	 *
	 * @param file the file in the project.
	 * @return the resource.
	 */
	public Resource getResourceFor(IFile file) {
		return getResourceFor(getProject(), file);
	}

	/** Replies the resource associated to the given file in the given project.
	 *
	 * @param project the project.
	 * @param file the file in the project.
	 * @return the resource.
	 */
	public Resource getResourceFor(IProject project, IFile file) {
		URI uri = uri(file);
		ResourceSet resourceSet = this.resourceSetProvider.get(project);
		Resource resource = resourceSet.getResource(uri, false);
		if (resource == null) {
			resource = resourceSet.createResource(uri);
		}
		return resource;
	}

	/** Create a SARL file with a SARL agent inside.
	 *
	 * @param fileName the filename.
	 * @param type the type of the expected element.
	 * @param content the content.
	 * @return the SARL script.
	 * @throws Exception
	 */
	public <T extends XtendTypeDeclaration> T sarlTypeDeclaration(
			String fileName, Class<T> type,
			String content) throws Exception {
		SarlScript script = sarlScript(fileName, content);
		Object latest = null;
		for (XtendTypeDeclaration declaration : script.getXtendTypes()) {
			if (type.isInstance(declaration)) {
				latest = declaration;
			}
		}
		if (latest != null) {
			return type.cast(latest);
		}
		throw new NoSuchElementException();
	}

	/** Create a SARL file with a SARL agent inside.
	 *
	 * @param type the type of the expected element.
	 * @param content the content.
	 * @return the SARL script.
	 * @throws Exception
	 */
	public <T extends XtendTypeDeclaration> T sarlTypeDeclaration(
			Class<T> type,
			String content) throws Exception {
		return sarlTypeDeclaration(generateFilename(), type, content);
	}

	/** Replies the resource set for the testing project.
	 *
	 * @return the resource set.
	 */
	public ResourceSet getResourceSet() {
		return this.resourceSetProvider.get(getProject());
	}

	/** Replies the identifier of the editor.
	 *
	 * @return the identifier of the editor.
	 */
	public String getEditorID() {
		return this.editorInfo.getEditorId();
	}

	/** Replies the content of the given file.
	 *
	 * @param file the name of the file.
	 * @return the content of the file.
	 * @throws Exception
	 */
	public String getContents(IFile file) throws Exception {
		try (InputStream inputStream = file.getContents()) {
			byte[] buffer = new byte[2048];
			int bytesRead = 0;
			StringBuffer b = new StringBuffer();
			do {
				bytesRead = inputStream.read(buffer);
				if (bytesRead != -1)
					b.append(new String(buffer, 0, bytesRead));
			} while (bytesRead != -1);
			return b.toString();
		}
	}

	/** Close the welcome page.
	 *
	 * @throws InterruptedException
	 */
	public void closeWelcomePage() throws InterruptedException {
		if (PlatformUI.getWorkbench().getIntroManager().getIntro() != null) {
			PlatformUI.getWorkbench().getIntroManager().closeIntro(
					PlatformUI.getWorkbench().getIntroManager().getIntro());
		}
	}
	
	/** Create a project.
	 *
	 * @param name the name of the project.
	 * @param creator the creator of project.
	 * @param requiredBundles the bundles to add into the build path.
	 * @return the project.
	 * @throws CoreException
	 */
	public IProject createPluginProject(String name, ProjectCreator creator, String... requiredBundles) throws CoreException {
		// If no required bundle is given, use the defaults.
		String[] bundleDependencies;
		if (requiredBundles == null || requiredBundles.length == 0) {
			bundleDependencies = new String[WorkbenchTestHelper.DEFAULT_REQ_BUNDLES.size()];
			WorkbenchTestHelper.DEFAULT_REQ_BUNDLES.toArray(bundleDependencies);
		} else {
			bundleDependencies = requiredBundles;
		}
		
		// Disable "Build automatically"
		try {
			IWorkspace workspace = ResourcesPlugin.getWorkspace();
			IWorkspaceDescription workspaceDescription = workspace.getDescription();
			workspaceDescription.setAutoBuilding(false);
			workspace.setDescription(workspaceDescription);
		} catch (Throwable exception) {
			throw new CoreException(new Status(IStatus.ERROR, "io.sarl.tests.api", //$NON-NLS-1$
					exception.getLocalizedMessage(), exception));
		}
		
		
		PluginProjectFactory projectFactory = creator.getProjectFactory();
		projectFactory.setProjectName(name);
		this.lastProjectName = name;
		JavaVersion jVersion = creator.getJavaVersion();
		if (jVersion == null) {
			jVersion = JavaVersion.JAVA7;
		}
		String jseVersion;
		switch (jVersion) {
		case JAVA8:
			jseVersion = JavaCore.VERSION_1_8;
			break;
		case JAVA6:
			jseVersion = JavaCore.VERSION_1_6;
			break;
		case JAVA5:
			jseVersion = JavaCore.VERSION_1_5;
			break;
		case JAVA7:
		default:
			jseVersion = JavaCore.VERSION_1_7;
			break;
		}
		projectFactory.setBreeToUse("JavaSE-" + jseVersion); //$NON-NLS-1$
		projectFactory.addFolders(creator.getSourceFolders());
		IPath srcGenFolder = Path.fromPortableString(creator.getGenerationFolder());
		projectFactory.addBuilderIds(creator.getBuilderIds());
		projectFactory.addProjectNatures(creator.getNatures());
		IProject result = projectFactory.createProject(null, null);
		IJavaProject javaProject = JavaCore.create(result);
		makeCompliantFor(javaProject, jVersion);
		creator.addJreClasspathEntry(javaProject);

		IPath workspaceRoot;
		IPath platformLocation;

		try {
			workspaceRoot = ResourcesPlugin.getWorkspace().getRoot().getLocation();
			Location location = Platform.getInstallLocation();
			java.net.URI uri = org.eclipse.core.runtime.URIUtil.toURI(location.getURL());
			platformLocation = URIUtil.toPath(uri);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		
		// Retreive the bundles
		List<IClasspathEntry> classpathEntries = new ArrayList<>(bundleDependencies.length);
		for(String bundleName : bundleDependencies) {
			URL url;
			try {
				url = new URL(bundleName);
			} catch (Throwable exception) {
				url = null;
			}
			if (url != null) {
				URL newUrl;
				try {
					newUrl = FileLocator.toFileURL(url);
				} catch (Exception exception) {
					throw new RuntimeException("Reference library not found: " + url, exception); //$NON-NLS-1$
				}
				if (newUrl == null) {
					throw new RuntimeException("Reference library not found: " + url); //$NON-NLS-1$
				}
				
				IPath path = Path.fromPortableString(newUrl.getPath());

				IClasspathEntry classPathEntry = JavaCore.newLibraryEntry(
						path,
						null,
						null);
				classpathEntries.add(classPathEntry);
			} else {
				Bundle bundle = Platform.getBundle(bundleName);
				if (bundle == null) {
					throw new RuntimeException("Reference library not found: " + bundleName); //$NON-NLS-1$
				}
	
				IPath bundlePath = computeBundlePath(bundle, workspaceRoot, platformLocation);
				if (bundlePath == null) {
					throw new RuntimeException("Reference library not found: " + bundleName); //$NON-NLS-1$
				}
	
				IPath binPath = computeBinaryPathForBundle(bundlePath, workspaceRoot);
				if (binPath == null) {
					throw new RuntimeException("Reference library not found: " + bundleName); //$NON-NLS-1$
				}
	
				IClasspathEntry classPathEntry = JavaCore.newLibraryEntry(
						binPath,
						null,
						null);
				classpathEntries.add(classPathEntry);
			}
		}

		if (!classpathEntries.isEmpty()) {
			creator.addToClasspath(javaProject, false, classpathEntries);
		}

		// Configure SARL source folder
		SARLPreferences.setSpecificSARLConfigurationFor(result, srcGenFolder);

		// Enable "Build automatically"
		try {
			IWorkspace workspace = ResourcesPlugin.getWorkspace();
			IWorkspaceDescription workspaceDescription = workspace.getDescription();
			workspaceDescription.setAutoBuilding(true);
			workspace.setDescription(workspaceDescription);
		} catch (Throwable exception) {
			throw new CoreException(new Status(IStatus.ERROR, "io.sarl.tests.api", //$NON-NLS-1$
					exception.getLocalizedMessage(), exception));
		}
		
		awaitAutoBuild();
		
		return result;
	}
	
	private static IPath computeBundlePath(Bundle bundle, IPath workspaceRoot, IPath platformLocation) {
		IPath bundlePath;
		try {
			File file = FileLocator.getBundleFile(bundle);
			bundlePath = Path.fromOSString(file.getPath());
		} catch (IOException e1) {
			throw new RuntimeException(e1);
		}

		// Ensure that the bundle path is absolute (mandatory for beeing a classpath entry)
		if (!bundlePath.isAbsolute()) {
			IPath newBundlePath = workspaceRoot.append(bundlePath);
			if (!newBundlePath.toFile().exists()) {
				newBundlePath = platformLocation.append(bundlePath);
			}
			bundlePath = newBundlePath;
		}
		assert (bundlePath.isAbsolute()) : "The bundle path is not absolute: " + bundlePath; //$NON-NLS-1$
		return bundlePath;
	}

	private static IPath computeBinaryPathForBundle(IPath bundlePath, IPath workspaceRoot) {
		// Determine the path from the output folders of the Java projects in the current workspace.
		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(bundlePath.lastSegment());
		IPath newBundlePath = null;
		try {
			if (project != null && project.hasNature(JavaCore.NATURE_ID)) {
				IJavaProject javaProject = JavaCore.create(project);
				newBundlePath = javaProject.getOutputLocation();
				if (newBundlePath != null) {
					newBundlePath = workspaceRoot.append(newBundlePath);
					// Test if the bundle path exists
					if (newBundlePath != null && !newBundlePath.toFile().exists()) {
						newBundlePath = null;
					}
				}
			}
		} catch (Exception e) {
			// Ignore the exceptions since they are not useful (hopefully)
		}

		if (newBundlePath != null) {
			assert (newBundlePath.isAbsolute()) : "The bundle path is not absolute: " + newBundlePath; //$NON-NLS-1$
			return newBundlePath;
		}

		// Detect the binary folder in the bundle.
		//
		// TODO: Replace by a dynamic detection based on Jdt API.
		File localFile = bundlePath.toFile();
		File binFolder = new File(new File(localFile, "target"), "classes"); //$NON-NLS-1$//$NON-NLS-2$
		if (binFolder.exists()) {
			newBundlePath = bundlePath.append("target").append("classes"); //$NON-NLS-1$//$NON-NLS-2$
		} else {
			binFolder = new File(localFile, "bin"); //$NON-NLS-1$
			if (binFolder.exists()) {
				newBundlePath = bundlePath.append("bin"); //$NON-NLS-1$
			} else {
				newBundlePath = bundlePath;
			}
		}

		assert (newBundlePath.isAbsolute()) : "The bundle path is not absolute: " + bundlePath; //$NON-NLS-1$
		return newBundlePath;
	}

	/** Make the given project compliant for the given version.
	 *
	 * @param javaProject the project.
	 * @param javaVersion the Java version.
	 */
	public static void makeCompliantFor(IJavaProject javaProject, JavaVersion javaVersion) {
		Map<String, String> options = javaProject.getOptions(false);
		String jreLevel;
		switch (javaVersion) {
		case JAVA8:
			jreLevel = JavaCore.VERSION_1_8;
			break;
		case JAVA6:
			jreLevel = JavaCore.VERSION_1_6;
			break;
		case JAVA5:
			jreLevel = JavaCore.VERSION_1_5;
			break;
		case JAVA7:
		default:
			jreLevel = JavaCore.VERSION_1_7;
		}
		options.put(JavaCore.COMPILER_COMPLIANCE, jreLevel);
		options.put(JavaCore.COMPILER_SOURCE, jreLevel);
		options.put(JavaCore.COMPILER_CODEGEN_TARGET_PLATFORM, jreLevel);
		options.put(JavaCore.COMPILER_PB_ASSERT_IDENTIFIER, JavaCore.ERROR);
		options.put(JavaCore.COMPILER_PB_ENUM_IDENTIFIER, JavaCore.ERROR);
		options.put(JavaCore.COMPILER_CODEGEN_INLINE_JSR_BYTECODE, JavaCore.ENABLED);
		options.put(JavaCore.COMPILER_LOCAL_VARIABLE_ATTR, JavaCore.GENERATE);
		options.put(JavaCore.COMPILER_LINE_NUMBER_ATTR, JavaCore.GENERATE);
		options.put(JavaCore.COMPILER_SOURCE_FILE_ATTR, JavaCore.GENERATE);
		options.put(JavaCore.COMPILER_CODEGEN_UNUSED_LOCAL, JavaCore.PRESERVE);
		javaProject.setOptions(options);
	}

	/** Add exported packages into the project.
	 *
	 * @param project the project.
	 * @param exportedPackages the exported packages.
	 * @throws Exception
	 */
	public static void addExportedPackages(IProject project, String ... exportedPackages) throws Exception{
		IFile manifest = project.getFile("META-INF/MANIFEST.MF"); //$NON-NLS-1$
		Manifest mf = new Manifest(manifest.getContents());
		String value = mf.getMainAttributes().getValue("Export-Package"); //$NON-NLS-1$
		for (String exported : exportedPackages) {
			if (value == null) {
				value = exported;
			} else {
				value += ","+exported; //$NON-NLS-1$
			}
		}
		mf.getMainAttributes().putValue("Export-Package", value); //$NON-NLS-1$
		ByteArrayOutputStream stream = new ByteArrayOutputStream();
		mf.write(stream);
		manifest.setContents(new ByteArrayInputStream(stream.toByteArray()), true, true, null);
	}
	
	/** Add required plugin into the project.
	 *
	 * @param project the project.
	 * @param requiredBundles the required bundles.
	 * @throws Exception
	 */
	public static void addRequiredBundle(IProject project, Iterable<String> requiredBundles) throws Exception{
		IFile manifest = project.getFile("META-INF/MANIFEST.MF"); //$NON-NLS-1$
		Manifest mf = new Manifest(manifest.getContents());
		String value = mf.getMainAttributes().getValue("Required-Bundles"); //$NON-NLS-1$
		for (String required : requiredBundles) {
			if (value == null) {
				value = required;
			} else {
				value += ","+required; //$NON-NLS-1$
			}
		}
		mf.getMainAttributes().putValue("Required-Bundles", value); //$NON-NLS-1$
		ByteArrayOutputStream stream = new ByteArrayOutputStream();
		mf.write(stream);
		manifest.setContents(new ByteArrayInputStream(stream.toByteArray()), true, true, null);
	}

	/** Delete the given project.
	 *
	 * @param project the project to delete.
	 * @throws CoreException
	 */
	public static void deleteProject(IProject project) throws CoreException {
		if (project != null && project.exists()) {
			project.delete(true, true, null);
		}
	}

	/** Open the default editor for the given file.
	 *
	 * @param file the file to open.
	 * @return the editor.
	 * @throws Exception
	 */
	public XtextEditor openEditor(IFile file) throws Exception {
		IEditorPart openEditor = openEditor(file, getEditorID());
		XtextEditor xtextEditor = EditorUtils.getXtextEditor(openEditor);
		if (xtextEditor != null) {
			xtextEditor.selectAndReveal(0, 0);
			return xtextEditor;
		} else if (openEditor instanceof ErrorEditorPart) {
			Field field = openEditor.getClass().getDeclaredField("error"); //$NON-NLS-1$
			field.setAccessible(true);
			throw new IllegalStateException("Couldn't open the editor.", //$NON-NLS-1$
					((Status) field.get(openEditor)).getException());
		} else {
			fail("Opened Editor with id:" + getEditorID() + ", is not an XtextEditor"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return null;
	}

	/** Open the specific editor for the given file.
	 *
	 * @param file the file to open.
	 * @param editorId the identifier of the editor.
	 * @return the editor.
	 * @throws PartInitException
	 */
	public IEditorPart openEditor(IFile file, String editorId) throws PartInitException {
		return this.workbench.getActiveWorkbenchWindow().getActivePage().openEditor(new FileEditorInput(file), editorId);
	}

	/** Open the text editor for the given file.
	 *
	 * @param file the file to open.
	 * @return the editor.
	 * @throws PartInitException
	 */
	public ITextEditor openLikeTextEditor(IFile file) throws PartInitException {
		IEditorPart editor = IDE.openEditor(this.workbench.getActiveWorkbenchWindow().getActivePage(), file);
		if (editor instanceof ITextEditor) {
			return (ITextEditor) editor;
		}
		return null;
	}

	/** Close the editor.
	 *
	 * @param editor the editor.
	 * @param save indicates if the content must be saved before closing.
	 * @return success state.
	 */
	public boolean closeEditor(IEditorPart editor, boolean save) {
		return this.workbench.getActiveWorkbenchWindow().getActivePage().closeEditor(editor, save);
	}

	/** Close all the editors.
	 *
	 * @param save indicates if the contents must be saved before closing.
	 * @return success state.
	 */
	public boolean closeAllEditors(boolean save) {
		return this.workbench.getActiveWorkbenchWindow().getActivePage().closeAllEditors(save);
	}

	/** Save the content of the given editor.
	 *
	 * @param editor the editor.
	 * @param confirm indicates if the confirmation  box must be shown.
	 * @return success state.
	 */
	public boolean saveEditor(IEditorPart editor, boolean confirm) {
		return this.workbench.getActiveWorkbenchWindow().getActivePage().saveEditor(editor, confirm);
	}

	/**
	 * Wait for an update in the UI.
	 * 
	 * @param test
	 * 		tester function that returns true if the target state of the UI has been reached
	 * @param timeout
	 * 		the time after which waiting is canceled
	 */
	public void awaitUIUpdate(Functions.Function0<Boolean> test, final long timeout) {
		long startTime = System.currentTimeMillis();
		final Display display = Display.getCurrent();
		new Thread("Display alarm") { //$NON-NLS-1$
			@Override public void run() {
				try {
					Thread.sleep(timeout);
					display.wake();
				} catch (InterruptedException e) {
					//
				}
			}
		}.start();
		while (!test.apply() && System.currentTimeMillis() - startTime < timeout) {
			boolean hasWork = display.sleep();
			while (hasWork) {
				hasWork = display.readAndDispatch();
			}
		}
	}

	/** Wait for the end of auto-build.
	 */
	public void awaitAutoBuild() {
		boolean wasInterrupted = false;
		do {
			try {
				Job.getJobManager().join(ResourcesPlugin.FAMILY_AUTO_BUILD,
						null);
				wasInterrupted = false;
			} catch (OperationCanceledException e) {
				e.printStackTrace();
			} catch (InterruptedException e) {
				wasInterrupted = true;
			}
		} while (wasInterrupted);
	}

	/** Do a full build.
	 *
	 * @param resources the resources that should be build before launching ann Eclipse full build.
	 * @throws CoreException if the build cannot be done.
	 */
	public void fullBuild(Resource... resources) throws CoreException {
		if (resources != null) {
			for (final Resource resource : resources) {
				resource.getContents();
			}
		}
		ResourcesPlugin.getWorkspace().build(
				IncrementalProjectBuilder.FULL_BUILD, new NullProgressMonitor());
		boolean wasInterrupted = false;
		do {
			try {
				Job.getJobManager().join(ResourcesPlugin.FAMILY_MANUAL_BUILD,
						null);
				wasInterrupted = false;
			} catch (OperationCanceledException e) {
				e.printStackTrace();
			} catch (InterruptedException e) {
				wasInterrupted = true;
			}
		} while (wasInterrupted);
	}

	/** Generate a filename for a resource that does not exist yet.
	 *
	 * @param pathElements the elements of the path (directories and basename), without the extension.
	 * @return the filename.
	 */
	public String generateFilename(String... pathElements) {
		int filenameCounter = 0;
		String oFilename = pathStr(pathElements);
		String filename = oFilename;
		boolean foundFile = isFileInSourceFolder(filename);
		while (foundFile) {
			++filenameCounter;
			filename = oFilename + Integer.toString(filenameCounter);
			foundFile = isFileInSourceFolder(filename);
		}
		return filename;
	}

	/** Generate a filename for a resource that does not exist yet.
	 *
	 * @return the filename.
	 */
	public String generateFilename() {
		String p = getDefaultTestPackage() + ".unittest"; //$NON-NLS-1$
		return generateFilename(p.split("\\.")); //$NON-NLS-1$
	}

	/** Replies the default package name where unit test files are located.
	 * 
	 * @return the package name.
	 */
	public String getDefaultTestPackage() {
		return "io.sarl.tests"; //$NON-NLS-1$
	}

	/** Replies if a file exists in the source folder.
	 *
	 * @param basename the basename of the file in the source folder.
	 * @return <code>true</code> if the file exists, otherwise <code>false</code>.
	 */
	public boolean isFileInSourceFolder(String basename) {
		String fullFileName = getFullFileNameInSources(basename);
		return this.workspace.getRoot().exists(new Path(fullFileName));
	}

	/** Build a path.
	 *
	 * @param path path elements.
	 * @return the path.
	 */
	public static IPath path(String... path) {
		assert(path != null && path.length > 0);
		IPath p = new Path(path[0]);
		for(int i=1; i<path.length; ++i) {
			p = p.append(path[i]);
		}
		return p;
	}

	/** Build a path.
	 *
	 * @param path path elements.
	 * @return the path.
	 */
	public static String pathStr(String... path) {
		return path(path).toOSString();
	}

	/** Replies the Eclipse shell used by this workbench.
	 *
	 * @return the shell.
	 */
	public Shell getShell() {
		return this.workbench.getActiveWorkbenchWindow().getShell();
	}

	/** Factory of a project.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public interface ProjectCreator {
		
		/** Replies the injector.
		 * 
		 * @return the injector.
		 */
		Injector getInjector();
		
		/** Replies the project factory.
		 *
		 * @return the factory.
		 */
		PluginProjectFactory getProjectFactory();
		
		/** Replies the version of Java that is supported by the project.
		 *
		 * @return the java version.
		 */
		JavaVersion getJavaVersion();

		/** Replies the source folders.
		 *
		 * @return the source folders.
		 */
		List<String> getSourceFolders();
		
		/** Replies the folder in which the sources are generated.
		 *
		 * @return the generation folder.
		 */
		String getGenerationFolder();

		/** Replies the identifiers of the builders.
		 *
		 * @return the identifiers.
		 */
		String[] getBuilderIds();

		/** Replies the identifiers of the natures
		 *
		 * @return the identifiers.
		 */
		String[] getNatures();

		/** Add a JRE library to the class path.
		 *
		 * @param javaProject the proejct to update.
		 * @throws JavaModelException
		 */
		void addJreClasspathEntry(IJavaProject javaProject) throws JavaModelException;
		
		/** Add a library to the class path.
		 *
		 * @param javaProject the proejct to update.
		 * @param newClassPathEntry the entry to add.
		 * @throws JavaModelException
		 */
		void addToClasspath(IJavaProject javaProject, IClasspathEntry newClassPathEntry) throws JavaModelException;

		/** Add libraries to the class path.
		 *
		 * @param javaProject the proejct to update.
		 * @param autobuild indicates if the function should wait for end of autobuild.
		 * @param newClassPathEntry the entry to add.
		 * @throws JavaModelException
		 */
		void addToClasspath(IJavaProject javaProject,
				boolean autobuild,
				Iterable<IClasspathEntry> newClassPathEntry) throws JavaModelException;

	}

}
