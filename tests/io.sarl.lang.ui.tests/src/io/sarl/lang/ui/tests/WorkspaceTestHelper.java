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
package io.sarl.lang.ui.tests;

import static com.google.common.collect.Sets.newHashSet;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.TopElement;
import io.sarl.lang.ui.internal.SARLActivator;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Set;

import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil;
import org.eclipse.xtext.junit4.ui.util.JavaProjectSetupUtil;
import org.eclipse.xtext.resource.FileExtensionProvider;
import org.eclipse.xtext.ui.XtextProjectHelper;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.ui.util.JavaProjectFactory;
import org.eclipse.xtext.util.StringInputStream;
import org.junit.Assert;
import org.osgi.framework.Bundle;

import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Singleton;
import com.google.inject.name.Named;

/** Provides tools for setting up and managing the Eclipse workspace.
 * This class was adapted from the Xtext test suite.
 * 
 * @author $Author: sgalland$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class WorkspaceTestHelper extends Assert {

	/** Name of the Eclipse project in which the tests will be done.
	 */
	public static final String TESTPROJECT_NAME = "test.project"; //$NON-NLS-1$

	/** List of the Bundles that are required for testing.
	 */
	public static final String[] DEFAULT_REQUIRED_BUNDLES = new String[] {
			"com.google.inject", //$NON-NLS-1$
			"org.eclipse.xtext.xbase.lib", //$NON-NLS-1$
			"io.sarl.lang", //$NON-NLS-1$
			"io.sarl.lang.core", //$NON-NLS-1$
			"io.sarl.lang.ui"}; //$NON-NLS-1$
	
	/** Relative path of the source folder.
	 */
	public static String SOURCE_FOLDER = "src"; //$NON-NLS-1$
	
	private Set<IFile> files = newHashSet();

	@Inject
	@Named(Constants.LANGUAGE_NAME)
	private String languageName;

	@Inject
	private Injector injector;

	@Inject
	private FileExtensionProvider fileExtensionProvider;

	@Inject
	private IWorkspace workspace;

	@Inject
	private IResourceSetProvider resourceSetProvider;
	
	/** Replies the SARL injector.
	 * 
	 * @return the injector.
	 */
	private static Injector getSARLInjector() {
		return SARLActivator.getInstance().getInjector(SARLActivator.IO_SARL_LANG_SARL);
	}

	/** Replies the instance of the workbench test helper.
	 * 
	 * @param bindableObject - the object to bind.
	 */
	public static void bind(Object bindableObject) {
		Injector injector = getSARLInjector();
		if (bindableObject != null) {
			injector.injectMembers(bindableObject);
		}
	}
	
	/** Create a project in the workspace with the dependencies
	 * defined in {@link #DEFAULT_REQUIRED_BUNDLES}.
	 * 
	 * @param name - the name of the project.
	 * @return the project.
	 * @throws CoreException
	 * @see #createProjectWithDependencies(String, String...)
	 */
	public static IProject createProject(String name) throws CoreException {
		return createProjectWithDependencies(name, DEFAULT_REQUIRED_BUNDLES);
	}

	/** Create a project in the workspace with the given dependencies.
	 * 
	 * @param name - the name of the project.
	 * @param requiredBundles - the bundles required by the project. 
	 * @return the project.
	 * @throws CoreException
	 * @see #createProject(String)
	 * @see #DEFAULT_REQUIRED_BUNDLES
	 */
	public static IProject createProjectWithDependencies(String name, String... requiredBundles) throws CoreException {
		Injector injector = getSARLInjector();
		JavaProjectFactory projectFactory = injector.getInstance(JavaProjectFactory.class);
		projectFactory.setProjectName(name);
		projectFactory.addFolders(Collections.singletonList(SOURCE_FOLDER));
		projectFactory.addBuilderIds(
				XtextProjectHelper.BUILDER_ID,
				JavaCore.BUILDER_ID);
		projectFactory.addProjectNatures(
				XtextProjectHelper.NATURE_ID,
				JavaCore.NATURE_ID);
		
		IProject result = projectFactory.createProject(new NullProgressMonitor(), null);
		IJavaProject javaProject = JavaCore.create(result);
		JavaProjectSetupUtil.makeJava5Compliant(javaProject);
		JavaProjectSetupUtil.addJreClasspathEntry(javaProject);
		
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
		for(String bundleName : requiredBundles) {
			Bundle bundle = Platform.getBundle(bundleName);
			if (bundle == null) {
				throw new RuntimeException("Reference library not found: " + bundleName); //$NON-NLS-1$
			}

			IPath bundlePath = computeBundlePath(bundle, workspaceRoot, platformLocation);
			IPath binPath = computeBinaryPathForBundle(bundlePath, workspaceRoot);

			// Create the classpath entry
			IClasspathEntry classPathEntry = JavaCore.newLibraryEntry(
					binPath,
					null,
					null);
			JavaProjectSetupUtil.addToClasspath(javaProject, classPathEntry);
		}
		
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

	/** Delete the given project, if it is existing, and the related directories.
	 * 
	 * @param project - the project to delete.
	 * @throws CoreException
	 */
	public static void deleteProject(IProject project) throws CoreException {
		if (project != null && project.exists()) {
			project.delete(true, true, null);
		}
	}

	/** Replies the injector.
	 * 
	 * @return the injector.
	 */
	public Injector getInjector() {
		return this.injector;
	}

	/** Create an instance of the given class.
	 * 
	 * @param clazz - type of the instance to create.
	 * @return the instance.
	 */
	public <T> T newInstance(Class<T> clazz) {
		return getInjector().getInstance(clazz);
	}

	/** Replies the list of the created files.
	 * 
	 * @return the created files.
	 */
	public Set<IFile> getFiles() {
		return this.files;
	}

	/** Replies the testing project.
	 * If a project was not created, create
	 * a new one.s
	 * 
	 * @return the testing project.
	 */
	public IProject getProject() {
		return getProject(true);
	}

	/** Replies the testing project.
	 *
	 * @param createOnDemand - indicates if the project should be created
	 * if it was not already created.
	 * @return the testing project.
	 */
	protected IProject getProject(boolean createOnDemand) {
		IProject project = this.workspace.getRoot().getProject(TESTPROJECT_NAME);
		if (createOnDemand && !project.exists()) {
			try {
				project = createProject(TESTPROJECT_NAME);
			} catch (CoreException e) {
				throw new RuntimeException(e);
			}
		}
		return project;
	}

	/** Replies the identifier of the editors that are supporting the
	 * SARL language.
	 * 
	 * @return the identifier for the SARL editors.
	 */
	public String getEditorID() {
		return this.languageName;
	}

	/** Create a file inside the source folder.
	 * 
	 * @param basename - the basename of the file in the source folder.
	 * @param content - the content of the file.
	 * @return the created file.
	 * @throws Exception
	 */
	public IFile createFileInSourceFolder(String basename, String content) throws Exception {
		String fullFileName = convertBasenameToWorkspace(getProject(), basename);
		return createFile(fullFileName, content);
	}

	/** Create a file inside the workspace.
	 * 
	 * @param filename - the name of the file relative to the workspace directory.
	 * @param content - the content of the file.
	 * @return the created file.
	 * @throws Exception
	 */
	public IFile createFile(String filename, String content) throws Exception {
		IFile file = IResourcesSetupUtil.createFile(filename, content);
		getFiles().add(file);
		return file;
	}

	/** Replies if a file exists in the source folder.
	 * 
	 * @param basename - the basename of the file in the source folder.
	 * @return <code>true</code> if the file exists, otherwise <code>false</code>.
	 */
	public boolean isFileInSourceFolder(String basename) {
		String fullFileName = convertBasenameToWorkspace(getProject(), basename);
		return this.workspace.getRoot().exists(new Path(fullFileName));
	}

	/** Replies a file in the source folder.
	 * 
	 * @param basename - the basename of the file.
	 * @return the filename relative to the workspace directory.
	 */
	public IFile getFileInSourceFolder(String basename) {
		return this.workspace.getRoot().getFile(new Path(convertBasenameToWorkspace(getProject(), basename)));
	}

	/** Replies a file in the workspace.
	 * 
	 * @param filename - the filename relative to the workspace directory.
	 * @return the file.
	 */
	public IFile getFileInWorkspace(String filename) {
		return this.workspace.getRoot().getFile(new Path(filename));
	}

	/** Convert a filename from the source folder to the workspace.
	 * 
	 * @param project - project that is containing the source folder.
	 * @param basename - the filename without extension relative to the source folder.
	 * @return the filename relative to the workspace.
	 */
	protected String convertBasenameToWorkspace(IProject project, String basename) {
		String extension = (basename.indexOf(".") != -1) ? "" : "." + getFileExtension(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		String fullFileName = project.getName() + File.separator + SOURCE_FOLDER
				+ File.separator + basename + extension;
		return fullFileName;
	}

	/** Replies the file extension for a SARL script.
	 * 
	 * @return the file extension for a SARL script.
	 */
	public String getFileExtension() {
		return this.fileExtensionProvider.getFileExtensions().iterator().next();
	}

	/** Replies the URI of trhe given file.
	 * 
	 * @param file - the file
	 * @return the URI of the file.
	 */
	@SuppressWarnings("static-method")
	public URI uri(IFile file) {
		return URI.createPlatformResourceURI(file.getFullPath().toString(), true);
	}

	/** Replies the set of resources inside the current project.
	 * 
	 * @return the set of resources.
	 */
	public ResourceSet getResourceSet() {
		return this.resourceSetProvider.get(getProject());
	}

	/** Replies the content of the given file.
	 * 
	 * @param file - the filename.
	 * @return the content of the file.
	 * @throws Exception
	 */
	@SuppressWarnings("static-method")
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

	/** Create and compile a SARL script in the source folder.
	 * 
	 * @param basename - the basename of the file in the source folder. 
	 * @param content - the content of the file.
	 * @return the parsed SARL script.
	 * @throws Exception
	 */
	public SarlScript createSARLScript(String basename, String content) throws Exception {
		IFile file = createFileInSourceFolder(basename, content);
		return createSARLScript(file, content);
	}

	/** Create and compile a SARL script in the source folder.
	 * 
	 * @param file - the file in the source folder. 
	 * @param content - the content of the file.
	 * @return the parsed SARL script.
	 * @throws Exception
	 */
	public SarlScript createSARLScript(IFile file, String content) throws Exception {
		Resource resource = getResourceSet().createResource(uri(file));
		try (StringInputStream s = new StringInputStream(content)) {
			resource.load(s, null);
			assertEquals(resource.getErrors().toString(), 0, resource.getErrors().size());
			SarlScript sarlScript = (SarlScript) resource.getContents().get(0);
			return sarlScript;
		}
	}

	/** Create and compile a SARL script in the source folder.
	 * 
	 * @param file - the file in the source folder. 
	 * @param content - the content of the file.
	 * @return the parsed SARL script.
	 * @throws Exception
	 */
	public Resource createSARLScriptResource(IFile file, String content) throws Exception {
		Resource resource = getResourceSet().createResource(uri(file));
		try (StringInputStream s = new StringInputStream(content)) {
			resource.load(s, null);
			assertEquals(resource.getErrors().toString(), 0, resource.getErrors().size());
			SarlScript sarlScript = (SarlScript) resource.getContents().get(0);
			assertNotNull(sarlScript);
			return resource;
		}
	}

	/** Create and compile a SARL script in the source folder,
	 * and reply the top element of the given type at the given
	 * position in the SARL script.
	 * 
	 * @param basename - the basename of the file in the source folder.
	 * @param type - the type of the element to reply.
	 * @param position -( the index of the top element to reply.
	 * @param content - the content of the file.
	 * @return the SARL top element.
	 * @throws NoSuchElementException if there is no element of the given type.
	 * @throws Exception
	 */
	public <T extends TopElement> T createSARLTopElement(String basename,
			Class<T> type, int position, String content) throws Exception {
		SarlScript script = createSARLScript(basename, content);
		EList<TopElement> list = script.getElements();
		if (list != null && position < list.size()) {
			TopElement topElement = list.get(position);
			if (type.isInstance(topElement)) {
				return type.cast(topElement);
			}
		}
		throw new NoSuchElementException();
	}

	/** Create and compile a SARL script in the source folder,
	 * and reply the first top element of the given type.
	 * 
	 * @param basename - the basename of the file in the source folder.
	 * @param type - the type of the element to reply 
	 * @param content - the content of the file.
	 * @return the SARL top element.
	 * @throws NoSuchElementException if there is no element of the given type.
	 * @throws Exception
	 */
	public <T extends TopElement> T createSARLTopElement(String basename,
			Class<T> type, String content) throws Exception {
		return createSARLTopElement(basename, type, 0, content);
	}

	/** Create and compile a SARL script in the source folder of the given project.
	 * 
	 * @param project - the project in which creating the file.
	 * @param basename - the basename of the file in the source folder. 
	 * @param content - the content of the file.
	 * @return the parsed SARL script.
	 * @throws Exception
	 */
	public SarlScript createSARLScript(IProject project, String basename, String content) throws Exception {
		IFile file = createFile(convertBasenameToWorkspace(project, basename), content);
		Resource resource = this.resourceSetProvider.get(project).createResource(uri(file));
		try (StringInputStream s = new StringInputStream(content)) {
			resource.load(s, null);
			assertEquals(resource.getErrors().toString(), 0, resource.getErrors().size());
			SarlScript sarlScript = (SarlScript) resource.getContents().get(0);
			return sarlScript;
		}
	}

}
