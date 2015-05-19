/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.ui.preferences.SARLPreferences;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.jar.Manifest;
import java.util.logging.Logger;

import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.osgi.service.datalocation.Location;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.internal.ErrorEditorPart;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil;
import org.eclipse.xtext.junit4.ui.util.JavaProjectSetupUtil;
import org.eclipse.xtext.resource.FileExtensionProvider;
import org.eclipse.xtext.ui.XtextProjectHelper;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.XtextEditorInfo;
import org.eclipse.xtext.ui.editor.utils.EditorUtils;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.ui.util.PluginProjectFactory;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.xbase.compiler.JavaVersion;
import org.eclipse.xtext.xbase.lib.Functions;
import org.osgi.framework.Bundle;

import com.google.common.collect.ImmutableList;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Singleton;

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
	private Injector injector;

	private boolean isLazyCreatedProject = false;

	/** Dispose the helper for the unit test.
	 * 
	 * @throws Exception
	 */
	public void tearDown() throws Exception {
		if (this.workbench.getActiveWorkbenchWindow() != null)
			this.workbench.getActiveWorkbenchWindow().getActivePage().closeAllEditors(false);
		new WorkspaceModifyOperation() {

			@SuppressWarnings("synthetic-access")
			@Override
			protected void execute(IProgressMonitor monitor) throws CoreException,
			InvocationTargetException, InterruptedException {
				for (IFile file : getFiles()) {
					try {
						file.delete(true, null);
					} catch (Exception exc) {
						throw new RuntimeException(exc);
					}
				}
				getFiles().clear();
				IFolder binFolder = getProject(false).getFolder("bin"); //$NON-NLS-1$
				if (binFolder.exists()) {
					for (IResource binMember : binFolder.members()) {
						try {
							binMember.delete(true, null);
						} catch (Exception exc) {
							throw new RuntimeException(exc);
						}
					}
				}
				if (WorkbenchTestHelper.this.isLazyCreatedProject) {
					deleteProject(getProject(false));
					WorkbenchTestHelper.this.isLazyCreatedProject = false;
				}
			}
		}.run(null);
		IResourcesSetupUtil.waitForAutoBuild();
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

	/** Replies the current project.
	 * 
	 * @param createOnDemand - create the project if it does not exist yet.
	 * @return the project.
	 */
	protected IProject getProject(boolean createOnDemand) {
		IProject project = this.workspace.getRoot().getProject(TESTPROJECT_NAME);
		if (createOnDemand && !project.exists()) {
			try {
				this.isLazyCreatedProject = true;
				project = createPluginProject(this.injector, TESTPROJECT_NAME);
			} catch (CoreException e) {
				throw new RuntimeException(e);
			}
		}
		return project;
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
	 * see {@link #getFullFileName(String)}
	 */
	public IFile createFile(String fileName, String content) throws Exception {
		String fullFileName = getFullFileName(fileName);
		return createFileImpl(fullFileName, content);
	}

	/** Create a file.
	 * 
	 * @param fullFileName the name of the file to create.
	 * @param content the content of the file.
	 * @return the file.
	 * @throws Exception
	 */
	public IFile createFileImpl(String fullFileName, String content) throws Exception {
		IFile file = IResourcesSetupUtil.createFile(fullFileName, content);
		getFiles().add(file);
		return file;
	}

	/** Replies the file with the given name.
	 * 
	 * @param fileName the name without the file extension.
	 * @return the file.
	 */
	public IFile getFile(String fileName) {
		return this.workspace.getRoot().getFile(new Path(getFullFileName(fileName)));
	}

	/** Compute the full filename.
	 * 
	 * @param fileName the name without the file extension.
	 * @return the full filename.
	 */
	public String getFullFileName(String fileName) {
		String extension = (fileName.indexOf(".") != -1) ? "" : "." + getFileExtension(); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
		String fullFileName = getProject().getName() + "/src/" + fileName + extension; //$NON-NLS-1$
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
		return this.resourceSetProvider.get(project).createResource(uri(file));
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
		T latest = null;
		for (XtendTypeDeclaration declaration : script.getXtendTypes()) {
			if (type.isInstance(declaration)) {
				latest = type.cast(declaration);
			}
		}
		if (latest != null) {
			return latest;
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
	 * @param injector the injector.
	 * @param name the name of the project.
	 * @return the project.
	 * @throws CoreException
	 */
	public static IProject createPluginProject(Injector injector, String name) throws CoreException {
		return createPluginProject(injector, name, DEFAULT_REQ_BUNDLES.toArray(new String[DEFAULT_REQ_BUNDLES.size()]));
	}

	/** Create a project.
	 *
	 * @param injector the injector.
	 * @param name the name of the project.
	 * @param javaVersion the version of the Java to consider.
	 * @return the project.
	 * @throws CoreException
	 */
	public static IProject createPluginProject(Injector injector, String name, JavaVersion javaVersion) throws CoreException {
		return createPluginProject(injector, name, javaVersion, DEFAULT_REQ_BUNDLES.toArray(new String[DEFAULT_REQ_BUNDLES.size()]));
	}

	/** Create a project.
	 *
	 * @param injector the injector.
	 * @param name the name of the project.
	 * @param requiredBundles the bundles to add into the build path.
	 * @return the project.
	 * @throws CoreException
	 */
	public static IProject createPluginProject(Injector injector, String name, String... requiredBundles) throws CoreException {
		return createPluginProject(injector, name, null, requiredBundles);
	}

	/** Create a project.
	 *
	 * @param injector the injector.
	 * @param name the name of the project.
	 * @param javaVersion the version of the Java to consider.
	 * @param requiredBundles the bundles to add into the build path.
	 * @return the project.
	 * @throws CoreException
	 */
	public static IProject createPluginProject(Injector injector, String name, JavaVersion javaVersion, String... requiredBundles) throws CoreException {
		PluginProjectFactory projectFactory = injector.getInstance(PluginProjectFactory.class);
		projectFactory.setProjectName(name);
		JavaVersion jVersion = javaVersion;
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
		projectFactory.addFolders(Arrays.asList("src", "src-gen")); //$NON-NLS-1$ //$NON-NLS-2$
		IPath srcGenFolder = Path.fromPortableString("src-gen"); //$NON-NLS-1$
		projectFactory.addBuilderIds(
				XtextProjectHelper.BUILDER_ID,
				JavaCore.BUILDER_ID);
		projectFactory.addProjectNatures(
				XtextProjectHelper.NATURE_ID,
				JavaCore.NATURE_ID,
				NATURE_ID);
		IProject result = projectFactory.createProject(new NullProgressMonitor(), null);
		IJavaProject javaProject = JavaCore.create(result);
		makeCompliantFor(javaProject, jVersion);
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
			if (bundlePath == null) {
				throw new RuntimeException("Reference library not found: " + bundleName); //$NON-NLS-1$
			}

			IPath binPath = computeBinaryPathForBundle(bundlePath, workspaceRoot);
			if (binPath == null) {
				throw new RuntimeException("Reference library not found: " + bundleName); //$NON-NLS-1$
			}

			// Create the classpath entry
			IClasspathEntry classPathEntry = JavaCore.newLibraryEntry(
					binPath,
					null,
					null);
			JavaProjectSetupUtil.addToClasspath(javaProject, classPathEntry);
		}

		// Configure SARL source folder
		SARLPreferences.setSpecificSARLConfigurationFor(result, srcGenFolder);

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
		@SuppressWarnings("unchecked")
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
		IResourcesSetupUtil.waitForAutoBuild();
	}

	/** Do a full build.
	 * 
	 * @throws CoreException if the build cannot be done.
	 */
	public void fullBuild() throws CoreException {
		IResourcesSetupUtil.fullBuild();
	}

	/** Generate a filename for a resource that does not exist yet.
	 *
	 * @param pathElements - the elements of the path (directories and basename), without the extension.
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
	 * @param basename - the basename of the file in the source folder.
	 * @return <code>true</code> if the file exists, otherwise <code>false</code>.
	 */
	public boolean isFileInSourceFolder(String basename) {
		String fullFileName = getFullFileName(basename);
		return this.workspace.getRoot().exists(new Path(fullFileName));
	}

	/** Build a path.
	 *
	 * @param path - path elements.
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
	 * @param path - path elements.
	 * @return the path.
	 */
	public static String pathStr(String... path) {
		return path(path).toOSString();
	}

}
