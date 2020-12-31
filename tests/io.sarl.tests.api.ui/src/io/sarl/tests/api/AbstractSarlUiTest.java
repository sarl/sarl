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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.inject.Binder;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.Singleton;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementImageProvider;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.ui.XtextProjectHelper;
import org.eclipse.xtext.ui.util.JREContainerProvider;
import org.eclipse.xtext.ui.util.PluginProjectFactory;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.Strings;
import org.junit.ComparisonFailure;
import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.ui.tests.SARLUiInjectorProvider;
import io.sarl.tests.api.WorkbenchTestHelper.ProjectCreator;

/** This class is inspired from AbstractXbaseUITestCase of Xtext.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@InjectWith(SARLUiInjectorProvider.class)
@SuppressWarnings("all")
public abstract class AbstractSarlUiTest extends AbstractSarlTest {

	/** This rule permits to tear down the workbench helper.
	 */
	@Rule
	public TestWatcher rootSarlUiWatchter = new TestWatcher() {

		@Override
		protected void starting(Description description) {
			try {
				helper().setUp();
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
			TestProject projectAnnotation = description.getAnnotation(TestProject.class);
			if (projectAnnotation == null) {
				Class<?> type = description.getTestClass();
				while (projectAnnotation == null && type != null) {
					projectAnnotation = type.getAnnotation(TestProject.class);
					type = type.getDeclaringClass();
				}
			}

			TestClasspath classPathAnnotation = description.getAnnotation(TestClasspath.class);
			if (classPathAnnotation == null) {
				Class<?> type = description.getTestClass();
				while (classPathAnnotation == null && type != null) {
					classPathAnnotation = type.getAnnotation(TestClasspath.class);
					type = type.getDeclaringClass();
				}
			}

			String[] buildPath;
			if (classPathAnnotation != null) {
				String[] addedBundles = classPathAnnotation.value();
				if (classPathAnnotation.includeDefaultBundles()) {
					buildPath = new String[WorkbenchTestHelper.DEFAULT_REQ_BUNDLES.size() + addedBundles.length];
					WorkbenchTestHelper.DEFAULT_REQ_BUNDLES.toArray(buildPath);
					for (int i = WorkbenchTestHelper.DEFAULT_REQ_BUNDLES.size(), j = 0;
							i < buildPath.length && j < addedBundles.length; ++i, ++j) {
						buildPath[i] = addedBundles[j];
					}
				} else {
					buildPath = addedBundles;
				}
			} else {
				buildPath = null;
			}

			if (buildPath == null) {
				AbstractSarlUiTest.this.initialClasspath = new String[WorkbenchTestHelper.DEFAULT_REQ_BUNDLES.size()];
				WorkbenchTestHelper.DEFAULT_REQ_BUNDLES.toArray(AbstractSarlUiTest.this.initialClasspath);
			} else {
				AbstractSarlUiTest.this.initialClasspath = buildPath;
			}

			if (projectAnnotation != null && projectAnnotation.clearWorkspaceAtStartup()) {
				helper().clearWorkspace();
			}

			if (projectAnnotation == null || projectAnnotation.automaticProjectCreation()) {
				try {
					createDefaultTestProject(AbstractSarlUiTest.this.initialClasspath);
				} catch (CoreException e) {
					throw new RuntimeException(e);
				}
			}
		}
		@Override
		protected void finished(Description description) {
			helper().clearWorkspace();
			try {
				helper().tearDown();
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
	};

	@Inject
	private Injector injectedInjector;

	/** Helper for interaction with the Eclipse workbench.
	 */
	private WorkbenchTestHelper workbenchHelper;

	private volatile String[] initialClasspath;

	/** Replies the classpath that is specified by {@link TestClasspath}.
	 *
	 * @return the classpath.
	 * @since 0.3.0
	 */
	protected String[] getTestClasspath() {
		return this.initialClasspath;
	}

	/** Create the default test project with the given classpath.
	 *
	 * @param classpath the bundles on the classpath.
	 * @return the project.
	 * @throws CoreException if the project cannot be created.
	 * @since 0.3.0
	 */
	protected IProject createDefaultTestProject(String[] classpath) throws CoreException {
		return createDefaultTestProject(classpath, WorkbenchTestHelper.TESTPROJECT_NAME);
	}

	/** Create the default test project with the given classpath.
	 *
	 * @param classpath the bundles on the classpath.
	 * @param projectName the name of the project.
	 * @return the project.
	 * @throws CoreException if the project cannot be created.
	 * @since 0.3.0
	 */
	protected IProject createDefaultTestProject(String[] classpath, String projectName) throws CoreException {
		ProjectCreator creator = getInjector().getInstance(ProjectCreator.class);
		if (classpath == null) {
			return this.workbenchHelper.createPluginProject(projectName, creator);
		}
		return this.workbenchHelper.createPluginProject(projectName, creator, classpath);
	}

	/** Replies the injected injector.
	 *
	 * @return the injector.
	 */
	public final Injector getInjectedInjector() {
		return this.injectedInjector;
	}
	
	/** Replies the injection modules to be used.
	 *
	 * @return the injection modules.
	 */
	protected Module[] getInjectionModules() {
		return new Module[] {
				new Module() {
					@Override
					public void configure(Binder binder) {
						final JavaVersion version = JavaVersion.JAVA8;
						binder.bind(JavaVersion.class).toProvider(() -> version);
						binder.bind(ProjectCreator.class).toProvider(() -> {
							return new JavaProjectCreator(version);
						}).asEagerSingleton();
					}
				},	
		};
	}

	/** Replies the injector.
	 *
	 * @return the injector.
	 */
	public Injector getInjector() {
		return getInjectedInjector().createChildInjector(getInjectionModules());
	}

	/** Assert the given image descriptor is for an image in a bundle.
	 *
	 * @param filename the name of the image file.
	 * @param desc the image descriptor to test.
	 */
	protected static void assertBundleImage(String filename, ImageDescriptor desc) {
		assertNotNull(desc);
		String s = desc.toString();
		String regex = Pattern.quote("URLImageDescriptor(bundleentry://") //$NON-NLS-1$
				+ "[^/]+" //$NON-NLS-1$
				+ Pattern.quote("/icons/") //$NON-NLS-1$
				+ "([^/]+[/])*" //$NON-NLS-1$
				+ Pattern.quote(filename + ")"); //$NON-NLS-1$
		if (!Pattern.matches(regex, s)) {
			if (desc instanceof JavaElementImageDescriptor) {
				JavaElementImageDescriptor jeid = (JavaElementImageDescriptor) desc;
				try {
					Field field = JavaElementImageDescriptor.class.getDeclaredField("fBaseImage");
					boolean isAcc = field.isAccessible(); 
					field.setAccessible(true);
					try {
						ImageDescriptor id = (ImageDescriptor) field.get(jeid);
						s = id.toString();
						assertTrue("Invalid image: " + filename //$NON-NLS-1$
								+ ". Actual: " + s, Pattern.matches(regex, s)); //$NON-NLS-1$
					} finally {
						field.setAccessible(isAcc);
					}
				} catch (NoSuchFieldException | SecurityException | IllegalArgumentException | IllegalAccessException e) {
					fail("Invalid background image descriptor: " + jeid.getClass().getName());
				}
			}
		}
	}

	private static String getImage(ImageDescriptor d) throws Exception {
		String regex = Pattern.quote("URLImageDescriptor(bundleentry://") //$NON-NLS-1$
				+ "[^/]+" //$NON-NLS-1$
				+ Pattern.quote("/icons/") //$NON-NLS-1$
				+ "(?:[^/]+[/])*" //$NON-NLS-1$
				+ "(.+?)" //$NON-NLS-1$
				+ Pattern.quote(")"); //$NON-NLS-1$
		Pattern pattern = Pattern.compile(regex);
		if (d instanceof JavaElementImageDescriptor) {
			JavaElementImageDescriptor expectedDescriptor = (JavaElementImageDescriptor) d;
			Field field = JavaElementImageDescriptor.class.getDeclaredField("fBaseImage");
			boolean isAcc = field.isAccessible(); 
			field.setAccessible(true);
			try {
				ImageDescriptor id = (ImageDescriptor) field.get(expectedDescriptor);
				Matcher matcher = pattern.matcher(id.toString());
				if (matcher.find()) {
					return matcher.group(1);
				}
			} finally {
				field.setAccessible(isAcc);
			}
		}
		Matcher matcher = pattern.matcher(d.toString());
		if (matcher.find()) {
			return matcher.group(1);
		}
		return "";
	}

	/** Assert the given image descriptors are the equal.
	 *
	 * @param expected the expected image descriptor.
	 * @param actual the current image descriptor.
	 * @throws Exception if the test cannot be done.
	 */
	protected static void assertImageDescriptors(ImageDescriptor expected, ImageDescriptor actual) throws Exception {
		String expectedImage = getImage(expected);
		String actualImage = getImage(actual);
		if (!Strings.equal(expectedImage, actualImage)) {
			throw new ComparisonFailure("Not same image descriptors", expectedImage, actualImage);
		}
	}


	/** Assert the given image descriptor is for an image in the platform.
	 *
	 * @param filename the name of the image file.
	 * @param desc the image descriptor to test.
	 */
	protected static void assertPlaformImage(String filename, ImageDescriptor desc) {
		assertNotNull(desc);
		String s = desc.toString();
		String regex = Pattern.quote("URLImageDescriptor(platform:") //$NON-NLS-1$
				+ "([/][^/]+)*" //$NON-NLS-1$
				+ Pattern.quote("/icons/") //$NON-NLS-1$
				+ "([^/]+[/])*" //$NON-NLS-1$
				+ Pattern.quote(filename + ")"); //$NON-NLS-1$
		assertTrue("Image not found: " + filename //$NON-NLS-1$
				+ ". Actual: " + s, Pattern.matches(regex, s)); //$NON-NLS-1$
	}

	/** Assert the given image descriptor is for an image given by JDT.
	 *
	 * @param expected the expected base image descriptor.
	 * @param expectedFlags the additional expected flags.
	 * @param actual the image descriptor to test.
	 * @throws Exception if the test cannot be done.
	 */
	protected static void assertJdtImage(ImageDescriptor expected, int expectedFlags, ImageDescriptor actual) throws Exception {
		assertNotNull(actual);
		assertTrue(actual instanceof JavaElementImageDescriptor);
		assertImageDescriptors(expected, actual);
		assertEquals("Not the same flags", expectedFlags,
				((JavaElementImageDescriptor) actual).getAdronments());
		assertEquals("Not the same size.", JavaElementImageProvider.BIG_SIZE,
				((JavaElementImageDescriptor) actual).getImageSize());
	}

	@Override
	protected SarlScript file(String string, boolean validate) throws Exception {
		return helper().sarlScript(
				helper().generateFilename("io", "sarl", "tests", getClass().getSimpleName()),
				string,
				validate);
	}

	/** Replies the workspace test helper.
	 *
	 * @return the helper.
	 */
	protected synchronized WorkbenchTestHelper helper() {
		if (this.workbenchHelper == null) {
			this.workbenchHelper = getInjector().getInstance(WorkbenchTestHelper.class);
		}
		return this.workbenchHelper;
	}

	/** Factory of a Java project.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Singleton
	private class JavaProjectCreator implements ProjectCreator {

		@Nullable
		private final JavaVersion javaVersion;

		JavaProjectCreator(JavaVersion version) {
			this.javaVersion = version;
		}
		
		@Override
		public Injector getInjector() {
			return getInjectedInjector();
		}

		@Override
		public PluginProjectFactory getProjectFactory() {
			return getInjector().getInstance(PluginProjectFactory.class);
		}

		@Override
		public JavaVersion getJavaVersion() {
			return this.javaVersion;
		}

		@Override
		public List<String> getSourceFolders() {
			return Arrays.asList("src", "src-gen");  //$NON-NLS-1$//$NON-NLS-2$
		}

		@Override
		public String getGenerationFolder() {
			return "src-gen"; //$NON-NLS-1$
		}

		@Override
		public String[] getBuilderIds() {
			return new String[] {
					XtextProjectHelper.BUILDER_ID,
					JavaCore.BUILDER_ID
			};
		}

		@Override
		public String[] getNatures() {
			return new String[] {
					XtextProjectHelper.NATURE_ID,
					JavaCore.NATURE_ID,
					WorkbenchTestHelper.NATURE_ID
			};
		}

		@Override
		public void addJreClasspathEntry(IJavaProject javaProject) throws JavaModelException {
			IClasspathEntry existingJreContainerClasspathEntry = JREContainerProvider.getJREContainerEntry(javaProject);
			if (existingJreContainerClasspathEntry == null) {
				addToClasspath(javaProject, JREContainerProvider.getDefaultJREContainerEntry());
			}
		}

		@Override
		public void addToClasspath(IJavaProject javaProject, IClasspathEntry newClassPathEntry) throws JavaModelException {
			IClasspathEntry[] newClassPath;
			IClasspathEntry[] classPath = javaProject.getRawClasspath();
			for (IClasspathEntry classPathEntry : classPath) {
				if (classPathEntry.equals(newClassPathEntry)) {
					return;
				}
			}
			newClassPath = new IClasspathEntry[classPath.length + 1];
			System.arraycopy(classPath, 0, newClassPath, 1, classPath.length);
			newClassPath[0] = newClassPathEntry;
			javaProject.setRawClasspath(newClassPath, null);
			helper().awaitAutoBuild();
		}

		@Override
		public void addToClasspath(IJavaProject javaProject,
				boolean autobuild,
				Iterable<IClasspathEntry> newClassPathEntries) throws JavaModelException {
			List<IClasspathEntry> newClassPath = new ArrayList<>(Arrays.asList(javaProject.getRawClasspath()));
			for (IClasspathEntry classPathEntry : newClassPathEntries) {
				if (!newClassPath.contains(classPathEntry)) {
					newClassPath.add(classPathEntry);
				}
			}
			IClasspathEntry[] classPath = new IClasspathEntry[newClassPath.size()];
			newClassPath.toArray(classPath);
			javaProject.setRawClasspath(classPath, null);
			if (autobuild) {
				helper().awaitAutoBuild();
			}
		}

	}

}
