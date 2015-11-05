/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jdt.annotation.Nullable;

import org.eclipse.xtext.ui.util.PluginProjectFactory;
import com.google.inject.Provider;
import com.google.inject.Binder;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.Singleton;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementImageProvider;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil;
import org.eclipse.xtext.junit4.ui.util.JavaProjectSetupUtil;
import org.eclipse.xtext.ui.XtextProjectHelper;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.JavaVersion;
import org.junit.ComparisonFailure;
import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import io.sarl.lang.SARLUiInjectorProvider;
import io.sarl.lang.sarl.SarlScript;
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
		
		@SuppressWarnings("synthetic-access")
		@Override
		protected void starting(Description description) {
			try {
				helper().setUp();
			} catch (Exception e) {
				throw new RuntimeException(e);
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
				buildPath = new String[WorkbenchTestHelper.DEFAULT_REQ_BUNDLES.size() + addedBundles.length];
				WorkbenchTestHelper.DEFAULT_REQ_BUNDLES.toArray(buildPath);
				for (int i = WorkbenchTestHelper.DEFAULT_REQ_BUNDLES.size(), j = 0;
						i < buildPath.length && j < addedBundles.length; ++i, ++j) {
					buildPath[i] = addedBundles[j];
				}
			} else {
				buildPath = null;
			}
			
			try {
				ProjectCreator creator = getInjector().getInstance(ProjectCreator.class);
				if (buildPath == null) {
					WorkbenchTestHelper.createPluginProject(WorkbenchTestHelper.TESTPROJECT_NAME, creator);
				} else {
					WorkbenchTestHelper.createPluginProject(WorkbenchTestHelper.TESTPROJECT_NAME, creator, buildPath);
				}
			} catch (CoreException e) {
				throw new RuntimeException(e);
			}
		}
		@Override
		protected void finished(Description description) {
			try {
				IResourcesSetupUtil.cleanWorkspace();
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
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
	
	/** Replies the injected injector.
	 *
	 * @return the injector.
	 */
	public final Injector getInjectedInjector() {
		return this.injectedInjector;
	}	
	
	/** Replies the injector.
	 *
	 * @return the injector.
	 */
	public Injector getInjector() {
		return getInjectedInjector().createChildInjector(new Module() {
			@Override
			public void configure(Binder binder) {
				binder.bind(ProjectCreator.class).to(JavaProjectCreator.class);
				binder.bind(JavaVersion.class).toProvider(new Provider<JavaVersion>() {
					@Override
					public JavaVersion get() {
						return null;
					}
				});
			}
		});
	}
	
	/** Assert the given image descriptor is for an image in a bundle.
	 *
	 * @param filename - the name of the image file.
	 * @param desc - the image descriptor to test.
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
	 * @param expected - the expected image descriptor.
	 * @param actual - the current image descriptor.
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
	 * @param filename - the name of the image file.
	 * @param desc - the image descriptor to test.
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
	 * @param expected - the expected base image descriptor.
	 * @param expectedFlags - the additional expected flags.
	 * @param actual - the image descriptor to test.
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

	/** Create an instance of class.
	 */
	@Override
	protected SarlScript file(String string, boolean validate) throws Exception {
		return helper().sarlScript(
				helper().generateFilename("io", "sarl", "tests", getClass().getSimpleName()),
				string);
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
	private static class JavaProjectCreator implements ProjectCreator {
		
		@Inject
		private Injector injector;

		@Inject
		@Nullable
		private JavaVersion javaVersion;
		
		@Override
		public Injector getInjector() {
			return this.injector;
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
			JavaProjectSetupUtil.addJreClasspathEntry(javaProject);
		}

		@Override
		public void addToClasspath(IJavaProject javaProject, IClasspathEntry newClassPathEntry)throws JavaModelException {
			JavaProjectSetupUtil.addToClasspath(javaProject, newClassPathEntry);
		}

	}

}
