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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import io.sarl.lang.SARLUiInjectorProvider;

import java.util.regex.Pattern;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementImageProvider;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil;
import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import com.google.inject.Inject;
import com.google.inject.Injector;

/** This class is inspired from AbstractXbaseUITestCase of Xtext.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@InjectWith(SARLUiInjectorProvider.class)
public abstract class AbstractSarlUiTest extends AbstractSarlTest {

	@Inject
	private Injector injector;

	/** This rule permits to create a project and clean the workspace.
	 */
	@Rule
	public TestWatcher sarlUiWatchter = new TestWatcher() {
		@SuppressWarnings("synthetic-access")
		@Override
		protected void starting(Description description) {
			try {
				IResourcesSetupUtil.cleanWorkspace();
				String[] classpath = WorkspaceTestHelper.DEFAULT_REQUIRED_BUNDLES;
				TestClasspath annot2 = description.getAnnotation(TestClasspath.class);
				if (annot2 == null) {
					Class<?> type = description.getTestClass();
					while (type != null && annot2 == null) {
						annot2 = type.getAnnotation(TestClasspath.class);
						type = type.getEnclosingClass();
					}
				}
				if (annot2 != null) {
					classpath = merge(classpath, annot2.value());
				}
				AbstractSarlUiTest.this.helper.createProjectWithDependencies(
						AbstractSarlUiTest.this.injector,
						WorkspaceTestHelper.TESTPROJECT_NAME,
						classpath);
			} catch (CoreException e) {
				throw new RuntimeException(e);
			}
		}
		@Override
		protected void finished(Description description) {
			if (description.getAnnotation(CleanWorkspaceAfter.class) != null) {
				try {
					IResourcesSetupUtil.cleanWorkspace();
				} catch (CoreException e) {
					throw new RuntimeException(e);
				}
			}
		}
	};

	/** Helper for interaction with the Eclipse workbench.
	 */
	@Inject
	protected WorkspaceTestHelper helper;

	/** Merge two arrays.
	 *
	 * @param operand1 - the first array.
	 * @param operand2 - the second array.
	 * @return the merge.
	 */
	protected static String[] merge(String[] operand1, String[] operand2) {
		if (operand1 == null) {
			if (operand2 == null) {
				return new String[0];
			}
			return operand2;
		}
		if (operand2 == null) {
			return operand1;
		}
		String[] tab = new String[operand1.length + operand2.length];
		System.arraycopy(
				operand1, 0,
				tab, 0,
				operand1.length);
		System.arraycopy(
				operand2, 0,
				tab, operand1.length,
				operand2.length);
		return tab;
	}

	/** Create an instance of the given class.
	 *
	 * @param clazz - type of the instance to create.
	 * @return the instance.
	 */
	public <T> T get(Class<T> clazz) {
		return this.helper.newInstance(clazz);
	}

	/** Parse the given code with the current project classpath.
	 *
	 * @param code - the multiline code to parse.
	 * @return the parsed code tree.
	 * @throws Exception - when parsing cannot be done.
	 */
	public XtendFile parseWithProjectClasspath(Object... code) throws Exception {
		return this.helper.createSARLScript(
				pathStr("io","sarl","mypackage","test"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				multilineString(code));
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
		assertTrue("Image not found: " + filename //$NON-NLS-1$
				+ ". Actual: " + s, Pattern.matches(regex, s)); //$NON-NLS-1$
	}

	/** Assert the given image descriptor is for an image given by JDT.
	 *
	 * @param expected - the expected base image descriptor.
	 * @param expectedFlags - the additional expected flags.
	 * @param actual - the image descriptor to test.
	 */
	protected static void assertJdtImage(ImageDescriptor expected, int expectedFlags, ImageDescriptor actual) {
		assertNotNull(actual);
		assertTrue(actual instanceof JavaElementImageDescriptor);
		assertEquals("Not the same JDT image descriptor.", //$NON-NLS-1$
				expected.hashCode() | expectedFlags | JavaElementImageProvider.BIG_SIZE.hashCode(),
				actual.hashCode());
		assertEquals(expectedFlags, ((JavaElementImageDescriptor) actual).getAdronments());
	}

}
