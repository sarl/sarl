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

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementImageProvider;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.validation.Issue;
import org.junit.Assume;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.internal.AssumptionViolatedException;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
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
				WorkbenchTestHelper.createPluginProject(getInjector(), WorkbenchTestHelper.TESTPROJECT_NAME);
			} catch (CoreException e) {
				throw new RuntimeException(e);
			}
		}
		@Override
		protected void finished(Description description) {
			try {
				helper().tearDown();
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
			try {
				IResourcesSetupUtil.cleanWorkspace();
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
	};

	@Inject
	private Injector injector;

	/** Helper for interaction with the Eclipse workbench.
	 */
	@Inject
	private WorkbenchTestHelper helper;
	
	/** Replies the injector.
	 *
	 * @return the injector
	 */
	public Injector getInjector() {
		return injector;
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

	/** Create an instance of class.
	 */
	@Override
	protected XtendFile file(String string, boolean validate) throws Exception {
		return helper().sarlFile(
				helper().generateFilename("io", "sarl", "tests", getClass().getSimpleName()),
				string);
	}
	
	/** Replies the workspace test helper.
	 *
	 * @return the helper.
	 */
	protected WorkbenchTestHelper helper() {
		return this.helper;
	}

}
