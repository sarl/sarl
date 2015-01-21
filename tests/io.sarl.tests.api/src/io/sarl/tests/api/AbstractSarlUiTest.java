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

package io.sarl.tests.api;

import io.sarl.lang.sarl.SarlScript;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil;
import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import com.google.inject.Inject;

/** This class is inspired from AbstractXbaseUITestCase of Xtext.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSarlUiTest extends AbstractSarlTest {

	/** This rule permits to create a project and clean the workspace.
	 */
	@Rule
	public TestWatcher sarlUiWatchter = new TestWatcher() {
		@Override
		protected void starting(Description description) {
			try {
				IResourcesSetupUtil.cleanWorkspace();
				SARLNatureNeededForTest annot = description.getAnnotation(SARLNatureNeededForTest.class);
				boolean isSARL = (annot != null);
				String[] classpath = WorkspaceTestHelper.DEFAULT_REQUIRED_BUNDLES;
				if (isSARL) {
					assert (annot != null);
					classpath = merge(classpath, annot.moreBundles());
				}
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
				WorkspaceTestHelper.createProjectWithDependencies(WorkspaceTestHelper.TESTPROJECT_NAME, isSARL, classpath);
			} catch (CoreException e) {
				throw new RuntimeException(e);
			}
			WorkspaceTestHelper.bind(AbstractSarlUiTest.this);
		}
		@Override
		protected void finished(Description description) {
			try {
				IResourcesSetupUtil.cleanWorkspace();
			} catch (CoreException e) {
				throw new RuntimeException(e);
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
	public SarlScript parseWithProjectClasspath(Object... code) throws Exception {
		return this.helper.createSARLScript(
				pathStr("io","sarl","mypackage","test"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				multilineString(code));
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
