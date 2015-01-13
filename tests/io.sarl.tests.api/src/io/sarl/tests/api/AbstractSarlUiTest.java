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
				WorkspaceTestHelper.createProject(WorkspaceTestHelper.TESTPROJECT_NAME, isSARLNatureAddedToProject());
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
	
	/** Build a path.
	 * 
	 * @param path - path elements.
	 * @return the path.
	 */
	protected static IPath path(String... path) {
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
	protected static String pathStr(String... path) {
		return path(path).toOSString();
	}

	/** Create an instance of the given class.
	 * 
	 * @param clazz - type of the instance to create.
	 * @return the instance.
	 */
	public <T> T get(Class<T> clazz) {
		return this.helper.newInstance(clazz);
	}

	/** Replies if the default project should have the SARL nature.
	 * 
	 * @return <code>true</code> if the SARL nature should be added to
	 * the default project.
	 */
	@SuppressWarnings("static-method")
	protected boolean isSARLNatureAddedToProject() {
		return false;
	}

}
