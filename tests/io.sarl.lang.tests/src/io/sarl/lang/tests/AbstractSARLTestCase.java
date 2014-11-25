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

package io.sarl.lang.tests;

import io.sarl.lang.sarl.SarlScript;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;

import com.google.inject.Inject;

/** Abstract implementation of a test case for SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSARLTestCase extends Assert {

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

	/**
	 * @throws Exception
	 */
	@Before
	public void setUp() throws Exception {
		WorkspaceTestHelper.bind(this);
	}
	
	/**
	 * @throws Exception
	 */
	@Before
	public void tearDown() throws Exception {
		//
	}
	
	/** Initialize the test case class.
	 * This implementation clean the workspace and create the testing project.
	 * 
	 * @throws Exception
	 */
	@BeforeClass
	public static void setUpProject() throws Exception {
		IResourcesSetupUtil.cleanWorkspace();
		WorkspaceTestHelper.createProject(WorkspaceTestHelper.TESTPROJECT_NAME);
	}

	/** Clean up after all the tests associated to this test case.
	 * This implementation is cleaning the workspace.
	 * 
	 * @throws Exception
	 */
	@AfterClass
	public static void tearDownProject() throws Exception {
		IResourcesSetupUtil.cleanWorkspace();
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
	 * @param text - the code to parse.
	 * @return the parsed code tree.
	 * @throws Exception - when parsing cannot be done.
	 */
	public SarlScript parseWithCP(CharSequence text) throws Exception {
		return this.helper.createSARLScript(
				pathStr("io","sarl","mypackage","test"),   //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$ //$NON-NLS-4$
				text == null ? null : text.toString());
	}

}
