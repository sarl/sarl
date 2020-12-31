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

package io.sarl.lang.ui.tests.refactory.rename;

import static org.junit.Assert.assertTrue;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.ui.refactoring.RenameSupport;
import org.eclipse.ui.internal.Workbench;
import org.junit.Before;

import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.Nullable;
import io.sarl.tests.api.WorkbenchTestHelper;

/** Abstract class for creating tests of renaming features.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractFileBasedRenamingTest extends AbstractSarlUiTest {

	/** The file name of the source code (without extension).
	 */
	@Nullable
	protected String filename;

	/** The file name of the expected code (without extension).
	 */
	@Nullable
	protected String newFilename;

	/** The file of the source code.
	 */
	@Nullable
	protected IFile file;

	/** The source code.
	 */
	@Nullable
	protected String source;

	/** The expected code.
	 */
	@Nullable
	protected String expected;

	/** Replies the SARL source code to test.
	 *
	 * @return the code.
	 */
	protected abstract String getFullSourceCode();

	/** Replies the SARL expected code after test.
	 *
	 * @return the code.
	 */
	protected abstract String getFullExpectedCode();

	/** Replies the package of the source code.
	 *
	 * @return the package.
	 */
	protected abstract String getSourcePackageName();

	/** Replies the package of the expected code.
	 *
	 * @return the package.
	 */
	protected abstract String getExpectedPackageName();

	/** Set up.
	 * @throws Exception
	 */
	@Before
	public void setUp() throws Exception {
		this.source = getFullSourceCode();
		this.expected = getFullExpectedCode();
		final String filename = getSourcePackageName() + ".unittest"; //$NON-NLS-1$
		this.filename = helper().generateFilename(filename.split("\\.")); //$NON-NLS-1$
		this.file = helper().createFile(this.filename, this.source);
		helper().openEditor(this.file);
		IPath path = WorkbenchTestHelper.path(getExpectedPackageName().split("\\.")); //$NON-NLS-1$
		String basename = this.file.getLocation().lastSegment();
		path = path.append(basename);
		this.newFilename = path.toOSString();
	}

	/** Validate the refactoring given by the support.
	 *
	 * @param support the support.
	 * @throws Exception
	 */
	protected void validateRefactoring(RenameSupport support) throws Exception {
		IStatus status = support.preCheck();
		assertTrue(status.isOK());

		support.perform(helper().getShell(), Workbench.getInstance().getActiveWorkbenchWindow());

		helper().awaitAutoBuild();

		String newContent = helper().getContents(helper().getFile(this.newFilename));
		assertEquals(this.expected, newContent);
	}

}
