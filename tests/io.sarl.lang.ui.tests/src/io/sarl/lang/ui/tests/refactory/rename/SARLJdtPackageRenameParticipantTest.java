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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.ui.refactoring.RenameSupport;
import org.eclipse.ui.internal.Workbench;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.Nullable;

/** Test the refactoring of the package from the JDT UI.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/584"
 */
@RunWith(Suite.class)
@SuiteClasses({
	SARLJdtPackageRenameParticipantTest.NoFileComment1.class,
	SARLJdtPackageRenameParticipantTest.NoFileComment2.class,
	SARLJdtPackageRenameParticipantTest.FileComment1.class,
	SARLJdtPackageRenameParticipantTest.FileComment2.class,
	SARLJdtPackageRenameParticipantTest.FileComment3.class,
	SARLJdtPackageRenameParticipantTest.FileComment4.class,
})
@SuppressWarnings("all")
public class SARLJdtPackageRenameParticipantTest {

	private static abstract class AbstractParticipantTest extends AbstractFileBasedRenamingTest {

		private static final String SARL_CODE = multilineString(
				"{0}package {1}",
				"import io.sarl.core.AgentKilled",
				"import io.sarl.core.AgentSpawned",
				"agent A {",
				"  on AgentKilled { }",
				"  on AgentSpawned { }",
				"}");

		private String doReplacement(String source, String packageName) {
			return source.replace("{0}", getFileComment()).replace("{1}", packageName);
		}

		protected abstract String getFileComment();

		@Override
		protected String getFullSourceCode() {
			return doReplacement(SARL_CODE, getSourcePackageName());
		}

		@Override
		protected String getFullExpectedCode() {
			return doReplacement(SARL_CODE, getExpectedPackageName());
		}

		@Test
		public void refactoring() throws Exception {
			IFolder folder = (IFolder) this.file.getParent();
			IJavaElement element = JavaCore.create(folder);
			assertNotNull(element);
			assertInstanceOf(IPackageFragment.class, element);
			IPackageFragment epackage = (IPackageFragment) element;

			RenameSupport support = RenameSupport.create(epackage, getExpectedPackageName(), RenameSupport.UPDATE_REFERENCES);

			validateRefactoring(support);
		}

	}

	public static class NoFileComment1 extends AbstractParticipantTest {

		private static final String SOURCE_PACKAGE_NAME = "io.sarl.demos.basic.inheritance";

		private static final String EXPECTED_PACKAGE_NAME = "io.sarl.demos.inheritance";

		@Override
		protected String getSourcePackageName() {
			return SOURCE_PACKAGE_NAME;
		}

		@Override
		protected String getExpectedPackageName() {
			return EXPECTED_PACKAGE_NAME;
		}

		@Override
		protected String getFileComment() {
			return "";
		}

	}

	public static class NoFileComment2 extends AbstractParticipantTest {

		private static final String SOURCE_PACKAGE_NAME = "io.sarl.demos.inheritance";

		private static final String EXPECTED_PACKAGE_NAME = "io.sarl.article.inheritance";

		@Override
		protected String getSourcePackageName() {
			return SOURCE_PACKAGE_NAME;
		}

		@Override
		protected String getExpectedPackageName() {
			return EXPECTED_PACKAGE_NAME;
		}

		@Override
		protected String getFileComment() {
			return "";
		}

	}

	public static class FileComment1 extends AbstractParticipantTest {

		private static final String SOURCE_PACKAGE_NAME = "io.sarl.demos.basic.inheritance";

		private static final String EXPECTED_PACKAGE_NAME = "io.sarl.demos.inheritance";

		@Override
		protected String getSourcePackageName() {
			return SOURCE_PACKAGE_NAME;
		}

		@Override
		protected String getExpectedPackageName() {
			return EXPECTED_PACKAGE_NAME;
		}

		@Override
		protected String getFileComment() {
			return multilineString("/*", "*", "*", "*/");
		}

	}

	public static class FileComment2 extends AbstractParticipantTest {

		private static final String SOURCE_PACKAGE_NAME = "io.sarl.demos.inheritance";

		private static final String EXPECTED_PACKAGE_NAME = "io.sarl.article.inheritance";

		@Override
		protected String getSourcePackageName() {
			return SOURCE_PACKAGE_NAME;
		}

		@Override
		protected String getExpectedPackageName() {
			return EXPECTED_PACKAGE_NAME;
		}

		@Override
		protected String getFileComment() {
			return multilineString("/*", "*", "*", "*/");
		}

	}

	public static class FileComment3 extends AbstractParticipantTest {

		private static final String SOURCE_PACKAGE_NAME = "io.sarl.demos.basic.inheritance";

		private static final String EXPECTED_PACKAGE_NAME = "io.sarl.demos.inheritance";

		@Override
		protected String getSourcePackageName() {
			return SOURCE_PACKAGE_NAME;
		}

		@Override
		protected String getExpectedPackageName() {
			return EXPECTED_PACKAGE_NAME;
		}

		@Override
		protected String getFileComment() {
			return multilineString("/*", "*", "*", "*/\n");
		}

	}

	public static class FileComment4 extends AbstractParticipantTest {

		private static final String SOURCE_PACKAGE_NAME = "io.sarl.demos.inheritance";

		private static final String EXPECTED_PACKAGE_NAME = "io.sarl.article.inheritance";

		@Override
		protected String getSourcePackageName() {
			return SOURCE_PACKAGE_NAME;
		}

		@Override
		protected String getExpectedPackageName() {
			return EXPECTED_PACKAGE_NAME;
		}

		@Override
		protected String getFileComment() {
			return multilineString("/*", "*", "*", "*/\n");
		}

	}

}
