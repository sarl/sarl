/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.api.bspl.tests.bugs;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Generate many Java files from a single SARL construct.
 *
 * <p>https://github.com/sarl/sarl/issues/1156
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1156"
 */
@DisplayName("Bug #1156")
@SuppressWarnings("all")
@Tag("core")
public class Bug1156Test extends AbstractSarlTest {

	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1156",
			"protocol Prt {",
			"}");

	private static final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1156;",
			"",
			"@SuppressWarnings(\"all\")",
			"public interface Prt {",
			"}",
			"");

	private static final String JAVA_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1156;",
			"",
			"@SuppressWarnings(\"all\")",
			"public class PrtImpl {",
			"}",
			"");

	//@Test
	@Tag("compileToJava")
	@DisplayName("compile: interface Prt")
	public void compiling_interface_prj_0() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug1156.Prt");
			assertEquals(JAVA_CODE_01, actual);
		});
	}

	//@Test
	@Tag("compileToJava")
	@DisplayName("compile: class PrtImpl")
	public void compiling_class_prj_0() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug1156.PrtImpl");
			assertEquals(JAVA_CODE_02, actual);
		});
	}

}
