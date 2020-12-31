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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.general.parsing.general;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.SyntaxIssueCodes;
import io.sarl.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: general syntax")
@Tag("core")
public class GeneralSyntaxTest extends AbstractSarlTest {

	@Test
	@Tag("sarlValidation")
	public void wildCardImports() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.docs.reference.gsr",
				"import org.eclipse.xtext.xbase.lib.Procedures.*",
				"agent A {",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XtypePackage.eINSTANCE.getXImportDeclaration(),
				IssueCodes.IMPORT_WILDCARD_DEPRECATED,
				"The use of wildcard imports is deprecated");
	}

	@Test
	@Tag("sarlValidation")
	public void noParamNoReturnActionInClass() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"abstract class Light {",
				"	abstract def turnOn",
				"	abstract def turnOff",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("sarlValidation")
	public void noParamNoReturnActionInInterface() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"interface Light {",
				"	abstract def turnOn",
				"	abstract def turnOff",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}
	
	@Test
	@Tag("sarlValidation")
	public void sarlKeywordAsIdentifier() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.behavior.mypackage",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlScript(),
				SyntaxIssueCodes.USED_RESERVED_KEYWORD,
				"'behavior' is a reserved keyword which is not allowed as identifier.");
	}

	@Test
	@Tag("sarlValidation")
	public void javaKeywordAsIdentifier() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.null.mypackage",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlScript(),
				SyntaxIssueCodes.USED_RESERVED_KEYWORD,
				"'null' is a reserved keyword which is not allowed as identifier.");
	}

}
