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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: extension")
@Tag("core")
public class ExtensionTest extends AbstractSarlTest {

	@Test
	@Tag("sarlValidation")
	public void localExtension() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"class LocalExtensionTest {",
				"	def doSomething(obj : Object) { }",
				"	def extensionCall(obj : Object) {",
				"		obj.doSomething // calls this.doSomething(obj)",
				"	}",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("sarlValidation")
	public void extensionImports() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import java.util.List",
				"import static extension java.util.Collections.singletonList",
				"class ExtensionImportsTest {",
				"	static def extensionCall : List<ExtensionImportsTest> {",
				"		var obj = new ExtensionImportsTest",
				"		return obj.singletonList",
				"	}",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("sarlValidation")
	public void extensionProviderOnFieldVariable_valid() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import java.util.ArrayList",
				"class ExtensionProviderTest {",
				"	extension var f : ArrayList<String> = newArrayList",
				"	def extensionCall : boolean {",
				"		\"str\".contains",
				"	}",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	/** The type of the field must be given, otherwise there is a problem with
	 * the reentrant type resolving.
	 *
	 * @throws Exception - any problem.
	 */
	@Test
	@Tag("sarlValidation")
	public void extensionProviderOnFieldVariable_missedType() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import java.util.ArrayList",
				"class ExtensionProviderTest {",
				"	extension var f = newArrayList",
				"	def extensionCall : boolean {",
				"		\"str\".contains",
				"	}",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtext.diagnostics.Diagnostic.SYNTAX_DIAGNOSTIC,
				"mismatched input '=' expecting ':'");
	}

	@Test
	@Tag("sarlValidation")
	public void extensionProviderOnFieldValue() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import java.util.ArrayList",
				"class ExtensionProviderTest {",
				"	extension val f : ArrayList<String> = newArrayList",
				"	def extensionCall : boolean {",
				"		\"str\".contains",
				"	}",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("sarlValidation")
	public void extensionProviderOnParameter() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"class ExtensionProvider {",
				"	def doSomething(e : Object) : int { 0 }",
				"}",
				"class ExtensionProviderTest {",
				"	def extensionCall(parameter : Object, extension o : ExtensionProvider) : int {",
				"		parameter.doSomething",
				"	}",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("sarlValidation")
	public void extensionProviderOnLocalVariable() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"class ExtensionProvider {",
				"	def doSomething(e : Object) : int { 0 }",
				"}",
				"class ExtensionProviderTest {",
				"	def extensionCall(obj : Object) : int {",
				"		extension var o = new ExtensionProvider",
				"		obj.doSomething",
				"	}",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("sarlValidation")
	public void extensionProviderOnLocalValue() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"class ExtensionProvider {",
				"	def doSomething(e : Object) : int { 0 }",
				"}",
				"class ExtensionProviderTest {",
				"	def extensionCall(obj : Object) : int {",
				"		extension val o = new ExtensionProvider",
				"		obj.doSomething",
				"	}",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

}
