/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import org.eclipse.xtext.xbase.XbasePackage;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.Test;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class ExtensionTest extends AbstractSarlTest {

	@Test
	public void localExtension() throws Exception {
		SarlScript mas = file(multilineString(
				"class LocalExtensionTest {",
				"	def doSomething(obj : Object) { }",
				"	def extensionCall(obj : Object) {",
				"		obj.doSomething // calls this.doSomething(obj)",
				"	}",
				"}",
				""));
		validate(mas).assertNoIssues();
	}

	@Test
	public void extensionImports() throws Exception {
		SarlScript mas = file(multilineString(
				"import java.util.List",
				"import static extension java.util.Collections.singletonList",
				"class ExtensionImportsTest {",
				"	static def extensionCall : List<ExtensionImportsTest> {",
				"		var obj = new ExtensionImportsTest",
				"		return obj.singletonList",
				"	}",
				"}",
				""));
		validate(mas).assertNoIssues();
	}

	@Test
	public void extensionProviderOnFieldVariable_valid() throws Exception {
		SarlScript mas = file(multilineString(
				"import java.util.ArrayList",
				"class ExtensionProviderTest {",
				"	extension var f : ArrayList<String> = newArrayList",
				"	def extensionCall : boolean {",
				"		\"str\".contains",
				"	}",
				"}",
				""));
		validate(mas).assertNoIssues();
	}

	/** The type of the field must be given, otherwise there is a problem with
	 * the reentrant type resolving.
	 *
	 * @throws Exception - any problem.
	 */
	@Test
	public void extensionProviderOnFieldVariable_missedType() throws Exception {
		SarlScript mas = file(multilineString(
				"import java.util.ArrayList",
				"class ExtensionProviderTest {",
				"	extension var f = newArrayList",
				"	def extensionCall : boolean {",
				"		\"str\".contains",
				"	}",
				"}",
				""));
		validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtext.diagnostics.Diagnostic.SYNTAX_DIAGNOSTIC,
				"mismatched input '=' expecting ':'");
	}

	@Test
	public void extensionProviderOnFieldValue() throws Exception {
		SarlScript mas = file(multilineString(
				"import java.util.ArrayList",
				"class ExtensionProviderTest {",
				"	extension val f : ArrayList<String> = newArrayList",
				"	def extensionCall : boolean {",
				"		\"str\".contains",
				"	}",
				"}",
				""));
		validate(mas).assertNoIssues();
	}

	@Test
	public void extensionProviderOnParameter() throws Exception {
		SarlScript mas = file(multilineString(
				"class ExtensionProvider {",
				"	def doSomething(e : Object) : int { 0 }",
				"}",
				"class ExtensionProviderTest {",
				"	def extensionCall(parameter : Object, extension o : ExtensionProvider) : int {",
				"		parameter.doSomething",
				"	}",
				"}",
				""));
		validate(mas).assertNoIssues();
	}

	@Test
	public void extensionProviderOnLocalVariable() throws Exception {
		SarlScript mas = file(multilineString(
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
		validate(mas).assertNoIssues();
	}

	@Test
	public void extensionProviderOnLocalValue() throws Exception {
		SarlScript mas = file(multilineString(
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
		validate(mas).assertNoIssues();
	}

}
