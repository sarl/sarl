/*
 * Copyright (C) 2014-2017 the original authors or authors.
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
package io.sarl.pythongenerator.tests;

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class PyValidatorTest extends AbstractSarlTest {

	@Test
	public void basic() throws Exception {
		String source = "class C1 { }";
		SarlScript mas = file(source);
		validate(mas).assertNoErrors();
	}

	@Test
	public void variable() throws Exception {
		String source = "class C1 { var v = 45 }";
		SarlScript mas = file(source);
		validate(mas).assertNoErrors();
	}

	@Test
	public void field_0() throws Exception {
		String source = "class C1 { var x : int }";
		SarlScript mas = file(source);
		validate(mas).assertNoErrors();
	}

	@Test
	public void agent_0() throws Exception {
		String source = multilineString(
				"event E1",
				"agent A1 {",
				"  var attr = 6",
				"  def myfct { }",
				"  on E1 { }",
				"  on E1 [ attr > 0 ] { }",
				"  on E1 [ occurrence.isFromMe ] { }",
				"}");
		SarlScript mas = file(source);
		validate(mas).assertNoErrors();
	}

	@Test
	public void capacityuses_0() throws Exception {
		String source = multilineString(
				"capacity C1 {",
				"  def fct1 : int",
				"  def fct2(a : char)",
				"}",
				"behavior B1 {",
				"  uses C1",
				"  def myfct {",
				"    var a = fct1 + 1",
				"  }",
				"}");
		SarlScript mas = file(source);
		validate(mas).assertNoErrors();
	}

	@Test
	public void importStatement_0() throws Exception {
		String source = multilineString(
				"import java.net.URL",
				"class C1 {",
				"  def myfct {",
				"    var url : URL",
				"  }",
				"}");
		SarlScript mas = file(source);
		validate(mas).assertError(
				XtypePackage.eINSTANCE.getXImportDeclaration(),
				IssueCodes.IMPORT_CONFLICT);
	}

}
