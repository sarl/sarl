/*
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.lang.tests.parsing.general;

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;

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
public class SuppressWarningsTest extends AbstractSarlTest {

	@Test
	public void noAnnotation() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"agent A2 extends A1 {",
				"	def name { }",
				"}"), false);
		validate(mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void methodAnnotation_all() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"agent A2 extends A1 {",
				"   @SuppressWarnings(\"all\")",
				"	def name { }",
				"}"), false);
		validate(mas).assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void methodAnnotation_correctId() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"agent A2 extends A1 {",
				"   @SuppressWarnings(\"" + org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE + "\")",
				"	def name { }",
				"}"), false);
		validate(mas).assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void methodAnnotation_invalidId() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"agent A2 extends A1 {",
				"   @SuppressWarnings(\"x.y.z\")",
				"	def name { }",
				"}"), false);
		validate(mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void methodAnnotation_all_inlist() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"agent A2 extends A1 {",
				"   @SuppressWarnings(\"a\", \"b\", \"c\", \"all\")",
				"	def name { }",
				"}"), false);
		validate(mas).assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void methodAnnotation_correctId_inlist() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"agent A2 extends A1 {",
				"   @SuppressWarnings(\"a\", \"b\", \"c\", \""
						+ org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE + "\")",
				"	def name { }",
				"}"), false);
		validate(mas).assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void methodAnnotation_invalidId_inlist() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"agent A2 extends A1 {",
				"   @SuppressWarnings(\"a\", \"b\", \"c\", \"x.y.z\")",
				"	def name { }",
				"}"), false);
		validate(mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void typeAnnotation_all() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"@SuppressWarnings(\"all\")",
				"agent A2 extends A1 {",
				"	def name { }",
				"}"), false);
		validate(mas).assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void typeAnnotation_correctId() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"@SuppressWarnings(\"" + org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE + "\")",
				"agent A2 extends A1 {",
				"	def name { }",
				"}"), false);
		validate(mas).assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void typeAnnotation_invalidId() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"@SuppressWarnings(\"x.y.z\")",
				"agent A2 extends A1 {",
				"	def name { }",
				"}"), false);
		validate(mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void typeAnnotation_all_inlist() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"@SuppressWarnings(\"a\", \"b\", \"c\", \"all\")",
				"agent A2 extends A1 {",
				"	def name { }",
				"}"), false);
		validate(mas).assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void typeAnnotation_correctId_inlist() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"@SuppressWarnings(\"a\", \"b\", \"c\", \""
					+ org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE + "\")",
				"agent A2 extends A1 {",
				"	def name { }",
				"}"), false);
		validate(mas).assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void typeAnnotation_invalidId_inlist() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"abstract agent A1 {",
				"	abstract def name",
				"}",
				"@SuppressWarnings(\"a\", \"b\", \"c\", \"x.y.z\")",
				"agent A2 extends A1 {",
				"	def name { }",
				"}"), false);
		validate(mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlAction(),
				org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE);
	}

	@Test
	public void expression_noSuppression() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"agent A1 {",
				"	def mytst : void {",
				"     if (1==1) {",
				"     }",
				"   }",
				"}"), false);
		validate(mas).assertWarning(
				XbasePackage.eINSTANCE.getXBinaryOperation(),
				org.eclipse.xtext.xbase.validation.IssueCodes.CONSTANT_BOOLEAN_CONDITION);
	}

	@Test
	public void expression_suppression_01() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"agent A1 {",
				"   @SuppressWarnings(\"constant_condition\")",
				"	def mytst : void {",
				"     if (1==1) {",
				"     }",
				"   }",
				"}"), false);
		validate(mas).assertNoWarnings(
				XbasePackage.eINSTANCE.getXBinaryOperation(),
				org.eclipse.xtext.xbase.validation.IssueCodes.CONSTANT_BOOLEAN_CONDITION);
	}

	@Test
	public void expression_suppression_02() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"@SuppressWarnings(\"constant_condition\")",
				"agent A1 {",
				"	def mytst : void {",
				"     if (1==1) {",
				"     }",
				"   }",
				"}"), false);
		validate(mas).assertNoWarnings(
				XbasePackage.eINSTANCE.getXBinaryOperation(),
				org.eclipse.xtext.xbase.validation.IssueCodes.CONSTANT_BOOLEAN_CONDITION);
	}

}
