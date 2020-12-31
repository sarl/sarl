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

package io.sarl.lang.tests.bugs.to00699;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

/** Issue 483: Events are not cloned before they are given to the behavior units.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see https://github.com/sarl/sarl/issues/483
 */
@DisplayName("Bug #483")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlValidation")
public class Bug483Test extends AbstractSarlTest {

	@Test
	public void updatePrimitiveType() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"event E1 {",
			"  var attr : int",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"        occurrence.attr = 1",
			"    }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.INVALID_OCCURRENCE_READONLY_USE);
	}

	@Test
	public void updateUnmodifiableObject() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"event E1 {",
			"  var attr : String",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"        occurrence.attr = \"\"",
			"    }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.INVALID_OCCURRENCE_READONLY_USE);
	}

	@Test
	public void updateObjectSetter_01() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def setValue(p : int) { }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"        occurrence.attr.value = 1",
			"    }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.INVALID_OCCURRENCE_READONLY_USE);
	}

	@Test
	public void updateObjectSetter_02() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def setValue(p : int) { }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"        occurrence.attr.setValue(1)",
			"    }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE);
	}

	@Test
	public void updateObjectSetter_03() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def setValue(p : int) : int { }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"        var x = occurrence.attr.setValue(1)",
			"        fct(p)",
			"    }",
			"    def fct(p : int) {}",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE);
	}

	@Test
	public void updateObjectSetter_04() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def setValue(p : int) : int { }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       if (occurrence.attr.setValue(1) == 1) {",
			"       }",
			"    }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE);
	}

	@Test
	public void updateObjectGetter_01() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def getValue() : int { 0 }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"        var x = occurrence.attr.value",
			"        myfct(x)",
			"    }",
			"    def myfct(a : int) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void updateObjectGetter_02() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def getValue() : int { 0 }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"        if (occurrence.attr.value == 1) {",
			"        }",
			"    }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void updateObjectGetter_03() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def getValue() : Object { null }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"        var x = occurrence.attr.value",
			"        myfct(x)",
			"    }",
			"    def myfct(a : Object) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE);
	}

	@Test
	public void updateObjectSetter_05() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def getValue() : int { 0 }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       myfct(occurrence.attr.value);",
			"    }",
			"    def myfct(p : int) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void updateObjectSetter_06() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def getValue() : Object { null }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       myfct(occurrence.attr.value);",
			"    }",
			"    def myfct(p : Object) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE);
	}

	@Test
	public void updateObjectSetter_07() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def getValue() : int { 0 }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       myfct(occurrence.attr.value);",
			"    }",
			"    def myfct(p : Integer) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void updateObjectSetter_08() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def getValue() : String { null }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       myfct(occurrence.attr.value);",
			"    }",
			"    def myfct(p : String) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void updateObjectSetter_09() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def getValue() : Integer { null }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       myfct(occurrence.attr.value);",
			"    }",
			"    def myfct(p : Integer) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void updateObjectSetter_10() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"enum MyEnum { CST1, CST2 }",
			"class XXX {",
			"  def getValue() : MyEnum { MyEnum.CST2 }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       myfct(occurrence.attr.value);",
			"    }",
			"    def myfct(p : MyEnum) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void updateObjectSetter_11() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"import java.math.BigInteger",
			"class XXX {",
			"  def getValue() : BigInteger { null }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       myfct(occurrence.attr.value);",
			"    }",
			"    def myfct(p : BigInteger) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE);
	}

	@Test
	public void iterableInForStatement_01() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"event E1 {",
			"  var attr : Iterable<Object>",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       for (x : occurrence.attr) {",
			"       }",
			"    }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void iterableInForStatement_02() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def getValue() : Iterable<Object> { null }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       for (x : occurrence.attr.value) {",
			"       }",
			"    }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void inArithmeticExpression_01() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"event E1 {",
			"  var attr : int",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       var x = 1 + occurrence.attr",
			"       myfct(x)",
			"    }",
			"    def myfct(p : int) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void inArithmeticExpression_02() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def getValue() : int { 0 }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       var x = 1 + occurrence.attr.value",
			"       myfct(x)",
			"    }",
			"    def myfct(p : int) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void inArithmeticExpression_03() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def setValue(v : int) : int { 0 }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       var x = 1 + occurrence.attr.setValue(1)",
			"       myfct(x)",
			"    }",
			"    def myfct(p : int) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE);
	}

	@Test
	public void inArithmeticExpression_04() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"event E1 {",
			"  var attr : int",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       var x = -occurrence.attr",
			"       myfct(x)",
			"    }",
			"    def myfct(p : int) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void inArithmeticExpression_05() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def getValue() : int { 0 }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       var x = -occurrence.attr.value",
			"       myfct(x)",
			"    }",
			"    def myfct(p : int) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	public void inArithmeticExpression_06() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def setValue(v : int) : int { 0 }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       var x = -occurrence.attr.setValue(1)",
			"       myfct(x)",
			"    }",
			"    def myfct(p : int) { }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE);
	}

	@Test
	public void copyInLocalVariable_01() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def setValue(v : int) { }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       var x = occurrence.attr",
			"       x.setValue(1)",
			"    }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE);
	}

	@Test
	public void copyInLocalVariable_02() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"event E1 {",
			"  var attr : int",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       var x = occurrence.attr",
			"       x = 1",
			"    }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXVariableDeclaration(),
				org.eclipse.xtext.xbase.validation.IssueCodes.UNUSED_LOCAL_VARIABLE);
	}

	@Test
	public void copyInLocalVariable_03() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"package io.sarl.lang.tests.bug483",
			"class XXX {",
			"  def setValue(v : int) { }",
			"}",
			"event E1 {",
			"  var attr : XXX",
			"}",
			"agent TestAgent {",
			"    on E1 {",
			"       var x = occurrence.attr",
			"       x.setValue(1)",
			"    }",
			"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE);
	}

}
