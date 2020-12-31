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

import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsPackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: active annotations")
@Tag("core")
public class ActiveAnnotationTest {

	@Nested
	@DisplayName("Syntax: @Accessors")
	public class AccessorsTest extends AbstractSarlTest {
		
		@Test
		@Tag("sarlValidation")
		public void inClassField_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"class C1 {",
					"	@Accessors var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		public void inClassField_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"class C1 {",
					"	@Accessors(PROTECTED_SETTER) var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		public void inClass_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"@Accessors class C1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		public void inClass_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"@Accessors(PROTECTED_SETTER) class C1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		public void inAgentField_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"agent A1 {",
					"	@Accessors var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
		}

		@Test
		@Tag("sarlValidation")
		public void inAgentField_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"agent A1 {",
					"	@Accessors(PROTECTED_SETTER) var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
		}

		@Test
		@Tag("sarlValidation")
		public void inAgent_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"@Accessors agent A1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
		}

		@Test
		@Tag("sarlValidation")
		public void inAgent_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"@Accessors(PROTECTED_SETTER) agent A1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
		}

		@Test
		@Tag("sarlValidation")
		public void inBehaviorField_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"behavior B1 {",
					"	@Accessors var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
		}

		@Test
		@Tag("sarlValidation")
		public void inBehaviorField_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"behavior B1 {",
					"	@Accessors(PROTECTED_SETTER) var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
		}

		@Test
		@Tag("sarlValidation")
		public void inBehavior_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"@Accessors behavior B1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
		}

		@Test
		@Tag("sarlValidation")
		public void inBehavior_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"@Accessors(PROTECTED_SETTER) behavior B1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
		}

		@Test
		@Tag("sarlValidation")
		public void inSkillField_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	@Accessors var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
		}

		@Test
		@Tag("sarlValidation")
		public void inSkillField_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	@Accessors(PROTECTED_SETTER) var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
		}

		@Test
		@Tag("sarlValidation")
		public void inSkill_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"capacity C1 { }",
					"@Accessors skill S1 implements C1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
		}

		@Test
		@Tag("sarlValidation")
		public void inSkill_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"capacity C1 { }",
					"@Accessors(PROTECTED_SETTER) skill S1 implements C1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE);
		}

		@Test
		@Tag("sarlValidation")
		public void inEventField_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"event E1 {",
					"	@Accessors var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inEventField_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"event E1 {",
					"	@Accessors(PROTECTED_SETTER) var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inEvent_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"@Accessors event E1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inEvent_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"@Accessors(PROTECTED_SETTER) event E1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inCapacity_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"@Accessors capacity C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inCapacity_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"@Accessors(PROTECTED_SETTER) capacity C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

	}

	@Nested
	@DisplayName("Syntax: @Data")
	public class DataTest extends AbstractSarlTest {
		
		@Test
		@Tag("sarlValidation")
		public void inClass_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Data",
					"@Data class C1 {",
					"	val field : int",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		public void inAgent_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Data",
					"@Data agent A1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inBehavior_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Data",
					"@Data behavior B1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inSkill_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Data",
					"capacity C1 { }",
					"@Data skill S1 implements C1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}
		
		@Test
		@Tag("sarlValidation")
		public void inEvent_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Data",
					"@Data event E1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inCapacity_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Data",
					"@Data capacity C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

	}

	@Nested
	@DisplayName("Syntax: @Delegate")
	public class DelegateTest extends AbstractSarlTest {
		
		@Test
		@Tag("sarlValidation")
		public void inClass_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Delegate",
					"interface I1 {",
					"  def myFct",
					"}",
					"class C1 implements I1 {",
					"	@Delegate var field : I1",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		public void inClass_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Delegate",
					"interface I1 {",
					"  def myFct",
					"}",
					"class MyDelegate implements I1 {",
					"   def myFct {}",
					"}",
					"class C1 implements I1 {",
					"	@Delegate def provideDelegate : I1 {",
					"      return new MyDelegate",
					"   }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		public void inAgent_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Delegate",
					"@Delegate agent A1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inBehavior_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Delegate",
					"@Delegate behavior B1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inSkill_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Delegate",
					"capacity C1 { }",
					"@Delegate skill S1 implements C1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}
		
		@Test
		@Tag("sarlValidation")
		public void inEvent_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Delegate",
					"@Delegate event E1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inCapacity_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.Delegate",
					"@Delegate capacity C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

	}

	@Nested
	@DisplayName("Syntax: @ToString")
	public class ToStringTest extends AbstractSarlTest {
		
		@Test
		@Tag("sarlValidation")
		public void inClass_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.ToString",
					"@ToString class C1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("sarlValidation")
		public void inAgent_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.ToString",
					"@ToString agent A1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inBehavior_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.ToString",
					"@ToString behavior B1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inSkill_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.ToString",
					"capacity C1 { }",
					"@ToString skill S1 implements C1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}
		
		@Test
		@Tag("sarlValidation")
		public void inEvent_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.ToString",
					"@ToString event E1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inCapacity_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.ToString",
					"@ToString capacity C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

	}

	@Nested
	@DisplayName("Syntax: @EqualsHashCode")
	public class EqualsHashCodeTest extends AbstractSarlTest {
		
		@Test
		@Tag("sarlValidation")
		public void inClass_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.EqualsHashCode",
					"@EqualsHashCode class C1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inAgent_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.EqualsHashCode",
					"@EqualsHashCode agent A1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inBehavior_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.EqualsHashCode",
					"@EqualsHashCode behavior B1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inSkill_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.EqualsHashCode",
					"capacity C1 { }",
					"@EqualsHashCode skill S1 implements C1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}
		
		@Test
		@Tag("sarlValidation")
		public void inEvent_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.EqualsHashCode",
					"@EqualsHashCode event E1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inCapacity_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.EqualsHashCode",
					"@EqualsHashCode capacity C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

	}

	@Nested
	@DisplayName("Syntax: @FinalFieldsConstructor")
	public class FinalFieldsConstructorTest extends AbstractSarlTest {
		
		@Test
		@Tag("sarlValidation")
		public void inClass_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor",
					"@FinalFieldsConstructor class C1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inAgent_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor",
					"@FinalFieldsConstructor agent A1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inBehavior_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor",
					"@FinalFieldsConstructor behavior B1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inSkill_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor",
					"capacity C1 { }",
					"@FinalFieldsConstructor skill S1 implements C1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}
		
		@Test
		@Tag("sarlValidation")
		public void inEvent_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor",
					"@FinalFieldsConstructor event E1 {",
					"	var field : double = 0",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

		@Test
		@Tag("sarlValidation")
		public void inCapacity_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor",
					"@FinalFieldsConstructor capacity C1 {",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE,
					"Forbidden annotation");
		}

	}

}
