/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.tests.parsing;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.google.common.base.Strings;
import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	EventParsingTest.TopElementTest.class,
	EventParsingTest.AttributeTest.class,
	EventParsingTest.ConstructorTest.class,
})
@SuppressWarnings("all")
public class EventParsingTest extends AbstractSarlTest {

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class TopElementTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void invalidExtend_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"capacity C1 {",
					"}",
					"event E1 extends C1 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getEvent(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Invalid supertype. Expecting: class");
		}

		@Test
		public void invalidExtend_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"agent A1 {",
					"}",
					"event E1 extends A1 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getEvent(),
					IssueCodes.INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Event'.");
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class AttributeTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void missedFinalFieldInitialization() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1 {",
					"val field1 : int = 5",
					"val field2 : String",
					"}"
					));
			this.validator.assertError(mas,
					TypesPackage.eINSTANCE.getJvmConstructor(),
					org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_INITIALIZATION,
					"The blank final field 'field2' may not have been initialized");
		}

		@Test
		public void completeFinalFieldInitialization() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1 {",
					"val field1 : int = 5",
					"val field2 : String = \"\"",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(1, mas.getElements().size());
			//
			assertTrue(Strings.isNullOrEmpty(mas.getName()));
			//
			Event event = (Event) mas.getElements().get(0);
			assertEquals("E1", event.getName());
			assertTypeReferenceIdentifiers(event.getSuperTypes());
			assertEquals(2, event.getFeatures().size());
			//
			Attribute attr1 = (Attribute) event.getFeatures().get(0);
			assertEquals("field1", attr1.getName());
			assertTypeReferenceIdentifier(attr1.getType(), "int");
			assertXExpression(attr1.getInitialValue(), XNumberLiteral.class, "5");
			//
			Attribute attr2 = (Attribute) event.getFeatures().get(1);
			assertEquals("field2", attr2.getName());
			assertTypeReferenceIdentifier(attr2.getType(), "java.lang.String");
			assertXExpression(attr2.getInitialValue(), XStringLiteral.class, "");
		}

		@Test
		public void invalidAttributeName_0() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1 {",
					"var myfield1 = 4.5",
					"var ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
					"var myfield2 = true",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					IssueCodes.INVALID_MEMBER_NAME,
					"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.");
		}

		@Test
		public void invalidAttributeName_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1 {",
					"val myfield1 = 4.5",
					"val ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"",
					"val myfield2 = true",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					IssueCodes.INVALID_MEMBER_NAME,
					"Invalid attribute name '___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.");
		}

		@Test
		public void multipleVariableDefinition() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1 {",
					"var myfield : int",
					"var myfield1 : String",
					"var myfield : double",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					IssueCodes.DUPLICATE_FIELD,
					"Duplicate field in 'E1': myfield");
		}

		@Test
		public void multipleValueDefinition() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E1 {",
					"val myfield : int = 4",
					"val myfield1 : String = \"\"",
					"val myfield : double = 5",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					IssueCodes.DUPLICATE_FIELD,
					"Duplicate field in 'E1': myfield");
		}

		@Test
		public void fieldNameShadowing() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"event E0 {",
					"val field1 : int = 5",
					"val field2 : int = 6",
					"}",
					"event E1 extends E0 {",
					"val field1 : int = 5",
					"}"
					));
			this.validator.assertWarning(mas,
					SarlPackage.eINSTANCE.getAttribute(),
					org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
					"The field 'field1' in 'E1' is hidding the inherited field 'E0.field1'.");
		}

	}

	@RunWith(XtextRunner.class)
	@InjectWith(SARLInjectorProvider.class)
	public static class ConstructorTest extends AbstractSarlTest {

		@Inject
		private ParseHelper<SarlScript> parser;

		@Inject
		private ValidationTestHelper validator;

		@Test
		public void validImplicitSuperConstructor() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"package io.sarl.test",
					"event E1 {",
					"}",
					"event E2 extends E1 {",
					"new (a : int) {",
					"}",
					"}"
					));
			this.validator.assertNoErrors(mas);
			assertEquals(2, mas.getElements().size());
			//
			assertEquals("io.sarl.test", mas.getName());
			//
			Event event1 = (Event) mas.getElements().get(0);
			assertEquals("E1", event1.getName());
			assertTypeReferenceIdentifiers(event1.getSuperTypes());
			assertEquals(0, event1.getFeatures().size());
			//
			Event event2 = (Event) mas.getElements().get(1);
			assertEquals("E2", event2.getName());
			assertTypeReferenceIdentifiers(event2.getSuperTypes(), "io.sarl.test.E1");
			assertEquals(1, event2.getFeatures().size());
			//
			Constructor constructor = (Constructor) event2.getFeatures().get(0);
			assertParameterNames(constructor.getParams(), "a");
			assertParameterTypes(constructor.getParams(), "int");
			assertParameterDefaultValues(constructor.getParams(), (Object) null);
		}

		@Test
		public void missedImplicitSuperConstructor_1() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"package io.sarl.test",
					"event E1 {",
					"new (a : char) {",
					"}",
					"}",
					"event E2 extends E1 {",
					"new (a : int) {",
					"}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getConstructor(),
					IssueCodes.MISSING_CONSTRUCTOR,
					"Undefined default constructor in the super-type");
		}

		@Test
		public void missedImplicitSuperConstructor_2() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"package io.sarl.test",
					"event E1 {",
					"new (a : int) {",
					"}",
					"}",
					"event E2 extends E1 {",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getEvent(),
					IssueCodes.MISSING_CONSTRUCTOR,
					"The constructor E1() is undefined.");
		}

		@Test
		public void missedImplicitSuperConstructor_3() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"package io.sarl.test",
					"event E1 {",
					"new (a : int) {",
					"}",
					"}",
					"event E2 extends E1 {",
					"new (a : int) {",
					"}",
					"}"
					));
			this.validator.assertError(mas,
					SarlPackage.eINSTANCE.getConstructor(),
					IssueCodes.MISSING_CONSTRUCTOR,
					"Undefined default constructor in the super-type");
		}

		@Test
		public void invalidArgumentTypeToSuperConstructor() throws Exception {
			SarlScript mas = this.parser.parse(multilineString(
					"package io.sarl.test",
					"event E1 {",
					"new (a : int) {",
					"}",
					"}",
					"event E2 extends E1 {",
					"new (a : int) {",
					"super(\"\")",
					"}",
					"}"
					));
			this.validator.assertError(mas,
					XbasePackage.eINSTANCE.getXStringLiteral(),
					org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
					"Type mismatch: cannot convert from String to int");
		}

	}
	
}
