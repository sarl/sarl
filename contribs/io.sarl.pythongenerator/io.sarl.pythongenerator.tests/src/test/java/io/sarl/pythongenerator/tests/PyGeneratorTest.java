/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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
package io.sarl.pythongenerator.tests;

import org.junit.Test;

import io.sarl.pythongenerator.configuration.PyOutputConfigurationProvider;
import io.sarl.pythongenerator.generator.PyGenerator;
import io.sarl.tests.api.AbstractExtraLanguageGeneratorTest;
import io.sarl.tests.api.AbstractExtraLanguageGeneratorTest.GeneratorTest;


/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class PyGeneratorTest extends AbstractExtraLanguageGeneratorTest {

	@Override
	protected String getOutputConfigurationName() {
		return PyOutputConfigurationProvider.OUTPUT_CONFIGURATION_NAME;
	}
	
	@Test
	public void basic() throws Exception {
		String source = "class C1 { }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void variable() throws Exception {
		String source = "class C1 { var v = 45 }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef __init__(self):",
				"\t\tself.v = 45");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void value() throws Exception {
		String source = "class C1 { val v = 45 }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef __init__(self):",
				"\t\tself.v = 45");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void method_0() throws Exception {
		String source = "class C1 { def fct { 4 } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self):",
				"\t\t4",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void method_1() throws Exception {
		String source = "class C1 { def fct(a : int) { a } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\ta",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void method_2() throws Exception {
		String source = "class C1 { def fct(a : int*) { 5 } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, *a):",
				"\t\t5",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void method_3() throws Exception {
		String source = "class C1 { def fct(a : int = 6) { 5 } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\t5",
				"\tdef fct(self):",
				"\t\tself.fct(6)",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void method_4() throws Exception {
		String source = "class C1 { def fct : int { 4 } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self):",
				"\t\treturn 4",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void method_5() throws Exception {
		String source = "class C1 { def fct(a : int) : int { a } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\treturn a",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void method_6() throws Exception {
		String source = "class C1 { def fct(a : int*) : int { 5 } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, *a):",
				"\t\treturn 5",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void method_7() throws Exception {
		String source = "class C1 { def fct(a : int = 6) : int { 5 } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\treturn 5",
				"\tdef fct(self):",
				"\t\treturn self.fct(6)",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void methodOverriding_explicitReturnType() throws Exception {
		String source = multilineString(
				"package io.sarl.docs.reference.oop",
				"class Person {",
				"	var firstName : String",
				"	var lastName : String",
				"	def getFullName : String {",
				"		this.firstName + \" \" + this.lastName",
				"	}",
				"}",
				"class PersonEx extends Person {",
				"	var title : String",
				"	override getFullName : String {",
				"		return title + \" \" + super.fullName",
				"	}",
				"}");
		String expected1 = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class Person(object):",
				"\tdef getFullName(self):",
				"\t\treturn self.firstName + u\" \" + self.lastName",
				"\tdef __init__(self):",
				"\t\tself.firstName = None",
				"\t\tself.lastName = None");
		String expected2 = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"from io.sarl.docs.reference.oop import Person",
				"",
				"class PersonEx(Person):",
				"\tdef getFullName(self):",
				"\t\treturn self.title + u\" \" + super().getFullName()",
				"\tdef __init__(self):",
				"\t\tself.title = None");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("io.sarl.docs.reference.oop.Person", expected1);
		gen.assertTypeDefinition("io.sarl.docs.reference.oop.PersonEx", expected2);
	}
	
	@Test
	public void methodOverriding_inferredReturnType() throws Exception {
		String source = multilineString(
				"package io.sarl.docs.reference.oop",
				"class Person {",
				"	var firstName : String",
				"	var lastName : String",
				"	def getFullName : String {",
				"		this.firstName + \" \" + this.lastName",
				"	}",
				"}",
				"class PersonEx extends Person {",
				"	var title : String",
				"	override getFullName {",
				"		return title + \" \" + super.fullName",
				"	}",
				"}");
		String expected1 = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class Person(object):",
				"\tdef getFullName(self):",
				"\t\treturn self.firstName + u\" \" + self.lastName",
				"\tdef __init__(self):",
				"\t\tself.firstName = None",
				"\t\tself.lastName = None");
		String expected2 = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"from io.sarl.docs.reference.oop import Person",
				"",
				"class PersonEx(Person):",
				"\tdef getFullName(self):",
				"\t\treturn self.title + u\" \" + super().getFullName()",
				"\tdef __init__(self):",
				"\t\tself.title = None");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("io.sarl.docs.reference.oop.Person", expected1);
		gen.assertTypeDefinition("io.sarl.docs.reference.oop.PersonEx", expected2);
	}

	@Test
	public void postfixOperator() throws Exception {
		String source = "class C1 { def fct(a : int) : int { a++; return a } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\ta += 1",
				"\t\treturn a",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void incrementOperator() throws Exception {
		String source = "class C1 { def fct(a : int) : int { a += 5 ; return a } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\ta += 5",
				"\t\treturn a",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void unaryOperator() throws Exception {
		String source = "class C1 { def fct(a : int) : int { a = -a + 6 ; return a } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\ta = -a + 6",
				"\t\treturn a",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void whileStatement() throws Exception {
		String source = "class C1 { def fct(a : int) { while (a > 0) { a-- } } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\twhile a > 0:",
				"\t\t\ta -= 1",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void doWhileStatement() throws Exception {
		String source = "class C1 { def fct(a : int) { do { a-- } while ((a > 0) } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\ta -= 1",
				"\t\twhile a > 0:",
				"\t\t\ta -= 1",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void basicForLoopStatement() throws Exception {
		String source = "class C1 { def fct(a : int) { for(var i = 0; i < 5; i++ { a-- } } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\ti = 0",
				"\t\twhile i < 5:",
				"\t\t\ta -= 1",
				"\t\t\ti += 1",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void forLoopStatement() throws Exception {
		String source = "class C1 { def fct(a : int[]) : int { var s = 0; for(e : a) { s += e }; return s } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\ts = 0",
				"\t\tfor e in a:",
				"\t\t\ts += e",
				"\t\treturn s",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void ifStatement() throws Exception {
		String source = "class C1 { def fct(a : int) : int { if (a > 0) a + 5 } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\tif a > 0:",
				"\t\t\treturn a + 5",
				"\t\telse:",
				"\t\t\treturn 0",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void ifElseStatement() throws Exception {
		String source = "class C1 { def fct(a : int) : int { if (a > 0) a + 5 else a - 6 } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\tif a > 0:",
				"\t\t\treturn a + 5",
				"\t\telse:",
				"\t\t\treturn a - 6",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void switchStatement() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct(a : Object) : int {",
				"    switch (a) {",
				"    case 1: return 1",
				"    case 32: return 2",
				"    case (a as Integer) > 32: return 3",
				"    String: return 4",
				"    String case \"34\": return 5",
				"    case 2,",
				"    case 3: return 6",
				"    default: return 0",
				"    }",
				"  }",
				"}");
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\t___expression = a",
				"\t\tif ((1) == ___expression):",
				"\t\t\treturn 1",
				"\t\telif ((32) == ___expression):",
				"\t\t\treturn 2",
				"\t\telif ((a > 32)):",
				"\t\t\treturn 3",
				"\t\telif (isinstance(___expression, unicode)):",
				"\t\t\treturn 4",
				"\t\telif (isinstance(___expression, unicode) and (u\"34\") == ___expression):",
				"\t\t\treturn 5",
				"\t\telif ((2) == ___expression) or ((3) == ___expression):",
				"\t\t\treturn 6",
				"\t\telse:",
				"\t\t\treturn 0",
				"\t\t",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void tryCatchStatement() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct(a : int) : int {",
				"    try {",
				"      return a + 1",
				"    } catch (e : Exception) {",
				"      return a + 2",
				"    } finally {",
				"      return a + 3",
				"    }",
				"  }",
				"}");
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\ttry:",
				"\t\t\treturn a + 1",
				"\t\texcept Exception, e:",
				"\t\t\treturn a + 2",
				"\t\tfinally:",
				"\t\t\treturn a + 3",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void constructorCallStatement() throws Exception {
		String source = multilineString(
				"class C1 {",
				" new(a : int) { }",
				"}",
				"class C2 {",
				"  def fct : C1 {",
				"    return new C1(5)",
				"  }",
				"}");
		String expected1 = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef __init__(self, a):",
				"\t\tpass");
		String expected2 = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C2(object):",
				"\tdef fct(self):",
				"\t\treturn C1(5)",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected1);
		gen.assertTypeDefinition("C2", expected2);
	}

	@Test
	public void closureStatement() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct(f : (int) => float) {",
				"  }",
				"  def fct2 {",
				"    fct [ it + 2.0 ]",
				"  }",
				"}");
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, f):",
				"\t\tpass",
				"\tdef fct2(self):",
				"\t\tdef __closure(self, it):",
				"\t\t\treturn it + 2.0",
				"\t\tself.fct(__closure)",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void anonStatement() throws Exception {
		String source = multilineString(
				"interface C1 {",
				"  def fct(a : int) : float",
				"}",
				"class C2 {",
				"  def fct(f : C1) {",
				"  }",
				"  def fct2 {",
				"    fct(new C1 {",
				"      def fct(a : int) { 0 }",
				"    })",
				"  }",
				"}");
		String expected1 = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef fct(self, a):",
				"\t\traise Exception(\"Unimplemented function\")",
				"\tdef __init__(self):",
				"\t\tpass");
		String expected2 = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C2(object):",
				"\tdef fct(self, f):",
				"\t\tpass",
				"\tdef fct2(self):",
				"\t\tclass AnonClass(C1,object):",
				"\t\t\tdef fct(self, a):",
				"\t\t\t\t0",
				"\t\t\tdef __init__(self):",
				"\t\t\t\tpass",
				"\t\t\t",
				"\t\t",
				"\t\tself.fct(AnonClass)",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected1);
		gen.assertTypeDefinition("C2", expected2);
	}

	@Test
	public void constructor_0() throws Exception {
		String source = "class C1 { var x : int new { x = 4 } }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef __init__(self):",
				"\t\tself.x = 4");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
	}

	@Test
	public void field_0() throws Exception {
		String source = "class C1 { var x : int }";
		String expected = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"class C1(object):",
				"\tdef __init__(self):",
				"\t\tself.x = 0");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected);
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
		String expected1 = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"from io.sarl.lang.core import Event",
				"",
				"class E1(Event):",
				"\tdef __init__(self):",
				"\t\tpass");
		String expected2 = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"from io.sarl.lang.core import Agent",
				"",
				"class A1(Agent):",
				"\tdef myfct(self):",
				"\t\tpass",
				"\tdef on_E1(self, occurrence):",
				"\t\tpass",
				"\tdef on_E1_1(self, occurrence):",
				"\t\tpass",
				"\tdef on_E1_2(self, occurrence):",
				"\t\tpass",
				"\tdef __guard_E1__(self, occurrence):",
				"\t\tit = occurrence",
				"\t\t__event_handles = list",
				"\t\t__event_handles.add(on_E1)",
				"\t\tif self.attr > 0:",
				"\t\t\t__event_handles.add(on_E1_1)",
				"\t\tif self.isFromMe(occurrence):",
				"\t\t\t__event_handles.add(on_E1_2)",
				"\t\treturn __event_handles",
				"\tdef __init__(self):",
				"\t\tself.attr = 6");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("E1", expected1);
		gen.assertTypeDefinition("A1", expected2);
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
		String expected1 = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"from io.sarl.lang.core import Capacity",
				"",
				"class C1(Capacity):",
				"\tdef fct1(self):",
				"\t\traise Exception(\"Unimplemented function\")",
				"\tdef fct2(self, a):",
				"\t\traise Exception(\"Unimplemented function\")",
				"\tdef __init__(self):",
				"\t\tpass");
		String expected2 = multilineString(
				PyGenerator.PYTHON_FILE_HEADER,
				"",
				"from io.sarl.lang.core import Behavior",
				"",
				"class B1(Behavior):",
				"\tdef myfct(self):",
				"\t\ta = self.getSkill(C1).fct1() + 1",
				"\tdef __init__(self):",
				"\t\tpass");
		GeneratorTest gen = compile(source);
		gen.assertTypeDefinition("C1", expected1);
		gen.assertTypeDefinition("B1", expected2);
	}

}
