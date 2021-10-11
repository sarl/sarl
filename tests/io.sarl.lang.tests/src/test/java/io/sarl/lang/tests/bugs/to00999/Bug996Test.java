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

package io.sarl.lang.tests.bugs.to00999;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid Java code for a cast of the null value.
 *
 * <p>https://github.com/sarl/sarl/issues/996
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/996"
 */
@DisplayName("Bug #996")
@SuppressWarnings("all")
@Tag("core")
public class Bug996Test extends AbstractSarlTest {

	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug996",
			"import java.net.URL",
			"class Base {",
			"	@Pure static def getResource(classname : Class<?>, path : String) : URL { null }",
			"   ",
			"	@Pure static def getResource(classLoader : ClassLoader, path : String) : URL { null }",
			"}",
			"",
			"class Bug996Case {",
			"  def f(path : String) : URL {",
			"    Base::getResource(null as ClassLoader, path)",
			"  }",
			"}");

	private static final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug996;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.net.URL;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")", 
			"@SuppressWarnings(\"all\")", 
			"public class Bug996Case {",
			"  @Pure",
			"  public URL f(final String path) {",
			"    return Base.getResource(((ClassLoader)null), path);",
			"  }",
			"  ", 
			"  @SyntheticMember",
			"  public Bug996Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	@DisplayName("Parsing ambiguous function call")
	public void parsingAmbiguousFunctionCall() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	@DisplayName("Compilnig ambiguous function call")
	public void compileAmbiguousFunctionCall() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug996.Bug996Case");
			assertEquals(JAVA_CODE_01, actual);
		});
	}

	private static final String SARL_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug996",
			"class A {}",
			"class B extends A {}",
			"class C extends A {}",
			"class Bug996Case {",
			"  def fct(param : A) {",
			"    if (param instanceof B) {",
			"       onB(param as B)",
			"    } else if (param instanceof C) {",
			"       onC(param as C)",
			"    }",
			"  }",
			"  def onB(param : B) {}",
			"  def onC(param : C) {}",
			"}");

	private static final String JAVA_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug996;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")", 
			"@SuppressWarnings(\"all\")", 
			"public class Bug996Case {",
			"  public void fct(final A param) {",
			"    if ((param instanceof B)) {",
			"      this.onB(((B)param));",
			"    } else {",
			"      if ((param instanceof C)) {",
			"        this.onC(((C)param));",
			"      }",
			"    }",
			"  }",
			"  ",
			"  public void onB(final B param) {",
			"  }",
			"  ",
			"  public void onC(final C param) {",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Bug996Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	@DisplayName("Parsing casting into instanceof block")
	public void parsingCastingIntoInstanceof() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.OBSOLETE_CAST,
				"B to B")
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.OBSOLETE_CAST,
				"C to C");
	}

	@Test
	@Tag("compileToJava")
	@DisplayName("Compiling casting into instanceof block")
	public void compileCastingIntoInstanceof() throws Exception {
		getCompileHelper().compile(SARL_CODE_02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug996.Bug996Case");
			assertEquals(JAVA_CODE_02, actual);
		});
	}

	private static final String SARL_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug996",
			"class A {}",
			"class B extends A {}",
			"class C extends A {}",
			"class Bug996Case {",
			"  def fct(param : A) {",
			"    if (param instanceof B) {",
			"        onB(param)",
			"    } else if (param instanceof C) {",
			"       onC(param)",
			"    }",
			"  }",
			"  def onB(param : B) {}",
			"  def onC(param : C) {}",
			"}");

	private static final String JAVA_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug996;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")", 
			"@SuppressWarnings(\"all\")", 
			"public class Bug996Case {",
			"  public void fct(final A param) {",
			"    if ((param instanceof B)) {",
			"      this.onB(((B)param));",
			"    } else {",
			"      if ((param instanceof C)) {",
			"        this.onC(((C)param));",
			"      }",
			"    }",
			"  }",
			"  ",
			"  public void onB(final B param) {",
			"  }",
			"  ",
			"  public void onC(final C param) {",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Bug996Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	@DisplayName("Parsing implicit casting into instanceof block")
	public void parsingImplicitCastingIntoInstanceof() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	@DisplayName("Compiling implicit casting into instanceof block")
	public void compileImplicitCastingIntoInstanceof() throws Exception {
		getCompileHelper().compile(SARL_CODE_03, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug996.Bug996Case");
			assertEquals(JAVA_CODE_03, actual);
		});
	}

	private static final String SARL_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug996",
			"import java.net.URL",
			"class Base {",
			"	@Pure static def getResource(classname : Class<?>, path : String) : URL { null }",
			"   ",
			"	@Pure static def getResource(classLoader : ClassLoader, path : String) : URL { null }",
			"}",
			"",
			"class Bug996Case {",
			"  def f(path : String) : URL {",
			"    var xxx : Object = null",
			"    Base::getResource(xxx as ClassLoader, path)",
			"  }",
			"}");

	private static final String JAVA_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug996;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.net.URL;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")", 
			"@SuppressWarnings(\"all\")",
			"public class Bug996Case {",
			"  @Pure",
			"  public URL f(final String path) {",
			"    URL _xblockexpression = null;",
			"    {",
			"      Object xxx = null;",
			"      _xblockexpression = Base.getResource(((ClassLoader) xxx), path);",
			"    }",
			"    return _xblockexpression;",
			"  }",
			"  ", 
			"  @SyntheticMember",
			"  public Bug996Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	@DisplayName("Parsing ambiguous function call with variable")
	public void parsingAmbiguousFunctionCallWithVariable() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_04);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	@DisplayName("Compiling ambiguous function call with variable")
	public void compileAmbiguousFunctionCallWithVariable() throws Exception {
		getCompileHelper().compile(SARL_CODE_04, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug996.Bug996Case");
			assertEquals(JAVA_CODE_04, actual);
		});
	}

	private static final String SARL_CODE_05 = multilineString(
			"package io.sarl.lang.tests.bug996",
			"import java.net.URL",
			"class Base {",
			"	@Pure static def getResource(classname : Class<?>, path : String) : URL { null }",
			"   ",
			"	@Pure static def getResource(classLoader : ClassLoader, path : String) : URL { null }",
			"}",
			"",
			"class Bug996Case {",
			"  def f(path : String) : URL {",
			"    var xxx : ClassLoader = null as ClassLoader",
			"    Base::getResource(xxx, path)",
			"  }",
			"}");

	private static final String JAVA_CODE_05 = multilineString(
			"package io.sarl.lang.tests.bug996;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.net.URL;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")", 
			"@SuppressWarnings(\"all\")",
			"public class Bug996Case {",
			"  @Pure",
			"  public URL f(final String path) {",
			"    URL _xblockexpression = null;",
			"    {",
			"      ClassLoader xxx = ((ClassLoader)null);",
			"      _xblockexpression = Base.getResource(xxx, path);",
			"    }",
			"    return _xblockexpression;",
			"  }",
			"  ", 
			"  @SyntheticMember",
			"  public Bug996Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	@DisplayName("Parsing implicit casting with variable into instanceof block")
	public void parsingImplicitCastingWithVariableIntoInstanceof() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_05);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
			TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
			IssueCodes.OBSOLETE_CAST,
			"null to ClassLoader");
	}

	@Test
	@Tag("compileToJava")
	@DisplayName("Compiling implicit casting with variable into instanceof block")
	public void compileImplicitCastingWithVariableIntoInstanceof() throws Exception {
		getCompileHelper().compile(SARL_CODE_05, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug996.Bug996Case");
			assertEquals(JAVA_CODE_05, actual);
		});
	}

	private static final String SARL_CODE_06 = multilineString(
			"package io.sarl.lang.tests.bug996",
			"class A {}",
			"class B extends A {}",
			"class C extends A {}",
			"class Bug996Case {",
			"  def fct(param : A) : A {",
			"    if (param instanceof B) {",
			"        onB(param)",
			"        var xxx = param",
			"        return xxx",
			"    } else if (param instanceof C) {",
			"       onC(param)",
			"       var xxx = param",
			"       return xxx",
			"    }",
			"  }",
			"  def onB(param : B) {}",
			"  def onC(param : C) {}",
			"}");

	private static final String JAVA_CODE_06 = multilineString(
			"package io.sarl.lang.tests.bug996;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")", 
			"@SuppressWarnings(\"all\")", 
			"public class Bug996Case {",
			"  public A fct(final A param) {",
			"    if ((param instanceof B)) {",
			"      this.onB(((B)param));",
			"      B xxx = ((B)param);",
			"      return xxx;",
			"    } else {",
			"      if ((param instanceof C)) {",
			"        this.onC(((C)param));",
			"        C xxx_1 = ((C)param);",
			"        return xxx_1;",
			"      }",
			"    }",
			"    return null;",
			"  }",
			"  ",
			"  public void onB(final B param) {",
			"  }",
			"  ",
			"  public void onC(final C param) {",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Bug996Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	@DisplayName("Parsing multiple implicit casts into instanceof")
	public void parsingMultipleInmplicitCastsIntoInstanceof() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_06);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	@DisplayName("Compiling multiple implicit casts into instanceof")
	public void compileMultipleInmplicitCastsIntoInstanceof() throws Exception {
		getCompileHelper().compile(SARL_CODE_06, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug996.Bug996Case");
			assertEquals(JAVA_CODE_06, actual);
		});
	}

	private static final String SARL_CODE_07 = multilineString(
			"package io.sarl.lang.tests.bug996",
			"class A {}",
			"class B extends A {}",
			"class C extends A {}",
			"class Bug996Case {",
			"  def fct(param : A) : A {",
			"    if (param instanceof B) {",
			"        var xxx = param as B",
			"        return xxx",
			"    } else if (param instanceof C) {",
			"       var xxx = param as C",
			"       return xxx",
			"    }",
			"  }",
			"}");

	private static final String JAVA_CODE_07 = multilineString(
			"package io.sarl.lang.tests.bug996;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")", 
			"@SuppressWarnings(\"all\")", 
			"public class Bug996Case {",
			"  @Pure",
			"  public A fct(final A param) {",
			"    if ((param instanceof B)) {",
			"      B xxx = ((B)param);",
			"      return xxx;",
			"    } else {",
			"      if ((param instanceof C)) {",
			"        C xxx_1 = ((C)param);",
			"        return xxx_1;",
			"      }",
			"    }",
			"    return null;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Bug996Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	@DisplayName("Parsing explicit cast to variable into instanceof")
	public void parsingExplicitCastToVariableIntoInstanceof() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_07);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.OBSOLETE_CAST,
				"B to B")
			.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.OBSOLETE_CAST,
				"C to C");
	}

	@Test
	@Tag("compileToJava")
	@DisplayName("Compiling explicit cast to variable into instanceof")
	public void compileExplicitCastToVariableIntoInstanceof() throws Exception {
		getCompileHelper().compile(SARL_CODE_07, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug996.Bug996Case");
			assertEquals(JAVA_CODE_07, actual);
		});
	}


	private static final String SARL_CODE_08 = multilineString(
			"package io.sarl.lang.tests.bug996",
			"class A {}",
			"class B extends A {}",
			"class C extends A {}",
			"class Bug996Case {",
			"  def fct(param : A) : A {",
			"    if (param instanceof B) {",
			"        var xxx = param",
			"        return xxx",
			"    } else if (param instanceof C) {",
			"       var xxx = param",
			"       return xxx",
			"    }",
			"  }",
			"}");

	private static final String JAVA_CODE_08 = multilineString(
			"package io.sarl.lang.tests.bug996;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")", 
			"@SuppressWarnings(\"all\")", 
			"public class Bug996Case {",
			"  @Pure",
			"  public A fct(final A param) {",
			"    if ((param instanceof B)) {",
			"      B xxx = ((B)param);",
			"      return xxx;",
			"    } else {",
			"      if ((param instanceof C)) {",
			"        C xxx_1 = ((C)param);",
			"        return xxx_1;",
			"      }",
			"    }",
			"    return null;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Bug996Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	@DisplayName("Parsing implicit cast to variable into instanceof")
	public void parsingImplicitCastToVariableIntoInstanceof() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_08);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	@DisplayName("Parsing implicit cast to variable into instanceof")
	public void compileimplicitCastToVariableIntoInstanceof() throws Exception {
		getCompileHelper().compile(SARL_CODE_08, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug996.Bug996Case");
			assertEquals(JAVA_CODE_08, actual);
		});
	}

}
