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
package io.sarl.lang.tests.general.compilation.general;

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.resource.FileExtensionProvider;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper.Result;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.compiler.GeneratorConfig2;
import io.sarl.lang.compiler.GeneratorConfigProvider2;
import io.sarl.lang.compiler.IGeneratorConfigProvider2;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Compilation: @Inline")
@Tag("core")
@Tag("compileToJava")
public class InlineFunctionTest extends AbstractSarlTest {

	@Inject
	private FileExtensionProvider extensionProvider;
	
	@Inject
	private IGeneratorConfigProvider2 generatorConfigProvider2;

	private GeneratorConfig2 config;

	private void compileWithInline(CharSequence source, IAcceptor<Result> acceptor) throws IOException {
		String fileName = "MyFile." + extensionProvider.getPrimaryFileExtension();
		CompilationTestHelper compiler = getCompileHelper();
		ResourceSet set = compiler.resourceSet(new Pair<String, CharSequence>(fileName, source));
		if (this.config == null) {
			this.config = this.generatorConfigProvider2.get(null);
			((GeneratorConfigProvider2) this.generatorConfigProvider2).install(set, config);
		}
		this.config.setGenerateInlineAnnotation(true);
		compiler.compile(set, acceptor);
	}
	
	@Test
	public void noInlineWithStaticInheritedAnnotation() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static def fct : int { return 1 }",
				"}",
				"class C2 extends C1 { }",
				"class C3 {",
				"  def fct2 : int {",
				"    var x = new C2",
				"    var y = x.fct",
				"    return y",
				"  }",
				"}",
				"");
		final String expectedC3 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C3 {",
				"  @Pure",
				"  public int fct2() {",
				"    C2 x = new C2();",
				"    int y = 1;",
				"    return y;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C3() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC3, r.getGeneratedCode("C3"));
		});
	}
	
	@Test
	public void inlineWithStaticDirectAnnotation() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static def fct : int { return 1 }",
				"}",
				"class C2 {",
				"  def fct2 : int {",
				"    var x = new C1",
				"    var y = x.fct",
				"    return y",
				"  }",
				"}",
				"");
		final String expectedC2 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C2 {",
				"  @Pure",
				"  public int fct2() {",
				"    C1 x = new C1();",
				"    int y = 1;",
				"    return y;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C2() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC2, r.getGeneratedCode("C2"));
		});
	}

	@Test
	public void noInlineWithInheritedAnnotation() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct : int { return 1 }",
				"}",
				"class C2 extends C1 {",
				"  override fct : int { return super.fct * 100 }",
				"}",
				"class C3 {",
				"  def fct2 : int {",
				"    var x = new C2",
				"    var y = x.fct",
				"    return y",
				"  }",
				"}",
				"");
		final String expectedC3 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C3 {",
				"  @Pure",
				"  public int fct2() {",
				"    C2 x = new C2();",
				"    int y = x.fct();",
				"    return y;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C3() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC3, r.getGeneratedCode("C3"));
		});
	}
	
	@Test
	public void inlineWithDirectAnnotation01() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct : int { return 1 }",
				"}",
				"class C2 {",
				"  def fct2 : int {",
				"    var x = new C1",
				"    var y = x.fct",
				"    return y",
				"  }",
				"}",
				"");
		final String expectedC2 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C2 {",
				"  @Pure",
				"  public int fct2() {",
				"    C1 x = new C1();",
				"    int y = x.fct();",
				"    return y;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C2() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC2, r.getGeneratedCode("C2"));
		});
	}

	@Test
	public void inlineWithDirectAnnotation02() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  final def fct : int { return 1 }",
				"}",
				"class C2 {",
				"  def fct2 : int {",
				"    var x = new C1",
				"    var y = x.fct",
				"    return y",
				"  }",
				"}",
				"");
		final String expectedC2 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C2 {",
				"  @Pure",
				"  public int fct2() {",
				"    C1 x = new C1();",
				"    int y = 1;",
				"    return y;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C2() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC2, r.getGeneratedCode("C2"));
		});
	}

	@Test
	public void booleanLiteral_true() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static def fct { return true }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"true\", constantExpression = true)",
				"  @Pure",
				"  public static boolean fct() {",
				"    return true;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void booleanLiteral_false() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static def fct { return false }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"false\", constantExpression = true)",
				"  @Pure",
				"  public static boolean fct() {",
				"    return false;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void nullLiteral() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static def fct { return null }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"(Object)null\", constantExpression = true)",
				"  @Pure",
				"  public static Object fct() {",
				"    return null;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void numberLiteral() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static def fct { return 123.456 }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"123.456\", constantExpression = true)",
				"  @Pure",
				"  public static double fct() {",
				"    return 123.456;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void numericExpression() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static def fct { return 123.456 + 34 / 2 }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"140.45600000000002\", constantExpression = true)",
				"  @Pure",
				"  public static double fct() {",
				"    return (123.456 + (34 / 2));",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void stringLiteral() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static def fct { return \"abc\" }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"\\\"abc\\\"\", constantExpression = true)",
				"  @Pure",
				"  public static String fct() {",
				"    return \"abc\";",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void stringExpression01() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct { return \"abc\" + \"xyz\" }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Pure",
				"  public String fct() {",
				"    return (\"abc\" + \"xyz\");",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void stringExpression02() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  final def fct { return \"abc\" + \"xyz\" }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"\\\"abcxyz\\\"\", constantExpression = true)",
				"  @Pure",
				"  public final String fct() {",
				"    return (\"abc\" + \"xyz\");",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void stringExpression03() throws Exception {
		String source = multilineString(
				"final class C1 {",
				"  def fct { return \"abc\" + \"xyz\" }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public final class C1 {",
				"  @Inline(value = \"\\\"abcxyz\\\"\", constantExpression = true)",
				"  @Pure",
				"  public String fct() {",
				"    return (\"abc\" + \"xyz\");",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void typeLiteral() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static def fct { return typeof(Integer) }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"Integer.class\", constantExpression = true)",
				"  @Pure",
				"  public static Class<Integer> fct() {",
				"    return Integer.class;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void typeExpression() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static def fct { return Integer }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"Integer.class\", constantExpression = true)",
				"  @Pure",
				"  public static Class<Integer> fct() {",
				"    return Integer.class;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void castOperator() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static def fct { return null as Integer }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"(Integer)null\", constantExpression = true)",
				"  @Pure",
				"  public static Integer fct() {",
				"    return ((Integer)null);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void instanceofOperator() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static def fct { return null instanceof Integer }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"(null) instanceof Integer\", constantExpression = true)",
				"  @Pure",
				"  public static boolean fct() {",
				"    return (null instanceof Integer);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				""
				);
		compileWithInline(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

}
