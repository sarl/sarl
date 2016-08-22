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
package io.sarl.lang.tests.compilation.general;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class InlineFunctionTest extends AbstractSarlTest {

	@Inject
	private CompilationTestHelper compiler;

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
				"import io.sarl.lang.annotation.SarlSpecification;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C3 {",
				"  public int fct2() {",
				"    C2 x = new C2();",
				"    int y = 1;",
				"    return y;",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
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
				"import io.sarl.lang.annotation.SarlSpecification;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C2 {",
				"  public int fct2() {",
				"    C1 x = new C1();",
				"    int y = 1;",
				"    return y;",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
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
				"import io.sarl.lang.annotation.SarlSpecification;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C3 {",
				"  public int fct2() {",
				"    C2 x = new C2();",
				"    int y = x.fct();",
				"    return y;",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC3, r.getGeneratedCode("C3"));
		});
	}
	
	@Test
	public void inlineWithDirectAnnotation() throws Exception {
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
				"import io.sarl.lang.annotation.SarlSpecification;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C2 {",
				"  public int fct2() {",
				"    C1 x = new C1();",
				"    int y = 1;",
				"    return y;",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC2, r.getGeneratedCode("C2"));
		});
	}

	@Test
	public void booleanLiteral_true() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct { return true }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"true\", constantExpression = true)",
				"  public boolean fct() {",
				"    return true;",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void booleanLiteral_false() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct { return false }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"false\", constantExpression = true)",
				"  public boolean fct() {",
				"    return false;",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void nullLiteral() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct { return null }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"null\", constantExpression = true)",
				"  public Object fct() {",
				"    return null;",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void numberLiteral() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct { return 123.456 }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"123.456\", constantExpression = true)",
				"  public double fct() {",
				"    return 123.456;",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void numericExpression() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct { return 123.456 + 34 / 2 }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"140.45600000000002\", constantExpression = true)",
				"  public double fct() {",
				"    return (123.456 + (34 / 2));",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void stringLiteral() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct { return \"abc\" }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"\\\"abc\\\"\", constantExpression = true)",
				"  public String fct() {",
				"    return \"abc\";",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void stringExpression() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct { return \"abc\" + \"xyz\" }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"\\\"abcxyz\\\"\", constantExpression = true)",
				"  public String fct() {",
				"    return (\"abc\" + \"xyz\");",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void typeLiteral() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct { return typeof(Integer) }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"Integer.class\", constantExpression = true)",
				"  public Class<Integer> fct() {",
				"    return Integer.class;",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void typeExpression() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct { return Integer }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"Integer.class\", constantExpression = true)",
				"  public Class<Integer> fct() {",
				"    return Integer.class;",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void castOperator() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct { return null as Integer }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"(Integer)null\", constantExpression = true)",
				"  public Integer fct() {",
				"    return ((Integer) null);",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

	@Test
	public void instanceofOperator() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def fct { return null instanceof Integer }",
				"}",
				"");
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inline(value = \"null instanceof Integer\", constantExpression = true)",
				"  public boolean fct() {",
				"    return (null instanceof Integer);",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1, r.getGeneratedCode("C1"));
		});
	}

}