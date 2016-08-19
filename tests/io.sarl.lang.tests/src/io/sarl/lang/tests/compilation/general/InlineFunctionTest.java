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

import static org.junit.Assert.assertEquals;

import com.google.inject.Inject;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper.Result;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

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
				"  @Inline(value = \"true\")",
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
				"  @Inline(value = \"false\")",
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
				"  @Inline(value = \"null\")",
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
				"  @Inline(value = \"123.456\")",
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
				"  @Inline(value = \"\\\"abc\\\"\")",
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
	public void typeLiteral_typeof() throws Exception {
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
				"  @Inline(value = \"Integer.class\")",
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
				"  @Inline(value = \"(Integer)null\")",
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
				"  @Inline(value = \"null instanceof Integer\")",
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