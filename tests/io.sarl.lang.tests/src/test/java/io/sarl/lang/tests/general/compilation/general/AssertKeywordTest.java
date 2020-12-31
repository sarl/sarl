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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Compilation: assert")
@Tag("core")
@Tag("compileToJava")
public class AssertKeywordTest extends AbstractSarlTest {

	@Test
	public void assertTrue() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert true",
				"    return x + 1",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @Pure",
				"  protected int fct(final int x) {",
				"    assert true;",
				"    return (x + 1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void assertBooleanTrue() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert Boolean::TRUE",
				"    return x + 1",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @Pure",
				"  protected int fct(final int x) {",
				"    assert true;",
				"    return (x + 1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void assertFalse() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert false",
				"    return x + 1",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @Pure",
				"  protected int fct(final int x) {",
				"    assert false;",
				"    return (x + 1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void assertBooleanFalse() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert Boolean::FALSE",
				"    return x + 1",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @Pure",
				"  protected int fct(final int x) {",
				"    assert false;",
				"    return (x + 1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void assertOnParameter() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert x > 0",
				"    return x + 1",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @Pure",
				"  protected int fct(final int x) {",
				"    class $AssertEvaluator$ {",
				"      final boolean $$result;",
				"      $AssertEvaluator$() {",
				"        this.$$result = (x > 0);",
				"      }",
				"    }",
				"    assert new $AssertEvaluator$().$$result;",
				"    return (x + 1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void assertComplexBooleanExpression() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    var y = x + 1",
				"    assert x > 0 && y < 100",
				"    return x + 1",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @Pure",
				"  protected int fct(final int x) {",
				"    int y = (x + 1);",
				"    class $AssertEvaluator$ {",
				"      final boolean $$result;",
				"      $AssertEvaluator$(final int y) {",
				"        this.$$result = ((x > 0) && (y < 100));",
				"      }",
				"    }",
				"    assert new $AssertEvaluator$(y).$$result;",
				"    return (x + 1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void assertTrueWithMessage() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert true, \"Hello world!\"",
				"    return x + 1",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @Pure",
				"  protected int fct(final int x) {",
				"    assert true : \"Hello world!\";",
				"    return (x + 1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void assertComplexBooleanExpressionWithMessage() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    var y = x + 1",
				"    assert x > 0 && y < 100, \"That's all, folks!\"",
				"    return x + 1",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @Pure",
				"  protected int fct(final int x) {",
				"    int y = (x + 1);",
				"    class $AssertEvaluator$ {",
				"      final boolean $$result;",
				"      $AssertEvaluator$(final int y) {",
				"        this.$$result = ((x > 0) && (y < 100));",
				"      }",
				"    }",
				"    assert new $AssertEvaluator$(y).$$result : \"That\\'s all, folks!\";",
				"    return (x + 1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Inject",
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
				"    super(arg0, arg1, arg2);",
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void assertWithNotFinalLocalVariable() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(b : boolean) : Object {",
				"    val x = 1",
				"    var y = x + 1",
				"    var node = new Object",
				"    if (b) {",
				"      node = new Integer(1)",
				"    }",
				"    assert node.toString !== null && b && x < 100 && y > 0",
				"    return node",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @Pure",
				"  protected Object fct(final boolean b) {",
				"    final int x = 1;", 
				"    int y = (x + 1);", 
				"    Object node = new Object();", 
				"    if (b) {", 
				"      Integer _integer = new Integer(1);", 
				"      node = _integer;", 
				"    }", 
				"    class $AssertEvaluator$ {", 
				"      final boolean $$result;", 
				"      $AssertEvaluator$(final Object node, final int y) {", 
				"        this.$$result = ((((node.toString() != null) && b) && (x < 100)) && (y > 0));", 
				"      }", 
				"    }", 
				"    assert new $AssertEvaluator$(node, y).$$result;", 
				"    return node;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void multipleAssertsWithNotFinalLocalVariable() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(b : boolean) : Object {",
				"    val x = 1",
				"    var y = x + 1",
				"    var node = new Object",
				"    if (b) {",
				"      node = new Integer(1)",
				"    }",
				"    assert node.toString !== null && b",
				"    assert x < 100 && y > 0",
				"    return node",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @Pure",
				"  protected Object fct(final boolean b) {",
				"    final int x = 1;", 
				"    int y = (x + 1);", 
				"    Object node = new Object();", 
				"    if (b) {", 
				"      Integer _integer = new Integer(1);", 
				"      node = _integer;", 
				"    }", 
				"    class $AssertEvaluator$ {", 
				"      final boolean $$result;", 
				"      $AssertEvaluator$(final Object node) {", 
				"        this.$$result = ((node.toString() != null) && b);", 
				"      }", 
				"    }", 
				"    assert new $AssertEvaluator$(node).$$result;", 
				"    class $AssertEvaluator$_1 {", 
				"      final boolean $$result;", 
				"      $AssertEvaluator$_1(final int y) {", 
				"        this.$$result = ((x < 100) && (y > 0));", 
				"      }", 
				"    }", 
				"    assert new $AssertEvaluator$_1(y).$$result;", 
				"    return node;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void assertWithFields() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  var field1 : int",
				"  val field2 : int = 4",
				"  def fct : int {",
				"    assert field1 > 0 && field2 < 100",
				"    return this.field2",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  private int field1;",
				"  ",
				"  private final int field2 = 4;",
				"  ",
				"  @Pure",
				"  protected int fct() {",
				"    class $AssertEvaluator$ {", 
				"      final boolean $$result;", 
				"      $AssertEvaluator$() {", 
				"        this.$$result = ((A1.this.field1 > 0) && (A1.this.field2 < 100));", 
				"      }", 
				"    }", 
				"    assert new $AssertEvaluator$().$$result;", 
				"    return this.field2;",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    A1 other = (A1) obj;",
				"    if (other.field1 != this.field1)",
				"      return false;",
				"    if (other.field2 != this.field2)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.field1);",
				"    result = prime * result + Integer.hashCode(this.field2);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void assertWithFieldsInEnclosingType() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  var field1 : int",
				"  val field2 : int = 4",
				"  class InnerType {",
				"    def fct : int {",
				"      assert field1 > 0 && field2 < 100",
				"      return A1.this.field2",
				"    }",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
				"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")", 
				"  protected class InnerType {", 
				"    @Pure", 
				"    public int fct() {", 
				"      class $AssertEvaluator$ {", 
				"        final boolean $$result;", 
				"        $AssertEvaluator$() {", 
				"          this.$$result = ((A1.this.field1 > 0) && (A1.this.field2 < 100));", 
				"        }", 
				"      }", 
				"      assert new $AssertEvaluator$().$$result;", 
				"      return A1.this.field2;", 
				"    }", 
				"    ", 
				"    @SyntheticMember", 
				"    public InnerType() {", 
				"      super();", 
				"    }", 
				"  }",
				"  ",
				"  private int field1;",
				"  ",
				"  private final int field2 = 4;",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    A1 other = (A1) obj;",
				"    if (other.field1 != this.field1)",
				"      return false;",
				"    if (other.field2 != this.field2)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.field1);",
				"    result = prime * result + Integer.hashCode(this.field2);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

}
