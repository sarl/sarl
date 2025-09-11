/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.tests.api.AbstractSarlTest;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 */
@SuppressWarnings("all")
@DisplayName("Compilation: break")
@Tag("core")
@Tag("compileToJava")
public class BreakKeywordTest extends AbstractSarlTest {

	@Test
	public void insideWhileWithBranch() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    var b = a",
				"    while (b > 0) {",
				"      if (a == 5) break",
				"      b--",
				"    }",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import jakarta.inject.Inject;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void fct(final int a) {",
				"    int b = a;",
				"    while ((b > 0)) {",
				"      {",
				"        if ((a == 5)) {",
				"          break;",
				"        }",
				"        b--;",
				"      }",
				"    }",
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
	public void insideDoWhileWithBranch() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    var b = a",
				"    do {",
				"      if (a == 5) break",
				"      b--",
				"    } while (b > 0)",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import jakarta.inject.Inject;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void fct(final int a) {",
				"    int b = a;",
				"    do {",
				"      {",
				"        if ((a == 5)) {",
				"          break;",
				"        }",
				"        b--;",
				"      }",
				"    } while((b > 0));",
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
	public void insideForWithoutBranch() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(b : 0..a) {",
				"      break",
				"    }",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import jakarta.inject.Inject;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.IntegerRange;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void fct(final int a) {",
				"    IntegerRange _upTo = new IntegerRange(0, a);",
				"    for (final Integer b : _upTo) {",
				"      break;",
				"    }",
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
	public void insideForWithBranch() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(b : 0..a) {",
				"      if (b == 5) break",
				"    }",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import jakarta.inject.Inject;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.IntegerRange;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void fct(final int a) {",
				"    IntegerRange _upTo = new IntegerRange(0, a);",
				"    for (final Integer b : _upTo) {",
				"      if ((b != null && (b.intValue() == 5))) {",
				"        break;",
				"      }",
				"    }",
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
	public void insideBasicForWithoutBranch() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(var b = 0; b < a; b++) {",
				"      break",
				"    }",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import jakarta.inject.Inject;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void fct(final int a) {",
				"    for (int b = 0; (b < a); b++) {",
				"      break;",
				"    }",
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
	public void insideBasicForWithBranch() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(var b = 0; b < a; b++) {",
				"      if (b == 5) break",
				"    }",
				"  }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import jakarta.inject.Inject;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void fct(final int a) {",
				"    for (int b = 0; (b < a); b++) {",
				"      if ((b == 5)) {",
				"        break;",
				"      }",
				"    }",
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
