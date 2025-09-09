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

package io.sarl.lang.tests.bugs.to00999;

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.tests.api.AbstractSarlTest;

/** Testing class for issue: @Accessors does not work on inner classes?
 *
 * <p>https://github.com/sarl/sarl/issues/891
 *
 * @author $Author: sgalland$
 * @author $Author: alombard$
 * @version compiler.tests 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 * @see "https://github.com/sarl/sarl/issues/891"
 */
@DisplayName("Bug #891")
@SuppressWarnings("all")
@Tag("core")
@Tag("compileToJava")
public class Bug891Test extends AbstractSarlTest {

	@Inject
	private CompilationTestHelper compiler;

	private static final String PUBLIC_GETTER_IN_CLASS_SARL = multilineString(
			"package io.sarl.lang.tests.bug891",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"import org.eclipse.xtend.lib.annotations.AccessorType",
			"class X {",
			"  static class Y {",
			"    @Accessors(AccessorType::PUBLIC_GETTER)",
			"    var field : int",
			"  }",
			"  def test(y : Y) : int {",
			"    y.field",
			"  }",
			"}");

	private static final String PUBLIC_GETTER_IN_CLASS_JAVA = multilineString(
			"package io.sarl.lang.tests.bug891;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtend.lib.annotations.AccessorType;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"  @XbaseGenerated",
			"  public static class Y {",
			"    @Accessors(AccessorType.PUBLIC_GETTER)",
			"    private int field;",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public boolean equals(final Object obj) {",
			"      if (this == obj)",
			"        return true;",
			"      if (obj == null)",
			"        return false;",
			"      if (getClass() != obj.getClass())",
			"        return false;",
			"      Y other = (Y) obj;",
			"      if (other.field != this.field)",
			"        return false;",
			"      return super.equals(obj);",
			"    }",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public int hashCode() {",
			"      int result = super.hashCode();",
			"      final int prime = 31;",
			"      result = prime * result + Integer.hashCode(this.field);",
			"      return result;",
			"    }",
			"    ",
			"    @SyntheticMember",
			"    public Y() {",
			"      super();",
			"    }",
			"    ",
			"    @Pure",
			"    public int getField() {",
			"      return this.field;",
			"    }",
			"  }",
			"  ",
			"  @Pure",
			"  public int test(final X.Y y) {",
			"    return y.field;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void publicGetterInClass() throws Exception {
		this.compiler.compile(PUBLIC_GETTER_IN_CLASS_SARL, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug891.X");
			assertEquals(PUBLIC_GETTER_IN_CLASS_JAVA, actual);
		});
	}

	private static final String PUBLIC_GETTER_IN_AGENT_SARL = multilineString(
			"package io.sarl.lang.tests.bug891",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"import org.eclipse.xtend.lib.annotations.AccessorType",
			"agent X {",
			"  static class Y {",
			"    @Accessors(AccessorType::PUBLIC_GETTER)",
			"    var field : int",
			"  }",
			"  def test(y : Y) : int {",
			"    y.field",
			"  }",
			"}");

	private static final String PUBLIC_GETTER_IN_AGENT_JAVA = multilineString(
			"package io.sarl.lang.tests.bug891;",
			"",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import jakarta.inject.Inject;",
			"import java.util.UUID;",
			"import org.eclipse.xtend.lib.annotations.AccessorType;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class X extends Agent {",
			"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"  @XbaseGenerated",
			"  protected static class Y {",
			"    @Accessors(AccessorType.PUBLIC_GETTER)",
			"    private int field;",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public boolean equals(final Object obj) {",
			"      if (this == obj)",
			"        return true;",
			"      if (obj == null)",
			"        return false;",
			"      if (getClass() != obj.getClass())",
			"        return false;",
			"      Y other = (Y) obj;",
			"      if (other.field != this.field)",
			"        return false;",
			"      return super.equals(obj);",
			"    }",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public int hashCode() {",
			"      int result = super.hashCode();",
			"      final int prime = 31;",
			"      result = prime * result + Integer.hashCode(this.field);",
			"      return result;",
			"    }",
			"    ",
			"    @SyntheticMember",
			"    public Y() {",
			"      super();",
			"    }",
			"    ",
			"    @Pure",
			"    public int getField() {",
			"      return this.field;",
			"    }",
			"  }",
			"  ",
			"  @Pure",
			"  protected int test(final X.Y y) {",
			"    return y.field;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public X(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	public void publicGetterInAgent() throws Exception {
		this.compiler.compile(PUBLIC_GETTER_IN_AGENT_SARL, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug891.X");
			assertEquals(PUBLIC_GETTER_IN_AGENT_JAVA, actual);
		});
	}

}
