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

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Invalid pure annotation for static function with exception.
 *
 * <p>https://github.com/sarl/sarl/issues/929
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/929"
 */
@DisplayName("Bug #929")
@SuppressWarnings("all")
@Tag("core")
@Tag("compileToJava")
public class Bug929Test extends AbstractSarlTest {

	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug929",
			"class X {",
			"    static def neverReturn(param : Object = null) {",
			"       throw new Exception",
			"    }",
			"}",
			"",
			"class Test {",
			"   def fail(m : String) : void {println(m)}",
			"   def test {",
			"      try {",
			"         X::neverReturn",
			"         fail(\"Early exit exception is expected\")",
			"      } catch (e : Exception) {",
			"      }",
			"   }",
			"}");

	private static final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug929;",
			"",
			"import io.sarl.lang.annotation.DefaultValue;",
			"import io.sarl.lang.annotation.DefaultValueSource;",
			"import io.sarl.lang.annotation.DefaultValueUse;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSourceCode;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Exceptions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  @DefaultValueSource",
			"  public static void neverReturn(@DefaultValue(\"io.sarl.lang.tests.bug929.X#NEVERRETURN_0\") final Object param) {",
			"    try {",
			"      throw new Exception();",
			"    } catch (Throwable _e) {",
			"      throw Exceptions.sneakyThrow(_e);",
			"    }",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter param",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"null\")",
			"  private static Object $DEFAULT_VALUE$NEVERRETURN_0() {",
			"    return null;",
			"  }",
			"  ",
			"  @DefaultValueUse(\"java.lang.Object\")",
			"  @SyntheticMember",
			"  public static void neverReturn() {",
			"    neverReturn($DEFAULT_VALUE$NEVERRETURN_0());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void compiling01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug929.X");
			assertEquals(JAVA_CODE_01, actual);
		});
	}

	private static final String SARL_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug929",
			"abstract class K {",
			"  abstract def getContext : Object",
			"}",
			"class X {",
			"    var k : K",
			"    private def ensureKernelInstance : K {",
			"       this.k = null",
			"       return this.k",
			"    }",
			"    def startWithoutAgent : Object {",
			"       ensureKernelInstance().context",
			"    }",
			"}");

	private static final String JAVA_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug929;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  private K k;",
			"  ",
			"  private K ensureKernelInstance() {",
			"    this.k = null;",
			"    return this.k;",
			"  }",
			"  ",
			"  public Object startWithoutAgent() {",
			"    return this.ensureKernelInstance().getContext();",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    return super.equals(obj);",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    return result;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void compiling02() throws Exception {
		getCompileHelper().compile(SARL_CODE_02, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug929.X");
			assertEquals(JAVA_CODE_02, actual);
		});
	}

}
