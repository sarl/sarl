/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Invalid generic default value generation.
 *
 * <p>https://github.com/sarl/sarl/issues/723
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug723 extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug723",
			"class MyClass", 
			"{", 
			"  static def getSystemPropertyAsClass(type : Class<S>, name : String,", 
			"		defaultValue : Class<? extends S> = null) : Class<? extends S> with S {",
			"  }",
			"}");

	private final String EXPECTED1 = multilineString(
			"package io.sarl.lang.tests.bug723;",
			"",
			"import io.sarl.lang.annotation.DefaultValue;",
			"import io.sarl.lang.annotation.DefaultValueSource;",
			"import io.sarl.lang.annotation.DefaultValueUse;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSourceCode;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class MyClass {",
			"  @DefaultValueSource",
			"  @Pure",
			"  public static <S extends Object> Class<? extends S> getSystemPropertyAsClass(final Class<S> type, final String name, @DefaultValue(\"io.sarl.lang.tests.bug723.MyClass#GETSYSTEMPROPERTYASCLASS_0\") final Class<? extends S> defaultValue) {",
			"    return null;",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter defaultValue",
			"   */",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"null\")",
			"  private final static Class $DEFAULT_VALUE$GETSYSTEMPROPERTYASCLASS_0 = null;",
			"  ",
			"  @DefaultValueUse(\"java.lang.Class<S>,java.lang.String,java.lang.Class<? extends S>\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public static <S extends Object> Class<? extends S> getSystemPropertyAsClass(final Class<S> type, final String name) {",
			"    return getSystemPropertyAsClass(type, name, $DEFAULT_VALUE$GETSYSTEMPROPERTYASCLASS_0);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public MyClass() {",
			"    super();",
			"  }",
			"}",
			"");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug723",
			"class MyClass", 
			"{", 
			"  static def getSystemPropertyAsEnum(type : Class<S>, name : String,", 
			"		defaultValue : S = null) : S with S extends Enum<S> {",
			"  }",
			"}");

	private final String EXPECTED2 = multilineString(
			"package io.sarl.lang.tests.bug723;",
			"",
			"import io.sarl.lang.annotation.DefaultValue;",
			"import io.sarl.lang.annotation.DefaultValueSource;",
			"import io.sarl.lang.annotation.DefaultValueUse;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSourceCode;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class MyClass {",
			"  @DefaultValueSource",
			"  @Pure",
			"  public static <S extends Enum<S>> S getSystemPropertyAsEnum(final Class<S> type, final String name, @DefaultValue(\"io.sarl.lang.tests.bug723.MyClass#GETSYSTEMPROPERTYASENUM_0\") final S defaultValue) {",
			"    return null;",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter defaultValue",
			"   */",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"null\")",
			"  private final static Enum $DEFAULT_VALUE$GETSYSTEMPROPERTYASENUM_0 = null;",
			"  ",
			"  @DefaultValueUse(\"java.lang.Class<S>,java.lang.String,S\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public static <S extends Enum<S>> S getSystemPropertyAsEnum(final Class<S> type, final String name) {",
			"    return getSystemPropertyAsEnum(type, name, (S) $DEFAULT_VALUE$GETSYSTEMPROPERTYASENUM_0);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public MyClass() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parsing_01() throws Exception {
		SarlScript mas = file(SNIPSET1);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET1, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug723.MyClass");
			assertEquals(EXPECTED1, actual);
		});
	}

	@Test
	public void parsing_02() throws Exception {
		SarlScript mas = file(SNIPSET2);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET2, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug723.MyClass");
			assertEquals(EXPECTED2, actual);
		});
	}

}
