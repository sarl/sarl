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

package io.sarl.lang.tests.bugs.to00699;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Test issue #459: Purge task launched by a behavior when unregistering this behavior.
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see https://github.com/sarl/sarl/issues/459
 */
@DisplayName("Bug #459")
@SuppressWarnings("all")
@Tag("core")
public class Bug459Test {

	protected static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 { }");

	protected static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 {",
			"  def myfct(a : int, b : String = \"a\") : char",
			"}");

	protected static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 { def myfct1(a : int, b : String = null) }",
			"capacity C2 extends C1 { }");

	protected static final String SNIPSET4 = multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 { def myfct1(a : int, b : String = null) }",
			"capacity C2 extends C1 { def myfct2(a : int, b : float = 0) : double }");

	protected static final String SNIPSET5 = multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 { def myfct1(a : int, b : String = null) }",
			"capacity C2 { def myfct2(a : int) : char }",
			"capacity C3 extends C1, C2 { }");

	protected static final String SNIPSET6 = multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 { def myfct1(a : int, b : String = null) }",
			"capacity C2 { def myfct2(a : int) : char }",
			"capacity C3 extends C1, C2 { def myfct3(a : int, b : float = 0) : double }");

	protected static final String SNIPSET7 = multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 {",
			"  def myfct(a : int, b : String*) : char",
			"}");

	@Nested
	@Tag("compileToJava")
	public class Compilation extends AbstractSarlTest {

		@Test
		public void withoutFunction() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET1,
					multilineString(
							"package io.sarl.lang.bug459;",
							"",
							"import io.sarl.lang.annotation.SarlElementType;",
							"import io.sarl.lang.annotation.SarlSpecification;",
							"import io.sarl.lang.core.AgentTrait;",
							"import io.sarl.lang.core.Capacity;",
							"",
							"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
							"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
							"@SuppressWarnings(\"all\")",
							"public interface C1 extends Capacity {",
							"  /**",
							"   * @ExcludeFromApidoc",
							"   */",
							"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
							"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
							"      super(capacity, caller);",
							"    }",
							"  }",
							"}",
							""));
		}

		@Test
		public void withFunction() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET2,
					multilineString(
							"package io.sarl.lang.bug459;",
							"",
							"import io.sarl.lang.annotation.DefaultValue;",
							"import io.sarl.lang.annotation.DefaultValueSource;",
							"import io.sarl.lang.annotation.DefaultValueUse;",
							"import io.sarl.lang.annotation.SarlElementType;",
							"import io.sarl.lang.annotation.SarlSourceCode;",
							"import io.sarl.lang.annotation.SarlSpecification;",
							"import io.sarl.lang.annotation.SyntheticMember;",
							"import io.sarl.lang.core.AgentTrait;",
							"import io.sarl.lang.core.Capacity;",
							"import org.eclipse.xtext.xbase.lib.Pure;",
							"",
							"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
							"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
							"@SuppressWarnings(\"all\")",
							"public interface C1 extends Capacity {",
							"  @DefaultValueSource",
							"  char myfct(final int a, @DefaultValue(\"io.sarl.lang.bug459.C1#MYFCT_0\") final String b);",
							"  ",
							"  /**",
							"   * Default value for the parameter b",
							"   */",
							"  @Pure",
							"  @SyntheticMember",
							"  @SarlSourceCode(\"\\\"a\\\"\")",
							"  default String $DEFAULT_VALUE$MYFCT_0() {",
							"    return \"a\";",
							"  }",
							"  ",
							"  @DefaultValueUse(\"int,java.lang.String\")",
							"  @SyntheticMember",
							"  default char myfct(final int a) {",
							"    return myfct(a, $DEFAULT_VALUE$MYFCT_0());",
							"  }",
							"  ",
							"  /**",
							"   * @ExcludeFromApidoc",
							"   */",
							"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
							"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
							"      super(capacity, caller);",
							"    }",
							"    ",
							"    public char myfct(final int a, final String b) {",
							"      try {",
							"        ensureCallerInLocalThread();",
							"        return this.capacity.myfct(a, b);",
							"      } finally {",
							"        resetCallerInLocalThread();",
							"      }",
							"    }",
							"    ",
							"    public char myfct(final int a) {",
							"      try {",
							"        ensureCallerInLocalThread();",
							"        return this.capacity.myfct(a);",
							"      } finally {",
							"        resetCallerInLocalThread();",
							"      }",
							"    }",
							"  }",
							"}",
							""));
		}

		@Test
		public void singleInheritanceWithoutFunction() throws Exception {
			getCompileHelper().compile(SNIPSET3,
					(it) -> {
						assertEquals(multilineString(
							"package io.sarl.lang.bug459;",
							"",
							"import io.sarl.lang.annotation.SarlElementType;",
							"import io.sarl.lang.annotation.SarlSpecification;",
							"import io.sarl.lang.core.AgentTrait;",
							"",
							"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
							"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
							"@SuppressWarnings(\"all\")",
							"public interface C2 extends C1 {",
							"  /**",
							"   * @ExcludeFromApidoc",
							"   */",
							"  class ContextAwareCapacityWrapper<C extends C2> extends C1.ContextAwareCapacityWrapper<C> implements C2 {",
							"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
							"      super(capacity, caller);",
							"    }",
							"  }",
							"}",
							""), it.getGeneratedCode("io.sarl.lang.bug459.C2"));
					});
		}

		@Test
		public void singleInheritanceWithFunction() throws Exception {
			getCompileHelper().compile(SNIPSET4,
					(it) -> {
						assertEquals(multilineString(
							"package io.sarl.lang.bug459;",
							"",
							"import io.sarl.lang.annotation.DefaultValue;",
							"import io.sarl.lang.annotation.DefaultValueSource;",
							"import io.sarl.lang.annotation.DefaultValueUse;",
							"import io.sarl.lang.annotation.SarlElementType;",
							"import io.sarl.lang.annotation.SarlSourceCode;",
							"import io.sarl.lang.annotation.SarlSpecification;",
							"import io.sarl.lang.annotation.SyntheticMember;",
							"import io.sarl.lang.core.AgentTrait;",
							"import org.eclipse.xtext.xbase.lib.Pure;",
							"",
							"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
							"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
							"@SuppressWarnings(\"all\")",
							"public interface C2 extends C1 {",
							"  @DefaultValueSource",
							"  double myfct2(final int a, @DefaultValue(\"io.sarl.lang.bug459.C2#MYFCT2_0\") final float b);",
							"  ",
							"  /**",
							"   * Default value for the parameter b",
							"   */",
							"  @Pure",
							"  @SyntheticMember",
							"  @SarlSourceCode(\"0\")",
							"  default float $DEFAULT_VALUE$MYFCT2_0() {",
							"    return 0;",
							"  }",
							"  ",
							"  @DefaultValueUse(\"int,float\")",
							"  @SyntheticMember",
							"  default double myfct2(final int a) {",
							"    return myfct2(a, $DEFAULT_VALUE$MYFCT2_0());",
							"  }",
							"  ",
							"  /**",
							"   * @ExcludeFromApidoc",
							"   */",
							"  class ContextAwareCapacityWrapper<C extends C2> extends C1.ContextAwareCapacityWrapper<C> implements C2 {",
							"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
							"      super(capacity, caller);",
							"    }",
							"    ",
							"    public double myfct2(final int a, final float b) {",
							"      try {",
							"        ensureCallerInLocalThread();",
							"        return this.capacity.myfct2(a, b);",
							"      } finally {",
							"        resetCallerInLocalThread();",
							"      }",
							"    }",
							"    ",
							"    public double myfct2(final int a) {",
							"      try {",
							"        ensureCallerInLocalThread();",
							"        return this.capacity.myfct2(a);",
							"      } finally {",
							"        resetCallerInLocalThread();",
							"      }",
							"    }",
							"  }",
							"}",
							""), it.getGeneratedCode("io.sarl.lang.bug459.C2"));
					});
		}

		@Test
		public void multiInheritanceWithoutFunction() throws Exception {
			getCompileHelper().compile(SNIPSET5,
					(it) -> {
						assertEquals(multilineString(
							"package io.sarl.lang.bug459;",
							"",
							"import io.sarl.lang.annotation.SarlElementType;",
							"import io.sarl.lang.annotation.SarlSpecification;",
							"import io.sarl.lang.core.AgentTrait;",
							"",
							"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
							"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
							"@SuppressWarnings(\"all\")",
							"public interface C3 extends C1, C2 {",
							"  /**",
							"   * @ExcludeFromApidoc",
							"   */",
							"  class ContextAwareCapacityWrapper<C extends C3> extends C1.ContextAwareCapacityWrapper<C> implements C3 {",
							"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
							"      super(capacity, caller);",
							"    }",
							"    ",
							"    public char myfct2(final int a) {",
							"      try {",
							"        ensureCallerInLocalThread();",
							"        return this.capacity.myfct2(a);",
							"      } finally {",
							"        resetCallerInLocalThread();",
							"      }",
							"    }",
							"  }",
							"}",
							""), it.getGeneratedCode("io.sarl.lang.bug459.C3"));
					});
		}

		@Test
		public void multiInheritanceWithFunction() throws Exception {
			getCompileHelper().compile(SNIPSET6,
					(it) -> {
						assertEquals(multilineString(
								"package io.sarl.lang.bug459;",
								"",
								"import io.sarl.lang.annotation.DefaultValue;",
								"import io.sarl.lang.annotation.DefaultValueSource;",
								"import io.sarl.lang.annotation.DefaultValueUse;",
								"import io.sarl.lang.annotation.SarlElementType;",
								"import io.sarl.lang.annotation.SarlSourceCode;",
								"import io.sarl.lang.annotation.SarlSpecification;",
								"import io.sarl.lang.annotation.SyntheticMember;",
								"import io.sarl.lang.core.AgentTrait;",
								"import org.eclipse.xtext.xbase.lib.Pure;",
								"",
								"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
								"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
								"@SuppressWarnings(\"all\")",
								"public interface C3 extends C1, C2 {",
								"  @DefaultValueSource",
								"  double myfct3(final int a, @DefaultValue(\"io.sarl.lang.bug459.C3#MYFCT3_0\") final float b);",
								"  ",
								"  /**",
								"   * Default value for the parameter b",
								"   */",
								"  @Pure",
								"  @SyntheticMember",
								"  @SarlSourceCode(\"0\")",
								"  default float $DEFAULT_VALUE$MYFCT3_0() {",
								"    return 0;",
								"  }",
								"  ",
								"  @DefaultValueUse(\"int,float\")",
								"  @SyntheticMember",
								"  default double myfct3(final int a) {",
								"    return myfct3(a, $DEFAULT_VALUE$MYFCT3_0());",
								"  }",
								"  ",
								"  /**",
								"   * @ExcludeFromApidoc",
								"   */",
								"  class ContextAwareCapacityWrapper<C extends C3> extends C1.ContextAwareCapacityWrapper<C> implements C3 {",
								"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
								"      super(capacity, caller);",
								"    }",
								"    ",
								"    public double myfct3(final int a, final float b) {",
								"      try {",
								"        ensureCallerInLocalThread();",
								"        return this.capacity.myfct3(a, b);",
								"      } finally {",
								"        resetCallerInLocalThread();",
								"      }",
								"    }",
								"    ",
								"    public double myfct3(final int a) {",
								"      try {",
								"        ensureCallerInLocalThread();",
								"        return this.capacity.myfct3(a);",
								"      } finally {",
								"        resetCallerInLocalThread();",
								"      }",
								"    }",
								"    ",
								"    public char myfct2(final int a) {",
								"      try {",
								"        ensureCallerInLocalThread();",
								"        return this.capacity.myfct2(a);",
								"      } finally {",
								"        resetCallerInLocalThread();",
								"      }",
								"    }",
								"  }",
								"}",
								""), it.getGeneratedCode("io.sarl.lang.bug459.C3"));
					});
		}

		@Test
		public void withVarArg() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET7,
					multilineString(
							"package io.sarl.lang.bug459;",
							"",
							"import io.sarl.lang.annotation.SarlElementType;",
							"import io.sarl.lang.annotation.SarlSpecification;",
							"import io.sarl.lang.core.AgentTrait;",
							"import io.sarl.lang.core.Capacity;",
							"",
							"@FunctionalInterface",
							"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
							"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
							"@SuppressWarnings(\"all\")",
							"public interface C1 extends Capacity {",
							"  char myfct(final int a, final String... b);",
							"  ",
							"  /**",
							"   * @ExcludeFromApidoc",
							"   */",
							"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
							"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
							"      super(capacity, caller);",
							"    }",
							"    ",
							"    public char myfct(final int a, final String... b) {",
							"      try {",
							"        ensureCallerInLocalThread();",
							"        return this.capacity.myfct(a, b);",
							"      } finally {",
							"        resetCallerInLocalThread();",
							"      }",
							"    }",
							"  }",
							"}",
							""));
		}

	}

	@Nested
	@Tag("sarlValidation")
	public class Parsing extends AbstractSarlTest {

		@Test
		public void withoutFunction() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET1);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		public void withFunction() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET2);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		public void singleInheritanceWithoutFunction() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET3);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		public void singleInheritanceWithFunction() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET4);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		public void multiInheritanceWithoutFunction() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET5);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		public void multiInheritanceWithFunction() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET6);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		public void withVarArg() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET7);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

	}

}
