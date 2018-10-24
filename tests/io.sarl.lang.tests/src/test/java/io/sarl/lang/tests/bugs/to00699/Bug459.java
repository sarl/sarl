/*
 * Copyright (C) 2014-2018 the original authors or authors.
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

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

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
@RunWith(Suite.class)
@SuiteClasses({
	Bug459.Compilation.class,
	Bug459.Parsing.class,
})
@SuppressWarnings("all")
public class Bug459 {

	protected static final String SNIPSET1 = AbstractSarlTest.multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 { }");

	protected static final String SNIPSET2 = AbstractSarlTest.multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 {",
			"  def myfct(a : int, b : String = \"a\") : char",
			"}");

	protected static final String SNIPSET3 = AbstractSarlTest.multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 { def myfct1(a : int, b : String = null) }",
			"capacity C2 extends C1 { }");

	protected static final String SNIPSET4 = AbstractSarlTest.multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 { def myfct1(a : int, b : String = null) }",
			"capacity C2 extends C1 { def myfct2(a : int, b : float = 0) : double }");

	protected static final String SNIPSET5 = AbstractSarlTest.multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 { def myfct1(a : int, b : String = null) }",
			"capacity C2 { def myfct2(a : int) : char }",
			"capacity C3 extends C1, C2 { }");

	protected static final String SNIPSET6 = AbstractSarlTest.multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 { def myfct1(a : int, b : String = null) }",
			"capacity C2 { def myfct2(a : int) : char }",
			"capacity C3 extends C1, C2 { def myfct3(a : int, b : float = 0) : double }");

	protected static final String SNIPSET7 = AbstractSarlTest.multilineString(
			"package io.sarl.lang.bug459",
			"capacity C1 {",
			"  def myfct(a : int, b : String*) : char",
			"}");

	public static class Compilation extends AbstractSarlTest {

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
							"  public static class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
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
							"",
							"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
							"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
							"@SuppressWarnings(\"all\")",
							"public interface C1 extends Capacity {",
							"  @DefaultValueSource",
							"  public abstract char myfct(final int a, @DefaultValue(\"io.sarl.lang.bug459.C1#MYFCT_0\") final String b);",
							"  ",
							"  /**",
							"   * Default value for the parameter b",
							"   */",
							"  @SyntheticMember",
							"  @SarlSourceCode(\"\\\"a\\\"\")",
							"  public final static String $DEFAULT_VALUE$MYFCT_0 = \"a\";",
							"  ",
							"  @DefaultValueUse(\"int,java.lang.String\")",
							"  @SyntheticMember",
							"  public default char myfct(final int a) {",
							"    return myfct(a, $DEFAULT_VALUE$MYFCT_0);",
							"  }",
							"  ",
							"  /**",
							"   * @ExcludeFromApidoc",
							"   */",
							"  public static class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
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
							"import io.sarl.lang.bug459.C1;",
							"import io.sarl.lang.core.AgentTrait;",
							"",
							"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
							"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
							"@SuppressWarnings(\"all\")",
							"public interface C2 extends C1 {",
							"  /**",
							"   * @ExcludeFromApidoc",
							"   */",
							"  public static class ContextAwareCapacityWrapper<C extends C2> extends C1.ContextAwareCapacityWrapper<C> implements C2 {",
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
							"import io.sarl.lang.bug459.C1;",
							"import io.sarl.lang.core.AgentTrait;",
							"",
							"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
							"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
							"@SuppressWarnings(\"all\")",
							"public interface C2 extends C1 {",
							"  @DefaultValueSource",
							"  public abstract double myfct2(final int a, @DefaultValue(\"io.sarl.lang.bug459.C2#MYFCT2_0\") final float b);",
							"  ",
							"  /**",
							"   * Default value for the parameter b",
							"   */",
							"  @SyntheticMember",
							"  @SarlSourceCode(\"0\")",
							"  public final static float $DEFAULT_VALUE$MYFCT2_0 = 0;",
							"  ",
							"  @DefaultValueUse(\"int,float\")",
							"  @SyntheticMember",
							"  public default double myfct2(final int a) {",
							"    return myfct2(a, $DEFAULT_VALUE$MYFCT2_0);",
							"  }",
							"  ",
							"  /**",
							"   * @ExcludeFromApidoc",
							"   */",
							"  public static class ContextAwareCapacityWrapper<C extends C2> extends C1.ContextAwareCapacityWrapper<C> implements C2 {",
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
							"import io.sarl.lang.bug459.C1;",
							"import io.sarl.lang.bug459.C2;",
							"import io.sarl.lang.core.AgentTrait;",
							"",
							"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
							"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
							"@SuppressWarnings(\"all\")",
							"public interface C3 extends C1, C2 {",
							"  /**",
							"   * @ExcludeFromApidoc",
							"   */",
							"  public static class ContextAwareCapacityWrapper<C extends C3> extends C1.ContextAwareCapacityWrapper<C> implements C3 {",
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
								"import io.sarl.lang.bug459.C1;",
								"import io.sarl.lang.bug459.C2;",
								"import io.sarl.lang.core.AgentTrait;",
								"",
								"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
								"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
								"@SuppressWarnings(\"all\")",
								"public interface C3 extends C1, C2 {",
								"  @DefaultValueSource",
								"  public abstract double myfct3(final int a, @DefaultValue(\"io.sarl.lang.bug459.C3#MYFCT3_0\") final float b);",
								"  ",
								"  /**",
								"   * Default value for the parameter b",
								"   */",
								"  @SyntheticMember",
								"  @SarlSourceCode(\"0\")",
								"  public final static float $DEFAULT_VALUE$MYFCT3_0 = 0;",
								"  ",
								"  @DefaultValueUse(\"int,float\")",
								"  @SyntheticMember",
								"  public default double myfct3(final int a) {",
								"    return myfct3(a, $DEFAULT_VALUE$MYFCT3_0);",
								"  }",
								"  ",
								"  /**",
								"   * @ExcludeFromApidoc",
								"   */",
								"  public static class ContextAwareCapacityWrapper<C extends C3> extends C1.ContextAwareCapacityWrapper<C> implements C3 {",
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
							"  public abstract char myfct(final int a, final String... b);",
							"  ",
							"  /**",
							"   * @ExcludeFromApidoc",
							"   */",
							"  public static class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
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

	public static class Parsing extends AbstractSarlTest {

		@Test
		public void withoutFunction() throws Exception {
			SarlScript mas = file(SNIPSET1);
			validate(mas).assertNoErrors();
		}

		@Test
		public void withFunction() throws Exception {
			SarlScript mas = file(SNIPSET2);
			validate(mas).assertNoErrors();
		}

		@Test
		public void singleInheritanceWithoutFunction() throws Exception {
			SarlScript mas = file(SNIPSET3);
			validate(mas).assertNoErrors();
		}

		@Test
		public void singleInheritanceWithFunction() throws Exception {
			SarlScript mas = file(SNIPSET4);
			validate(mas).assertNoErrors();
		}

		@Test
		public void multiInheritanceWithoutFunction() throws Exception {
			SarlScript mas = file(SNIPSET5);
			validate(mas).assertNoErrors();
		}

		@Test
		public void multiInheritanceWithFunction() throws Exception {
			SarlScript mas = file(SNIPSET6);
			validate(mas).assertNoErrors();
		}

		@Test
		public void withVarArg() throws Exception {
			SarlScript mas = file(SNIPSET7);
			validate(mas).assertNoErrors();
		}

	}

}
