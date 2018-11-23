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
package io.sarl.lang.tests.bugs.to00399;

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

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	Bug312.CapacityTest.class,
	//Bug312.ClassTest.class,
})
@SuppressWarnings("all")
public class Bug312 {

	public static class CapacityTest extends AbstractSarlTest {

		private String snippet = multilineString(
				"class Vector2f {}",
				"class Vector2i {}",
				"capacity C1 {",
				"    def move(direction1 : Vector2f, changeHeading1 : boolean = false)",
				"    def move(direction2 : Vector2i, changeHeading2 : boolean = false)",
				"}");

		@Test
		public void bug312() throws Exception {
			SarlScript mas = file(snippet);
			validate(mas).assertNoErrors();
		}

		@Test
		public void testCompiler() throws Exception {
			final String expected = multilineString(
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
					"  public abstract void move(final Vector2f direction1, @DefaultValue(\"C1#MOVE_0\") final boolean changeHeading1);",
					"  ",
					"  /**",
					"   * Default value for the parameter changeHeading1",
					"   */",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"false\")",
					"  public final static boolean $DEFAULT_VALUE$MOVE_0 = false;",
					"  ",
					"  @DefaultValueSource",
					"  public abstract void move(final Vector2i direction2, @DefaultValue(\"C1#MOVE_1\") final boolean changeHeading2);",
					"  ",
					"  /**",
					"   * Default value for the parameter changeHeading2",
					"   */",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"false\")",
					"  public final static boolean $DEFAULT_VALUE$MOVE_1 = false;",
					"  ",
					"  @DefaultValueUse(\"Vector2f,boolean\")",
					"  @SyntheticMember",
					"  public default void move(final Vector2f direction1) {",
					"    move(direction1, $DEFAULT_VALUE$MOVE_0);",
					"  }",
					"  ",
					"  @DefaultValueUse(\"Vector2i,boolean\")",
					"  @SyntheticMember",
					"  public default void move(final Vector2i direction2) {",
					"    move(direction2, $DEFAULT_VALUE$MOVE_1);",
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
					"    public void move(final Vector2f direction1, final boolean changeHeading1) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.move(direction1, changeHeading1);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void move(final Vector2i direction2, final boolean changeHeading2) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.move(direction2, changeHeading2);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void move(final Vector2f direction1) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.move(direction1);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void move(final Vector2i direction2) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.move(direction2);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					"");
			getCompileHelper().compile(this.snippet, (r) -> assertEquals(expected, r.getGeneratedCode("C1")));
		}

	}

}
