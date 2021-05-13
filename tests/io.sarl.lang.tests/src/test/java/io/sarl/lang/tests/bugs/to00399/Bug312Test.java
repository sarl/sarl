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
package io.sarl.lang.tests.bugs.to00399;

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

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #312")
@SuppressWarnings("all")
@Tag("core")
public class Bug312Test {

	@Nested
	public class CapacityTest extends AbstractSarlTest {

		private String snippet = multilineString(
				"class Vector2f {}",
				"class Vector2i {}",
				"capacity C1 {",
				"    def move(direction1 : Vector2f, changeHeading1 : boolean = false)",
				"    def move(direction2 : Vector2i, changeHeading2 : boolean = false)",
				"}");

		@Test
		@Tag("sarlValidation")
		public void bug312() throws Exception {
			SarlScript mas = file(getParseHelper(), snippet);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@Tag("compileToJava")
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
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  @DefaultValueSource",
					"  void move(final Vector2f direction1, @DefaultValue(\"C1#MOVE_0\") final boolean changeHeading1);",
					"  ",
					"  @DefaultValueSource",
					"  void move(final Vector2i direction2, @DefaultValue(\"C1#MOVE_1\") final boolean changeHeading2);",
					"  ",
					"  /**",
					"   * Default value for the parameter changeHeading1",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"false\")",
					"  default boolean $DEFAULT_VALUE$MOVE_0() {",
					"    return false;",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter changeHeading2",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"false\")",
					"  default boolean $DEFAULT_VALUE$MOVE_1() {",
					"    return false;",
					"  }",
					"  ",
					"  @DefaultValueUse(\"Vector2f,boolean\")",
					"  @SyntheticMember",
					"  default void move(final Vector2f direction1) {",
					"    move(direction1, $DEFAULT_VALUE$MOVE_0());",
					"  }",
					"  ",
					"  @DefaultValueUse(\"Vector2i,boolean\")",
					"  @SyntheticMember",
					"  default void move(final Vector2i direction2) {",
					"    move(direction2, $DEFAULT_VALUE$MOVE_1());",
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
