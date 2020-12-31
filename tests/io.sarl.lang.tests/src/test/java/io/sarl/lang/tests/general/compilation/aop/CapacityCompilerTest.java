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
 *     http:"",www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.general.compilation.aop;

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Compilation: Capacity")
@Tag("core")
@Tag("compileToJava")
public class CapacityCompilerTest extends AbstractSarlTest {

	@Test
	public void basicCapacityCompile() throws Exception {
		String source = "capacity C1 { }";
		String expected = multilineString(
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
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void capacitymodifier_none() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"capacity C1 { }"
			),
			multilineString(
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
				""
			));
	}

	@Test
	public void capacitymodifier_public() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"public capacity C1 { }"
			),
			multilineString(
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
				""
			));
	}

	@Test
	public void capacitymodifier_private() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"private capacity C1 { }"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.AgentTrait;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
				"@SuppressWarnings(\"all\")",
				"interface C1 extends Capacity {",
				"  /**",
				"   * @ExcludeFromApidoc",
				"   */",
				"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
				"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
				"      super(capacity, caller);",
				"    }",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void actionmodifier_override() throws Exception {
		String source = multilineString(
				"capacity C1 {",
				"	def name",
				"}",
				"capacity C2 extends C1 {",
				"	override name",
				"}"
			);
		final String expectedC1 = multilineString(
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
				"  void name();",
				"  ",
				"  /**",
				"   * @ExcludeFromApidoc",
				"   */",
				"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
				"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
				"      super(capacity, caller);",
				"    }",
				"    ",
				"    public void name() {",
				"      try {",
				"        ensureCallerInLocalThread();",
				"        this.capacity.name();",
				"      } finally {",
				"        resetCallerInLocalThread();",
				"      }",
				"    }",
				"  }",
				"}",
				""
			);
		final String expectedC2 = multilineString(
				"import C1;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.AgentTrait;",
				"",
				"@FunctionalInterface",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
				"@SuppressWarnings(\"all\")",
				"public interface C2 extends C1 {",
				"  @Override",
				"  void name();",
				"  ",
				"  /**",
				"   * @ExcludeFromApidoc",
				"   */",
				"  class ContextAwareCapacityWrapper<C extends C2> extends C1.ContextAwareCapacityWrapper<C> implements C2 {",
				"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
				"      super(capacity, caller);",
				"    }",
				"    ",
				"    public void name() {",
				"      try {",
				"        ensureCallerInLocalThread();",
				"        this.capacity.name();",
				"      } finally {",
				"        resetCallerInLocalThread();",
				"      }",
				"    }",
				"  }",
				"}",
				""
			);
		getCompileHelper().compile(source, (r) -> {
				assertEquals(expectedC1, r.getGeneratedCode("C1"));
				assertEquals(expectedC2, r.getGeneratedCode("C2"));
			});
	}

	@Test
	public void actionmodifier_none() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"capacity C1 {",
				"	def name {}",
				"}"
			),
			multilineString(
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
				"  void name();",
				"  ",
				"  /**",
				"   * @ExcludeFromApidoc",
				"   */",
				"  class ContextAwareCapacityWrapper<C extends C1> extends Capacity.ContextAwareCapacityWrapper<C> implements C1 {",
				"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
				"      super(capacity, caller);",
				"    }",
				"    ",
				"    public void name() {",
				"      try {",
				"        ensureCallerInLocalThread();",
				"        this.capacity.name();",
				"      } finally {",
				"        resetCallerInLocalThread();",
				"      }",
				"    }",
				"  }",
				"}",
				""
			));
	}

}
