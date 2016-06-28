/*
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.lang.tests.compilation.oop;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.SARLVersion;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	EnumCompilerTest.TopLevelTest.class,
	EnumCompilerTest.InClassTest.class,
	EnumCompilerTest.InAgentTest.class,
})
@SuppressWarnings("all")
public class EnumCompilerTest {

	public static class TopLevelTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void basic() throws Exception {
			String source = "enum E1 { CST1, CST2 }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public enum E1 {",
					"  CST1,",
					"  ",
					"  CST2;",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

	public static class InClassTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void basic() throws Exception {
			String source = "class Container { enum E1 { CST1, CST2 } }";
			String expected = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  public enum E1 {",
					"    CST1,",
					"    ",
					"    CST2;",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

	public static class InAgentTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void basic() throws Exception {
			String source = "agent Container { enum E1 { CST1, CST2 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import java.util.UUID;",
					"import javax.annotation.Generated;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  protected enum E1 {",
					"    CST1,",
					"    ",
					"    CST2;",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public Container(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expected);
		}

	}

}
