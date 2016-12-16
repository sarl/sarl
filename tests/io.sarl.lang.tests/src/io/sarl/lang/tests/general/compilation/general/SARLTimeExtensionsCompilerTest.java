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
package io.sarl.lang.tests.general.compilation.general;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLTimeExtensionsCompilerTest extends AbstractSarlTest {

	@Inject
	private CompilationTestHelper compiler;

	@Test
	public void milliseconds() throws Exception {
		String source = multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.milliseconds",
					"}",
				"}"
			);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Object myaction0() {",
				"    return Long.valueOf((1234));",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @SyntheticMember",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void seconds() throws Exception {
		String source = multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.seconds",
					"}",
				"}"
			);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Object myaction0() {",
				"    return Long.valueOf((1234) * 1000);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @SyntheticMember",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void minutes() throws Exception {
		String source = multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.minutes",
					"}",
				"}"
			);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Object myaction0() {",
				"    return Long.valueOf((1234) * 60000);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @SyntheticMember",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void hours() throws Exception {
		String source = multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.hours",
					"}",
				"}"
			);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Object myaction0() {",
				"    return Long.valueOf((1234) * 3600000);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @SyntheticMember",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void days() throws Exception {
		String source = multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.days",
					"}",
				"}"
			);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Object myaction0() {",
				"    return Long.valueOf((1234) * 86400000);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @SyntheticMember",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void weeks() throws Exception {
		String source = multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.weeks",
					"}",
				"}"
			);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Object myaction0() {",
				"    return Long.valueOf((1234) * 604800000);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @SyntheticMember",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

}
