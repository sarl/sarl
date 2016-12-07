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
package io.sarl.lang.tests.general.compilation.aop;

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
public class AgentCompilerTest extends AbstractSarlTest {

	@Inject
	private CompilationTestHelper compiler;

	@Test
	public void basicAgentCompile() throws Exception {
		String source = "agent A1 { }";
		String expected = multilineString(
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
	public void trueGuardBehaviorUnit() throws Exception {
		final String expectedE1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  /**",
				"   * Construct an event. The source of the event is unknown.",
				"   */",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  /**",
				"   * Construct an event.",
				"   * @param source - address of the agent that is emitting this event.",
				"   */",
				"  @SyntheticMember",
				"  public E1(final Address source) {",
				"    super(source);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private final static long serialVersionUID = 588368462L;",
				"}",
				""
				);
		final String expectedA1 = multilineString(
				"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Collection;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @SyntheticMember",
				"  private void $behaviorUnit$E1$0(final E1 occurrence) {",
				"    System.out.println(occurrence);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$E1(final E1 occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$E1$0(occurrence));",
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
		String source = multilineString(
				"event E1",
				"agent A1 {",
				"  on E1 [ true ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedE1,r.getGeneratedCode("E1"));
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	public void falseGuardBehaviorUnit() throws Exception {
		final String expectedE1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  /**",
				"   * Construct an event. The source of the event is unknown.",
				"   */",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  /**",
				"   * Construct an event.",
				"   * @param source - address of the agent that is emitting this event.",
				"   */",
				"  @SyntheticMember",
				"  public E1(final Address source) {",
				"    super(source);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private final static long serialVersionUID = 588368462L;",
				"}",
				""
				);
		final String expectedA1 = multilineString(
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
		String source = multilineString(
				"event E1",
				"agent A1 {",
				"  on E1 [ false ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedE1, r.getGeneratedCode("E1"));
			assertEquals(expectedA1, r.getGeneratedCode("A1"));
		});
	}

	@Test
	public void generalGuardBehaviorUnit() throws Exception {
		final String expectedE1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  public int i;",
				"  ",
				"  /**",
				"   * Construct an event. The source of the event is unknown.",
				"   */",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  /**",
				"   * Construct an event.",
				"   * @param source - address of the agent that is emitting this event.",
				"   */",
				"  @SyntheticMember",
				"  public E1(final Address source) {",
				"    super(source);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    E1 other = (E1) obj;",
				"    if (other.i != this.i)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    final int prime = 31;",
				"    int result = super.hashCode();",
				"    result = prime * result + this.i;",
				"    return result;",
				"  }",
				"  ",
				"  /**",
				"   * Returns a String representation of the E1 event's attributes only.",
				"   */",
				"  @SyntheticMember",
				"  @Pure",
				"  protected String attributesToString() {",
				"    StringBuilder result = new StringBuilder(super.attributesToString());",
				"    result.append(\"i  = \").append(this.i);",
				"    return result.toString();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private final static long serialVersionUID = 588472998L;",
				"}",
				""
				);
		final String expectedA1 = multilineString(
				"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Collection;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @SyntheticMember",
				"  private void $behaviorUnit$E1$0(final E1 occurrence) {",
				"    System.out.println(occurrence);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$E1$0(final E1 it, final E1 occurrence) {",
				"    return (occurrence.i == 1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$E1(final E1 occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    if ($behaviorUnitGuard$E1$0(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$E1$0(occurrence));",
				"    }",
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
		String source = multilineString(
				"event E1 { var i : int }",
				"agent A1 {",
				"  on E1 [ occurrence.i === 1 ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedE1,r.getGeneratedCode("E1"));
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	public void valueVisibility_0() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						" val myval = 1",
						"}"
						),
				multilineString(
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
						"  protected final int myval = 1;",
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
						));
	}

	@Test
	public void variableVisibility_0() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						" var myval = 1",
						"}"
						),
				multilineString(
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
						"  protected int myval = 1;",
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
						));
	}

	@Test
	public void actionVisibility_0() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						" def myfct { }",
						"}"
						),
				multilineString(
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
						"  protected void myfct() {",
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
						));
	}

	@Test
	public void valueVisibility_1() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						" private val myval = 1",
						"}"
						),
				multilineString(
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
						"  private final int myval = 1;",
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
						));
	}

	@Test
	public void variableVisibility_1() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						" private var myval = 1",
						"}"
						),
				multilineString(
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
						"  private int myval = 1;",
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
						));
	}

	@Test
	public void actionVisibility_1() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						" private def myfct { }",
						"}"
						),
				multilineString(
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
						"  private void myfct() {",
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
						));
	}

	@Test
	public void agentmodifier_none() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 { }"
						),
				multilineString(
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
						));
	}

	@Test
	public void agentmodifier_public() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"public agent A1 { }"
						),
				multilineString(
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
						));
	}

	@Test
	public void agentmodifier_package() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"package agent A1 { }"
						),
				multilineString(
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SuppressWarnings(\"all\")",
						"class A1 extends Agent {",
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
						));
	}

	@Test
	public void agentmodifier_abstract() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"abstract agent A1 { }"
						),
				multilineString(
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SuppressWarnings(\"all\")",
						"public abstract class A1 extends Agent {",
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
						));
	}

	@Test
	public void agentmodifier_abstract_member() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	def fct",
						"}"
						),
				multilineString(
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SuppressWarnings(\"all\")",
						"public abstract class A1 extends Agent {",
						"  protected abstract void fct();",
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
						));
	}

	@Test
	public void agentmodifier_final() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"final agent A1 { }"
						),
				multilineString(
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SuppressWarnings(\"all\")",
						"public final class A1 extends Agent {",
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
						));
	}

	@Test
	public void fieldmodifier_none() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	var field : int",
						"}"
						),
				multilineString(
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
						"  protected int field;",
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
						));
	}

	@Test
	public void fieldmodifier_package() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	package var field : int",
						"}"
						),
				multilineString(
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
						"  int field;",
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
						));
	}

	@Test
	public void fieldmodifier_protected() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	protected var field : int",
						"}"
						),
				multilineString(
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
						"  protected int field;",
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
						));
	}

	@Test
	public void fieldmodifier_private() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	private var field : int",
						"}"
						),
				multilineString(
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
						"  private int field;",
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
						));
	}

	@Test
	public void actionmodifier_override() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"	def name",
				"}",
				"agent A2 extends A1 {",
				"	override name {}",
				"}"
				);
		final String expectedA1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public abstract class A1 extends Agent {",
				"  protected abstract void name();",
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
		final String expectedA2 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A2 extends A1 {",
				"  @Override",
				"  protected void name() {",
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
				"  public A2(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedA1, r.getGeneratedCode("A1"));
			assertEquals(expectedA2, r.getGeneratedCode("A2"));
		});
	}

	@Test
	public void actionmodifier_none() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	def name {}",
						"}"
						),
				multilineString(
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
						"  protected void name() {",
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
						));
	}

	@Test
	public void actionmodifier_private() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	private def name {}",
						"}"
						),
				multilineString(
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
						"  private void name() {",
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
						));
	}

	@Test
	public void actionmodifier_package() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	package def name {}",
						"}"
						),
				multilineString(
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
						"  void name() {",
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
						));
	}

	@Test
	public void actionmodifier_protected() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	protected def name {}",
						"}"
						),
				multilineString(
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
						"  protected void name() {",
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
						));
	}

	@Test
	public void actionmodifier_abstract() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	def name",
						"}"
						),
				multilineString(
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SuppressWarnings(\"all\")",
						"public abstract class A1 extends Agent {",
						"  protected abstract void name();",
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
						));
	}

	@Test
	public void actionmodifier_static() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	static def name {}",
						"}"
						),
				multilineString(
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
						"  protected static void name() {",
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
						));
	}

	@Test
	public void actionmodifier_dispatch() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	dispatch def name(a : Integer) {}",
						"}"
						),
				multilineString(
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
						"  protected void _name(final Integer a) {",
						"  }",
						"  ",
						"  protected void name(final Integer a) {",
						"    _name(a);",
						"    return;",
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
						));
	}

	@Test
	public void actionmodifier_dispatch_final() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	dispatch final def name(a : Integer) {}",
						"}"
						),
				multilineString(
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
						"  protected final void _name(final Integer a) {",
						"  }",
						"  ",
						"  protected void name(final Integer a) {",
						"    _name(a);",
						"    return;",
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
						));
	}

	@Test
	public void actionmodifier_final() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	final def name {}",
						"}"
						),
				multilineString(
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
						"  protected final void name() {",
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
						));
	}

	@Test
	public void actionmodifier_synchronized() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"	synchronized def name {}",
						"}"
						),
				multilineString(
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
						"  protected synchronized void name() {",
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
						));
	}

	@Test
	public void inlinedCapacityFunctionCall_nativeParameter() throws Exception {
		String source = "capacity C1 { def myfunction(v : double) } agent A1 { uses C1 def caller { myfunction(5) } }";
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@FunctionalInterface",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface C1 extends Capacity {",
				"  public abstract void myfunction(final double v);",
				"}",
				""
				);
		final String expectedA1 = multilineString(
				"import io.sarl.lang.annotation.ImportedCapacityFeature;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import io.sarl.lang.core.ClearableReference;",
				"import io.sarl.lang.core.Skill;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void caller() {",
				"    C1 _$CAPACITY_USE$C1$CALLER = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);",
				"    _$CAPACITY_USE$C1$CALLER.myfunction(5);",
				"  }",
				"  ",
				"  @Extension",
				"  @ImportedCapacityFeature(C1.class)",
				"  @SyntheticMember",
				"  private transient ClearableReference<Skill> $CAPACITY_USE$C1;",
				"  ",
				"  @Inline(value = \"$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1)\", imported = C1.class)",
				"  @SyntheticMember",
				"  @Pure",
				"  private C1 $CAPACITY_USE$C1$CALLER() {",
				"    if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {",
				"      this.$CAPACITY_USE$C1 = $getSkill(C1.class);",
				"    }",
				"    return $castSkill(C1.class, this.$CAPACITY_USE$C1);",
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
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1,r.getGeneratedCode("C1"));
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	public void inlinedCapacityFunctionCall_classParameter_baseName() throws Exception {
		String source = "capacity C1 { def myfunction(v : Class<?>) } agent A1 { uses C1 def caller { myfunction(Integer) } }";
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@FunctionalInterface",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface C1 extends Capacity {",
				"  public abstract void myfunction(final Class<?> v);",
				"}",
				""
				);
		final String expectedA1 = multilineString(
				"import io.sarl.lang.annotation.ImportedCapacityFeature;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import io.sarl.lang.core.ClearableReference;",
				"import io.sarl.lang.core.Skill;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void caller() {",
				"    C1 _$CAPACITY_USE$C1$CALLER = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);",
				"    _$CAPACITY_USE$C1$CALLER.myfunction(Integer.class);",
				"  }",
				"  ",
				"  @Extension",
				"  @ImportedCapacityFeature(C1.class)",
				"  @SyntheticMember",
				"  private transient ClearableReference<Skill> $CAPACITY_USE$C1;",
				"  ",
				"  @Inline(value = \"$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1)\", imported = C1.class)",
				"  @SyntheticMember",
				"  @Pure",
				"  private C1 $CAPACITY_USE$C1$CALLER() {",
				"    if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {",
				"      this.$CAPACITY_USE$C1 = $getSkill(C1.class);",
				"    }",
				"    return $castSkill(C1.class, this.$CAPACITY_USE$C1);",
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
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1,r.getGeneratedCode("C1"));
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	public void inlinedCapacityFunctionCall_classParameter_typeof() throws Exception {
		String source = "capacity C1 { def myfunction(v : Class<?>) } agent A1 { uses C1 def caller { myfunction(typeof(Integer)) } }";
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@FunctionalInterface",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface C1 extends Capacity {",
				"  public abstract void myfunction(final Class<?> v);",
				"}",
				""
				);
		final String expectedA1 = multilineString(
				"import io.sarl.lang.annotation.ImportedCapacityFeature;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import io.sarl.lang.core.ClearableReference;",
				"import io.sarl.lang.core.Skill;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void caller() {",
				"    C1 _$CAPACITY_USE$C1$CALLER = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);",
				"    _$CAPACITY_USE$C1$CALLER.myfunction(Integer.class);",
				"  }",
				"  ",
				"  @Extension",
				"  @ImportedCapacityFeature(C1.class)",
				"  @SyntheticMember",
				"  private transient ClearableReference<Skill> $CAPACITY_USE$C1;",
				"  ",
				"  @Inline(value = \"$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1)\", imported = C1.class)",
				"  @SyntheticMember",
				"  @Pure",
				"  private C1 $CAPACITY_USE$C1$CALLER() {",
				"    if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {",
				"      this.$CAPACITY_USE$C1 = $getSkill(C1.class);",
				"    }",
				"    return $castSkill(C1.class, this.$CAPACITY_USE$C1);",
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
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1,r.getGeneratedCode("C1"));
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	public void inlinedCapacityFunctionCall_variadicParameter01() throws Exception {
		String source = "capacity C1 { def myfunction(v1 : Class<?>, v2 : int*) } agent A1 { uses C1 def caller { "
				+ "myfunction(typeof(Integer))"
				+ "myfunction(typeof(Float), 1)"
				+ "myfunction(typeof(String), 2, 3)"
				+ "myfunction(typeof(String), #[])"
				+ " } }";
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@FunctionalInterface",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface C1 extends Capacity {",
				"  public abstract void myfunction(final Class<?> v1, final int... v2);",
				"}",
				""
				);
		final String expectedA1 = multilineString(
				"import io.sarl.lang.annotation.ImportedCapacityFeature;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import io.sarl.lang.core.ClearableReference;",
				"import io.sarl.lang.core.Skill;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void caller() {",
			    "    C1 _$CAPACITY_USE$C1$CALLER = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);",
				"    _$CAPACITY_USE$C1$CALLER.myfunction(Integer.class);",
				"    C1 _$CAPACITY_USE$C1$CALLER_1 = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);",
				"    _$CAPACITY_USE$C1$CALLER_1.myfunction(Float.class, 1);",
				"    C1 _$CAPACITY_USE$C1$CALLER_2 = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);",
				"    _$CAPACITY_USE$C1$CALLER_2.myfunction(String.class, 2, 3);",
				"    C1 _$CAPACITY_USE$C1$CALLER_3 = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);",
				"    _$CAPACITY_USE$C1$CALLER_3.myfunction(String.class, new int[] {});",
				"  }",
				"  ",
				"  @Extension",
				"  @ImportedCapacityFeature(C1.class)",
				"  @SyntheticMember",
				"  private transient ClearableReference<Skill> $CAPACITY_USE$C1;",
				"  ",
				"  @Inline(value = \"$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1)\", imported = C1.class)",
				"  @SyntheticMember",
				"  @Pure",
				"  private C1 $CAPACITY_USE$C1$CALLER() {",
				"    if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {",
				"      this.$CAPACITY_USE$C1 = $getSkill(C1.class);",
				"    }",
				"    return $castSkill(C1.class, this.$CAPACITY_USE$C1);",
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
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1,r.getGeneratedCode("C1"));
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	public void inlinedCapacityFunctionCall_variadicParameter02() throws Exception {
		String source = "capacity C1 { def myfunction(v2 : int*) } agent A1 { uses C1 def caller { "
				+ "myfunction;"
				+ "myfunction(1);"
				+ "myfunction(2, 3);"
				+ "myfunction(#[]);"
				+ " } }";
		final String expectedC1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@FunctionalInterface",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface C1 extends Capacity {",
				"  public abstract void myfunction(final int... v2);",
				"}",
				""
				);
		final String expectedA1 = multilineString(
				"import io.sarl.lang.annotation.ImportedCapacityFeature;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import io.sarl.lang.core.ClearableReference;",
				"import io.sarl.lang.core.Skill;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void caller() {",
				"    C1 _$CAPACITY_USE$C1$CALLER = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);",
				"    _$CAPACITY_USE$C1$CALLER.myfunction();",
				"    C1 _$CAPACITY_USE$C1$CALLER_1 = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);",
				"    _$CAPACITY_USE$C1$CALLER_1.myfunction(1);",
				"    C1 _$CAPACITY_USE$C1$CALLER_2 = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);",
				"    _$CAPACITY_USE$C1$CALLER_2.myfunction(2, 3);",
				"    C1 _$CAPACITY_USE$C1$CALLER_3 = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);",
				"    _$CAPACITY_USE$C1$CALLER_3.myfunction(new int[] {});",
				"  }",
				"  ",
				"  @Extension",
				"  @ImportedCapacityFeature(C1.class)",
				"  @SyntheticMember",
				"  private transient ClearableReference<Skill> $CAPACITY_USE$C1;",
				"  ",
				"  @Inline(value = \"$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1)\", imported = C1.class)",
				"  @SyntheticMember",
				"  @Pure",
				"  private C1 $CAPACITY_USE$C1$CALLER() {",
				"    if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {",
				"      this.$CAPACITY_USE$C1 = $getSkill(C1.class);",
				"    }",
				"    return $castSkill(C1.class, this.$CAPACITY_USE$C1);",
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
		this.compiler.compile(source, (r) -> {
			assertEquals(expectedC1,r.getGeneratedCode("C1"));
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	public void duplicateEventHandler() throws Exception {
		final String source = multilineString(
				"event Initialize {",
				"  val parameters = newArrayList",
				"}",
				"agent MyAgent {",
				"	on Initialize [ occurrence.parameters.empty ] {",
				"		println(\"Initialization without parameters\")",
				"	}",
				"	on Initialize [ ! occurrence.parameters.empty ] {",
				"		println(\"Initialization with parameters: \"",
				"			+ occurrence.parameters )",
				"	}",
				"}");
		final String expectedMyAgent = multilineString(
				"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Collection;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.InputOutput;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class MyAgent extends Agent {",
				"  @SyntheticMember",
				"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
				"    InputOutput.<String>println(\"Initialization without parameters\");",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$Initialize$0(final Initialize it, final Initialize occurrence) {",
				"    boolean _isEmpty = occurrence.parameters.isEmpty();",
				"    return _isEmpty;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private void $behaviorUnit$Initialize$1(final Initialize occurrence) {",
				"    InputOutput.<String>println(",
				"      (\"Initialization with parameters: \" + occurrence.parameters));",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$Initialize$1(final Initialize it, final Initialize occurrence) {",
				"    boolean _isEmpty = occurrence.parameters.isEmpty();",
				"    boolean _not = (!_isEmpty);",
				"    return _not;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    if ($behaviorUnitGuard$Initialize$0(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));",
				"    }",
				"    if ($behaviorUnitGuard$Initialize$1(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$1(occurrence));",
				"    }",
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
				"  public MyAgent(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> assertEquals(expectedMyAgent, r.getGeneratedCode("MyAgent")));
	}

	@Test
	public void multipleEventsWithoutUses() throws Exception {
		final String source = multilineString(
				"package foo.test",
				"event Initialize",
				"event Destroy",
				"agent MyAgent {",
				"	on Initialize {",
				"	}",
				"	on Destroy {",
				"	}",
				"}");
		final String expectedMyAgent = multilineString(
				"package foo.test;",
				"",
				"import foo.test.Destroy;",
				"import foo.test.Initialize;",
				"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Collection;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class MyAgent extends Agent {",
				"  @SyntheticMember",
				"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  private void $behaviorUnit$Destroy$1(final Destroy occurrence) {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$Destroy(final Destroy occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Destroy$1(occurrence));",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));",
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
				"  public MyAgent(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> assertEquals(expectedMyAgent, r.getGeneratedCode("foo.test.MyAgent")));
	}

	@Test
	public void multipleEventsWithoutUses_sameEventMultipleTimes() throws Exception {
		final String source = multilineString(
				"package foo.test",
				"event Initialize",
				"event Destroy",
				"agent MyAgent {",
				"	on Initialize {",
				"      println(1)",
				"	}",
				"	on Destroy {",
				"	}",
				"	on Initialize {",
				"      println(2)",
				"	}",
				"}");
		final String expectedMyAgent = multilineString(
				"package foo.test;",
				"",
				"import foo.test.Destroy;",
				"import foo.test.Initialize;",
				"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Collection;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.InputOutput;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class MyAgent extends Agent {",
				"  @SyntheticMember",
				"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
				"    InputOutput.<Integer>println(Integer.valueOf(1));",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  private void $behaviorUnit$Destroy$1(final Destroy occurrence) {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private void $behaviorUnit$Initialize$2(final Initialize occurrence) {",
				"    InputOutput.<Integer>println(Integer.valueOf(2));",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$Destroy(final Destroy occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Destroy$1(occurrence));",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));",
				"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$2(occurrence));",
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
				"  public MyAgent(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> assertEquals(expectedMyAgent, r.getGeneratedCode("foo.test.MyAgent")));
	}

	@Test
	public void multipleEventsWithoutUses_sameEventMultipleTimesWithGuards01() throws Exception {
		final String source = multilineString(
				"package foo.test",
				"event Initialize",
				"event Destroy",
				"agent MyAgent {",
				"   var xxx : int",
				"	on Initialize [xxx == 1] {",
				"      println(1)",
				"	}",
				"	on Destroy {",
				"	}",
				"	on Initialize [xxx == 2] {",
				"      println(2)",
				"	}",
				"}");
		final String expectedMyAgent = multilineString(
				"package foo.test;",
				"",
				"import foo.test.Destroy;",
				"import foo.test.Initialize;",
				"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Collection;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.InputOutput;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class MyAgent extends Agent {",
				"  protected int xxx;",
				"  ",
				"  @SyntheticMember",
				"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
				"    InputOutput.<Integer>println(Integer.valueOf(1));",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$Initialize$0(final Initialize it, final Initialize occurrence) {",
				"    return (this.xxx == 1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private void $behaviorUnit$Destroy$1(final Destroy occurrence) {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private void $behaviorUnit$Initialize$2(final Initialize occurrence) {",
				"    InputOutput.<Integer>println(Integer.valueOf(2));",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$Initialize$2(final Initialize it, final Initialize occurrence) {",
				"    return (this.xxx == 2);",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$Destroy(final Destroy occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Destroy$1(occurrence));",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    if ($behaviorUnitGuard$Initialize$0(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));",
				"    }",
				"    if ($behaviorUnitGuard$Initialize$2(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$2(occurrence));",
				"    }",
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
				"  public MyAgent(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> assertEquals(expectedMyAgent, r.getGeneratedCode("foo.test.MyAgent")));
	}

	@Test
	public void multipleEventsWithoutUses_sameEventMultipleTimesWithGuards02() throws Exception {
		final String source = multilineString(
				"package foo.test",
				"event Initialize",
				"event Destroy",
				"agent MyAgent {",
				"   var xxx : int",
				"	on Initialize [xxx == 1] {",
				"      println(1)",
				"	}",
				"	on Destroy {",
				"      println(3)",
				"	}",
				"	on Destroy [xxx == 5] {",
				"      println(4)",
				"	}",
				"	on Initialize [xxx == 2] {",
				"      println(2)",
				"	}",
				"}");
		final String expectedMyAgent = multilineString(
				"package foo.test;",
				"",
				"import foo.test.Destroy;",
				"import foo.test.Initialize;",
				"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Collection;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.InputOutput;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class MyAgent extends Agent {",
				"  protected int xxx;",
				"  ",
				"  @SyntheticMember",
				"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
				"    InputOutput.<Integer>println(Integer.valueOf(1));",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$Initialize$0(final Initialize it, final Initialize occurrence) {",
				"    return (this.xxx == 1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private void $behaviorUnit$Destroy$1(final Destroy occurrence) {",
				"    InputOutput.<Integer>println(Integer.valueOf(3));",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private void $behaviorUnit$Destroy$2(final Destroy occurrence) {",
				"    InputOutput.<Integer>println(Integer.valueOf(4));",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$Destroy$2(final Destroy it, final Destroy occurrence) {",
				"    return (this.xxx == 5);",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  private void $behaviorUnit$Initialize$3(final Initialize occurrence) {",
				"    InputOutput.<Integer>println(Integer.valueOf(2));",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$Initialize$3(final Initialize it, final Initialize occurrence) {",
				"    return (this.xxx == 2);",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$Destroy(final Destroy occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Destroy$1(occurrence));",
				"    if ($behaviorUnitGuard$Destroy$2(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Destroy$2(occurrence));",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    if ($behaviorUnitGuard$Initialize$0(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));",
				"    }",
				"    if ($behaviorUnitGuard$Initialize$3(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$3(occurrence));",
				"    }",
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
				"  public MyAgent(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> assertEquals(expectedMyAgent, r.getGeneratedCode("foo.test.MyAgent")));
	}

	@Test
	public void usePureCapacityFunction() throws Exception {
		final String source = multilineString(
				"package foo.test",
				"capacity C1 { @Pure def myfct : int }",
				"agent MyAgent {",
				"   uses C1",
				"	def testFct {",
				"      var v = myfct",
				"	}",
				"}");
		final String expectedMyAgent = multilineString(
				"package foo.test;",
				"",
				"import foo.test.C1;",
				"import io.sarl.lang.annotation.ImportedCapacityFeature;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import io.sarl.lang.core.ClearableReference;",
				"import io.sarl.lang.core.Skill;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class MyAgent extends Agent {",
				"  protected void testFct() {",
				"    C1 _$CAPACITY_USE$FOO_TEST_C1$CALLER = this.$castSkill(C1.class, (this.$CAPACITY_USE$FOO_TEST_C1 == null || this.$CAPACITY_USE$FOO_TEST_C1.get() == null) ? (this.$CAPACITY_USE$FOO_TEST_C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$FOO_TEST_C1);",
				"    int v = _$CAPACITY_USE$FOO_TEST_C1$CALLER.myfct();",
				"  }",
				"  ",
				"  @Extension",
				"  @ImportedCapacityFeature(C1.class)",
				"  @SyntheticMember",
				"  private transient ClearableReference<Skill> $CAPACITY_USE$FOO_TEST_C1;",
				"  ",
				"  @Inline(value = \"$castSkill(C1.class, (this.$CAPACITY_USE$FOO_TEST_C1 == null || this.$CAPACITY_USE$FOO_TEST_C1.get() == null) ? (this.$CAPACITY_USE$FOO_TEST_C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$FOO_TEST_C1)\", imported = C1.class)",
				"  @SyntheticMember",
				"  @Pure",
				"  private C1 $CAPACITY_USE$FOO_TEST_C1$CALLER() {",
				"    if (this.$CAPACITY_USE$FOO_TEST_C1 == null || this.$CAPACITY_USE$FOO_TEST_C1.get() == null) {",
				"      this.$CAPACITY_USE$FOO_TEST_C1 = $getSkill(C1.class);",
				"    }",
				"    return $castSkill(C1.class, this.$CAPACITY_USE$FOO_TEST_C1);",
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
				"  public MyAgent(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> assertEquals(expectedMyAgent, r.getGeneratedCode("foo.test.MyAgent")));
	}

	@Test
	public void useNotPureCapacityFunction() throws Exception {
		final String source = multilineString(
				"package foo.test",
				"capacity C1 { def myfct : int }",
				"agent MyAgent {",
				"   uses C1",
				"	def testFct {",
				"      var v = myfct",
				"	}",
				"}");
		final String expectedMyAgent = multilineString(
				"package foo.test;",
				"",
				"import foo.test.C1;",
				"import io.sarl.lang.annotation.ImportedCapacityFeature;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import io.sarl.lang.core.ClearableReference;",
				"import io.sarl.lang.core.Skill;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class MyAgent extends Agent {",
				"  protected void testFct() {",
				"    C1 _$CAPACITY_USE$FOO_TEST_C1$CALLER = this.$castSkill(C1.class, (this.$CAPACITY_USE$FOO_TEST_C1 == null || this.$CAPACITY_USE$FOO_TEST_C1.get() == null) ? (this.$CAPACITY_USE$FOO_TEST_C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$FOO_TEST_C1);",
				"    int v = _$CAPACITY_USE$FOO_TEST_C1$CALLER.myfct();",
				"  }",
				"  ",
				"  @Extension",
				"  @ImportedCapacityFeature(C1.class)",
				"  @SyntheticMember",
				"  private transient ClearableReference<Skill> $CAPACITY_USE$FOO_TEST_C1;",
				"  ",
				"  @Inline(value = \"$castSkill(C1.class, (this.$CAPACITY_USE$FOO_TEST_C1 == null || this.$CAPACITY_USE$FOO_TEST_C1.get() == null) ? (this.$CAPACITY_USE$FOO_TEST_C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$FOO_TEST_C1)\", imported = C1.class)",
				"  @SyntheticMember",
				"  @Pure",
				"  private C1 $CAPACITY_USE$FOO_TEST_C1$CALLER() {",
				"    if (this.$CAPACITY_USE$FOO_TEST_C1 == null || this.$CAPACITY_USE$FOO_TEST_C1.get() == null) {",
				"      this.$CAPACITY_USE$FOO_TEST_C1 = $getSkill(C1.class);",
				"    }",
				"    return $castSkill(C1.class, this.$CAPACITY_USE$FOO_TEST_C1);",
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
				"  public MyAgent(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.compile(source, (r) -> assertEquals(expectedMyAgent, r.getGeneratedCode("foo.test.MyAgent")));
	}

}
