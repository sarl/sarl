/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Compilation: Agent with generic event")
@Tag("core")
@Tag("compileToJava")
public class AgentGenericEventCompilerTest extends AbstractSarlTest {
	
	@Test
	@DisplayName("True guard")
	public void trueGuard() throws Exception {
		final String expectedA1 = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.Event;",
				"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.util.Collection;",
				"import java.util.Set;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  private void $behaviorUnit$E1$$String$$Double$0(final E1<String, Double> occurrence) {",
				"    System.out.println(occurrence);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$E1$$String$$Double(final E1<String, Double> occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$E1$$String$$Double$0(occurrence));",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
				"    super.$getSupportedEvents(toBeFilled);",
				"    toBeFilled.add(E1.class);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
				"    if (E1.class.isAssignableFrom(event)) {",
				"      return true;",
				"    }",
				"    return false;", 
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
				"    super.$evaluateBehaviorGuards(event, callbacks);",
				"    if (event instanceof E1 occurrence) {",
				"      if (E1.$matchesTypeBounds(occurrence, String.class, Double.class)) {",
				"        $guardEvaluator$E1$$String$$Double(occurrence, callbacks);",
				"      }",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		String source = multilineString(
				"event E1<T1, T2 extends Number>",
				"agent A1 {",
				"  on E1<String, Double>  [ true ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	@DisplayName("False guard")
	public void falseGuard() throws Exception {
		final String expectedA1 = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		String source = multilineString(
				"event E1<T1, T2 extends Number>",
				"agent A1 {",
				"  on E1<String, Double> [ false ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedA1, r.getGeneratedCode("A1"));
		});
	}

	@Test
	@DisplayName("General guard")
	public void generalGuard() throws Exception {
		final String expectedA1 = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.Event;",
				"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.util.Collection;",
				"import java.util.Set;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  private void $behaviorUnit$E1$$String$$Double$0(final E1<String, Double> occurrence) {",
				"    System.out.println(occurrence);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$E1$$String$$Double$0(final E1<String, Double> it, final E1<String, Double> occurrence) {",
				"    return (occurrence.i != null && (occurrence.i.doubleValue() == 1));",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$E1$$String$$Double(final E1<String, Double> occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    if ($behaviorUnitGuard$E1$$String$$Double$0(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$E1$$String$$Double$0(occurrence));",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
				"    super.$getSupportedEvents(toBeFilled);",
				"    toBeFilled.add(E1.class);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
				"    if (E1.class.isAssignableFrom(event)) {",
				"      return true;",
				"    }",
				"    return false;", 
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
				"    super.$evaluateBehaviorGuards(event, callbacks);",
				"    if (event instanceof E1 occurrence) {",
				"      if (E1.$matchesTypeBounds(occurrence, String.class, Double.class)) {",
				"        $guardEvaluator$E1$$String$$Double(occurrence, callbacks);",
				"      }",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		String source = multilineString(
				"event E1<T1, T2 extends Number> { var i : T2 }",
				"agent A1 {",
				"  on E1<String, Double> [ occurrence.i == 1 ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	@DisplayName("With wildcard 1")
	public void withWildcard1() throws Exception {
		final String expectedA1 = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.Event;",
				"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.util.Collection;",
				"import java.util.Objects;",
				"import java.util.Set;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  private void $behaviorUnit$E1$$Object$$Double$0(final E1<?, Double> occurrence) {",
				"    System.out.println(occurrence);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$E1$$Object$$Double$0(final E1<?, Double> it, final E1<?, Double> occurrence) {",
				"    boolean _equals = Objects.equals(occurrence.a, Integer.valueOf(1));",
				"    return _equals;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$E1$$Object$$Double(final E1<?, Double> occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    if ($behaviorUnitGuard$E1$$Object$$Double$0(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$E1$$Object$$Double$0(occurrence));",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
				"    super.$getSupportedEvents(toBeFilled);",
				"    toBeFilled.add(E1.class);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
				"    if (E1.class.isAssignableFrom(event)) {",
				"      return true;",
				"    }",
				"    return false;", 
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
				"    super.$evaluateBehaviorGuards(event, callbacks);",
				"    if (event instanceof E1 occurrence) {",
				"      if (E1.$matchesTypeBounds(occurrence, Object.class, Double.class)) {",
				"        $guardEvaluator$E1$$Object$$Double(occurrence, callbacks);",
				"      }",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		String source = multilineString(
				"event E1<T1, T2 extends Number> { var a : T1; var b : T2 }",
				"agent A1 {",
				"  on E1<?, Double> [ occurrence.a == 1 ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	@DisplayName("With wildcard 2")
	public void withWildcard2() throws Exception {
		final String expectedA1 = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.Event;",
				"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.util.Collection;",
				"import java.util.Set;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  private void $behaviorUnit$E1$$Number$$Double$0(final E1<?, Double> occurrence) {",
				"    System.out.println(occurrence);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$E1$$Number$$Double$0(final E1<?, Double> it, final E1<?, Double> occurrence) {",
				"    return (occurrence.a != null && (occurrence.a.doubleValue() == 1));",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$E1$$Number$$Double(final E1<?, Double> occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    if ($behaviorUnitGuard$E1$$Number$$Double$0(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$E1$$Number$$Double$0(occurrence));",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
				"    super.$getSupportedEvents(toBeFilled);",
				"    toBeFilled.add(E1.class);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
				"    if (E1.class.isAssignableFrom(event)) {",
				"      return true;",
				"    }",
				"    return false;", 
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
				"    super.$evaluateBehaviorGuards(event, callbacks);",
				"    if (event instanceof E1 occurrence) {",
				"      if (E1.$matchesTypeBounds(occurrence, Number.class, Double.class)) {",
				"        $guardEvaluator$E1$$Number$$Double(occurrence, callbacks);",
				"      }",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		String source = multilineString(
				"event E1<T1 extends Number, T2 extends Number> { var a : T1; var b : T2 }",
				"agent A1 {",
				"  on E1<?, Double> [ occurrence.a == 1 ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	@DisplayName("Raw notation")
	public void rawNotation() throws Exception {
		final String expectedA1 = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.Event;",
				"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.util.Collection;",
				"import java.util.Objects;",
				"import java.util.Set;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  private void $behaviorUnit$E1$0(final E1 occurrence) {",
				"    System.out.println(occurrence);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$E1$0(final E1 it, final E1 occurrence) {",
				"    boolean _equals = Objects.equals(occurrence.a, Integer.valueOf(1));",
				"    return _equals;",
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
				"  @SyntheticMember",
				"  @Override",
				"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
				"    super.$getSupportedEvents(toBeFilled);",
				"    toBeFilled.add(E1.class);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
				"    if (E1.class.isAssignableFrom(event)) {",
				"      return true;",
				"    }",
				"    return false;", 
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
				"    super.$evaluateBehaviorGuards(event, callbacks);",
				"    if (event instanceof E1 occurrence) {",
				"      $guardEvaluator$E1(occurrence, callbacks);",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		String source = multilineString(
				"event E1<T1, T2 extends Number> { var a : T1; var b : T2 }",
				"agent A1 {",
				"  on E1 [ occurrence.a == 1 ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	@DisplayName("With two wildcards")
	public void withTwoWildcards() throws Exception {
		final String expectedA1 = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.Event;",
				"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.util.Collection;",
				"import java.util.Objects;",
				"import java.util.Set;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  private void $behaviorUnit$E1$0(final E1<?, ?> occurrence) {",
				"    System.out.println(occurrence);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$E1$0(final E1<?, ?> it, final E1<?, ?> occurrence) {",
				"    boolean _equals = Objects.equals(occurrence.a, Integer.valueOf(1));",
				"    return _equals;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$E1(final E1<?, ?> occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    if ($behaviorUnitGuard$E1$0(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$E1$0(occurrence));",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
				"    super.$getSupportedEvents(toBeFilled);",
				"    toBeFilled.add(E1.class);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
				"    if (E1.class.isAssignableFrom(event)) {",
				"      return true;",
				"    }",
				"    return false;", 
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
				"    super.$evaluateBehaviorGuards(event, callbacks);",
				"    if (event instanceof E1 occurrence) {",
				"      $guardEvaluator$E1(occurrence, callbacks);",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		String source = multilineString(
				"event E1<T1, T2 extends Number> { var a : T1; var b : T2 }",
				"agent A1 {",
				"  on E1<?, ?> [ occurrence.a == 1 ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

	@Test
	@DisplayName("With upper bound")
	public void withUpperBound() throws Exception {
		final String expectedA1 = multilineString(
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.Event;",
				"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.util.Collection;",
				"import java.util.Set;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  private void $behaviorUnit$E1$$Number$$Double$0(final E1<? extends Number, Double> occurrence) {",
				"    System.out.println(occurrence);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Pure",
				"  private boolean $behaviorUnitGuard$E1$$Number$$Double$0(final E1<? extends Number, Double> it, final E1<? extends Number, Double> occurrence) {",
				"    return (occurrence.a != null && (occurrence.a.doubleValue() == 1));",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$E1$$Number$$Double(final E1<? extends Number, Double> occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    if ($behaviorUnitGuard$E1$$Number$$Double$0(occurrence, occurrence)) {",
				"      ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$E1$$Number$$Double$0(occurrence));",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
				"    super.$getSupportedEvents(toBeFilled);",
				"    toBeFilled.add(E1.class);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
				"    if (E1.class.isAssignableFrom(event)) {",
				"      return true;",
				"    }",
				"    return false;", 
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Override",
				"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
				"    super.$evaluateBehaviorGuards(event, callbacks);",
				"    if (event instanceof E1 occurrence) {",
				"      if (E1.$matchesTypeBounds(occurrence, Number.class, Double.class)) {",
				"        $guardEvaluator$E1$$Number$$Double(occurrence, callbacks);",
				"      }",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		String source = multilineString(
				"event E1<T1, T2 extends Number> { var a : T1; var b : T2 }",
				"agent A1 {",
				"  on E1<? extends Number, Double> [ occurrence.a == 1 ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedA1,r.getGeneratedCode("A1"));
		});
	}

}
