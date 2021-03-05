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
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Compilation: Behavior")
@Tag("core")
@Tag("compileToJava")
public class BehaviorCompilerTest extends AbstractSarlTest {

	@Test
	public void basicBehaviorCompile() throws Exception {
		String source = "behavior B1 { }";
		String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void trueGuardBehaviorUnit() throws Exception {
		final String expectedE1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588368462L;",
				"}",
				""
				);
		final String expectedB1 = multilineString(
				"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"import io.sarl.lang.core.Event;",
				"import java.util.Collection;",
				"import java.util.Set;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
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
				"    if (event instanceof E1) {",
				"      final E1 occurrence = (E1) event;",
				"      $guardEvaluator$E1(occurrence, callbacks);",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
				);
		String source = multilineString(
				"event E1",
				"behavior B1 {",
				"  on E1 [ true ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		getCompileHelper().compile(source, (r) -> {
				assertEquals(expectedE1,r.getGeneratedCode("E1"));
				assertEquals(expectedB1,r.getGeneratedCode("B1"));
			});
	}

	@Test
	public void falseGuardBehaviorUnit() throws Exception {
		final String expectedE1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588368462L;",
				"}",
				""
				);
		final String expectedB1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
				);
		String source = multilineString(
				"event E1",
				"behavior B1 {",
				"  on E1 [ false ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		getCompileHelper().compile(source, (r) -> {
				assertEquals(expectedE1, r.getGeneratedCode("E1"));
				assertEquals(expectedB1, r.getGeneratedCode("B1"));
			});
	}

	@Test
	public void generalGuardBehaviorUnit() throws Exception {
		final String expectedE1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  public int i;",
				"  ",
				"  @SyntheticMember",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public E1(final Address arg0) {",
				"    super(arg0);",
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
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.i);",
				"    return result;",
				"  }",
				"  ",
				"  /**",
				"   * Returns a String representation of the E1 event's attributes only.",
				"   */",
				"  @SyntheticMember",
				"  @Pure",
				"  protected void toString(final ToStringBuilder builder) {",
				"    super.toString(builder);",
				"    builder.add(\"i\", this.i);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = 588472998L;",
				"}",
				""
				);
		final String expectedB1 = multilineString(
				"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"import io.sarl.lang.core.Event;",
				"import java.util.Collection;",
				"import java.util.Set;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
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
				"    if (event instanceof E1) {",
				"      final E1 occurrence = (E1) event;",
				"      $guardEvaluator$E1(occurrence, callbacks);",
				"    }",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
				);
		String source = multilineString(
				"event E1 { var i : int }",
				"behavior B1 {",
				"  on E1 [ occurrence.i === 1 ] {",
				"    System.out.println(occurrence)",
				"  }",
				"}"
				);
		getCompileHelper().compile(source, (r) -> {
				assertEquals(expectedE1,r.getGeneratedCode("E1"));
				assertEquals(expectedB1,r.getGeneratedCode("B1"));
			});
	}

	@Test
	public void valueVisibility_0() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				" val myval = 1",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  private final int myval = 1;",
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
				"    B1 other = (B1) obj;",
				"    if (other.myval != this.myval)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.myval);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void variableVisibility_0() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				" var myval = 1",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  private int myval = 1;",
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
				"    B1 other = (B1) obj;",
				"    if (other.myval != this.myval)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.myval);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void actionVisibility_0() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				" def myfct { }",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  public void myfct() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void valueVisibility_1() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				" private val myval = 1",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  private final int myval = 1;",
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
				"    B1 other = (B1) obj;",
				"    if (other.myval != this.myval)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.myval);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void variableVisibility_1() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				" private var myval = 1",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  private int myval = 1;",
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
				"    B1 other = (B1) obj;",
				"    if (other.myval != this.myval)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.myval);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void actionVisibility_1() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				" private def myfct { }",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  private void myfct() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void behaviormodifier_none() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 { }"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void behaviormodifier_public() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"public behavior B1 { }"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void behaviormodifier_package() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"package behavior B1 { }"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"class B1 extends Behavior {",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void behaviormodifier_abstract() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"abstract behavior B1 { }"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public abstract class B1 extends Behavior {",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void behaviormodifier_abstract_member() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	def fct",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public abstract class B1 extends Behavior {",
				"  public abstract void fct();",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void behaviormodifier_final() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"final behavior B1 { }"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public final class B1 extends Behavior {",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void fieldmodifier_none() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	var field : int",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  private int field;",
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
				"    B1 other = (B1) obj;",
				"    if (other.field != this.field)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.field);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void fieldmodifier_package() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	package var field : int",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  int field;",
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
				"    B1 other = (B1) obj;",
				"    if (other.field != this.field)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.field);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void fieldmodifier_protected() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	protected var field : int",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  protected int field;",
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
				"    B1 other = (B1) obj;",
				"    if (other.field != this.field)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.field);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void fieldmodifier_private() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	private var field : int",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  private int field;",
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
				"    B1 other = (B1) obj;",
				"    if (other.field != this.field)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.field);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void actionmodifier_override() throws Exception {
		String source = multilineString(
				"behavior B1 {",
				"	def name",
				"}",
				"behavior B2 extends B1 {",
				"	override name {}",
				"}"
			);
		final String expectedB1 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public abstract class B1 extends Behavior {",
				"  public abstract void name();",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			);
		final String expectedB2 = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B2 extends B1 {",
				"  @Override",
				"  public void name() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B2(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			);
		getCompileHelper().compile(source, (r) -> {
				assertEquals(expectedB1, r.getGeneratedCode("B1"));
				assertEquals(expectedB2, r.getGeneratedCode("B2"));
			});
	}

	@Test
	public void actionmodifier_none() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	def name {}",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  public void name() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void actionmodifier_private() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	private def name {}",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  private void name() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void actionmodifier_package() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	package def name {}",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  void name() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void actionmodifier_protected() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	protected def name {}",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  protected void name() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void actionmodifier_abstract() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	def name",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public abstract class B1 extends Behavior {",
				"  public abstract void name();",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void actionmodifier_static() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	static def name {}",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  public static void name() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void actionmodifier_dispatch() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	dispatch def name(a : Integer) {}",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  public void _name(final Integer a) {",
				"  }",
				"  ",
				"  public void name(final Integer a) {",
				"    _name(a);",
				"    return;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void actionmodifier_final() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	final def name {}",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  public final void name() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

	@Test
	public void actionmodifier_synchronized() throws Exception {
		getCompileHelper().assertCompilesTo(
			multilineString(
				"behavior B1 {",
				"	synchronized def name {}",
				"}"
			),
			multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
				"@SuppressWarnings(\"all\")",
				"public class B1 extends Behavior {",
				"  public synchronized void name() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public B1(final Agent arg0) {",
				"    super(arg0);",
				"  }",
				"}",
				""
			));
	}

}
