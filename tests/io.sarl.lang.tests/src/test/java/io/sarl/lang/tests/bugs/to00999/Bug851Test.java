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

package io.sarl.lang.tests.bugs.to00999;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Wrong error: Invalid use of the read-only occurrence
 *
 * <p>https://github.com/sarl/sarl/issues/851
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/851"
 */
@DisplayName("Bug #851")
@SuppressWarnings("all")
@Tag("core")
public class Bug851Test extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package io.sarl.lang.tests.bug851",
			"import java.util.UUID",
			"import java.util.Map",
			"event CarArrivedPercept {",
			"  var car : UUID",
			"  var floor : int",
			"}",
			"class Body {",
			"  public var floor : int",
			"}",
			"agent SomeAgent{",
			"  var cars : Map<UUID, Body>",
			"  on CarArrivedPercept {",
			"    cars.get(occurrence.car).floor = occurrence.floor",
			"  }",
			"}");

	private static final String EXPECTED01 = multilineString(
			"package io.sarl.lang.tests.bug851;",
			"",
			"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import java.util.Collection;",
			"import java.util.Map;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  private Map<UUID, Body> cars;",
			"  ",
			"  private void $behaviorUnit$CarArrivedPercept$0(final CarArrivedPercept occurrence) {",
			"    Body _get = this.cars.get(occurrence.car);",
			"    _get.floor = occurrence.floor;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @PerceptGuardEvaluator",
			"  private void $guardEvaluator$CarArrivedPercept(final CarArrivedPercept occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"    assert occurrence != null;",
			"    assert ___SARLlocal_runnableCollection != null;",
			"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$CarArrivedPercept$0(occurrence));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
			"    super.$getSupportedEvents(toBeFilled);",
			"    toBeFilled.add(CarArrivedPercept.class);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
			"    if (CarArrivedPercept.class.isAssignableFrom(event)) {",
			"      return true;",
			"    }",
			"    return false;", 
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
			"    super.$evaluateBehaviorGuards(event, callbacks);",
			"    if (event instanceof CarArrivedPercept) {",
			"      final CarArrivedPercept occurrence = (CarArrivedPercept) event;",
			"      $guardEvaluator$CarArrivedPercept(occurrence, callbacks);",
			"    }",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    return super.equals(obj);",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    return result;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	private static final String SNIPSET02 = multilineString(
			"package io.sarl.lang.tests.bug851",
			"import java.util.UUID",
			"import java.util.Map",
			"event CarArrivedPercept {",
			"  var car : UUID",
			"  var floor : int",
			"}",
			"class Body {",
			"  public var floor : int",
			"}",
			"agent SomeAgent{",
			"  var cars : Map<UUID, Body>",
			"  def operator_doubleArrow(m : Map<UUID, Body>, k : UUID) : Body { null }",
			"  on CarArrivedPercept {",
			"    (cars => occurrence.car).floor = occurrence.floor",
			"  }",
			"}");

	private static final String EXPECTED02 = multilineString(
			"package io.sarl.lang.tests.bug851;",
			"",
			"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import java.util.Collection;",
			"import java.util.Map;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  private Map<UUID, Body> cars;",
			"  ",
			"  @Pure",
			"  protected Body operator_doubleArrow(final Map<UUID, Body> m, final UUID k) {",
			"    return null;",
			"  }",
			"  ",
			"  private void $behaviorUnit$CarArrivedPercept$0(final CarArrivedPercept occurrence) {",
			"    Body _doubleArrow = this.operator_doubleArrow(this.cars, occurrence.car);",
			"    _doubleArrow.floor = occurrence.floor;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @PerceptGuardEvaluator",
			"  private void $guardEvaluator$CarArrivedPercept(final CarArrivedPercept occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"    assert occurrence != null;",
			"    assert ___SARLlocal_runnableCollection != null;",
			"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$CarArrivedPercept$0(occurrence));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
			"    super.$getSupportedEvents(toBeFilled);",
			"    toBeFilled.add(CarArrivedPercept.class);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
			"    if (CarArrivedPercept.class.isAssignableFrom(event)) {",
			"      return true;",
			"    }",
			"    return false;", 
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
			"    super.$evaluateBehaviorGuards(event, callbacks);",
			"    if (event instanceof CarArrivedPercept) {",
			"      final CarArrivedPercept occurrence = (CarArrivedPercept) event;",
			"      $guardEvaluator$CarArrivedPercept(occurrence, callbacks);",
			"    }",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    return super.equals(obj);",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    return result;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	private static final String SNIPSET03 = multilineString(
			"package io.sarl.lang.tests.bug851",
			"import java.util.UUID",
			"import java.util.Map",
			"event CarArrivedPercept {",
			"  var car : UUID",
			"  var floor : int",
			"}",
			"class Body {",
			"  public var floor : int",
			"}",
			"agent SomeAgent{",
			"  on CarArrivedPercept {",
			"    occurrence.car = UUID::randomUUID",
			"  }",
			"}");

	private static final String SNIPSET04 = multilineString(
			"package io.sarl.lang.tests.bug851",
			"import java.util.UUID",
			"import java.util.Map",
			"event CarArrivedPercept {",
			"  var car : UUID",
			"  var floor : int",
			"}",
			"class Body {",
			"  public var floor : int",
			"}",
			"agent SomeAgent{",
			"  var cars : Map<UUID, Body>",
			"  on CarArrivedPercept {",
			"    var k = occurrence.car",
			"    cars.get(k).floor = occurrence.floor",
			"  }",
			"}");

	@Test
	@Tag("sarlValidation")
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug851.SomeAgent");
			assertEquals(EXPECTED01, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug851.SomeAgent");
			assertEquals(EXPECTED02, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_03() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.INVALID_OCCURRENCE_READONLY_USE,
				"left-side of an assignment operator");
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_04() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET04);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

}

