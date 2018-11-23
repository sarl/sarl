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

package io.sarl.lang.tests.bugs.to00999;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

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
@SuppressWarnings("all")
public class Bug851 extends AbstractSarlTest {

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
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.tests.bug851.Body;",
			"import io.sarl.lang.tests.bug851.CarArrivedPercept;",
			"import java.util.Collection;",
			"import java.util.Map;",
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
			"  @Deprecated",
			"  @Inject",
			"  public SomeAgent(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
			"    super(arg0, arg1, arg2);",
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
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.tests.bug851.Body;",
			"import io.sarl.lang.tests.bug851.CarArrivedPercept;",
			"import java.util.Collection;",
			"import java.util.Map;",
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
			"  @Deprecated",
			"  @Inject",
			"  public SomeAgent(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
			"    super(arg0, arg1, arg2);",
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
	public void parsing_01() throws Exception {
		SarlScript mas = file(SNIPSET01);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug851.SomeAgent");
			assertEquals(EXPECTED01, actual);
		});
	}

	@Test
	public void parsing_02() throws Exception {
		SarlScript mas = file(SNIPSET02);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug851.SomeAgent");
			assertEquals(EXPECTED02, actual);
		});
	}

	@Test
	public void parsing_03() throws Exception {
		SarlScript mas = file(SNIPSET03);
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.INVALID_OCCURRENCE_READONLY_USE,
				"left-side of an assignment operator");
	}

	@Test
	public void parsing_04() throws Exception {
		SarlScript mas = file(SNIPSET04);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

}

