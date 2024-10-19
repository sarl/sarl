/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.api.core.tests.bugs;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Tests for issue #864: Not suicide commit in "on Initialize" when the agent is the boot agent.
 *
 * <p>See: https://github.com/sarl/sarl/issues/864
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/864"
 */
@DisplayName("run: Bug #864")
@Tag("core")
public class Bug864Test extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package io.sarl.lang.tests.bug864", //$NON-NLS-1$
			"import io.sarl.api.core.Initialize", //$NON-NLS-1$
			"import io.sarl.api.core.Lifecycle", //$NON-NLS-1$
			"", //$NON-NLS-1$
			"agent Boot {", //$NON-NLS-1$
			"	uses Lifecycle", //$NON-NLS-1$
			"	on Initialize {", //$NON-NLS-1$
			"		killMe", //$NON-NLS-1$
			"	}", //$NON-NLS-1$
			"}"); //$NON-NLS-1$

	private static final String EXPECTED_01 = multilineString(
			"package io.sarl.lang.tests.bug864;", //$NON-NLS-1$
			"", //$NON-NLS-1$
			"import io.sarl.api.core.Initialize;", //$NON-NLS-1$
			"import io.sarl.api.core.Lifecycle;", //$NON-NLS-1$
			"import io.sarl.lang.core.Agent;", //$NON-NLS-1$
			"import io.sarl.lang.core.AtomicSkillReference;", //$NON-NLS-1$
			"import io.sarl.lang.core.DynamicSkillProvider;", //$NON-NLS-1$
			"import io.sarl.lang.core.Event;", //$NON-NLS-1$
			"import io.sarl.lang.core.annotation.ImportedCapacityFeature;", //$NON-NLS-1$
			"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;", //$NON-NLS-1$
			"import io.sarl.lang.core.annotation.SarlElementType;", //$NON-NLS-1$
			"import io.sarl.lang.core.annotation.SarlSpecification;", //$NON-NLS-1$
			"import io.sarl.lang.core.annotation.SyntheticMember;", //$NON-NLS-1$
			"import java.util.Collection;", //$NON-NLS-1$
			"import java.util.Set;", //$NON-NLS-1$
			"import java.util.UUID;", //$NON-NLS-1$
			"import javax.inject.Inject;", //$NON-NLS-1$
			"import org.eclipse.xtext.xbase.lib.Extension;", //$NON-NLS-1$
			"import org.eclipse.xtext.xbase.lib.Pure;", //$NON-NLS-1$
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;", //$NON-NLS-1$
			"", //$NON-NLS-1$
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", //$NON-NLS-1$ //$NON-NLS-2$
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")", //$NON-NLS-1$ //$NON-NLS-2$
			"@XbaseGenerated", //$NON-NLS-1$
			"@SuppressWarnings(\"all\")", //$NON-NLS-1$
			"public class Boot extends Agent {", //$NON-NLS-1$
			"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {", //$NON-NLS-1$
			"    Lifecycle _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER();", //$NON-NLS-1$
			"    _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER.killMe();", //$NON-NLS-1$
			"  }", //$NON-NLS-1$
			"  ", //$NON-NLS-1$
			"  @Extension", //$NON-NLS-1$
			"  @ImportedCapacityFeature(Lifecycle.class)", //$NON-NLS-1$
			"  @SyntheticMember", //$NON-NLS-1$
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE;", //$NON-NLS-1$
			"  ", //$NON-NLS-1$
			"  @SyntheticMember", //$NON-NLS-1$
			"  @Pure", //$NON-NLS-1$
			"  private Lifecycle $CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER() {", //$NON-NLS-1$
			"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE.get() == null) {", //$NON-NLS-1$
			"      this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE = $getSkill(Lifecycle.class);", //$NON-NLS-1$
			"    }", //$NON-NLS-1$
			"    return $castSkill(Lifecycle.class, this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE);", //$NON-NLS-1$
			"  }", //$NON-NLS-1$
			"  ", //$NON-NLS-1$
			"  @SyntheticMember", //$NON-NLS-1$
			"  @PerceptGuardEvaluator", //$NON-NLS-1$
			"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {", //$NON-NLS-1$
			"    assert occurrence != null;", //$NON-NLS-1$
			"    assert ___SARLlocal_runnableCollection != null;", //$NON-NLS-1$
			"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));", //$NON-NLS-1$
			"  }", //$NON-NLS-1$
			"  ", //$NON-NLS-1$
			"  @SyntheticMember", //$NON-NLS-1$
			"  @Override", //$NON-NLS-1$
			"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {", //$NON-NLS-1$
			"    super.$getSupportedEvents(toBeFilled);", //$NON-NLS-1$
			"    toBeFilled.add(Initialize.class);", //$NON-NLS-1$
			"  }", //$NON-NLS-1$
			"  ", //$NON-NLS-1$
			"  @SyntheticMember", //$NON-NLS-1$
			"  @Override", //$NON-NLS-1$
			"  public boolean $isSupportedEvent(final Class<? extends Event> event) {",  //$NON-NLS-1$
			"    if (Initialize.class.isAssignableFrom(event)) {", //$NON-NLS-1$
			"      return true;", //$NON-NLS-1$
			"    }", //$NON-NLS-1$
			"    return false;",  //$NON-NLS-1$
			"  }", //$NON-NLS-1$
			"  ", //$NON-NLS-1$
			"  @SyntheticMember", //$NON-NLS-1$
			"  @Override", //$NON-NLS-1$
			"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {", //$NON-NLS-1$
			"    super.$evaluateBehaviorGuards(event, callbacks);", //$NON-NLS-1$
			"    if (event instanceof Initialize occurrence) {", //$NON-NLS-1$
			"      $guardEvaluator$Initialize(occurrence, callbacks);", //$NON-NLS-1$
			"    }", //$NON-NLS-1$
			"  }", //$NON-NLS-1$
			"  ", //$NON-NLS-1$
			"  @SyntheticMember", //$NON-NLS-1$
			"  public Boot(final UUID arg0, final UUID arg1) {", //$NON-NLS-1$
			"    super(arg0, arg1);", //$NON-NLS-1$
			"  }", //$NON-NLS-1$
			"  ", //$NON-NLS-1$
			"  @SyntheticMember", //$NON-NLS-1$
			"  @Inject", //$NON-NLS-1$
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", //$NON-NLS-1$
			"    super(arg0, arg1, arg2);", //$NON-NLS-1$
			"  }", //$NON-NLS-1$
			"}", //$NON-NLS-1$
			""); //$NON-NLS-1$

	/**
	 * @throws Exception
	 */
	@Test
	@Tag("sarlValidation")
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET01);
		Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	/**
	 * @throws Exception
	 */
	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET01, it -> {
			String actual = it.getGeneratedCode("io.sarl.lang.tests.bug864.Boot"); //$NON-NLS-1$
			assertEquals(EXPECTED_01, actual);
		});
	}

}
