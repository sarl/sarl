/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/864"
 */
@DisplayName("run: Bug #864")
@Tag("core")
@SuppressWarnings("all")
public class Bug864Test extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package io.sarl.lang.tests.bug864",
			"import io.sarl.api.core.Initialize",
			"import io.sarl.api.core.Lifecycle",
			"",
			"agent Boot {",
			"	uses Lifecycle",
			"	on Initialize {",
			"		killMe",
			"	}",
			"}");

	private static final String EXPECTED_01 = multilineString(
			"package io.sarl.lang.tests.bug864;",
			"",
			"import io.sarl.api.core.Initialize;",
			"import io.sarl.api.core.Lifecycle;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import jakarta.inject.Inject;",
			"import java.util.Collection;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", //$NON-NLS-2$
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")", //$NON-NLS-2$
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Boot extends Agent {",
			"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
			"    Lifecycle _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER.killMe();",
			"  }",
			"  ",
			"  @Extension",
			"  @ImportedCapacityFeature(Lifecycle.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
			"  private Lifecycle $CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE = $getSkill(Lifecycle.class);",
			"    }",
			"    return $castSkill(Lifecycle.class, this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE);",
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
			"  @SyntheticMember",
			"  @Override",
			"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
			"    super.$getSupportedEvents(toBeFilled);",
			"    toBeFilled.add(Initialize.class);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
			"    if (Initialize.class.isAssignableFrom(event)) {",
			"      return true;",
			"    }",
			"    return false;", 
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public void $evaluateBehaviorGuards(final Class<?> eventType, final Object event, final Collection<Runnable> callbacks) {",
			"    assert eventType != null;",
			"    assert event != null;",
			"    super.$evaluateBehaviorGuards(eventType, event, callbacks);",
			"    if (Initialize.class.equals(eventType)) {",
			"      final var occurrence = (Initialize) event;",
			"      $guardEvaluator$Initialize(occurrence, callbacks);",
			"    }",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Boot(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public Boot(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

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
			String actual = it.getGeneratedCode("io.sarl.lang.tests.bug864.Boot");
			assertEquals(EXPECTED_01, actual);
		});
	}

}
