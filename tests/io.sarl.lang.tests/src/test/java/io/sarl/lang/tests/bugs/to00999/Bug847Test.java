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

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Unexpected ERROR at Validating the SARL source files.
 *
 * <p>https://github.com/sarl/sarl/issues/847
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/847"
 */
@DisplayName("Bug #847")
@SuppressWarnings("all")
@Tag("core")
public class Bug847Test extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package io.sarl.lang.tests.bug847",
			"import io.sarl.core.AgentSpawned",
			"import io.sarl.core.Logging",
			"agent SomeAgent{",
			"   uses Logging",
			"   on AgentSpawned {",
			"       info(\"Agent {0} of type {1} has been created successfully and is now alive!\",",
			"            occurrence.agentID, occurrence.agentType)",
			"   }",
			"}");

	private static final String EXPECTED01 = multilineString(
			"package io.sarl.lang.tests.bug847;",
			"",
			"import io.sarl.core.AgentSpawned;",
			"import io.sarl.core.Logging;",
			"import io.sarl.lang.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import java.util.Collection;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  private void $behaviorUnit$AgentSpawned$0(final AgentSpawned occurrence) {",
			"    Logging _$CAPACITY_USE$IO_SARL_CORE_LOGGING$CALLER = this.$CAPACITY_USE$IO_SARL_CORE_LOGGING$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_CORE_LOGGING$CALLER.info(\"Agent {0} of type {1} has been created successfully and is now alive!\", ",
			"      occurrence.agentID, occurrence.agentType);",
			"  }",
			"  ",
			"  @Extension",
			"  @ImportedCapacityFeature(Logging.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_CORE_LOGGING;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
			"  private Logging $CAPACITY_USE$IO_SARL_CORE_LOGGING$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_CORE_LOGGING == null || this.$CAPACITY_USE$IO_SARL_CORE_LOGGING.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_CORE_LOGGING = $getSkill(Logging.class);",
			"    }",
			"    return $castSkill(Logging.class, this.$CAPACITY_USE$IO_SARL_CORE_LOGGING);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @PerceptGuardEvaluator",
			"  private void $guardEvaluator$AgentSpawned(final AgentSpawned occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"    assert occurrence != null;",
			"    assert ___SARLlocal_runnableCollection != null;",
			"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$AgentSpawned$0(occurrence));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
			"    super.$getSupportedEvents(toBeFilled);",
			"    toBeFilled.add(AgentSpawned.class);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
			"    if (AgentSpawned.class.isAssignableFrom(event)) {",
			"      return true;",
			"    }",
			"    return false;", 
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
			"    super.$evaluateBehaviorGuards(event, callbacks);",
			"    if (event instanceof AgentSpawned) {",
			"      final AgentSpawned occurrence = (AgentSpawned) event;",
			"      $guardEvaluator$AgentSpawned(occurrence, callbacks);",
			"    }",
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
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug847.SomeAgent");
			assertEquals(EXPECTED01, actual);
		});
	}

	private static final String SNIPSET02 = multilineString(
			"package io.sarl.lang.tests.bug847;",
			"import io.sarl.core.AgentSpawned",
			"import io.sarl.core.Logging",
			"agent SomeAgent{",
			"   uses Logging",
			"   on AgentSpawned {",
			"       val agent_id = occurrence.agentID",
			"       val agent_type = occurrence.agentType",
			"       info(\"Agent {0} of type {1} has been created successfully and is now alive!\",",
			"            agent_id, agent_type)",
			"   }",
			"}");

	private static final String EXPECTED02 = multilineString(
			"package io.sarl.lang.tests.bug847;",
			"",
			"import io.sarl.core.AgentSpawned;",
			"import io.sarl.core.Logging;",
			"import io.sarl.lang.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import java.util.Collection;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  private void $behaviorUnit$AgentSpawned$0(final AgentSpawned occurrence) {",
			"    final UUID agent_id = occurrence.agentID;",
			"    final String agent_type = occurrence.agentType;",
			"    Logging _$CAPACITY_USE$IO_SARL_CORE_LOGGING$CALLER = this.$CAPACITY_USE$IO_SARL_CORE_LOGGING$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_CORE_LOGGING$CALLER.info(\"Agent {0} of type {1} has been created successfully and is now alive!\", agent_id, agent_type);",
			"  }",
			"  ",
			"  @Extension",
			"  @ImportedCapacityFeature(Logging.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_CORE_LOGGING;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
			"  private Logging $CAPACITY_USE$IO_SARL_CORE_LOGGING$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_CORE_LOGGING == null || this.$CAPACITY_USE$IO_SARL_CORE_LOGGING.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_CORE_LOGGING = $getSkill(Logging.class);",
			"    }",
			"    return $castSkill(Logging.class, this.$CAPACITY_USE$IO_SARL_CORE_LOGGING);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @PerceptGuardEvaluator",
			"  private void $guardEvaluator$AgentSpawned(final AgentSpawned occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"    assert occurrence != null;",
			"    assert ___SARLlocal_runnableCollection != null;",
			"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$AgentSpawned$0(occurrence));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
			"    super.$getSupportedEvents(toBeFilled);",
			"    toBeFilled.add(AgentSpawned.class);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
			"    if (AgentSpawned.class.isAssignableFrom(event)) {",
			"      return true;",
			"    }",
			"    return false;", 
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Override",
			"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
			"    super.$evaluateBehaviorGuards(event, callbacks);",
			"    if (event instanceof AgentSpawned) {",
			"      final AgentSpawned occurrence = (AgentSpawned) event;",
			"      $guardEvaluator$AgentSpawned(occurrence, callbacks);",
			"    }",
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
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug847.SomeAgent");
			assertEquals(EXPECTED02, actual);
		});
	}

}

