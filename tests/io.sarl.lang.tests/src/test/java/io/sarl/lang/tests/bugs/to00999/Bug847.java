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

import static org.junit.Assert.*;

import java.util.ArrayList;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.validation.UIStrings;
import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.util.Utils;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

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
@SuppressWarnings("all")
public class Bug847 extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package io.sarl.lang.tests.bug847",
			"import io.sarl.core.AgentSpawned",
			"import io.sarl.core.Logging",
			"agent SomeAgent{",
			"   uses Logging",
			"   on AgentSpawned {",
			"       info(\"Agent {0} of type {1} has been created successfully and is now alive!\",",
			"            occurrence.agentIdentifiers, occurrence.agentType)",
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
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Skill;",
			"import io.sarl.lang.util.ClearableReference;",
			"import java.util.Collection;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Inline;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  private void $behaviorUnit$AgentSpawned$0(final AgentSpawned occurrence) {",
			"    Logging _$CAPACITY_USE$IO_SARL_CORE_LOGGING$CALLER = this.$castSkill(Logging.class, (this.$CAPACITY_USE$IO_SARL_CORE_LOGGING == null || this.$CAPACITY_USE$IO_SARL_CORE_LOGGING.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_LOGGING = this.$getSkill(Logging.class)) : this.$CAPACITY_USE$IO_SARL_CORE_LOGGING);",
			"    _$CAPACITY_USE$IO_SARL_CORE_LOGGING$CALLER.info(\"Agent {0} of type {1} has been created successfully and is now alive!\", ",
			"      occurrence.agentIdentifiers, occurrence.agentType);",
			"  }",
			"  ",
			"  @Extension",
			"  @ImportedCapacityFeature(Logging.class)",
			"  @SyntheticMember",
			"  private transient ClearableReference<Skill> $CAPACITY_USE$IO_SARL_CORE_LOGGING;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
			"  @Inline(value = \"$castSkill(Logging.class, ($0$CAPACITY_USE$IO_SARL_CORE_LOGGING == null || $0$CAPACITY_USE$IO_SARL_CORE_LOGGING.get() == null) ? ($0$CAPACITY_USE$IO_SARL_CORE_LOGGING = $0$getSkill(Logging.class)) : $0$CAPACITY_USE$IO_SARL_CORE_LOGGING)\", imported = Logging.class)",
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

	@Test
	public void parsing_01() throws Exception {
		SarlScript mas = file(SNIPSET01);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
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
			"       val agent_ids = occurrence.agentIdentifiers",
			"       val agent_type = occurrence.agentType",
			"       info(\"Agent {0} of type {1} has been created successfully and is now alive!\",",
			"            agent_ids, agent_type)",
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
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Skill;",
			"import io.sarl.lang.util.ClearableReference;",
			"import java.util.Collection;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Inline;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  private void $behaviorUnit$AgentSpawned$0(final AgentSpawned occurrence) {",
			"    final Collection<UUID> agent_ids = occurrence.agentIdentifiers;",
			"    final String agent_type = occurrence.agentType;",
			"    Logging _$CAPACITY_USE$IO_SARL_CORE_LOGGING$CALLER = this.$castSkill(Logging.class, (this.$CAPACITY_USE$IO_SARL_CORE_LOGGING == null || this.$CAPACITY_USE$IO_SARL_CORE_LOGGING.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_LOGGING = this.$getSkill(Logging.class)) : this.$CAPACITY_USE$IO_SARL_CORE_LOGGING);",
			"    _$CAPACITY_USE$IO_SARL_CORE_LOGGING$CALLER.info(\"Agent {0} of type {1} has been created successfully and is now alive!\", agent_ids, agent_type);",
			"  }",
			"  ",
			"  @Extension",
			"  @ImportedCapacityFeature(Logging.class)",
			"  @SyntheticMember",
			"  private transient ClearableReference<Skill> $CAPACITY_USE$IO_SARL_CORE_LOGGING;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
			"  @Inline(value = \"$castSkill(Logging.class, ($0$CAPACITY_USE$IO_SARL_CORE_LOGGING == null || $0$CAPACITY_USE$IO_SARL_CORE_LOGGING.get() == null) ? ($0$CAPACITY_USE$IO_SARL_CORE_LOGGING = $0$getSkill(Logging.class)) : $0$CAPACITY_USE$IO_SARL_CORE_LOGGING)\", imported = Logging.class)",
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

	@Test
	public void parsing_02() throws Exception {
		SarlScript mas = file(SNIPSET02);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug847.SomeAgent");
			assertEquals(EXPECTED02, actual);
		});
	}

}

