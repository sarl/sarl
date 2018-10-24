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
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.eclipse.xtext.xbase.validation.UIStrings;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.util.Utils;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class for issue: 'final' error in generated Java.
 *
 * <p>https://github.com/sarl/sarl/issues/819
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/819"
 */
@SuppressWarnings("all")
public class Bug819 extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"import io.sarl.core.Logging",
			"import io.sarl.core.Initialize",
			"import io.sarl.core.DefaultContextInteractions",
			"import io.sarl.core.Lifecycle",
			"import java.util.UUID",
			"agent Agent2 {}",
			"event Hello",
			"agent Agent3 {",
			"	uses Logging",
			"	uses DefaultContextInteractions",
			"	uses Lifecycle",
			"",
			"	on Initialize {",
			"		var id : UUID",
			"		id = spawn(typeof(Agent2), \"Gilbert\")",
			"",
			"		emit(new Hello, [it.UUID == id])",
			"	}",
			"}");

	private static final String EXPECTED01 = multilineString(
			"import com.google.common.base.Objects;",
			"import io.sarl.core.DefaultContextInteractions;",
			"import io.sarl.core.Initialize;",
			"import io.sarl.core.Lifecycle;",
			"import io.sarl.core.Logging;",
			"import io.sarl.lang.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.Skill;",
			"import io.sarl.lang.util.ClearableReference;",
			"import java.util.Collection;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Inline;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class Agent3 extends Agent {",
			"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
			"    UUID id = null;",
			"    Lifecycle _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER = this.$castSkill(Lifecycle.class, (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = this.$getSkill(Lifecycle.class)) : this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE);",
			"    id = _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER.spawn(Agent2.class, \"Gilbert\");",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$castSkill(DefaultContextInteractions.class, (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = this.$getSkill(DefaultContextInteractions.class)) : this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS);",
			"    Hello _hello = new Hello();",
			"    final Scope<Address> _function = (Address it) -> {",
			"      UUID _uUID = it.getUUID();",
			"      return Objects.equal(_uUID, id);",
			"    };",
			"    _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(_hello, _function);",
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
			"  @Extension",
			"  @ImportedCapacityFeature(DefaultContextInteractions.class)",
			"  @SyntheticMember",
			"  private transient ClearableReference<Skill> $CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
			"  @Inline(value = \"$castSkill(DefaultContextInteractions.class, ($0$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null || $0$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) ? ($0$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = $0$getSkill(DefaultContextInteractions.class)) : $0$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS)\", imported = DefaultContextInteractions.class)",
			"  private DefaultContextInteractions $CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = $getSkill(DefaultContextInteractions.class);",
			"    }",
			"    return $castSkill(DefaultContextInteractions.class, this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS);",
			"  }",
			"  ",
			"  @Extension",
			"  @ImportedCapacityFeature(Lifecycle.class)",
			"  @SyntheticMember",
			"  private transient ClearableReference<Skill> $CAPACITY_USE$IO_SARL_CORE_LIFECYCLE;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
			"  @Inline(value = \"$castSkill(Lifecycle.class, ($0$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || $0$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) ? ($0$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = $0$getSkill(Lifecycle.class)) : $0$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE)\", imported = Lifecycle.class)",
			"  private Lifecycle $CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = $getSkill(Lifecycle.class);",
			"    }",
			"    return $castSkill(Lifecycle.class, this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE);",
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
			"  public Agent3(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Deprecated",
			"  @Inject",
			"  public Agent3(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public Agent3(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
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
			String actual = it.getGeneratedCode("Agent3");
			assertEquals(EXPECTED01, actual);
		});
	}

}

