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
@DisplayName("Bug #819")
@SuppressWarnings("all")
@Tag("core")
public class Bug819Test extends AbstractSarlTest {

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
			"		val id = UUID::randomUUID",
			"		spawn(typeof(Agent2), id, defaultContext, \"Gilbert\")",
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
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.util.SerializableProxy;",
			"import java.io.ObjectStreamException;",
			"import java.util.Collection;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class Agent3 extends Agent {",
			"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
			"    final UUID id = UUID.randomUUID();",
			"    Lifecycle _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER = this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER();",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER.spawn(Agent2.class, id, _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.getDefaultContext(), \"Gilbert\");",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER_1 = this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();",
			"    Hello _hello = new Hello();",
			"    class $SerializableClosureProxy implements Scope<Address> {",
			"      ",
			"      private final UUID id;",
			"      ",
			"      public $SerializableClosureProxy(final UUID id) {",
			"        this.id = id;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equal(_uUID, id);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getID();",
			"        return Objects.equal(_uUID, id);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, id);",
			"      }",
			"    };",
			"    _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER_1.emit(_hello, _function);",
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
			"  @Extension",
			"  @ImportedCapacityFeature(DefaultContextInteractions.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
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
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_CORE_LIFECYCLE;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
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
			"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
			"    super.$evaluateBehaviorGuards(event, callbacks);",
			"    if (event instanceof Initialize) {",
			"      final Initialize occurrence = (Initialize) event;",
			"      $guardEvaluator$Initialize(occurrence, callbacks);",
			"    }",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Agent3(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
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
			String actual = it.getGeneratedCode("Agent3");
			assertEquals(EXPECTED01, actual);
		});
	}

}

