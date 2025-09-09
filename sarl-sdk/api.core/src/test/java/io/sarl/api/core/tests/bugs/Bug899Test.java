/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.api.core.tests.bugs;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestUtils.multilineString2;
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

/** Testing class for issue: Invalid it support in serializable scopes (lambda).
 *
 * <p>https://github.com/sarl/sarl/issues/899
 *
 * @author $Author: sgalland$
 * @author $Author: alombard$
 * @version api.core 0.15.0 20250909-115748
 * @mavengroupid io.sarl.sdk
 * @mavenartifactid api.core
 * @see "https://github.com/sarl/sarl/issues/899"
 */
@DisplayName("Bug #899")
@SuppressWarnings("all")
@Tag("core")
public class Bug899Test extends AbstractSarlTest {

	private static final String SARL_CODE_EXPLICIT_IT = multilineString(
			"package io.sarl.lang.tests.bug899",
			"import io.sarl.api.core.Initialize",
			"import io.sarl.api.core.Lifecycle",
			"import io.sarl.api.core.DefaultContextInteractions",
			"import java.util.UUID",
			"event Hello",
			"agent Y {}",
			"agent X {",
			"  uses Lifecycle, DefaultContextInteractions",
			"  on Initialize {",
			"    val id = UUID::randomUUID",
			"    Y.spawn",
			"    emit(new Hello, [elt | elt.ID == id])",
			"    emit(new Hello, [it.ID == id])",
			"    killMe",
			"  }",
			"}");

	private static final String JAVA_CODE_EXPLICIT_IT = multilineString2(false,
			"package io.sarl.lang.tests.bug899;",
			"",
			"import io.sarl.api.core.DefaultContextInteractions;",
			"import io.sarl.api.core.Initialize;",
			"import io.sarl.api.core.Lifecycle;",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Collection;",
			"import java.util.Objects;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;", //$NON-NLS-1$
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class X extends Agent {",
			"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
			"    final UUID id = UUID.randomUUID();",
			"    Lifecycle _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER.spawn(Y.class);",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();",
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
			"      public boolean matches(final Address elt) {",
			"        UUID _iD = elt.getID();",
			"        return Objects.equals(_iD, id);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address elt) {",
			"        UUID _iD = elt.getID();",
			"        return Objects.equals(_iD, id);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, id);",
			"      }",
			"    };",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(_hello, _function);",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER_1 = this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();",
			"    Hello _hello_1 = new Hello();",
			"    class $SerializableClosureProxy_1 implements Scope<Address> {",
			"      ",
			"      private final UUID id;",
			"      ",
			"      public $SerializableClosureProxy_1(final UUID id) {",
			"        this.id = id;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _iD = it.getID();",
			"        return Objects.equals(_iD, id);",
			"      }",
			"    }",
			"    final Scope<Address> _function_1 = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _iD = it.getID();",
			"        return Objects.equals(_iD, id);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy_1.class, id);",
			"      }",
			"    };",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER_1.emit(_hello_1, _function_1);",
			"    Lifecycle _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER_1 = this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER_1.killMe();",
			"  }",
			"",
			"  @Extension",
			"  @ImportedCapacityFeature(Lifecycle.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE;",
			"",
			"  @SyntheticMember",
			"  @Pure",
			"  private Lifecycle $CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE = $getSkill(Lifecycle.class);",
			"    }",
			"    return $castSkill(Lifecycle.class, this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE);",
			"  }",
			"",
			"  @Extension",
			"  @ImportedCapacityFeature(DefaultContextInteractions.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS;",
			"",
			"  @SyntheticMember",
			"  @Pure",
			"  private DefaultContextInteractions $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS = $getSkill(DefaultContextInteractions.class);",
			"    }",
			"    return $castSkill(DefaultContextInteractions.class, this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @PerceptGuardEvaluator",
			"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"    assert occurrence != null;",
			"    assert ___SARLlocal_runnableCollection != null;",
			"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Override",
			"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
			"    super.$getSupportedEvents(toBeFilled);",
			"    toBeFilled.add(Initialize.class);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Override",
			"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
			"    if (Initialize.class.isAssignableFrom(event)) {",
			"      return true;",
			"    }",
			"    return false;", 
			"  }",
			"",
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
			"",
			"  @SyntheticMember",
			"  public X(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public X(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsingExplicitIt() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_EXPLICIT_IT);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void compilingExplicitIt() throws Exception {
		getCompileHelper().compile(SARL_CODE_EXPLICIT_IT, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug899.X");
			assertEquals(JAVA_CODE_EXPLICIT_IT, actual);
		});
	}

	private static final String SARL_CODE_IMPLICIT_IT = multilineString(
			"package io.sarl.lang.tests.bug899",
			"import io.sarl.api.core.Initialize",
			"import io.sarl.api.core.Lifecycle",
			"import io.sarl.api.core.DefaultContextInteractions",
			"import java.util.UUID",
			"event Hello",
			"agent Y {}",
			"agent X {",
			"  uses Lifecycle, DefaultContextInteractions",
			"  on Initialize {",
			"    val id = UUID::randomUUID",
			"    Y.spawn",
			"    emit(new Hello, [elt | elt.ID == id])",
			"    emit(new Hello, [ID == id])",
			"    killMe",
			"  }",
			"}");

	private static final String JAVA_CODE_IMPLICIT_IT = multilineString2(false,
			"package io.sarl.lang.tests.bug899;",
			"",
			"import io.sarl.api.core.DefaultContextInteractions;",
			"import io.sarl.api.core.Initialize;",
			"import io.sarl.api.core.Lifecycle;",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.Scope;",
			"import io.sarl.lang.core.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.util.SerializableProxy;",
			"import jakarta.inject.Inject;",
			"import java.io.ObjectStreamException;",
			"import java.util.Collection;",
			"import java.util.Objects;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class X extends Agent {",
			"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
			"    final UUID id = UUID.randomUUID();",
			"    Lifecycle _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER.spawn(Y.class);",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();",
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
			"      public boolean matches(final Address elt) {",
			"        UUID _iD = elt.getID();",
			"        return Objects.equals(_iD, id);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address elt) {",
			"        UUID _iD = elt.getID();",
			"        return Objects.equals(_iD, id);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, id);",
			"      }",
			"    };",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(_hello, _function);",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER_1 = this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();",
			"    Hello _hello_1 = new Hello();",
			"    class $SerializableClosureProxy_1 implements Scope<Address> {",
			"      ",
			"      private final UUID id;",
			"      ",
			"      public $SerializableClosureProxy_1(final UUID id) {",
			"        this.id = id;",
			"      }",
			"      ",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _iD = it.getID();",
			"        return Objects.equals(_iD, id);",
			"      }",
			"    }",
			"    final Scope<Address> _function_1 = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _iD = it.getID();",
			"        return Objects.equals(_iD, id);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy_1.class, id);",
			"      }",
			"    };",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER_1.emit(_hello_1, _function_1);",
			"    Lifecycle _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER_1 = this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER_1.killMe();",
			"  }",
			"",
			"  @Extension",
			"  @ImportedCapacityFeature(Lifecycle.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE;",
			"",
			"  @SyntheticMember",
			"  @Pure",
			"  private Lifecycle $CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE = $getSkill(Lifecycle.class);",
			"    }",
			"    return $castSkill(Lifecycle.class, this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE);",
			"  }",
			"",
			"  @Extension",
			"  @ImportedCapacityFeature(DefaultContextInteractions.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS;",
			"",
			"  @SyntheticMember",
			"  @Pure",
			"  private DefaultContextInteractions $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS = $getSkill(DefaultContextInteractions.class);",
			"    }",
			"    return $castSkill(DefaultContextInteractions.class, this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @PerceptGuardEvaluator",
			"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"    assert occurrence != null;",
			"    assert ___SARLlocal_runnableCollection != null;",
			"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Override",
			"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
			"    super.$getSupportedEvents(toBeFilled);",
			"    toBeFilled.add(Initialize.class);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Override",
			"  public boolean $isSupportedEvent(final Class<? extends Event> event) {", 
			"    if (Initialize.class.isAssignableFrom(event)) {",
			"      return true;",
			"    }",
			"    return false;", 
			"  }",
			"",
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
			"",
			"  @SyntheticMember",
			"  public X(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public X(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsingImplicitIt() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_IMPLICIT_IT);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void compilingImplicitIt() throws Exception {
		getCompileHelper().compile(SARL_CODE_IMPLICIT_IT, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug899.X");
			assertEquals(JAVA_CODE_IMPLICIT_IT, actual);
		});
	}

}
