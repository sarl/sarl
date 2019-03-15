/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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
import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class for issue: Invalid it support in serializable scopes (lambda).
 *
 * <p>https://github.com/sarl/sarl/issues/899
 *
 * @author $Author: sgalland$
 * @author $Author: alombard$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/899"
 */
@SuppressWarnings("all")
public class Bug899 extends AbstractSarlTest {

	private static final String SARL_CODE_EXPLICIT_IT = multilineString(
			"package io.sarl.lang.tests.bug899",
			"import io.sarl.core.Initialize",
			"import io.sarl.core.Lifecycle",
			"import io.sarl.core.DefaultContextInteractions",
			"event Hello",
			"agent Y {}",
			"agent X {",
			"  uses Lifecycle, DefaultContextInteractions",
			"  on Initialize {",
			"    var id = Y.spawn",
			"    emit(new Hello, [elt | elt.UUID == id])",
			"    emit(new Hello, [it.UUID == id])",
			"    killMe",
			"  }",
			"}");

	private static final String JAVA_CODE_EXPLICIT_IT = multilineString(
			"package io.sarl.lang.tests.bug899;",
			"",
			"import com.google.common.base.Objects;",
			"import io.sarl.core.DefaultContextInteractions;",
			"import io.sarl.core.Initialize;",
			"import io.sarl.core.Lifecycle;",
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
			"import io.sarl.lang.tests.bug899.Hello;",
			"import io.sarl.lang.tests.bug899.Y;",
			"import io.sarl.lang.util.ClearableReference;",
			"import io.sarl.lang.util.SerializableProxy;",
			"import java.io.ObjectStreamException;",
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
			"public class X extends Agent {",
			"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
			"    Lifecycle _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER = this.$castSkill(Lifecycle.class, (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = this.$getSkill(Lifecycle.class)) : this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE);",
			"    UUID id = _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER.spawn(Y.class);",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$castSkill(DefaultContextInteractions.class, (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = this.$getSkill(DefaultContextInteractions.class)) : this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS);",
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
			"        UUID _uUID = elt.getUUID();",
			"        return Objects.equal(_uUID, id);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address elt) {",
			"        UUID _uUID = elt.getUUID();",
			"        return Objects.equal(_uUID, id);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, id);",
			"      }",
			"    };",
			"    _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(_hello, _function);",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER_1 = this.$castSkill(DefaultContextInteractions.class, (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = this.$getSkill(DefaultContextInteractions.class)) : this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS);",
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
			"        UUID _uUID = it.getUUID();",
			"        return Objects.equal(_uUID, id);",
			"      }",
			"    }",
			"    final Scope<Address> _function_1 = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getUUID();",
			"        return Objects.equal(_uUID, id);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy_1.class, id);",
			"      }",
			"    };",
			"    _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER_1.emit(_hello_1, _function_1);",
			"    Lifecycle _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER_1 = this.$castSkill(Lifecycle.class, (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = this.$getSkill(Lifecycle.class)) : this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE);",
			"    _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER_1.killMe();",
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
			"  @SyntheticMember",
			"  @PerceptGuardEvaluator",
			"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"    assert occurrence != null;",
			"    assert ___SARLlocal_runnableCollection != null;",
			"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Deprecated",
			"  @Inject",
			"  public X(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public X(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	public void parsingExplicitIt() throws Exception {
		SarlScript mas = file(SARL_CODE_EXPLICIT_IT);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void compilingExplicitIt() throws Exception {
		getCompileHelper().compile(SARL_CODE_EXPLICIT_IT, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug899.X");
			assertEquals(JAVA_CODE_EXPLICIT_IT, actual);
		});
	}

	private static final String SARL_CODE_IMPLICIT_IT = multilineString(
			"package io.sarl.lang.tests.bug899",
			"import io.sarl.core.Initialize",
			"import io.sarl.core.Lifecycle",
			"import io.sarl.core.DefaultContextInteractions",
			"event Hello",
			"agent Y {}",
			"agent X {",
			"  uses Lifecycle, DefaultContextInteractions",
			"  on Initialize {",
			"    var id = Y.spawn",
			"    emit(new Hello, [elt | elt.UUID == id])",
			"    emit(new Hello, [UUID == id])",
			"    killMe",
			"  }",
			"}");

	private static final String JAVA_CODE_IMPLICIT_IT = multilineString(
			"package io.sarl.lang.tests.bug899;",
			"",
			"import com.google.common.base.Objects;",
			"import io.sarl.core.DefaultContextInteractions;",
			"import io.sarl.core.Initialize;",
			"import io.sarl.core.Lifecycle;",
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
			"import io.sarl.lang.tests.bug899.Hello;",
			"import io.sarl.lang.tests.bug899.Y;",
			"import io.sarl.lang.util.ClearableReference;",
			"import io.sarl.lang.util.SerializableProxy;",
			"import java.io.ObjectStreamException;",
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
			"public class X extends Agent {",
			"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
			"    Lifecycle _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER = this.$castSkill(Lifecycle.class, (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = this.$getSkill(Lifecycle.class)) : this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE);",
			"    UUID id = _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER.spawn(Y.class);",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$castSkill(DefaultContextInteractions.class, (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = this.$getSkill(DefaultContextInteractions.class)) : this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS);",
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
			"        UUID _uUID = elt.getUUID();",
			"        return Objects.equal(_uUID, id);",
			"      }",
			"    }",
			"    final Scope<Address> _function = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address elt) {",
			"        UUID _uUID = elt.getUUID();",
			"        return Objects.equal(_uUID, id);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy.class, id);",
			"      }",
			"    };",
			"    _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(_hello, _function);",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER_1 = this.$castSkill(DefaultContextInteractions.class, (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = this.$getSkill(DefaultContextInteractions.class)) : this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS);",
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
			"        UUID _uUID = it.getUUID();",
			"        return Objects.equal(_uUID, id);",
			"      }",
			"    }",
			"    final Scope<Address> _function_1 = new Scope<Address>() {",
			"      @Override",
			"      public boolean matches(final Address it) {",
			"        UUID _uUID = it.getUUID();",
			"        return Objects.equal(_uUID, id);",
			"      }",
			"      private Object writeReplace() throws ObjectStreamException {",
			"        return new SerializableProxy($SerializableClosureProxy_1.class, id);",
			"      }",
			"    };",
			"    _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER_1.emit(_hello_1, _function_1);",
			"    Lifecycle _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER_1 = this.$castSkill(Lifecycle.class, (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = this.$getSkill(Lifecycle.class)) : this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE);",
			"    _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER_1.killMe();",
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
			"  @SyntheticMember",
			"  @PerceptGuardEvaluator",
			"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"    assert occurrence != null;",
			"    assert ___SARLlocal_runnableCollection != null;",
			"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Deprecated",
			"  @Inject",
			"  public X(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public X(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	public void parsingImplicitIt() throws Exception {
		SarlScript mas = file(SARL_CODE_IMPLICIT_IT);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void compilingImplicitIt() throws Exception {
		getCompileHelper().compile(SARL_CODE_IMPLICIT_IT, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug899.X");
			assertEquals(JAVA_CODE_IMPLICIT_IT, actual);
		});
	}

}
