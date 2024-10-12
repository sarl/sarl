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
 * http://www.apache.org/licenses/LICENSE-2.0
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

/** Tests for issue #861: Cannot use emit function in the Initialize handler.
 * 
 * <p>See: https://github.com/sarl/sarl/issues/861
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/861"
 * @since 0.11
 */
@DisplayName("Bug #861")
@Tag("core")
public class Bug861Test extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
		"package io.sarl.lang.tests.bug861", //$NON-NLS-1$
		"import io.sarl.api.core.DefaultContextInteractions", //$NON-NLS-1$
		"import io.sarl.api.core.Initialize", //$NON-NLS-1$
		"import io.sarl.api.core.Lifecycle", //$NON-NLS-1$
		"import io.sarl.api.core.Logging", //$NON-NLS-1$
		"event Initialized", //$NON-NLS-1$
		"event CommitSuicide", //$NON-NLS-1$
		"agent EmptyAgent {", //$NON-NLS-1$
		"	uses Lifecycle, DefaultContextInteractions, Logging", //$NON-NLS-1$
		"	on Initialize {", //$NON-NLS-1$
		"		info(\"Empty-Init\")", //$NON-NLS-1$
		"		emit(new Initialized)[it.UUID == occurrence.spawner]", //$NON-NLS-1$
		"	}", //$NON-NLS-1$
		"	on CommitSuicide {", //$NON-NLS-1$
		"		info(\"Empty-Suicide\")", //$NON-NLS-1$
		"		killMe", //$NON-NLS-1$
		"	}", //$NON-NLS-1$
		"}", //$NON-NLS-1$
		"agent BootAgent {", //$NON-NLS-1$
		"    uses Lifecycle, DefaultContextInteractions, Logging", //$NON-NLS-1$
		"    on Initialize {", //$NON-NLS-1$
		"    	info('Boot-Init')", //$NON-NLS-1$
		"		spawn(EmptyAgent)", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"	on Initialized {", //$NON-NLS-1$
		"		info('Boot-Reception')", //$NON-NLS-1$
		"		emit(new CommitSuicide)", //$NON-NLS-1$
		"	}",  //$NON-NLS-1$
		"	on CommitSuicide {", //$NON-NLS-1$
		"		info('Boot-Suicide')", //$NON-NLS-1$
		"		killMe", //$NON-NLS-1$
		"	}", //$NON-NLS-1$
		"}"); //$NON-NLS-1$

	private static final String EXPECTED_BOOTAGENT_01 = multilineString2(false,
		"package io.sarl.lang.tests.bug861;", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"import io.sarl.api.core.DefaultContextInteractions;", //$NON-NLS-1$
		"import io.sarl.api.core.Initialize;", //$NON-NLS-1$
		"import io.sarl.api.core.Lifecycle;", //$NON-NLS-1$
		"import io.sarl.api.core.Logging;", //$NON-NLS-1$
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
		"import org.eclipse.xtext.xbase.lib.Pure;", "", //$NON-NLS-1$ //$NON-NLS-2$
		"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", //$NON-NLS-1$ //$NON-NLS-2$
		"@SarlElementType(" + SarlPackage.SARL_AGENT + ")", //$NON-NLS-1$ //$NON-NLS-2$
		"@SuppressWarnings(\"all\")", //$NON-NLS-1$
		"public class BootAgent extends Agent {", //$NON-NLS-1$
		"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {", //$NON-NLS-1$
		"    Logging _$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER();", //$NON-NLS-1$
		"    _$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER.info(\"Boot-Init\");", //$NON-NLS-1$
		"    Lifecycle _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER();", //$NON-NLS-1$
		"    _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER.spawn(EmptyAgent.class);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  private void $behaviorUnit$Initialized$1(final Initialized occurrence) {", //$NON-NLS-1$
		"    Logging _$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER();", //$NON-NLS-1$
		"    _$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER.info(\"Boot-Reception\");", //$NON-NLS-1$
		"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();", //$NON-NLS-1$
		"    CommitSuicide _commitSuicide = new CommitSuicide();", //$NON-NLS-1$
		"    _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(_commitSuicide);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  private void $behaviorUnit$CommitSuicide$2(final CommitSuicide occurrence) {", //$NON-NLS-1$
		"    Logging _$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER();", //$NON-NLS-1$
		"    _$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER.info(\"Boot-Suicide\");", //$NON-NLS-1$
		"    Lifecycle _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER();", //$NON-NLS-1$
		"    _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER.killMe();", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @Extension", //$NON-NLS-1$
		"  @ImportedCapacityFeature(Lifecycle.class)", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE;", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Pure", //$NON-NLS-1$
		"  private Lifecycle $CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER() {", //$NON-NLS-1$
		"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE.get() == null) {", //$NON-NLS-1$
		"      this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE = $getSkill(Lifecycle.class);", "    }", //$NON-NLS-1$ //$NON-NLS-2$
		"    return $castSkill(Lifecycle.class, this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @Extension", //$NON-NLS-1$
		"  @ImportedCapacityFeature(DefaultContextInteractions.class)", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS;", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Pure", //$NON-NLS-1$
		"  private DefaultContextInteractions $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER() {", //$NON-NLS-1$
		"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) {", //$NON-NLS-1$
		"      this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS = $getSkill(DefaultContextInteractions.class);", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    return $castSkill(DefaultContextInteractions.class, this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @Extension", //$NON-NLS-1$
		"  @ImportedCapacityFeature(Logging.class)", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_LOGGING;", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Pure", //$NON-NLS-1$
		"  private Logging $CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER() {", //$NON-NLS-1$
		"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING == null || this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING.get() == null) {", //$NON-NLS-1$
		"      this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING = $getSkill(Logging.class);", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    return $castSkill(Logging.class, this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @PerceptGuardEvaluator", //$NON-NLS-1$
		"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {", //$NON-NLS-1$
		"    assert occurrence != null;", //$NON-NLS-1$
		"    assert ___SARLlocal_runnableCollection != null;", //$NON-NLS-1$
		"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @PerceptGuardEvaluator", //$NON-NLS-1$
		"  private void $guardEvaluator$CommitSuicide(final CommitSuicide occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {", //$NON-NLS-1$
		"    assert occurrence != null;", //$NON-NLS-1$
		"    assert ___SARLlocal_runnableCollection != null;", //$NON-NLS-1$
		"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$CommitSuicide$2(occurrence));", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @PerceptGuardEvaluator", //$NON-NLS-1$
		"  private void $guardEvaluator$Initialized(final Initialized occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {", //$NON-NLS-1$
		"    assert occurrence != null;", //$NON-NLS-1$
		"    assert ___SARLlocal_runnableCollection != null;", //$NON-NLS-1$
		"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialized$1(occurrence));", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Override", //$NON-NLS-1$
		"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {", //$NON-NLS-1$
		"    super.$getSupportedEvents(toBeFilled);", //$NON-NLS-1$
		"    toBeFilled.add(Initialize.class);", //$NON-NLS-1$
		"    toBeFilled.add(CommitSuicide.class);", //$NON-NLS-1$
		"    toBeFilled.add(Initialized.class);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Override", //$NON-NLS-1$
		"  public boolean $isSupportedEvent(final Class<? extends Event> event) {",  //$NON-NLS-1$
		"    if (Initialize.class.isAssignableFrom(event)) {", //$NON-NLS-1$
		"      return true;", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    if (CommitSuicide.class.isAssignableFrom(event)) {", //$NON-NLS-1$
		"      return true;", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    if (Initialized.class.isAssignableFrom(event)) {", //$NON-NLS-1$
		"      return true;", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    return false;",  //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Override", //$NON-NLS-1$
		"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {", //$NON-NLS-1$
		"    super.$evaluateBehaviorGuards(event, callbacks);", //$NON-NLS-1$
		"    if (event instanceof Initialize) {", //$NON-NLS-1$
		"      final Initialize occurrence = (Initialize) event;", //$NON-NLS-1$
		"      $guardEvaluator$Initialize(occurrence, callbacks);", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    if (event instanceof CommitSuicide) {", //$NON-NLS-1$
		"      final CommitSuicide occurrence = (CommitSuicide) event;", //$NON-NLS-1$
		"      $guardEvaluator$CommitSuicide(occurrence, callbacks);", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    if (event instanceof Initialized) {", //$NON-NLS-1$
		"      final Initialized occurrence = (Initialized) event;", //$NON-NLS-1$
		"      $guardEvaluator$Initialized(occurrence, callbacks);", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  public BootAgent(final UUID arg0, final UUID arg1) {", //$NON-NLS-1$
		"    super(arg0, arg1);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Inject", //$NON-NLS-1$
		"  public BootAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", //$NON-NLS-1$
		"    super(arg0, arg1, arg2);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"}", //$NON-NLS-1$
		""); //$NON-NLS-1$

	private static final String EXPECTED_EMPTYAGENT_01 = multilineString2(false,
		"package io.sarl.lang.tests.bug861;", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"import com.google.common.base.Objects;", //$NON-NLS-1$
		"import io.sarl.api.core.DefaultContextInteractions;", //$NON-NLS-1$
		"import io.sarl.api.core.Initialize;", //$NON-NLS-1$
		"import io.sarl.api.core.Lifecycle;", //$NON-NLS-1$
		"import io.sarl.api.core.Logging;", //$NON-NLS-1$
		"import io.sarl.lang.core.Address;", //$NON-NLS-1$
		"import io.sarl.lang.core.Agent;", //$NON-NLS-1$
		"import io.sarl.lang.core.AtomicSkillReference;", //$NON-NLS-1$
		"import io.sarl.lang.core.DynamicSkillProvider;", //$NON-NLS-1$
		"import io.sarl.lang.core.Event;", //$NON-NLS-1$
		"import io.sarl.lang.core.Scope;", //$NON-NLS-1$
		"import io.sarl.lang.core.annotation.ImportedCapacityFeature;", //$NON-NLS-1$
		"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;", //$NON-NLS-1$
		"import io.sarl.lang.core.annotation.SarlElementType;", //$NON-NLS-1$
		"import io.sarl.lang.core.annotation.SarlSpecification;", //$NON-NLS-1$
		"import io.sarl.lang.core.annotation.SyntheticMember;", //$NON-NLS-1$
		"import io.sarl.lang.core.util.SerializableProxy;", //$NON-NLS-1$
		"import java.io.ObjectStreamException;", //$NON-NLS-1$
		"import java.util.Collection;", //$NON-NLS-1$
		"import java.util.Set;", //$NON-NLS-1$
		"import java.util.UUID;", //$NON-NLS-1$
		"import javax.inject.Inject;", //$NON-NLS-1$
		"import org.eclipse.xtext.xbase.lib.Extension;", //$NON-NLS-1$
		"import org.eclipse.xtext.xbase.lib.Pure;", "", //$NON-NLS-1$ //$NON-NLS-2$
		"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", //$NON-NLS-1$ //$NON-NLS-2$
		"@SarlElementType(" + SarlPackage.SARL_AGENT + ")", //$NON-NLS-1$ //$NON-NLS-2$
		"@SuppressWarnings(\"all\")", //$NON-NLS-1$
		"public class EmptyAgent extends Agent {", //$NON-NLS-1$
		"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {", //$NON-NLS-1$
		"    Logging _$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER();", //$NON-NLS-1$
		"    _$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER.info(\"Empty-Init\");", //$NON-NLS-1$
		"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();", //$NON-NLS-1$
		"    Initialized _initialized = new Initialized();", //$NON-NLS-1$
		"    class $SerializableClosureProxy implements Scope<Address> {", //$NON-NLS-1$
		"      ", //$NON-NLS-1$
		"      private final UUID $_spawner;", //$NON-NLS-1$
		"      ", //$NON-NLS-1$
		"      public $SerializableClosureProxy(final UUID $_spawner) {", //$NON-NLS-1$
		"        this.$_spawner = $_spawner;", //$NON-NLS-1$
		"      }", //$NON-NLS-1$
		"      ", //$NON-NLS-1$
		"      @Override", //$NON-NLS-1$
		"      public boolean matches(final Address it) {", //$NON-NLS-1$
		"        UUID _uUID = it.getID();", //$NON-NLS-1$
		"        return Objects.equal(_uUID, $_spawner);", //$NON-NLS-1$
		"      }", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    final Scope<Address> _function = new Scope<Address>() {", //$NON-NLS-1$
		"      @Override", //$NON-NLS-1$
		"      public boolean matches(final Address it) {", //$NON-NLS-1$
		"        UUID _uUID = it.getID();", //$NON-NLS-1$
		"        return Objects.equal(_uUID, occurrence.spawner);", //$NON-NLS-1$
		"      }", //$NON-NLS-1$
		"      private Object writeReplace() throws ObjectStreamException {", //$NON-NLS-1$
		"        return new SerializableProxy($SerializableClosureProxy.class, occurrence.spawner);", //$NON-NLS-1$
		"      }", //$NON-NLS-1$
		"    };", //$NON-NLS-1$
		"    _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(_initialized, _function);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  private void $behaviorUnit$CommitSuicide$1(final CommitSuicide occurrence) {", //$NON-NLS-1$
		"    Logging _$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER();", //$NON-NLS-1$
		"    _$CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER.info(\"Empty-Suicide\");", //$NON-NLS-1$
		"    Lifecycle _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER();", //$NON-NLS-1$
		"    _$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER.killMe();", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @Extension", //$NON-NLS-1$
		"  @ImportedCapacityFeature(Lifecycle.class)", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE;", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Pure", //$NON-NLS-1$
		"  private Lifecycle $CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE$CALLER() {", //$NON-NLS-1$
		"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE.get() == null) {", //$NON-NLS-1$
		"      this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE = $getSkill(Lifecycle.class);", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    return $castSkill(Lifecycle.class, this.$CAPACITY_USE$IO_SARL_API_CORE_LIFECYCLE);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @Extension", //$NON-NLS-1$
		"  @ImportedCapacityFeature(DefaultContextInteractions.class)", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS;", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Pure", //$NON-NLS-1$
		"  private DefaultContextInteractions $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER() {", //$NON-NLS-1$
		"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) {", //$NON-NLS-1$
		"      this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS = $getSkill(DefaultContextInteractions.class);", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    return $castSkill(DefaultContextInteractions.class, this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @Extension", //$NON-NLS-1$
		"  @ImportedCapacityFeature(Logging.class)", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_LOGGING;", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Pure", //$NON-NLS-1$
		"  private Logging $CAPACITY_USE$IO_SARL_API_CORE_LOGGING$CALLER() {", //$NON-NLS-1$
		"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING == null || this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING.get() == null) {", //$NON-NLS-1$
		"      this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING = $getSkill(Logging.class);", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    return $castSkill(Logging.class, this.$CAPACITY_USE$IO_SARL_API_CORE_LOGGING);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @PerceptGuardEvaluator", //$NON-NLS-1$
		"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {", //$NON-NLS-1$
		"    assert occurrence != null;", //$NON-NLS-1$
		"    assert ___SARLlocal_runnableCollection != null;", //$NON-NLS-1$
		"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @PerceptGuardEvaluator", //$NON-NLS-1$
		"  private void $guardEvaluator$CommitSuicide(final CommitSuicide occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {", //$NON-NLS-1$
		"    assert occurrence != null;", //$NON-NLS-1$
		"    assert ___SARLlocal_runnableCollection != null;", //$NON-NLS-1$
		"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$CommitSuicide$1(occurrence));", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Override", //$NON-NLS-1$
		"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {", //$NON-NLS-1$
		"    super.$getSupportedEvents(toBeFilled);", //$NON-NLS-1$
		"    toBeFilled.add(Initialize.class);", //$NON-NLS-1$
		"    toBeFilled.add(CommitSuicide.class);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Override", //$NON-NLS-1$
		"  public boolean $isSupportedEvent(final Class<? extends Event> event) {",  //$NON-NLS-1$
		"    if (Initialize.class.isAssignableFrom(event)) {", //$NON-NLS-1$
		"      return true;", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    if (CommitSuicide.class.isAssignableFrom(event)) {", //$NON-NLS-1$
		"      return true;", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    return false;",  //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Override", //$NON-NLS-1$
		"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {", //$NON-NLS-1$
		"    super.$evaluateBehaviorGuards(event, callbacks);", //$NON-NLS-1$
		"    if (event instanceof Initialize) {", //$NON-NLS-1$
		"      final Initialize occurrence = (Initialize) event;", //$NON-NLS-1$
		"      $guardEvaluator$Initialize(occurrence, callbacks);", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"    if (event instanceof CommitSuicide) {", //$NON-NLS-1$
		"      final CommitSuicide occurrence = (CommitSuicide) event;", //$NON-NLS-1$
		"      $guardEvaluator$CommitSuicide(occurrence, callbacks);", //$NON-NLS-1$
		"    }", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  public EmptyAgent(final UUID arg0, final UUID arg1) {", //$NON-NLS-1$
		"    super(arg0, arg1);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"", //$NON-NLS-1$
		"  @SyntheticMember", //$NON-NLS-1$
		"  @Inject", //$NON-NLS-1$
		"  public EmptyAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", //$NON-NLS-1$
		"    super(arg0, arg1, arg2);", //$NON-NLS-1$
		"  }", //$NON-NLS-1$
		"}", //$NON-NLS-1$
		""); //$NON-NLS-1$

	@SuppressWarnings("javadoc")
	@Test
	@Tag("sarlValidation")
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET01);
		Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@SuppressWarnings("javadoc")
	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET01, it -> {
			assertEquals(EXPECTED_BOOTAGENT_01, it.getGeneratedCode("io.sarl.lang.tests.bug861.BootAgent")); //$NON-NLS-1$
			assertEquals(EXPECTED_EMPTYAGENT_01, it.getGeneratedCode("io.sarl.lang.tests.bug861.EmptyAgent")); //$NON-NLS-1$
		});
	}

}
