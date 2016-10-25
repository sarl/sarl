package io.sarl.demos.gameoflife.environment.agent;

import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.Initialize;
import io.sarl.core.Lifecycle;
import io.sarl.demos.gameoflife.environment.agent.DefaultGridManagerSkill;
import io.sarl.demos.gameoflife.environment.agent.EnvironmentListener;
import io.sarl.demos.gameoflife.environment.agent.GridManager;
import io.sarl.lang.annotation.ImportedCapacityFeature;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Skill;
import java.util.ArrayList;
import java.util.Collection;
import java.util.UUID;
import javax.inject.Inject;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Inline;

/**
 * @author Jérôme BoULMIER
 */
@SarlSpecification("0.4")
@SuppressWarnings("all")
public class Environment extends Agent {
  protected final ArrayList<EnvironmentListener> listeners = CollectionLiterals.<EnvironmentListener>newArrayList();
  
  @SyntheticMember
  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {
    Object _get = occurrence.parameters[0];
    Integer width = ((Integer) _get);
    Object _get_1 = occurrence.parameters[1];
    Integer height = ((Integer) _get_1);
    DefaultGridManagerSkill _defaultGridManagerSkill = new DefaultGridManagerSkill((width).intValue(), (height).intValue());
    this.<DefaultGridManagerSkill>$setSkill(_defaultGridManagerSkill, GridManager.class);
  }
  
  @Extension
  @ImportedCapacityFeature(DefaultContextInteractions.class)
  @SyntheticMember
  private transient DefaultContextInteractions $CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS;
  
  @Inline(value = "$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null ? (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = getSkill(DefaultContextInteractions.class)) : this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS")
  @SyntheticMember
  private DefaultContextInteractions $CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER() {
    if (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null) {
      this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = getSkill(DefaultContextInteractions.class);
    }
    return this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS;
  }
  
  @Extension
  @ImportedCapacityFeature(Lifecycle.class)
  @SyntheticMember
  private transient Lifecycle $CAPACITY_USE$IO_SARL_CORE_LIFECYCLE;
  
  @Inline(value = "$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null ? (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = getSkill(Lifecycle.class)) : this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE")
  @SyntheticMember
  private Lifecycle $CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER() {
    if (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null) {
      this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = getSkill(Lifecycle.class);
    }
    return this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE;
  }
  
  @SyntheticMember
  @PerceptGuardEvaluator
  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {
    assert occurrence != null;
    assert ___SARLlocal_runnableCollection != null;
    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));
  }
  
  /**
   * Construct an agent.
   * @param builtinCapacityProvider - provider of the built-in capacities.
   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.
   */
  @Inject
  @SyntheticMember
  public Environment(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {
    super(builtinCapacityProvider, parentID, agentID);
  }
  
  @SyntheticMember
  @Override
  protected <S extends Skill> S $setSkill(final S skill, final Class<? extends Capacity>... capacities) {
    this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = null;
    this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = null;
    return super.$setSkill(skill, capacities);
  }
  
  @SyntheticMember
  @Override
  protected <S extends Capacity> S clearSkill(final Class<S> capacity) {
    this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = null;
    this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = null;
    return super.clearSkill(capacity);
  }
}
