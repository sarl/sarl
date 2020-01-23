/**
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
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.sre.tests.testutils;

import com.google.common.collect.Iterables;
import io.sarl.core.Initialize;
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.ImportedCapacityFeature;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.DynamicSkillProvider;
import io.sarl.lang.core.Skill;
import io.sarl.lang.util.ClearableReference;
import io.sarl.sre.services.executor.EarlyExitException;
import io.sarl.sre.tests.testutils.AbstractSreRunTest;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.inject.Inject;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure0;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Abstract implementation of an agent that is used for testing SRE.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("potential_field_synchronization_problem")
@SarlSpecification("0.11")
@SarlElementType(19)
public abstract class TestingAgent extends Agent {
  /**
   * Type of action to apply after the testing function.
   * 
   * @author $Author: sgalland$
   * @version $FullVersion$
   * @mavengroupid $GroupId$
   * @mavenartifactid $ArtifactId$
   */
  @SarlSpecification("0.11")
  @SarlElementType(12)
  protected enum RunPolicy {
    /**
     * Run the agent in its normal way.
     */
    STANDARD,
    
    /**
     * Kill the agent at the return of the test function.
     */
    IMMEDIATE_KILLING,
    
    /**
     * Kill the agent after a small amount of time.
     */
    DIFFERED_KILLING;
  }
  
  private Map<UUID, List<Object>> results;
  
  private Object[] initializationParameters;
  
  /**
   * Replies the simple name of the given type name
   * 
   * @param typeName the fully qualifed name of a type.
   */
  protected static String simpleName(final String typeName) {
    int index1 = typeName.lastIndexOf("$");
    int index2 = typeName.lastIndexOf(".");
    int index = Math.max(index1, index2);
    if ((index >= 0)) {
      return typeName.substring((index + 1));
    }
    return typeName;
  }
  
  /**
   * Add a result associated to the current agent.
   * 
   * @param result - the result.
   */
  protected boolean addResult(final Object result) {
    boolean _xifexpression = false;
    if ((this.results != null)) {
      boolean _xblockexpression = false;
      {
        List<Object> list = null;
        synchronized (this.results) {
          final UUID aid = this.getID();
          list = this.results.get(aid);
          if ((list == null)) {
            list = CollectionLiterals.<Object>newArrayList();
            this.results.put(aid, list);
          }
        }
        boolean _xifexpression_1 = false;
        if ((list != null)) {
          boolean _xsynchronizedexpression = false;
          synchronized (list) {
            _xsynchronizedexpression = list.add(result);
          }
          _xifexpression_1 = _xsynchronizedexpression;
        }
        _xblockexpression = _xifexpression_1;
      }
      _xifexpression = _xblockexpression;
    }
    return _xifexpression;
  }
  
  /**
   * Replies the number of results provided by the ran platform.
   * 
   * @return the number of results.
   */
  @Pure
  protected int getNumberOfResults() {
    if ((this.results != null)) {
      List<Object> list = null;
      synchronized (this.results) {
        list = this.results.get(this.getID());
      }
      if ((list != null)) {
        synchronized (list) {
          return list.size();
        }
      }
    }
    return 0;
  }
  
  /**
   * Add a result.
   * 
   * @param result - the result.
   */
  protected boolean addResults(final Collection<?> results) {
    boolean _xifexpression = false;
    if ((this.results != null)) {
      boolean _xblockexpression = false;
      {
        List<Object> list = null;
        synchronized (this.results) {
          list = this.results.get(this.getID());
          if ((list != null)) {
            list = CollectionLiterals.<Object>newArrayList();
            this.results.put(this.getID(), list);
          }
        }
        boolean _xifexpression_1 = false;
        if ((list != null)) {
          boolean _xsynchronizedexpression = false;
          synchronized (list) {
            _xsynchronizedexpression = Iterables.<Object>addAll(list, results);
          }
          _xifexpression_1 = _xsynchronizedexpression;
        }
        _xblockexpression = _xifexpression_1;
      }
      _xifexpression = _xblockexpression;
    }
    return _xifexpression;
  }
  
  /**
   * Replies a unmodifiable view on the results for the current agent.
   * @return the results.
   */
  @Pure
  protected List<Object> getUnmodifiableResults() {
    if ((this.results != null)) {
      List<Object> list = null;
      synchronized (this.results) {
        list = this.results.get(this.getID());
      }
      if ((list != null)) {
        synchronized (list) {
          return Collections.<Object>unmodifiableList(list);
        }
      }
    }
    return CollectionLiterals.<Object>emptyList();
  }
  
  /**
   * Replies a modifiable view on the results for the current agent.
   * @return the results.
   */
  @Pure
  protected List<Object> getModifiableResults() {
    if ((this.results != null)) {
      List<Object> list = null;
      synchronized (this.results) {
        list = this.results.get(this.getID());
        if ((list == null)) {
          list = CollectionLiterals.<Object>newArrayList();
          this.results.put(this.getID(), list);
        }
      }
      return list;
    }
    return CollectionLiterals.<Object>emptyList();
  }
  
  /**
   * Replies the map of the results for all the agents.
   * @return the results.
   */
  @Pure
  protected Map<UUID, List<Object>> getRawResultMap() {
    return this.results;
  }
  
  /**
   * Replies the initialization parameters of the agents.
   * 
   * @return the initialization parameters.
   */
  @Pure
  protected Object[] getAgentInitializationParameters() {
    return this.initializationParameters;
  }
  
  /**
   * Wait until the condition is true or time out; and run the code.
   * 
   * @param condition the condition to validate.
   * @param timeout the time out duration.
   * @param code the code to execute after waiting for evaluating the condition to true.
   */
  @DefaultValueSource
  @SuppressWarnings("discouraged_reference")
  protected void waitAndDo(final Function0<? extends Boolean> condition, @DefaultValue("io.sarl.sre.tests.testutils.TestingAgent#WAITANDDO_0") final int timeout, final Procedure0 code) {
    try {
      class $AssertEvaluator$ {
        final boolean $$result;
        $AssertEvaluator$() {
          this.$$result = (condition != null);
        }
      }
      assert new $AssertEvaluator$().$$result;
      class $AssertEvaluator$_1 {
        final boolean $$result;
        $AssertEvaluator$_1() {
          this.$$result = (code != null);
        }
      }
      assert new $AssertEvaluator$_1().$$result;
      long endTime = 0;
      if ((timeout >= 0)) {
        long _currentTimeMillis = System.currentTimeMillis();
        endTime = (_currentTimeMillis + (timeout * 1000));
      } else {
        endTime = (-1);
      }
      while (((!((condition.apply()) == null ? false : (condition.apply()).booleanValue())) && ((endTime == (-1)) || (System.currentTimeMillis() <= endTime)))) {
        Thread.sleep(100);
      }
      if (((endTime == (-1)) || (System.currentTimeMillis() <= endTime))) {
        code.apply();
      }
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Default value for the parameter timeout
   */
  @SyntheticMember
  @SarlSourceCode("AbstractSreRunTest::STANDARD_TIMEOUT")
  private static final int $DEFAULT_VALUE$WAITANDDO_0 = AbstractSreRunTest.STANDARD_TIMEOUT;
  
  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {
    try {
      this.initializationParameters = occurrence.parameters;
      Object _get = occurrence.parameters[0];
      this.results = ((Map<UUID, List<Object>>) _get);
      try {
        TestingAgent.RunPolicy policy = this.runAgentTest();
        if ((policy != null)) {
          if (policy != null) {
            switch (policy) {
              case IMMEDIATE_KILLING:
                Lifecycle _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER = this.$castSkill(Lifecycle.class, (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = this.$getSkill(Lifecycle.class)) : this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE);
                _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER.killMe();
              case DIFFERED_KILLING:
                Schedules _$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER = this.$castSkill(Schedules.class, (this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES == null || this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES = this.$getSkill(Schedules.class)) : this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES);
                final Procedure1<Agent> _function = (Agent it) -> {
                  Lifecycle _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER_1 = this.$castSkill(Lifecycle.class, (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) ? (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = this.$getSkill(Lifecycle.class)) : this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE);
                  _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER_1.killMe();
                };
                _$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER.in(1000, _function);
                break;
              case STANDARD:
                break;
              default:
                break;
            }
          }
        }
      } catch (final Throwable _t) {
        if (_t instanceof Throwable) {
          final Throwable exception = (Throwable)_t;
          if ((!(exception instanceof EarlyExitException))) {
            this.addResult(exception);
          }
          throw exception;
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Invoked to run the unit test. This function is invoked at agent initialization
   * 
   * @return <code>true</code> for killing the agent during its initialization.
   */
  protected abstract TestingAgent.RunPolicy runAgentTest();
  
  @Extension
  @ImportedCapacityFeature(Lifecycle.class)
  @SyntheticMember
  private transient ClearableReference<Skill> $CAPACITY_USE$IO_SARL_CORE_LIFECYCLE;
  
  @SyntheticMember
  @Pure
  @Inline(value = "$castSkill(Lifecycle.class, ($0$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || $0$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) ? ($0$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = $0$getSkill(Lifecycle.class)) : $0$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE)", imported = Lifecycle.class)
  private Lifecycle $CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER() {
    if (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) {
      this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = $getSkill(Lifecycle.class);
    }
    return $castSkill(Lifecycle.class, this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE);
  }
  
  @Extension
  @ImportedCapacityFeature(Schedules.class)
  @SyntheticMember
  private transient ClearableReference<Skill> $CAPACITY_USE$IO_SARL_CORE_SCHEDULES;
  
  @SyntheticMember
  @Pure
  @Inline(value = "$castSkill(Schedules.class, ($0$CAPACITY_USE$IO_SARL_CORE_SCHEDULES == null || $0$CAPACITY_USE$IO_SARL_CORE_SCHEDULES.get() == null) ? ($0$CAPACITY_USE$IO_SARL_CORE_SCHEDULES = $0$getSkill(Schedules.class)) : $0$CAPACITY_USE$IO_SARL_CORE_SCHEDULES)", imported = Schedules.class)
  private Schedules $CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER() {
    if (this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES == null || this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES.get() == null) {
      this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES = $getSkill(Schedules.class);
    }
    return $castSkill(Schedules.class, this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES);
  }
  
  @SyntheticMember
  @PerceptGuardEvaluator
  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {
    assert occurrence != null;
    assert ___SARLlocal_runnableCollection != null;
    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));
  }
  
  /**
   * Wait until the condition is true or time out; and run the code.
   * 
   * @param condition the condition to validate.
   * @optionalparam timeout the time out duration.
   * @param code the code to execute after waiting for evaluating the condition to true.
   */
  @DefaultValueUse("()=>boolean,int,()=>void")
  @SyntheticMember
  @SuppressWarnings("discouraged_reference")
  protected final void waitAndDo(final Function0<? extends Boolean> condition, final Procedure0 code) {
    waitAndDo(condition, $DEFAULT_VALUE$WAITANDDO_0, code);
  }
  
  @Override
  @Pure
  @SyntheticMember
  public boolean equals(final Object obj) {
    return super.equals(obj);
  }
  
  @Override
  @Pure
  @SyntheticMember
  public int hashCode() {
    int result = super.hashCode();
    return result;
  }
  
  @SyntheticMember
  public TestingAgent(final UUID arg0, final UUID arg1) {
    super(arg0, arg1);
  }
  
  @SyntheticMember
  @Deprecated
  @Inject
  public TestingAgent(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {
    super(arg0, arg1, arg2);
  }
  
  @SyntheticMember
  @Inject
  public TestingAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {
    super(arg0, arg1, arg2);
  }
}
