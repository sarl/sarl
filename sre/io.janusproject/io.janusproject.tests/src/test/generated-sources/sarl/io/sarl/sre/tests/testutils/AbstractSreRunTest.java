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
import io.sarl.bootstrap.SRE;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.PrivateAPI;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.scoping.extensions.time.TimeExtensions;
import io.sarl.sre.Kernel;
import io.sarl.sre.boot.SreMain;
import io.sarl.sre.boot.configs.subconfigs.BootConfig;
import io.sarl.sre.services.logging.LoggingService;
import io.sarl.sre.tests.testutils.AbstractSreTest;
import io.sarl.sre.tests.testutils.SreRun;
import io.sarl.sre.tests.testutils.TestingAgent;
import io.sarl.tests.api.Nullable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeoutException;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import org.arakhne.afc.bootique.log4j.configs.Log4jIntegrationConfig;
import org.arakhne.afc.bootique.variables.VariableNames;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.junit.Assert;
import org.junit.ComparisonFailure;
import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

/**
 * Abstract class for creating unit tests that needs to launch a SRE instance.
 * 
 * @param <S> - the type of the service.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SarlSpecification("0.11")
@SarlElementType(10)
@SuppressWarnings("all")
public abstract class AbstractSreRunTest extends AbstractSreTest {
  /**
   * The logging level when logging is enable during tests.
   * 
   * @since 0.7
   */
  public static final Level TEST_LOGGING_LEVEL = Level.ALL;
  
  /**
   * No timeout.
   * 
   * @see #STANDARD_TIMEOUT
   * @see #EXTRA_TIMEOUT
   */
  public static final int NO_TIMEOUT = (-1);
  
  /**
   * Reference to the instance of the SRE kernel.
   */
  protected Kernel sreKernel;
  
  /**
   * Reference to the instance of the SRE bootstrap.
   */
  protected SreMain bootstrap;
  
  @Nullable
  private Map<UUID, List<Object>> results;
  
  @Rule
  public TestWatcher sreRunWatcher = new TestWatcher() {
    @Override
    protected void starting(final Description description) {
      SRE.resetServiceLoader();
      SRE.setBootstrap(null);
      final SreRun skipRun = description.<SreRun>getAnnotation(SreRun.class);
      if ((skipRun != null)) {
        AbstractSreRunTest.this.runSre(skipRun.type(), skipRun.enableLogging());
      }
    }
    
    @Override
    protected void finished(final Description description) {
      SRE.resetServiceLoader();
      SRE.setBootstrap(null);
      if ((AbstractSreRunTest.this.sreKernel != null)) {
        AbstractSreRunTest.this.sreKernel = null;
      }
    }
  };
  
  /**
   * Replies the identifier of the lastly booted agent.
   * 
   * @return the identifier of the agent, or {@code null} if no agent was booted.
   * @since 0.8
   */
  @Pure
  protected UUID getBootAgent() {
    return this.bootstrap.getBootAgentIdentifier();
  }
  
  /**
   * Replies result at the given index of the run of the agent.
   * 
   * @param source - the source of the data.
   * @param type - the type of the result.
   * @param index - the index of the result.
   * @return the value; or <code>null</code> if no result.
   */
  @Pure
  protected static <T extends Object> T get(final List<?> source, final Class<T> type, final int index) {
    Object element = source.get(index);
    if (((element == null) || type.isInstance(element))) {
      return type.cast(element);
    }
    return null;
  }
  
  /**
   * Replies result at the given index of the run of the agent.
   * 
   * @param agentId the identifier of the agent.
   * @param type - the type of the result.
   * @param index - the index of the result.
   * @return the value; or <code>null</code> if no result.
   */
  @DefaultValueSource
  @Pure
  protected <T extends Object> T getResult(@DefaultValue("io.sarl.sre.tests.testutils.AbstractSreRunTest#GETRESULT_0") final UUID agentId, final Class<T> type, final int index) {
    if ((this.results != null)) {
      try {
        List<Object> res = null;
        synchronized (this.results) {
          UUID _elvis = null;
          if (agentId != null) {
            _elvis = agentId;
          } else {
            UUID _bootAgent = this.getBootAgent();
            _elvis = _bootAgent;
          }
          res = this.results.get(_elvis);
        }
        if ((res != null)) {
          synchronized (res) {
            return type.cast(res.get(index));
          }
        }
      } catch (final Throwable _t) {
        if (_t instanceof Throwable) {
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
    }
    return null;
  }
  
  /**
   * Default value for the parameter agentId
   */
  @SyntheticMember
  @SarlSourceCode("null")
  private static final UUID $DEFAULT_VALUE$GETRESULT_0 = null;
  
  /**
   * Replies the number of results that are provided by the agent.
   * 
   * @return the number of results.
   */
  @DefaultValueSource
  @Pure
  protected int getNumberOfResults(@DefaultValue("io.sarl.sre.tests.testutils.AbstractSreRunTest#GETNUMBEROFRESULTS_0") final UUID agentId) {
    if ((this.results != null)) {
      List<Object> list = null;
      synchronized (this.results) {
        UUID _elvis = null;
        if (agentId != null) {
          _elvis = agentId;
        } else {
          UUID _bootAgent = this.getBootAgent();
          _elvis = _bootAgent;
        }
        list = this.results.get(_elvis);
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
   * Default value for the parameter agentId
   */
  @SyntheticMember
  @SarlSourceCode("null")
  private static final UUID $DEFAULT_VALUE$GETNUMBEROFRESULTS_0 = null;
  
  /**
   * Test if the number of results provided by the SRE platform is equal to the given number.
   * 
   * @param expected - the expected number of results.
   */
  @Pure
  protected void assertNumberOfResults(final int expected) {
    Assert.assertEquals("Invalid number of results provided by the platform.", expected, this.getNumberOfResults());
  }
  
  /**
   * Replies result for the boot agent or the agent with the given identifier.
   * 
   * @param agentId the identifier of the agent for which the results must be retrieved.
   *     If {@code null} or not provided, the results associated to the boot agent are
   *     replied.
   * @return the results.
   */
  @DefaultValueSource
  @Pure
  protected List<Object> getResults(@DefaultValue("io.sarl.sre.tests.testutils.AbstractSreRunTest#GETRESULTS_0") final UUID agentId) {
    if ((this.results != null)) {
      List<Object> res = null;
      synchronized (this.results) {
        UUID _elvis = null;
        if (agentId != null) {
          _elvis = agentId;
        } else {
          UUID _bootAgent = this.getBootAgent();
          _elvis = _bootAgent;
        }
        res = this.results.get(_elvis);
      }
      if ((res != null)) {
        synchronized (res) {
          return Collections.<Object>unmodifiableList(res);
        }
      }
    }
    return CollectionLiterals.<Object>emptyList();
  }
  
  /**
   * Default value for the parameter agentId
   */
  @SyntheticMember
  @SarlSourceCode("null")
  private static final UUID $DEFAULT_VALUE$GETRESULTS_0 = null;
  
  /**
   * Replies all the results for all the agents.
   * @return the results.
   */
  @Pure
  protected Map<UUID, List<Object>> getAllResultsPerAgent() {
    if ((this.results != null)) {
      return this.results;
    }
    return CollectionLiterals.<UUID, List<Object>>emptyMap();
  }
  
  /**
   * Replies all the results for all the agents.
   * @return the results.
   */
  @Pure
  protected List<Object> getAllResults() {
    if ((this.results != null)) {
      ArrayList<Object> all = CollectionLiterals.<Object>newArrayList();
      synchronized (this.results) {
        Collection<List<Object>> _values = this.results.values();
        for (final List<Object> values : _values) {
          Iterables.<Object>addAll(all, values);
        }
      }
      return all;
    }
    return CollectionLiterals.<Object>emptyList();
  }
  
  /**
   * Replies the initialization parameters for the agents.
   * @return the parameters.
   */
  @Pure
  protected Object[] getAgentInitializationParameters() {
    return new Object[] { this.results };
  }
  
  /**
   * Replies the index of the first result of the given type starting at the given index.
   * 
   * @param agentId the identifier of the agent.
   * @param type - the type of the result.
   * @param fromIndex - the start index.
   * @return the index; or <code>-1</code> if not found.
   */
  @DefaultValueSource
  @Pure
  protected int indexOfResult(@DefaultValue("io.sarl.sre.tests.testutils.AbstractSreRunTest#INDEXOFRESULT_0") final UUID agentId, final Class<?> type, @DefaultValue("io.sarl.sre.tests.testutils.AbstractSreRunTest#INDEXOFRESULT_1") final int fromIndex) {
    if ((this.results != null)) {
      try {
        List<Object> res = null;
        synchronized (this.results) {
          UUID _elvis = null;
          if (agentId != null) {
            _elvis = agentId;
          } else {
            UUID _bootAgent = this.getBootAgent();
            _elvis = _bootAgent;
          }
          res = this.results.get(_elvis);
        }
        if ((res != null)) {
          synchronized (res) {
            for (int i = fromIndex; (i < res.size()); i++) {
              {
                Object r = res.get(i);
                boolean _isInstance = type.isInstance(r);
                if (_isInstance) {
                  return i;
                }
              }
            }
          }
        }
      } catch (final Throwable _t) {
        if (_t instanceof Throwable) {
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
    }
    return (-1);
  }
  
  /**
   * Default value for the parameter agentId
   */
  @SyntheticMember
  @SarlSourceCode("null")
  private static final UUID $DEFAULT_VALUE$INDEXOFRESULT_0 = null;
  
  /**
   * Default value for the parameter fromIndex
   */
  @SyntheticMember
  @SarlSourceCode("0")
  private static final int $DEFAULT_VALUE$INDEXOFRESULT_1 = 0;
  
  /**
   * Start the SRE platform.
   * 
   * @param type - the type of the agent to launch at start-up.
   * @param enableLogging - indicates if the logging is enable or not.
   * @param trackLogErrors indicates if the logged errors should be tracked.
   * @param timeout - the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
   *     See {@link #STANDARD_TIMEOUT}, {@link #EXTRA_TIMEOUT} or {@link #NO_TIMEOUT}.
   * @return the kernel.
   * @throws Exception - if the kernel cannot be launched.
   */
  @DefaultValueSource
  @SuppressWarnings("use_reserved_sarl_annotation")
  @PrivateAPI(isCallerOnly = true)
  protected Kernel runSre(final Class<? extends TestingAgent> type, @DefaultValue("io.sarl.sre.tests.testutils.AbstractSreRunTest#RUNSRE_0") final boolean enableLogging, @DefaultValue("io.sarl.sre.tests.testutils.AbstractSreRunTest#RUNSRE_1") final boolean trackLogErrors, @DefaultValue("io.sarl.sre.tests.testutils.AbstractSreRunTest#RUNSRE_2") final int timeout) {
    try {
      final Kernel kern = this.setupTheSreKernel(type, enableLogging, trackLogErrors);
      try {
        this.waitForTheKernel(timeout);
      } catch (final Throwable _t) {
        if (_t instanceof TimeoutException) {
          final TimeoutException exception = (TimeoutException)_t;
          Kernel.executeKernelStopWhenNoAgentAlive(kern);
          throw exception;
        } else {
          throw Exceptions.sneakyThrow(_t);
        }
      }
      return kern;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Default value for the parameter enableLogging
   */
  @SyntheticMember
  @SarlSourceCode("true")
  private static final boolean $DEFAULT_VALUE$RUNSRE_0 = true;
  
  /**
   * Default value for the parameter trackLogErrors
   */
  @SyntheticMember
  @SarlSourceCode("true")
  private static final boolean $DEFAULT_VALUE$RUNSRE_1 = true;
  
  /**
   * Default value for the parameter timeout
   */
  @SyntheticMember
  @SarlSourceCode("STANDARD_TIMEOUT")
  private static final int $DEFAULT_VALUE$RUNSRE_2 = AbstractSreTest.STANDARD_TIMEOUT;
  
  /**
   * Replies the module that should be injected in order to proceed tests.
   * 
   * @return the module, never {@code null}.
   */
  @Pure
  protected Class<? extends com.google.inject.Module> getTestingModule() {
    return null;
  }
  
  /**
   * Assert the the given kernel has no error on its logs.
   * 
   * @param kern the kernel.
   */
  protected void assertNoErrorLog(final Kernel kern) {
    List<Object> _results = this.getResults(null);
    for (final Object obj : _results) {
      if ((obj instanceof LogRecord)) {
        String _string = ((LogRecord)obj).toString();
        throw new ComparisonFailure("Unexpected error log", "", _string);
      }
    }
  }
  
  /**
   * Set-up the SRE platform.
   * 
   * @param type - the type of the agent to launch at start-up.
   * @param enableLogging - indicates if the logging is enable or not, i.e. messages are output.
   * @param trackLogErrors indicates if the logged errors should be tracked.
   * @return the kernel.
   * @throws Exception - if the kernel cannot be launched.
   */
  protected Kernel setupTheSreKernel(final Class<? extends TestingAgent> type, final boolean enableLogging, final boolean trackLogErrors) {
    try {
      System.setProperty(VariableNames.toPropertyName(BootConfig.BOOT_AGENT_NAME), type.getName());
      Kernel k = this.setupTheSreKernel(enableLogging, trackLogErrors);
      this.bootstrap.startAgent(type, this.getAgentInitializationParameters());
      return k;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Set-up the SRE platform.
   * 
   * @param enableLogging - indicates if the logging is enable or not, i.e. messages are output.
   * @param trackLogErrors indicates if the logged errors should be tracked.
   * @return the kernel.
   * @throws Exception - if the kernel cannot be launched.
   */
  protected Kernel setupTheSreKernel(final boolean enableLogging, final boolean trackLogErrors) {
    Assert.assertNull("SRE already launched.", this.sreKernel);
    Level _xifexpression = null;
    if (enableLogging) {
      _xifexpression = AbstractSreRunTest.TEST_LOGGING_LEVEL;
    } else {
      _xifexpression = Level.OFF;
    }
    final Level logLevel = _xifexpression;
    System.setProperty(VariableNames.toPropertyName(Log4jIntegrationConfig.LEVEL), logLevel.getName());
    final Class<? extends com.google.inject.Module> module = this.getTestingModule();
    this.results = CollectionLiterals.<UUID, List<Object>>newHashMap();
    SreMain _sreMain = new SreMain();
    this.bootstrap = _sreMain;
    this.bootstrap.startWithoutAgent(module);
    this.sreKernel = this.bootstrap.getKernel();
    if (trackLogErrors) {
      this.sreKernel.<LoggingService>getService(LoggingService.class).getPlatformLogger().addHandler(new Handler() {
        @Override
        public void publish(final LogRecord record) {
          Level _level = record.getLevel();
          if ((_level == Level.SEVERE)) {
            Object _get = AbstractSreRunTest.this.getAgentInitializationParameters()[0];
            final List<Object> res = ((List<Object>) _get);
            if ((res != null)) {
              synchronized (res) {
                res.add(record);
              }
            }
          }
        }
        
        @Override
        public void flush() {
        }
        
        @Override
        public void close() {
        }
      });
    }
    return this.sreKernel;
  }
  
  /**
   * Wait for the end of the SRE platform.
   * 
   * @param timeout - the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
   * See {@link #STANDARD_TIMEOUT}, {@link #EXTRA_TIMEOUT} or {@link #NO_TIMEOUT}.
   * @throws Exception - if the kernel cannot be launched.
   */
  public void waitForTheKernel(final int timeout) {
    this.waitForTheKernel(timeout, null);
  }
  
  /**
   * Wait for the end of the SRE platform.
   * 
   * @param timeout - the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
   *     See {@link #STANDARD_TIMEOUT}, {@link #EXTRA_TIMEOUT} or {@link #NO_TIMEOUT}.
   * @param predicate the predicate to use as stop condition.
   * @throws Exception - if the kernel cannot be launched.
   */
  @SuppressWarnings("discouraged_reference")
  public void waitForTheKernel(final int timeout, final Function1<? super Map<UUID, List<Object>>, ? extends Boolean> predicate) {
    try {
      long endTime = 0;
      if ((timeout >= 0)) {
        long _currentTimeMillis = System.currentTimeMillis();
        long _seconds = (timeout) * TimeExtensions.MILLIS_IN_SECOND;
        endTime = (_currentTimeMillis + _seconds);
      } else {
        endTime = (-1);
      }
      boolean isSreRunning = this.sreKernel.isRunning();
      while ((isSreRunning && ((endTime == (-1)) || (System.currentTimeMillis() <= endTime)))) {
        {
          isSreRunning = (this.sreKernel.isRunning() || ((predicate != null) && (!((predicate.apply(this.results)) == null ? false : (predicate.apply(this.results)).booleanValue()))));
          Thread.sleep(100);
        }
      }
      if (isSreRunning) {
        throw new TimeoutException();
      }
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Forget any reference to a Janus kernel.
   * 
   * @since 0.9
   */
  protected void forgetTheKernel() {
    this.sreKernel = null;
  }
  
  /**
   * Replies result at the given index of the run of the agent.
   * 
   * @optionalparam agentId the identifier of the agent.
   * @param type - the type of the result.
   * @param index - the index of the result.
   * @return the value; or <code>null</code> if no result.
   */
  @DefaultValueUse("java.util.UUID,java.lang.Class<T>,int")
  @SyntheticMember
  @Pure
  protected final <T extends Object> T getResult(final Class<T> type, final int index) {
    return getResult($DEFAULT_VALUE$GETRESULT_0, type, index);
  }
  
  /**
   * Replies the number of results that are provided by the agent.
   * 
   * @return the number of results.
   */
  @DefaultValueUse("java.util.UUID")
  @SyntheticMember
  @Pure
  protected final int getNumberOfResults() {
    return getNumberOfResults($DEFAULT_VALUE$GETNUMBEROFRESULTS_0);
  }
  
  /**
   * Replies result for the boot agent or the agent with the given identifier.
   * 
   * @optionalparam agentId the identifier of the agent for which the results must be retrieved.
   *     If {@code null} or not provided, the results associated to the boot agent are
   *     replied.
   * @return the results.
   */
  @DefaultValueUse("java.util.UUID")
  @SyntheticMember
  @Pure
  protected final List<Object> getResults() {
    return getResults($DEFAULT_VALUE$GETRESULTS_0);
  }
  
  /**
   * Replies the index of the first result of the given type starting at the given index.
   * 
   * @optionalparam agentId the identifier of the agent.
   * @param type - the type of the result.
   * @optionalparam fromIndex - the start index.
   * @return the index; or <code>-1</code> if not found.
   */
  @DefaultValueUse("java.util.UUID,java.lang.Class<?>,int")
  @SyntheticMember
  @Pure
  protected final int indexOfResult(final Class<?> type) {
    return indexOfResult($DEFAULT_VALUE$INDEXOFRESULT_0, type, $DEFAULT_VALUE$INDEXOFRESULT_1);
  }
  
  /**
   * Replies the index of the first result of the given type starting at the given index.
   * 
   * @optionalparam agentId the identifier of the agent.
   * @param type - the type of the result.
   * @param fromIndex - the start index.
   * @return the index; or <code>-1</code> if not found.
   */
  @DefaultValueUse("java.util.UUID,java.lang.Class<?>,int")
  @SyntheticMember
  @Pure
  protected final int indexOfResult(final Class<?> type, final int fromIndex) {
    return indexOfResult($DEFAULT_VALUE$INDEXOFRESULT_0, type, fromIndex);
  }
  
  /**
   * Replies the index of the first result of the given type starting at the given index.
   * 
   * @param agentId the identifier of the agent.
   * @param type - the type of the result.
   * @optionalparam fromIndex - the start index.
   * @return the index; or <code>-1</code> if not found.
   */
  @DefaultValueUse("java.util.UUID,java.lang.Class<?>,int")
  @SyntheticMember
  @Pure
  protected final int indexOfResult(final UUID agentId, final Class<?> type) {
    return indexOfResult(agentId, type, $DEFAULT_VALUE$INDEXOFRESULT_1);
  }
  
  /**
   * Start the SRE platform.
   * 
   * @param type - the type of the agent to launch at start-up.
   * @optionalparam enableLogging - indicates if the logging is enable or not.
   * @optionalparam trackLogErrors indicates if the logged errors should be tracked.
   * @optionalparam timeout - the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
   *     See {@link #STANDARD_TIMEOUT}, {@link #EXTRA_TIMEOUT} or {@link #NO_TIMEOUT}.
   * @return the kernel.
   * @throws Exception - if the kernel cannot be launched.
   */
  @DefaultValueUse("java.lang.Class<? extends io.sarl.sre.tests.testutils.TestingAgent>,boolean,boolean,int")
  @SyntheticMember
  @SuppressWarnings("use_reserved_sarl_annotation")
  @PrivateAPI(isCallerOnly = true)
  protected final Kernel runSre(final Class<? extends TestingAgent> type) {
    return runSre(type, $DEFAULT_VALUE$RUNSRE_0, $DEFAULT_VALUE$RUNSRE_1, $DEFAULT_VALUE$RUNSRE_2);
  }
  
  /**
   * Start the SRE platform.
   * 
   * @param type - the type of the agent to launch at start-up.
   * @param enableLogging - indicates if the logging is enable or not.
   * @optionalparam trackLogErrors indicates if the logged errors should be tracked.
   * @optionalparam timeout - the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
   *     See {@link #STANDARD_TIMEOUT}, {@link #EXTRA_TIMEOUT} or {@link #NO_TIMEOUT}.
   * @return the kernel.
   * @throws Exception - if the kernel cannot be launched.
   */
  @DefaultValueUse("java.lang.Class<? extends io.sarl.sre.tests.testutils.TestingAgent>,boolean,boolean,int")
  @SyntheticMember
  @SuppressWarnings("use_reserved_sarl_annotation")
  @PrivateAPI(isCallerOnly = true)
  protected final Kernel runSre(final Class<? extends TestingAgent> type, final boolean enableLogging) {
    return runSre(type, enableLogging, $DEFAULT_VALUE$RUNSRE_1, $DEFAULT_VALUE$RUNSRE_2);
  }
  
  /**
   * Start the SRE platform.
   * 
   * @param type - the type of the agent to launch at start-up.
   * @optionalparam enableLogging - indicates if the logging is enable or not.
   * @optionalparam trackLogErrors indicates if the logged errors should be tracked.
   * @param timeout - the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
   *     See {@link #STANDARD_TIMEOUT}, {@link #EXTRA_TIMEOUT} or {@link #NO_TIMEOUT}.
   * @return the kernel.
   * @throws Exception - if the kernel cannot be launched.
   */
  @DefaultValueUse("java.lang.Class<? extends io.sarl.sre.tests.testutils.TestingAgent>,boolean,boolean,int")
  @SyntheticMember
  @SuppressWarnings("use_reserved_sarl_annotation")
  @PrivateAPI(isCallerOnly = true)
  protected final Kernel runSre(final Class<? extends TestingAgent> type, final int timeout) {
    return runSre(type, $DEFAULT_VALUE$RUNSRE_0, $DEFAULT_VALUE$RUNSRE_1, timeout);
  }
  
  /**
   * Start the SRE platform.
   * 
   * @param type - the type of the agent to launch at start-up.
   * @param enableLogging - indicates if the logging is enable or not.
   * @param trackLogErrors indicates if the logged errors should be tracked.
   * @optionalparam timeout - the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
   *     See {@link #STANDARD_TIMEOUT}, {@link #EXTRA_TIMEOUT} or {@link #NO_TIMEOUT}.
   * @return the kernel.
   * @throws Exception - if the kernel cannot be launched.
   */
  @DefaultValueUse("java.lang.Class<? extends io.sarl.sre.tests.testutils.TestingAgent>,boolean,boolean,int")
  @SyntheticMember
  @SuppressWarnings("use_reserved_sarl_annotation")
  @PrivateAPI(isCallerOnly = true)
  protected final Kernel runSre(final Class<? extends TestingAgent> type, final boolean enableLogging, final boolean trackLogErrors) {
    return runSre(type, enableLogging, trackLogErrors, $DEFAULT_VALUE$RUNSRE_2);
  }
  
  /**
   * Start the SRE platform.
   * 
   * @param type - the type of the agent to launch at start-up.
   * @param enableLogging - indicates if the logging is enable or not.
   * @optionalparam trackLogErrors indicates if the logged errors should be tracked.
   * @param timeout - the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
   *     See {@link #STANDARD_TIMEOUT}, {@link #EXTRA_TIMEOUT} or {@link #NO_TIMEOUT}.
   * @return the kernel.
   * @throws Exception - if the kernel cannot be launched.
   */
  @DefaultValueUse("java.lang.Class<? extends io.sarl.sre.tests.testutils.TestingAgent>,boolean,boolean,int")
  @SyntheticMember
  @SuppressWarnings("use_reserved_sarl_annotation")
  @PrivateAPI(isCallerOnly = true)
  protected final Kernel runSre(final Class<? extends TestingAgent> type, final boolean enableLogging, final int timeout) {
    return runSre(type, enableLogging, $DEFAULT_VALUE$RUNSRE_1, timeout);
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
  public AbstractSreRunTest() {
    super();
  }
}
