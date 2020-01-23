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

import com.google.common.base.Objects;
import com.google.common.util.concurrent.Service;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.sre.tests.testutils.AbstractSreTest;
import io.sarl.sre.tests.testutils.AvoidServiceStartForTest;
import io.sarl.sre.tests.testutils.StartServiceForTest;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.ManualMocking;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Pure;
import org.junit.Rule;
import org.junit.Test;
import org.junit.internal.runners.statements.RunBefores;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.Statement;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

/**
 * Abstract class that permits to test the implementation of a service.
 * 
 * @param <S> - the type of the service.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@ManualMocking
@SarlSpecification("0.11")
@SarlElementType(10)
@SuppressWarnings("all")
public abstract class AbstractServiceTest<S extends Service> extends AbstractSreTest {
  /**
   * @author $Author: sgalland$
   * @version $FullVersion$
   * @mavengroupid $GroupId$
   * @mavenartifactid $ArtifactId$
   */
  @SarlSpecification("0.11")
  @SarlElementType(10)
  protected static class SyncExecutor implements Executor {
    @Override
    public void execute(final Runnable command) {
      command.run();
    }
    
    @SyntheticMember
    public SyncExecutor() {
      super();
    }
  }
  
  @InjectMocks
  protected S service;
  
  /**
   * This rule permits to start the service according to the annotation {@link StartServiceForTest}.
   */
  @Rule
  public final TestWatcher serviceWatcher = new Function0<TestWatcher>() {
    @Override
    public TestWatcher apply() {
      abstract class __AbstractServiceTest_1 extends TestWatcher {
        abstract Statement nextStatement(final Statement n);
        
        protected abstract void starting(final Description description);
        
        protected abstract void finished(final Description description);
      }
      
      __AbstractServiceTest_1 ___AbstractServiceTest_1 = new __AbstractServiceTest_1() {
        Statement nextStatement(final Statement n) {
          try {
            final Field f = n.getClass().getDeclaredField("next");
            final boolean isAcc = f.isAccessible();
            try {
              f.setAccessible(true);
              boolean _isAssignableFrom = Statement.class.isAssignableFrom(f.getType());
              if (_isAssignableFrom) {
                return Statement.class.cast(f.get(n));
              }
            } finally {
              f.setAccessible(isAcc);
            }
          } catch (final Throwable _t) {
            if (_t instanceof Throwable) {
            } else {
              throw Exceptions.sneakyThrow(_t);
            }
          }
          return null;
        }
        
        @Override
        public Statement apply(final Statement base, final Description description) {
          try {
            Statement n = base;
            while (((n != null) && (!(n instanceof RunBefores)))) {
              n = this.nextStatement(n);
            }
            if ((n instanceof RunBefores)) {
              final Field f = ((RunBefores)n).getClass().getDeclaredField("befores");
              final boolean isAcc = f.isAccessible();
              try {
                f.setAccessible(true);
                boolean _isAssignableFrom = List.class.isAssignableFrom(f.getType());
                if (_isAssignableFrom) {
                  Object _get = f.get(n);
                  final List<FrameworkMethod> befores = ((List<FrameworkMethod>) _get);
                  boolean foundStart = false;
                  boolean foundInit = false;
                  Iterator<FrameworkMethod> iterator = befores.iterator();
                  while ((((!foundStart) && (!foundInit)) && iterator.hasNext())) {
                    {
                      final FrameworkMethod fm = iterator.next();
                      String _name = fm.getMethod().getName();
                      boolean _equals = Objects.equal("startService", _name);
                      if (_equals) {
                        foundStart = true;
                      }
                      String _name_1 = fm.getMethod().getName();
                      boolean _equals_1 = Objects.equal("createAndInitService", _name_1);
                      if (_equals_1) {
                        foundStart = true;
                      }
                    }
                  }
                  final StartServiceForTest startAnnot = description.getTestClass().<StartServiceForTest>getAnnotation(StartServiceForTest.class);
                  final AvoidServiceStartForTest avoidAnnot = description.<AvoidServiceStartForTest>getAnnotation(AvoidServiceStartForTest.class);
                  List<FrameworkMethod> tmp = null;
                  if ((((!foundInit) && (startAnnot != null)) && startAnnot.createAfterSetUp())) {
                    final Method createServiceMethod = AbstractServiceTest.class.getMethod("createAndInitService");
                    final FrameworkMethod fm = new FrameworkMethod(createServiceMethod);
                    tmp = CollectionLiterals.<FrameworkMethod>newArrayList(((FrameworkMethod[])Conversions.unwrapArray(befores, FrameworkMethod.class)));
                    tmp.add(fm);
                  }
                  if (((((!foundStart) && (avoidAnnot == null)) && (startAnnot != null)) && ((tmp != null) || startAnnot.startAfterSetUp()))) {
                    final Method startServiceMethod = AbstractServiceTest.class.getMethod("startService");
                    final FrameworkMethod fm_1 = new FrameworkMethod(startServiceMethod);
                    if ((tmp == null)) {
                      tmp = CollectionLiterals.<FrameworkMethod>newArrayList(((FrameworkMethod[])Conversions.unwrapArray(befores, FrameworkMethod.class)));
                    }
                    tmp.add(fm_1);
                  }
                  if ((tmp != null)) {
                    f.set(n, Collections.<FrameworkMethod>unmodifiableList(tmp));
                  }
                }
              } finally {
                f.setAccessible(isAcc);
              }
            }
            return super.apply(base, description);
          } catch (Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
          }
        }
        
        @Override
        protected void starting(final Description description) {
          final StartServiceForTest startAnnot = description.getTestClass().<StartServiceForTest>getAnnotation(StartServiceForTest.class);
          if (((startAnnot == null) || (!startAnnot.createAfterSetUp()))) {
            AbstractServiceTest.this.createAndInitService();
            final AvoidServiceStartForTest avoidAnnot = description.<AvoidServiceStartForTest>getAnnotation(AvoidServiceStartForTest.class);
            if ((((avoidAnnot == null) && (startAnnot != null)) && (!startAnnot.startAfterSetUp()))) {
              AbstractServiceTest.this.startService();
            }
          }
        }
        
        @Override
        protected void finished(final Description description) {
          if ((AbstractServiceTest.this.service != null)) {
            AbstractServiceTest.this.service.stopAsync();
            AbstractServiceTest.this.service = null;
          }
        }
      };
      return ___AbstractServiceTest_1;
    }
  }.apply();
  
  /**
   * Start the tested service.
   * 
   * This function should not be called directly. It is invoked by the watcher implemented in the {@link AbstractServiceTest}.
   */
  public void startService() {
    try {
      this.service.startAsync();
      this.service.awaitRunning(2, TimeUnit.SECONDS);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  /**
   * Create and initialize the service
   * 
   * This function should not be called directly. It is invoked by the watcher implemented in the {@link AbstractServiceTest}.
   * 
   * @throws Exception if the service cannot be created.
   */
  public void createAndInitService() {
    this.service = this.newService();
    MockitoAnnotations.initMocks(this);
  }
  
  /**
   * Replies the instance of the service under test.
   * 
   * @return the tested service.
   * @throws Exception if the service cannot be created.
   */
  public abstract S newService();
  
  @Test
  @AvoidServiceStartForTest
  public void notificationOnStart() {
    try {
      final Service.Listener listener = AbstractSarlTest.<Service.Listener>mock(Service.Listener.class);
      AbstractServiceTest.SyncExecutor _syncExecutor = new AbstractServiceTest.SyncExecutor();
      this.service.addListener(listener, _syncExecutor);
      this.service.startAsync();
      this.service.awaitRunning(10, TimeUnit.SECONDS);
      Mockito.<Service.Listener>verify(listener, Mockito.times(1)).starting();
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void notificationOnStop() {
    try {
      final Service.Listener listener = AbstractSarlTest.<Service.Listener>mock(Service.Listener.class);
      AbstractServiceTest.SyncExecutor _syncExecutor = new AbstractServiceTest.SyncExecutor();
      this.service.addListener(listener, _syncExecutor);
      this.service.stopAsync();
      this.service.awaitTerminated(10, TimeUnit.SECONDS);
      final ArgumentCaptor<Service.State> arg = ArgumentCaptor.<Service.State, Service.State>forClass(Service.State.class);
      Mockito.<Service.Listener>verify(listener, Mockito.times(1)).stopping(arg.capture());
      AbstractSarlTest.assertEquals(Service.State.RUNNING, arg.getValue());
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
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
  public AbstractServiceTest() {
    super();
  }
}
