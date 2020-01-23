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

import com.google.common.util.concurrent.Service;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.tests.api.AbstractSarlTest;
import java.util.Properties;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

/**
 * Abstract class that is providing useful tools for unit tests.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SarlSpecification("0.11")
@SarlElementType(10)
@SuppressWarnings("all")
public abstract class AbstractSreTest extends AbstractSarlTest {
  /**
   * Short timeout in seconds.
   * 
   * @see #EXTRA_TIMEOUT
   * @see #NO_TIMEOUT
   * @see #STANDARD_TIMEOUT
   */
  public static final int SHORT_TIMEOUT = 20;
  
  /**
   * Standard timeout in seconds.
   * 
   * @see #EXTRA_TIMEOUT
   * @see #NO_TIMEOUT
   * @see #SHORT_TIMEOUT
   */
  public static final int STANDARD_TIMEOUT = 40;
  
  /**
   * Extra timeout in seconds.
   * 
   * @see #STANDARD_TIMEOUT
   * @see #NO_TIMEOUT
   * @see #SHORT_TIMEOUT
   */
  public static final int EXTRA_TIMEOUT = 240;
  
  private Properties savedProperties;
  
  /**
   * This rule permits to clean automatically the fields at the end of the test.
   */
  @Rule
  public final TestWatcher rootSreWatchter = new TestWatcher() {
    @Override
    public void starting(final Description description) {
      final Properties props = System.getProperties();
      Object _clone = props.clone();
      AbstractSreTest.this.savedProperties = ((Properties) _clone);
    }
    
    @Override
    public void finished(final Description description) {
      final Properties sp = AbstractSreTest.this.savedProperties;
      AbstractSreTest.this.savedProperties = null;
      if ((sp != null)) {
        final Properties props = System.getProperties();
        props.clear();
        props.putAll(sp);
      }
    }
  };
  
  /**
   * Start the given service manually.
   * 
   * @param service the service to start.
   */
  @SuppressWarnings("discouraged_reference")
  protected void startServiceManually(final Service service) {
    service.startAsync();
    Service.State state = service.state();
    while (((((state != null) && (state != Service.State.STOPPING)) && (state != Service.State.FAILED)) && (state != Service.State.RUNNING))) {
      {
        try {
          Thread.sleep(1);
        } catch (final Throwable _t) {
          if (_t instanceof Throwable) {
          } else {
            throw Exceptions.sneakyThrow(_t);
          }
        }
        state = service.state();
      }
    }
    if ((state == Service.State.FAILED)) {
      Throwable _failureCause = service.failureCause();
      throw new RuntimeException(_failureCause);
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
  public AbstractSreTest() {
    super();
  }
}
