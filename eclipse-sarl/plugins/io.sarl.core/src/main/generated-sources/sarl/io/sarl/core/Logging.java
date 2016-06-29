/**
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.core;

import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.core.Capacity;
import javax.annotation.Generated;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Gives access to the logging mechanism associated to the agent.
 */
@SuppressWarnings("all")
public interface Logging extends Capacity {
  /**
   * Change the name used for logging.
   * 
   * @param name - the name used for logging.
   */
  public abstract void setLoggingName(final String name);
  
  /**
   * Print the given message at information logging level.
   * <p>
   * <strong>This function is deprecated.</strong> Please use
   * {@link #info(Object)} in place of <code>println</code>.
   * 
   * @param message - the elements to display.
   * @deprecated Use {@link #info(Object)}.
   */
  @Deprecated
  public abstract void println(final Object message);
  
  /**
   * Print the given message at error logging level.
   * 
   * @param message - the elements to display.
   * @param exception - the exception that is the cause of the error.
   */
  @DefaultValueSource
  public abstract void error(final Object message, @DefaultValue("io.sarl.core.Logging#ERROR_0") final Throwable exception);
  
  /**
   * Default value for the parameter exception
   */
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  @SarlSourceCode("null")
  public final static Throwable $DEFAULT_VALUE$ERROR_0 = null;
  
  /**
   * Print the given message at warning logging level.
   * 
   * @param message - the elements to display.
   * @param exception - the exception that is the cause of the error.
   */
  @DefaultValueSource
  public abstract void warning(final Object message, @DefaultValue("io.sarl.core.Logging#WARNING_0") final Throwable exception);
  
  /**
   * Default value for the parameter exception
   */
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  @SarlSourceCode("null")
  public final static Throwable $DEFAULT_VALUE$WARNING_0 = null;
  
  /**
   * Print the given message at information logging level.
   * 
   * @param message - the elements to display.
   */
  public abstract void info(final Object message);
  
  /**
   * Print the given message at debug logging level.
   * 
   * @param message - the elements to display.
   */
  public abstract void debug(final Object message);
  
  /**
   * Replies if the logging system is displaying the errors.
   * 
   * @return <code>true</code> if the errors are logged,
   *         <code>false</code> if not.
   */
  @Pure
  public abstract boolean isErrorLogEnabled();
  
  /**
   * Replies if the logging system is displaying the warnings.
   * 
   * @return <code>true</code> if the warnings are logged,
   *         <code>false</code> if not.
   */
  @Pure
  public abstract boolean isWarningLogEnabled();
  
  /**
   * Replies if the logging system is displaying the information messages.
   * 
   * @return <code>true</code> if the information messages are logged,
   *         <code>false</code> if not.
   */
  @Pure
  public abstract boolean isInfoLogEnabled();
  
  /**
   * Replies if the logging system is displaying the debugging messages.
   * 
   * @return <code>true</code> if the debugging messages are logged,
   *         <code>false</code> if not.
   */
  @Pure
  public abstract boolean isDebugLogEnabled();
  
  /**
   * Replies the logging level.
   * 
   * @return <code>0</code> for no logging, <code>1</code> for error,
   *         <code>2</code> for warning, <code>3</code> for info,
   *         <code>5</code> for debug.
   */
  @Pure
  public abstract int getLogLevel();
  
  /**
   * Set the logging level.
   * 
   * @param level - <code>0</code> for no logging, <code>1</code> for error,
   *         <code>2</code> for warning, <code>3</code> for info,
   *         <code>5</code> for debug.
   */
  public abstract void setLogLevel(final int level);
  
  /**
   * Print the given message at error logging level.
   * 
   * @param message - the elements to display.
   * @optionalparam exception - the exception that is the cause of the error.
   */
  @DefaultValueUse("java.lang.Object,java.lang.Throwable")
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  public abstract void error(final Object message);
  
  /**
   * Print the given message at warning logging level.
   * 
   * @param message - the elements to display.
   * @optionalparam exception - the exception that is the cause of the error.
   */
  @DefaultValueUse("java.lang.Object,java.lang.Throwable")
  @Generated("io.sarl.lang.jvmmodel.SARLJvmModelInferrer")
  public abstract void warning(final Object message);
}
