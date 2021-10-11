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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.doctest;

import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Event;

/** This is a class.
 */
@SuppressWarnings("all")
@SarlElementType(10)
public class MyTestClass {

	  @DefaultValueSource
	  public void myfunction(final int a, @DefaultValue("io.sarl.ClassTest#MYFUNCTION_0") final String b) {
	  }
	  
	  /**
	   * Default value for the parameter b
	   */
	  @Pure
	  @SyntheticMember
	  @SarlSourceCode("\"abc\"")
	  private final String $DEFAULT_VALUE$MYFUNCTION_0() {
	    return "abc";
	  }
	  
	  @DefaultValueUse("int,java.lang.String")
	  @SyntheticMember
	  public final void myfunction(final int a) {
	    myfunction(a, $DEFAULT_VALUE$MYFUNCTION_0());
	  }

}
