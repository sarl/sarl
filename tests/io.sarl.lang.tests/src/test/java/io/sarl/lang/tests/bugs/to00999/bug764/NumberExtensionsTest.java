/*
/*
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.tests.bugs.to00999.bug764;

import static org.junit.Assert.*;

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import com.google.common.base.Objects;
import com.google.inject.Inject;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/764"
 */
@SuppressWarnings("all")
public class NumberExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_2366 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Number, b : Number) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2366() throws Exception {
		validate(file(this.SNIPSET_2366)).assertNoErrors();
	}

	private String SNIPSET_2367 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Number, b : Number) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2367() throws Exception {
		validate(file(this.SNIPSET_2367)).assertNoErrors();
	}

	private String SNIPSET_2368 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Number, b : Number) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2368() throws Exception {
		validate(file(this.SNIPSET_2368)).assertNoErrors();
	}

	private String SNIPSET_2369 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Number, b : Number) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2369() throws Exception {
		validate(file(this.SNIPSET_2369)).assertNoErrors();
	}

	private String SNIPSET_2370 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Number, b : Number) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2370() throws Exception {
		validate(file(this.SNIPSET_2370)).assertNoErrors();
	}

	private String SNIPSET_2371 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Number, b : Number) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2371() throws Exception {
		validate(file(this.SNIPSET_2371)).assertNoErrors();
	}

	private String SNIPSET_2372 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Number, b : Number) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2372() throws Exception {
		validate(file(this.SNIPSET_2372)).assertNoErrors();
	}

	private String SNIPSET_2373 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Number, b : Number) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2373() throws Exception {
		validate(file(this.SNIPSET_2373)).assertNoErrors();
	}

	private String SNIPSET_2374 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Number, b : Number) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2374() throws Exception {
		validate(file(this.SNIPSET_2374)).assertNoErrors();
	}

	private String SNIPSET_2375 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Number, b : Number) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2375() throws Exception {
		validate(file(this.SNIPSET_2375)).assertNoErrors();
	}

	private String SNIPSET_2376 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Number, b : Number) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2376() throws Exception {
		validate(file(this.SNIPSET_2376)).assertNoErrors();
	}

	private String SNIPSET_2377 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Number, b : Number) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2377() throws Exception {
		validate(file(this.SNIPSET_2377)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
