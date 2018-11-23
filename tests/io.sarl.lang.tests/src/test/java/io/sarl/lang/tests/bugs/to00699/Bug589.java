/*
 * Copyright (C) 2014-2018 the original authors or authors.
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

package io.sarl.lang.tests.bugs.to00699;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Equals calls in guards are not pure calls.
 *
 * <p>https://github.com/sarl/sarl/issues/589
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug589 extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug589",
			"event MyEvent",
			"agent TestAgent {",
			"  on MyEvent [occurrence.equals(\"\")] {",
			"  }",
			"}");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug589",
			"event MyEvent",
			"agent TestAgent {",
			"  on MyEvent [occurrence == \"\"] {",
			"  }",
			"}");

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug589",
			"event MyEvent",
			"agent TestAgent {",
			"  def myfct(p : MyEvent) : boolean { true }",
			"  on MyEvent [occurrence.myfct] {",
			"  }",
			"}");

	private static final String SNIPSET4 = multilineString(
			"package io.sarl.lang.tests.bug589",
			"import org.eclipse.xtext.xbase.lib.Pure",
			"event MyEvent",
			"agent TestAgent {",
			"  @Pure",
			"  def myfct(p : MyEvent) : boolean { true }",
			"  on MyEvent [occurrence.myfct] {",
			"  }",
			"}");

	private static final String SNIPSET5 = multilineString(
			"package io.sarl.lang.tests.bug589",
			"event MyEvent",
			"agent TestAgent {",
			"  def isSomething(p : MyEvent) : boolean { true }",
			"  on MyEvent [occurrence.isSomething] {",
			"  }",
			"}");

	private static final String SNIPSET6 = multilineString(
			"package io.sarl.lang.tests.bug589",
			"event MyEvent",
			"agent TestAgent {",
			"  def hasSomething(p : MyEvent) : boolean { true }",
			"  on MyEvent [occurrence.hasSomething] {",
			"  }",
			"}");

	private static final String SNIPSET7 = multilineString(
			"package io.sarl.lang.tests.bug589",
			"event MyEvent",
			"agent TestAgent {",
			"  def getSomething(p : MyEvent) : boolean { true }",
			"  on MyEvent [occurrence.getSomething] {",
			"  }",
			"}");

	private static final String SNIPSET8 = multilineString(
			"package io.sarl.lang.tests.bug589",
			"event MyEvent",
			"agent TestAgent {",
			"  on MyEvent [occurrence.toString === null] {",
			"  }",
			"}");

	private static final String SNIPSET9 = multilineString(
			"package io.sarl.lang.tests.bug589",
			"event MyEvent",
			"agent TestAgent {",
			"  on MyEvent [occurrence.hashCode == 0] {",
			"  }",
			"}");

	private static final String SNIPSET10 = multilineString(
			"package io.sarl.lang.tests.bug589",
			"event MyEvent { var x : int }",
			"agent TestAgent {",
			"  def myfct(p : MyEvent) : boolean { p.x = 3 }",
			"  on MyEvent [occurrence.myfct] {",
			"  }",
			"}");

	@Test
	public void snipset1() throws Exception {
		SarlScript mas = file(SNIPSET1);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void snipset2() throws Exception {
		SarlScript mas = file(SNIPSET2);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void snipset3() throws Exception {
		SarlScript mas = file(SNIPSET3);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void snipset4() throws Exception {
		SarlScript mas = file(SNIPSET4);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void snipset5() throws Exception {
		SarlScript mas = file(SNIPSET5);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void snipset6() throws Exception {
		SarlScript mas = file(SNIPSET6);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void snipset7() throws Exception {
		SarlScript mas = file(SNIPSET7);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void snipset8() throws Exception {
		SarlScript mas = file(SNIPSET8);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void snipset9() throws Exception {
		SarlScript mas = file(SNIPSET9);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void snipset10() throws Exception {
		SarlScript mas = file(SNIPSET10);
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.INVALID_INNER_EXPRESSION,
				"side effect is not allowed");
	}

}
