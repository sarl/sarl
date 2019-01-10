/*
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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.tests.bugs.to00699;

import org.junit.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug509 extends AbstractSarlTest {

	@Test
	public void snipset1() throws Exception {
		SarlScript mas = file(multilineString(
				"event MyEvent",
				"agent TestAgent {",
				"  on MyEvent [occurrence.isFromMe] {",
				"  }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void snipset2() throws Exception {
		SarlScript mas = file(multilineString(
				"event MyEvent",
				"agent TestAgent {",
				"  on MyEvent [isFromMe] {",
				"  }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void snipset3() throws Exception {
		SarlScript mas = file(multilineString(
				"event MyEvent",
				"behavior TestBehavior {",
				"  on MyEvent [occurrence.isFromMe] {",
				"  }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	@Test
	public void snipset4() throws Exception {
		SarlScript mas = file(multilineString(
				"event MyEvent",
				"behavior TestBehavior {",
				"  on MyEvent [isFromMe] {",
				"  }",
				"}"));
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

}
