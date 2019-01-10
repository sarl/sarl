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
public class Bug411 extends AbstractSarlTest {

	private String snippet = multilineString(
			"event Initialize",
			"event Evt",
			"capacity DefaultContextinteractions {",
			"  def emit(e : Evt)",
			"}",
			"capacity Schedules {",
			"  def every(t : int, code : () => void)",
			"}",
			"agent A {",
			"    uses DefaultContextinteractions, Schedules",
			"    on Initialize {",
			"        every(1000) [",
			"             emit(new Evt)",
			"        ]",
			"     }",
			"}");

	@Test
	public void bug411() throws Exception {
		SarlScript mas = file(snippet);
		validate(mas).assertNoIssues();
	}

}
