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
package io.sarl.lang.tests.bugs.to00399;

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug295 extends AbstractSarlTest {

	private String snippet = multilineString(
			"capacity C1 {",
			"    def fct(a : Object=null, b : int=0, c : Object*)",
			"}",
			"agent A1 {",
			"    uses C1",
			"    def mytest {",
			"        fct(new Object)",
			"    }",
			"}",
			"");

	@Test
	public void bug295() throws Exception {
		SarlScript mas = file(snippet);
		validate(mas).assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.AMBIGUOUS_FEATURE_CALL,
				"both match");
	}

}
