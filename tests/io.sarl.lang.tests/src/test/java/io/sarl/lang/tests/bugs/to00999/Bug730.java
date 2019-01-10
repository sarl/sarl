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

package io.sarl.lang.tests.bugs.to00999;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.bugs.to00699.Bug558;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Ignore functions in deprecated types.
 *
 * <p>https://github.com/sarl/sarl/issues/730
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see Bug558
 */
@SuppressWarnings("all")
public class Bug730 extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug730",
			"event MyEvent",
			"@Deprecated",
			"capacity C1 {",
			"  def myfunction",
			"  def myfunction2",
			"}",
			"capacity C2 {",
			"  def myfunction",
			"  def myfunction3",
			"}",
			"agent TestAgent {",
			"  uses C1, C2",
			"  on MyEvent {",
			"    myfunction",
			"    myfunction2",
			"    myfunction3",
			"  }",
			"}");
	
	@Test
	public void snipset2() throws Exception {
		SarlScript mas = file(SNIPSET1);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

}
