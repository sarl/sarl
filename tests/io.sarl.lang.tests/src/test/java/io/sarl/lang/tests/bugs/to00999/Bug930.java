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
import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class for issue: Unnecessary warning on occurrence use.
 *
 * <p>https://github.com/sarl/sarl/issues/930
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/930"
 */
@SuppressWarnings("all")
public class Bug930 extends AbstractSarlTest {

	/** Expression elements are inside the same resource as the expression.
	 */
	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug930",
			"import java.util.UUID",
			"event E {",
			"  var id : UUID",
			"}",
			"capacity Log {",
			"    def info(m : String)",
			"}",
			"agent X {",
			"  uses Log",
			"  on E {",
			"     info(\"msg\" + occurrence.id)",
			"  }",
			"}");

	@Test
	public void parsing01() throws Exception {
		SarlScript mas = file(SARL_CODE_01);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	/** Expression elements are outside the same resource as the expression.
	 */
	private static final String SARL_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug930",
			"import io.sarl.core.Initialize",
			"import io.sarl.core.Logging",
			"agent X {",
			"  uses Logging",
			"  on Initialize {",
			"     info(\"msg\" + occurrence.parameters)",
			"  }",
			"}");

	@Test
	public void parsing02() throws Exception {
		SarlScript mas = file(SARL_CODE_02);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	/** Expression elements are outside the same resource as the expression.
	 */
	private static final String SARL_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug930",
			"import io.sarl.core.Initialize",
			"import io.sarl.core.DefaultContextInteractions",
			"event MyEvent {}",
			"agent X {",
			"  uses DefaultContextInteractions",
			"  on Initialize {",
			"     emit(new MyEvent) [it.UUID == occurrence.source.UUID]",
			"  }",
			"}");

	@Test
	public void parsing03() throws Exception {
		SarlScript mas = file(SARL_CODE_03);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

	/** Expression elements are inside the same resource as the expression.
	 */
	private static final String SARL_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug930",
			"import java.util.UUID",
			"event E {",
			"  var id : UUID",
			"}",
			"capacity Sp {",
			"    def emit(m : E, f : (E) => boolean)",
			"}",
			"agent X {",
			"  uses Sp",
			"  on E {",
			"     emit(null) [it.id == occurrence.source.UUID]",
			"  }",
			"}");

	@Test
	public void parsing04() throws Exception {
		SarlScript mas = file(SARL_CODE_04);
		final Validator validator = validate(mas);
		validator.assertNoIssues();
	}

}
