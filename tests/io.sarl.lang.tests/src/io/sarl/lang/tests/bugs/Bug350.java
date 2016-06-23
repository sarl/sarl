/*
 * $Id$
 * 
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.lang.tests.bugs;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.TestClasspath;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	Bug350.ParserTest.class,
})
@SuppressWarnings("all")
public class Bug350 {

	protected static String snippet = AbstractSarlTest.multilineString(
			"package io.sarl.tests",
			"",
			"import io.sarl.core.Destroy",
			"import io.sarl.core.Initialize",
			"import io.sarl.core.Logging",
			"import io.sarl.core.Schedules",
			"import io.sarl.core.Lifecycle",
			"",
			"agent AbstractAgent {",
			"",
			"	uses Logging",
			"",
			"	on Initialize {",
			"		println(\"Hello World in the super agent!\")",
			"	}",
			"",
			"	def sayGoodBye {",
			"		println(\"Goodbye World!\")",
			"	}",
			"",
			"}",
			"",
			"agent HelloChildAgent extends AbstractAgent{",
			"	",
			"	uses Logging, Lifecycle, Schedules",
			"	",
			"	on Initialize {",
			"		println(\"Hello World in the child agent!\")",
			"		in(2000)[",
			"			println(\"Kill myself\")",
			"			killMe",
			"		]",
			"	}",
			"",
			"	override sayGoodBye {",
			"		super.sayGoodBye()",
			"	}",
			"	",
			"	on Destroy {",
			"		sayGoodBye",
			"	}",
			"}");

	public static class ParserTest extends AbstractSarlUiTest {

		@Test
		@TestClasspath({"io.sarl.core"})
		public void bug350() throws Exception {
			SarlScript mas = file(snippet);
			validate(mas).assertNoIssues();
		}

	}

}
