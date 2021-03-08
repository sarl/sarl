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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.tests.bugs.to00999;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid overriding detection.
 *
 * <p>https://github.com/sarl/sarl/issues/739
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #739")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlValidation")
public class Bug739Test extends AbstractSarlTest {

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug739",
			"interface YYY {",
			"  def myfct(a : (int, boolean) => void)",
			"}",
			"class ZZZ implements YYY {",
			"	override myfct(a : (int, boolean) => void) {",
			"	}",
			"}");

	@Test
	public void snipset2() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET2);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug739",
			"import io.sarl.core.Behaviors",
			"import io.sarl.lang.core.Behavior",
			"import io.sarl.lang.core.Event",
			"import io.sarl.lang.core.EventListener",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import io.sarl.lang.util.ConcurrentCollection",
			"skill BehaviorsSkill implements Behaviors {",
			"	def asEventListener : EventListener {",
			"      null",
			"	}",
			"	def registerBehavior(attitude : Behavior, filter : (Event)=>boolean = null, params : Object*) : Behavior {",
			"		null",
			"	}",
			"	def unregisterBehavior(attitude : Behavior) : Behavior {",
			"		null",
			"	}",
			"	def wake(^event : Event, scope : Scope<Address> = null) {",
			"	}",
			"	def wake(beh : Behavior, ^event : Event) {",
			"	}",
			"	def wake(behs : Iterable<Behavior>, ^event : Event) {",
			"	}",
			"	def getRegisteredBehaviors : ConcurrentCollection<Behavior> {",
			"		null",
			"	}",
			"	def hasRegisteredBehavior : boolean {",
			"		false",
			"	}",
			"}",
			"");

	@Test
	public void snipset3() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET3);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

}

