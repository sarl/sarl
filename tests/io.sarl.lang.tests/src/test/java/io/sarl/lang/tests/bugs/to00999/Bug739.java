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
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Invalid overriding detection.
 *
 * <p>https://github.com/sarl/sarl/issues/739
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug739 extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug739",
			"import io.sarl.lang.core.Agent",
			"import io.sarl.lang.core.BuiltinCapacitiesProvider",
			"import io.sarl.lang.core.Capacity",
			"import io.sarl.lang.core.Skill",
			"class XXX implements BuiltinCapacitiesProvider {",
			"	override builtinCapacities(^agent : Agent, skillMappingCallback : (Class<? extends Capacity>, Skill)=>void) {",
			"	}",
			"}");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug739",
			"interface YYY {",
			"  def myfct(a : (int, boolean) => void)",
			"}",
			"class ZZZ implements YYY {",
			"	override myfct(a : (int, boolean) => void) {",
			"	}",
			"}");

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug739",
			"import io.sarl.core.Behaviors",
			"import io.sarl.lang.core.Behavior",
			"import io.sarl.lang.core.Event",
			"import io.sarl.lang.core.EventListener",
			"import io.sarl.lang.core.Scope",
			"import io.sarl.lang.core.Address",
			"import io.sarl.lang.util.SynchronizedIterable",
			"skill BehaviorsSkill implements Behaviors {",
			"	def asEventListener : EventListener {",
			"      null",
			"	}",
			"	def registerBehavior(attitude : Behavior, filter : (Event)=>boolean = null) : Behavior {",
			"		null",
			"	}",
			"	def unregisterBehavior(attitude : Behavior) : Behavior {",
			"		null",
			"	}",
			"	def wake(^event : Event, scope : Scope<Address> = null) {",
			"	}",
			"	def getRegisteredBehaviors : SynchronizedIterable<Behavior> {",
			"		null",
			"	}",
			"	def hasRegisteredBehavior : boolean {",
			"		false",
			"	}",
			"}",
			"");

	@Test
	public void snipset1() throws Exception {
		SarlScript mas = file(SNIPSET1);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void snipset2() throws Exception {
		SarlScript mas = file(SNIPSET2);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void snipset3() throws Exception {
		SarlScript mas = file(SNIPSET3);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

}

