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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.core.tests.core;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import io.sarl.lang.core.Behavior;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class BehaviorTest extends AbstractAgentTraitBehaviorTest {

	@Override
	protected Object createInstance() {
		return new Behavior(getAgent()) {
			//
		};
	}

	@Test
	public void contextAwareSkill() throws Exception {
		Skill1 skill = new Skill1();
		getAgent().setSkill_Fake(skill, Capacity1.class);
		//
		Object result = invoke(getInstance(), "getSkill", Capacity1.class);
		//
		assertInstanceOf(Capacity1.class, result);
		assertInstanceOf(Capacity1.ContextAwareCapacityWrapper.class, result);
	}

}
