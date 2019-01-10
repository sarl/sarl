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
package io.sarl.lang.tests.modules.sarl.impl;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import com.google.inject.Inject;
import org.junit.Test;

import io.sarl.lang.sarl.impl.SarlSkillImplCustom;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SarlSkillImplCustomTest extends AbstractSarlTest {

	@Inject
	private SarlSkillImplCustom skill;

	@Test
	public void modifier_abstract() throws Exception {
		assertFalse(this.skill.isAbstract());
		this.skill.getModifiers().add("abstract");
		assertTrue(this.skill.isAbstract());
	}

	@Test
	public void modifier_strictfp() throws Exception {
		assertFalse(this.skill.isStrictFloatingPoint());
		this.skill.getModifiers().add("strictfp");
		assertTrue(this.skill.isStrictFloatingPoint());
	}

}
