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
package io.sarl.lang.tests.modules.sarl.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.inject.Inject;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlFactory;
import io.sarl.lang.sarl.impl.SarlActionImplCustom;
import io.sarl.lang.sarl.impl.SarlAgentImplCustom;
import io.sarl.lang.sarl.impl.SarlBehaviorImplCustom;
import io.sarl.lang.sarl.impl.SarlCapacityImpl;
import io.sarl.lang.sarl.impl.SarlClassImplCustom;
import io.sarl.lang.sarl.impl.SarlSkillImplCustom;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("ecore: custom SarlAction")
@Tag("core")
@Tag("unit")
public class SarlActionImplCustomTest extends AbstractSarlTest {

	@Inject
	private SarlFactory factory;
	
	@Inject
	private SarlActionImplCustom action;
	
	@Inject
	private SarlAgentImplCustom agent;

	@Inject
	private SarlBehaviorImplCustom behavior;

	@Inject
	private SarlCapacityImpl capacity;

	@Inject
	private SarlSkillImplCustom skill;

	@Inject
	private SarlClassImplCustom clazz;

	@Test
	public void default_visibility_in_agent() throws Exception {
		this.agent.getMembers().add(this.action);
		assertEquals(JvmVisibility.PROTECTED, this.action.getVisibility());
	}

	@Test
	public void default_visibility_in_behavior() throws Exception {
		this.behavior.getMembers().add(this.action);
		assertEquals(JvmVisibility.PUBLIC, this.action.getVisibility());
	}

	@Test
	public void default_visibility_in_capacity() throws Exception {
		this.capacity.getMembers().add(this.action);
		assertEquals(JvmVisibility.PUBLIC, this.action.getVisibility());
	}

	@Test
	public void default_visibility_in_skill() throws Exception {
		this.skill.getMembers().add(this.action);
		assertEquals(JvmVisibility.PUBLIC, this.action.getVisibility());
	}

	@Test
	public void default_visibility_in_class() throws Exception {
		this.clazz.getMembers().add(this.action);
		assertEquals(JvmVisibility.PUBLIC, this.action.getVisibility());
	}

}
