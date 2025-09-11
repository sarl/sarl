/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
import io.sarl.lang.sarl.impl.SarlAgentImplCustom;
import io.sarl.lang.sarl.impl.SarlAnnotationTypeImplCustom;
import io.sarl.lang.sarl.impl.SarlBehaviorImplCustom;
import io.sarl.lang.sarl.impl.SarlCapacityImpl;
import io.sarl.lang.sarl.impl.SarlClassImplCustom;
import io.sarl.lang.sarl.impl.SarlSkillImplCustom;
import io.sarl.lang.tests.api.AbstractSarlTest;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("ecore: custom SarlAnnotationType")
@Tag("core")
@Tag("unit")
public class SarlAnnotationTypeImplCustomTest extends AbstractSarlTest {

	@Inject
	private SarlFactory factory;
	
	@Inject
	private SarlAnnotationTypeImplCustom annotationType;
	
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
		this.agent.getMembers().add(this.annotationType);
		assertEquals(JvmVisibility.PROTECTED, this.annotationType.getVisibility());
	}

	@Test
	public void default_visibility_in_behavior() throws Exception {
		this.behavior.getMembers().add(this.annotationType);
		assertEquals(JvmVisibility.PUBLIC, this.annotationType.getVisibility());
	}

	@Test
	public void default_visibility_in_capacity() throws Exception {
		this.capacity.getMembers().add(this.annotationType);
		assertEquals(JvmVisibility.PUBLIC, this.annotationType.getVisibility());
	}

	@Test
	public void default_visibility_in_skill() throws Exception {
		this.skill.getMembers().add(this.annotationType);
		assertEquals(JvmVisibility.PUBLIC, this.annotationType.getVisibility());
	}

	@Test
	public void default_visibility_in_class() throws Exception {
		this.clazz.getMembers().add(this.annotationType);
		assertEquals(JvmVisibility.PUBLIC, this.annotationType.getVisibility());
	}

}
