/*
 * Copyright (C) 2014-2015 the original authors or authors.
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
package io.sarl.lang.tests.sarl.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import com.google.inject.Inject;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.junit.Test;
import org.mockito.Mockito;

import io.sarl.lang.sarl.impl.SarlActionImplCustom;
import io.sarl.lang.sarl.impl.SarlAgentImplCustom;
import io.sarl.lang.sarl.impl.SarlBehaviorImplCustom;
import io.sarl.lang.sarl.impl.SarlCapacityImpl;
import io.sarl.lang.sarl.impl.SarlClassImplCustom;
import io.sarl.lang.sarl.impl.SarlFactoryImplCustom;
import io.sarl.lang.sarl.impl.SarlSkillImplCustom;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SarlAgentImplCustomTest extends AbstractSarlTest {

	@Inject
	private SarlAgentImplCustom agent;

	@Test
	public void modifier_abstract() throws Exception {
		assertFalse(this.agent.isAbstract());
		this.agent.getModifiers().add("abstract");
		assertTrue(this.agent.isAbstract());
	}

	@Test
	public void modifier_strictfp() throws Exception {
		assertFalse(this.agent.isStrictFloatingPoint());
		this.agent.getModifiers().add("strictfp");
		assertTrue(this.agent.isStrictFloatingPoint());
	}

}
