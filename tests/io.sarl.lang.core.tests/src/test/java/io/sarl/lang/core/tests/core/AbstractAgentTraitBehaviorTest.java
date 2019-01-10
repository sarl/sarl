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

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import java.lang.reflect.InvocationTargetException;
import java.util.UUID;

import javax.inject.Inject;

import com.google.common.base.Throwables;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.core.UnimplementedCapacityException;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public abstract class AbstractAgentTraitBehaviorTest extends AbstractSarlTest {

	@Inject
	private ReflectExtensions reflect;
	
	@Nullable
	private AgentMock agent;
	
	@Nullable
	private Object instance;
	
	protected abstract Object createInstance();

	protected Object getInstance() {
		return this.instance;
	}

	protected AgentMock getAgent() {
		return this.agent;
	}
	
	protected Object invoke(Object instance, String functionName, Object... parameters) throws Exception {
		try {
			return this.reflect.invoke(instance, functionName, parameters);
		} catch (InvocationTargetException e) {
			throw Throwables.propagate(e.getTargetException());
		}
	}

	@Before
	public void setUp() {
		this.agent = Mockito.spy(new AgentMock());
		this.instance = createInstance();
	}
	
	@Test
	public void setOwner() throws Exception {
		Agent newAgent = mock(Agent.class);
		Object instance = getInstance();
		assertSame(getAgent(), invoke(instance, "getOwner"));
		this.reflect.invoke(getInstance(), "setOwner", newAgent);
		assertSame(newAgent, invoke(instance, "getOwner"));
	}

	@Test
	public void getOwner() throws Exception {
		assertSame(getAgent(), invoke(getInstance(), "getOwner"));
	}

	@Test(expected = UnimplementedCapacityException.class)
	public void getSkill_unset() throws Exception {
		Object result = invoke(getInstance(), "getSkill", Capacity1.class);
	}

	@Test
	public void getSkill_set() throws Exception {
		Skill1 skill = new Skill1();
		getAgent().setSkill_Fake(skill, Capacity1.class);
		//
		Object result = invoke(getInstance(), "getSkill", Capacity1.class);
		//
		assertInstanceOf(Capacity1.class, result);
	}

	@Test
	public void hasSkill_unset() throws Exception {
		Object result = invoke(getInstance(), "hasSkill", Capacity1.class);
		assertEquals(Boolean.FALSE, result);
	}

	@Test
	public void hasSkill_set() throws Exception {
		Skill1 skill = new Skill1();
		getAgent().setSkill_Fake(skill, Capacity1.class);
		//
		Object result = invoke(getInstance(), "hasSkill", Capacity1.class);
		//
		assertEquals(Boolean.TRUE, result);
	}

	@Test
	public void setSkill() throws Exception {
		Skill1 skill = new Skill1();
		//
		Object result = invoke(getInstance(), "setSkill", skill, Capacity1.class);
		//
		assertSame(skill, result);
		//
		result = invoke(getInstance(), "getSkill", Capacity1.class);
		assertInstanceOf(Capacity1.class, result);
	}

	@Test
	public void operator_mappedTo() throws Exception {
		Skill1 skill = new Skill1();
		//
		invoke(getInstance(), "operator_mappedTo", Capacity1.class, skill);
		//
		Object result = invoke(getInstance(), "getSkill", Capacity1.class);
		assertInstanceOf(Capacity1.class, result);
	}

	@Test(expected = UnimplementedCapacityException.class)
	public void clearSkill_set() throws Exception {
		Skill1 skill = new Skill1();
		getAgent().setSkill_Fake(skill, Capacity1.class);
		//
		Object result = invoke(getInstance(), "clearSkill", Capacity1.class);
		//
		assertSame(skill, result);
		//
		invoke(getInstance(), "getSkill", Capacity1.class);
	}

	@Test
	public void clearSkill_unset() throws Exception {
		Object result = invoke(getInstance(), "clearSkill", Capacity1.class);
		//
		assertNull(result);
	}

	@Test
	public void isMeAddress() throws Exception {
		SpaceID spaceID = Mockito.mock(SpaceID.class);
		//
		UUID randomID = UUID.randomUUID();
		Address adr1 = new Address(spaceID, randomID);
		Object result = invoke(getInstance(), "isMe", adr1);
		assertEquals(Boolean.FALSE, result);
		//
		Address adr2 = new Address(spaceID, getAgent().getID());
		result = invoke(getInstance(), "isMe", adr2);
		assertEquals(Boolean.TRUE, result);
	}

	@Test
	public void isMeUUID() throws Exception {
		UUID randomID = UUID.randomUUID();
		Object result = invoke(getInstance(), "isMe", randomID);
		assertEquals(Boolean.FALSE, result);
		//
		result = invoke(getInstance(), "isMe", getAgent().getID());
		assertEquals(Boolean.TRUE, result);
	}

	@Test
	public void isFromMeEvent() throws Exception {
		SpaceID spaceID = Mockito.mock(SpaceID.class);
		//
		UUID randomID = UUID.randomUUID();
		Address adr1 = new Address(spaceID, randomID);
		Object result = invoke(getInstance(), "isFromMe", new Event(adr1) {});
		assertEquals(Boolean.FALSE, result);
		//
		Address adr2 = new Address(spaceID, getAgent().getID());
		result = invoke(getInstance(), "isFromMe", new Event(adr2) {});
		assertEquals(Boolean.TRUE, result);
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class AgentMock extends Agent {

		public AgentMock() {
			super(null, UUID.randomUUID(), UUID.randomUUID());
		}
		
		public <S extends Skill> S setSkill_Fake(S skill, Class<? extends Capacity>... capacity) {
			// Make the function public
			return setSkill(skill, capacity);
		}
		
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected static interface Capacity1 extends Capacity {
		public static class ContextAwareCapacityWrapper<C extends Capacity1> extends Capacity.ContextAwareCapacityWrapper<C> implements Capacity1 {
			public ContextAwareCapacityWrapper(C capacity, AgentTrait caller) {
				super(capacity, caller);
			}
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected static class Skill1 extends Skill implements Capacity1 {
		public Skill1() {
			//
		}

		@Override
		public void install() {
			// Make the function with a public access
			super.install();
		}

		@Override
		public void uninstall(UninstallationStage stage) {
			// Make the function with a public access
			super.uninstall(stage);
		}

	}

}
