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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.core.tests.core;

import static io.sarl.tests.api.tools.TestAssertions.assertException;
import static io.sarl.tests.api.tools.TestAssertions.assertInstanceOf;
import static io.sarl.tests.api.tools.TestReflections.invokeFunc;
import static io.sarl.tests.api.tools.TestReflections.invokeProc;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.core.UnimplementedCapacityException;
import io.sarl.lang.core.tests.core.AbstractAgentTraitBehaviorTest.Capacity1.ContextAwareCapacityWrapper;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("AbstractAgentTraitBehavior")
@Tag("unit")
@Tag("core")
public abstract class AbstractAgentTraitBehaviorTest extends AbstractSarlTest {

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

	@BeforeEach
	public void setUp() {
		this.agent = spy(new AgentMock());
		this.instance = createInstance();
	}
	
	@Test
	public void setOwner() throws Exception {
		Agent newAgent = mock(Agent.class);
		Object instance = getInstance();
		assertSame(getAgent(), invokeFunc(instance.getClass(), instance, Agent.class, "getOwner"));
		invokeProc(instance.getClass(), instance, "setOwner", new Class[] {Agent.class}, newAgent);
		assertSame(newAgent, invokeFunc(instance.getClass(), instance, Agent.class, "getOwner"));
	}

	@Test
	public void getOwner() throws Exception {
		Object instance = getInstance();
		assertSame(getAgent(), invokeFunc(instance.getClass(), instance, Agent.class, "getOwner"));
	}

	@Test
	public void getSkill_unset() throws Exception {
		assertException(UnimplementedCapacityException.class,() -> {
			Object instance = getInstance();
			invokeProc(instance.getClass(), instance, "getSkill", new Class[] {Class.class}, Capacity1.class);
		});
	}

	@Test
	public void getSkill_set() throws Exception {
		Skill1 skill = new Skill1();
		getAgent().setSkill_Fake(skill, Capacity1.class);
		//
		Object instance = getInstance();
		Capacity result = invokeFunc(instance.getClass(), instance, Capacity.class,
				"getSkill", new Class[] {Class.class}, Capacity1.class);
		//
		assertInstanceOf(Capacity1.class, result);
	}

	@Test
	public void hasSkill_unset() throws Exception {
		Object instance = getInstance();
		Object result = invokeFunc(instance.getClass(), instance, Boolean.class,
				"hasSkill", new Class[] {Class.class}, Capacity1.class);
		assertEquals(Boolean.FALSE, result);
	}

	@Test
	public void hasSkill_set() throws Exception {
		Skill1 skill = new Skill1();
		getAgent().setSkill_Fake(skill, Capacity1.class);
		//
		Object instance = getInstance();
		Object result = invokeFunc(instance.getClass(), instance, Boolean.class,
				"hasSkill", new Class[] {Class.class}, Capacity1.class);
		//
		assertEquals(Boolean.TRUE, result);
	}

	@Test
	public void setSkill() throws Exception {
		Skill1 skill = new Skill1();
		//
		Object instance = getInstance();
		Object result = invokeFunc(instance.getClass(), instance, Skill.class,
				"setSkill", new Class[] {Skill.class, Class[].class}, skill, new Class[] {Capacity1.class});
		//
		assertSame(skill, result);
		//
		result = invokeFunc(instance.getClass(), instance, Capacity.class, 
				"getSkill", new Class[] {Class.class}, Capacity1.class);
		assertInstanceOf(Capacity1.class, result);
	}

	@Test
	public void setSkillIfAbsent() throws Exception {
		Skill1 skill = new Skill1();
		//
		Object instance = getInstance();
		invokeProc(instance.getClass(), instance,
				"setSkillIfAbsent", new Class[] {Skill.class, Class[].class}, skill, new Class[] {Capacity1.class});
		Object result = invokeFunc(instance.getClass(), instance, Object.class,
						"getSkill", new Class[] {Class.class}, Capacity1.class);
		while (result instanceof Capacity.ContextAwareCapacityWrapper) {
			result = ((Capacity.ContextAwareCapacityWrapper<?>) result).getDelegate();
		}
		//
		assertSame(skill, result);
		//
		Skill1 skill2 = new Skill1();
		invokeProc(instance.getClass(), instance,
				"setSkillIfAbsent", new Class[] {Skill.class, Class[].class}, skill2, new Class[] {Capacity1.class});
		result = invokeFunc(instance.getClass(), instance, Object.class,
						"getSkill", new Class[] {Class.class}, Capacity1.class);
		while (result instanceof Capacity.ContextAwareCapacityWrapper) {
			result = ((Capacity.ContextAwareCapacityWrapper<?>) result).getDelegate();
		}
		//
		assertSame(skill, result);
	}

	@Test
	public void operator_mappedTo() throws Exception {
		Skill1 skill = new Skill1();
		//
		Object instance = getInstance();
		invokeProc(instance.getClass(), instance, "operator_mappedTo",
				new Class[] {Class.class, Skill.class}, Capacity1.class, skill);
		//
		Object result = invokeFunc(instance.getClass(), instance, Capacity.class,
				"getSkill", new Class[] {Class.class}, Capacity1.class);
		assertInstanceOf(Capacity1.class, result);
	}

	@Test
	public void clearSkill_set() throws Exception {
		assertException(UnimplementedCapacityException.class, () -> {
			Skill1 skill = new Skill1();
			getAgent().setSkill_Fake(skill, Capacity1.class);
			//
			Object instance = getInstance();
			Object result = invokeFunc(instance.getClass(), instance, Skill.class,
					"clearSkill", new Class[] {Class.class}, Capacity1.class);
			//
			assertSame(skill, result);
			//
			invokeProc(instance.getClass(), instance, "getSkill", new Class[] {Class.class}, Capacity1.class);
		});
	}

	@Test
	public void clearSkill_unset() throws Exception {
		Object instance = getInstance();
		Object result = invokeFunc(instance.getClass(), instance, Skill.class,
				"clearSkill", new Class[] {Class.class}, Capacity1.class);
		//
		assertNull(result);
	}

	@Test
	public void isMeAddress() throws Exception {
		SpaceID spaceID = mock(SpaceID.class);
		//
		Object instance = getInstance();
		UUID randomID = UUID.randomUUID();
		Address adr1 = new Address(spaceID, randomID);
		Object result = invokeFunc(instance.getClass(), instance, Boolean.class,
				"isMe", new Class[] {Address.class}, adr1);
		assertEquals(Boolean.FALSE, result);
		//
		Address adr2 = new Address(spaceID, getAgent().getID());
		result = invokeFunc(instance.getClass(), instance, Boolean.class,
				"isMe", new Class[] {Address.class}, adr2);
		assertEquals(Boolean.TRUE, result);
	}

	@Test
	public void isMeUUID() throws Exception {
		Object instance = getInstance();
		UUID randomID = UUID.randomUUID();
		Object result = invokeFunc(instance.getClass(), instance, Boolean.class,
				"isMe", new Class[] {UUID.class}, randomID);
		assertEquals(Boolean.FALSE, result);
		//
		result = invokeFunc(instance.getClass(), instance, Boolean.class,
				"isMe", new Class[] {UUID.class}, getAgent().getID());
		assertEquals(Boolean.TRUE, result);
	}

	@Test
	public void isFromMeEvent() throws Exception {
		SpaceID spaceID = mock(SpaceID.class);
		//
		Object instance = getInstance();
		UUID randomID = UUID.randomUUID();
		Address adr1 = new Address(spaceID, randomID);
		Object result = invokeFunc(instance.getClass(), instance, Boolean.class,
				"isFromMe", new Class[] {Event.class}, new Event(adr1) {});
		assertEquals(Boolean.FALSE, result);
		//
		Address adr2 = new Address(spaceID, getAgent().getID());
		result = invokeFunc(instance.getClass(), instance, Boolean.class,
				"isFromMe", new Class[] {Event.class}, new Event(adr2) {});
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
			super(UUID.randomUUID(), UUID.randomUUID());
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
	public static interface Capacity1 extends Capacity {
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
	public static class Skill1 extends Skill implements Capacity1 {
		public Skill1() {
			//
		}

		@Override
		public void install() {
			// Make the function with a public access
			super.install();
		}

		@Override
		public void prepareUninstallation() {
			// Make the function with a public access
			super.prepareUninstallation();
		}

		@Override
		public void uninstall() {
			// Make the function with a public access
			super.uninstall();
		}

	}

}
