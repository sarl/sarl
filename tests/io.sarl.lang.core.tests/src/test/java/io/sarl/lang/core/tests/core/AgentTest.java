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

import static org.mockito.Mockito.doReturn;

import java.lang.reflect.InvocationTargetException;
import java.security.InvalidParameterException;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import javax.inject.Inject;

import com.google.common.base.Throwables;
import org.junit.Before;
import org.junit.Test;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.DefaultSkill;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.UnimplementedCapacityException;
import io.sarl.lang.util.ClearableReference;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class AgentTest extends AbstractSarlTest {

	private UUID id;

	private AgentMock agent;
	
	@Inject
	private ReflectExtensions reflect;

	private static Address mockAddress(UUID agentID) {
		Address adr = mock(Address.class);
		doReturn(agentID).when(adr).getUUID();
		return adr;
	}

	private static Event mockEvent(UUID agentID) {
		Address adr = mockAddress(agentID);
		Event evt = mock(Event.class);
		doReturn(adr).when(evt).getSource();
		return evt;
	}

	private void assertNoSkill(Class<? extends Capacity> c) {
		try {
			try {
				this.reflect.invoke(this.agent, "getSkill", c);
			} catch (InvocationTargetException e) {
				throw Throwables.propagate(e.getTargetException());
			} catch (Exception e) {
				throw Throwables.propagate(e);
			}
			fail("Expecting the exception UnimplementedCapacityException, but got no exception."); //$NON-NLS-1$
		} catch (UnimplementedCapacityException exception) {
			//
		}
	}

	private void assertSkill(Class<? extends Capacity> c, Skill expected) {
		Object r;
		try {
			r = this.reflect.invoke(this.agent, "getSkill", c);
		} catch (InvocationTargetException e) {
			throw Throwables.propagate(e.getTargetException());
		} catch (Exception e) {
			throw Throwables.propagate(e);
		}
		assertSame(expected, r);
	}

	private void assertSkill(Class<? extends Capacity> c) {
		Object r;
		try {
			r = this.reflect.invoke(this.agent, "getSkill", c);
		} catch (InvocationTargetException e) {
			throw Throwables.propagate(e.getTargetException());
		} catch (Exception e) {
			throw Throwables.propagate(e);
		}
		assertNotNull(r);
	}

	@Before
	public void setUp() {
		this.id = UUID.randomUUID();
		this.agent = spy(new AgentMock(this.id));
	}

	@Test
	public void getID() {
		UUID aid = this.agent.getID();
		assertNotNull(aid);
		assertNotEquals(this.id, aid);
	}

	@Test
	public void getParentID() {
		assertSame(this.id, this.agent.getParentID());
	}

	@Test
	public void setSkill() {
		Skill s1, s2, r;

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		s1 = new Skill1();
		r = this.agent.setSkill_Fake(s1, Capacity1.class);
		assertSame(s1, r);
		assertSkill(Capacity1.class, s1);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		try {
			this.agent.setSkill_Fake(new Skill2(), Capacity1.class);
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		}
		catch(InvalidParameterException exception) {
			//
		}

		try {
			this.agent.setSkill_Fake(new Skill3(), Capacity1.class);
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		}
		catch(InvalidParameterException exception) {
			//
		}

		s2 = new Skill2();
		r = this.agent.setSkill_Fake(s2, Capacity2.class);
		assertSame(s2, r);
		assertSkill(Capacity1.class, s1);
		assertSkill(Capacity2.class, s2);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		try {
			this.agent.setSkill_Fake(new Skill1(), Capacity2.class);
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		}
		catch(InvalidParameterException exception) {
			//
		}

		try {
			this.agent.setSkill_Fake(new Skill3(), Capacity2.class);
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		}
		catch(InvalidParameterException exception) {
			//
		}
	}

	@Test
	public void setSkill_withoutCapacity() {
		Skill s4, r;

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);
		assertNoSkill(Skill4.class);

		s4 = new Skill4();
		r = this.agent.setSkill_Fake(s4);
		
		assertSame(s4, r);
		assertSkill(Capacity1.class, s4);
		assertSkill(Capacity2.class, s4);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);
	}

	@Test
	public void clearSkill_multipleCapacityImplementation() {
		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);
		assertNoSkill(Skill4.class);
		Skill s4 = new Skill4();
		this.agent.setSkill_Fake(s4);
		assertSkill(Capacity1.class, s4);
		assertSkill(Capacity2.class, s4);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		this.agent.clearSkill(Capacity1.class);

		assertNoSkill(Capacity1.class);
		assertSkill(Capacity2.class, s4);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		// again
		this.agent.clearSkill(Capacity1.class);

		assertNoSkill(Capacity1.class);
		assertSkill(Capacity2.class, s4);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		this.agent.clearSkill(Capacity2.class);

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);
	}

	@Test
	public void clearSkill() {
		this.agent.setSkill_Fake(new Skill1(), Capacity1.class);
		this.agent.setSkill_Fake(new Skill2(), Capacity2.class);
		assertSkill(Capacity1.class);
		assertSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		this.agent.clearSkill(Capacity1.class);

		assertNoSkill(Capacity1.class);
		assertSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		// again
		this.agent.clearSkill(Capacity1.class);

		assertNoSkill(Capacity1.class);
		assertSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		this.agent.clearSkill(Capacity2.class);

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);
	}

	@Test
	public void hasSkill() {
		this.agent.setSkill_Fake(new Skill1(), Capacity1.class);
		this.agent.setSkill_Fake(new Skill2(), Capacity2.class);
		assertTrue(this.agent.hasSkill(Capacity1.class));
		assertTrue(this.agent.hasSkill(Capacity2.class));

		this.agent.clearSkill(Capacity1.class);

		assertFalse(this.agent.hasSkill(Capacity1.class));
		assertTrue(this.agent.hasSkill(Capacity2.class));
		assertFalse(this.agent.hasSkill(Skill1.class));
		assertFalse(this.agent.hasSkill(Skill2.class));

		// again
		this.agent.clearSkill(Capacity1.class);

		assertFalse(this.agent.hasSkill(Capacity1.class));
		assertTrue(this.agent.hasSkill(Capacity2.class));
		assertFalse(this.agent.hasSkill(Skill1.class));
		assertFalse(this.agent.hasSkill(Skill2.class));

		this.agent.clearSkill(Capacity2.class);

		assertFalse(this.agent.hasSkill(Capacity1.class));
		assertFalse(this.agent.hasSkill(Capacity2.class));
		assertFalse(this.agent.hasSkill(Skill1.class));
		assertFalse(this.agent.hasSkill(Skill2.class));
	}

	@Test
	public void operator_mappedTo() {
		Skill s1, s2;

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		s1 = new Skill1();
		this.agent.operator_mappedTo(Capacity1.class, s1);
		assertSkill(Capacity1.class, s1);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		try {
			this.agent.operator_mappedTo(Capacity1.class, new Skill2());
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		}
		catch(InvalidParameterException exception) {
			//
		}

		try {
			this.agent.operator_mappedTo(Capacity1.class, new Skill3());
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		}
		catch(InvalidParameterException exception) {
			//
		}

		s2 = new Skill2();
		this.agent.operator_mappedTo(Capacity2.class, s2);
		assertSkill(Capacity1.class, s1);
		assertSkill(Capacity2.class, s2);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		try {
			this.agent.operator_mappedTo(Capacity2.class, new Skill1());
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		}
		catch(InvalidParameterException exception) {
			//
		}

		try {
			this.agent.operator_mappedTo(Capacity2.class, new Skill3());
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		}
		catch(InvalidParameterException exception) {
			//
		}
	}

	@Test
	public void isMeAddress() {
		Address adr;

		adr = mockAddress(this.agent.getID());
		assertTrue(this.agent.isMe(adr));

		adr = mockAddress(UUID.fromString(this.agent.getID().toString()));
		assertTrue(this.agent.isMe(adr));

		adr = mockAddress(UUID.randomUUID());
		assertFalse(this.agent.isMe(adr));
	}

	@Test
	public void isMeUUID() {
		assertTrue(this.agent.isMe(this.agent.getID()));
		assertTrue(this.agent.isMe(UUID.fromString(this.agent.getID().toString())));
		assertFalse(this.agent.isMe(UUID.randomUUID()));
	}

	@Test
	public void isFromMeEvent() {
		Event evt;

		evt = mockEvent(this.agent.getID());
		assertTrue(this.agent.isFromMe(evt));

		evt = mockEvent(UUID.fromString(this.agent.getID().toString()));
		assertTrue(this.agent.isFromMe(evt));

		evt = mockEvent(UUID.randomUUID());
		assertFalse(this.agent.isFromMe(evt));
	}

	@Test
	public void skillInstallation_withMultipleSetSkill() {
		Skill4 s4 = new Skill4();
		this.agent.setSkill_Fake(s4, Capacity1.class);
		
		assertEquals(1, s4.installCalls());
		
		this.agent.setSkill_Fake(s4, Capacity1.class);

		assertEquals(0, s4.installCalls());
	}

	@Test
	public void skillInstallation_withSingleSetSkill() {
		Skill4 s4 = new Skill4();
		this.agent.setSkill_Fake(s4);
		assertEquals(1, s4.installCalls());
	}

	@Test
	public void skillUninstallation_withClearSkill() {
		Skill4 s4 = new Skill4();
		this.agent.setSkill_Fake(s4);

		this.agent.clearSkill(Capacity1.class);
		assertEquals(0, s4.uninstallPreCalls());
		assertEquals(0, s4.uninstallPostCalls());

		this.agent.clearSkill(Capacity2.class);
		assertEquals(1, s4.uninstallPreCalls());
		assertEquals(1, s4.uninstallPostCalls());
	}

	@Test(expected = UnimplementedCapacityException.class)
	public void getSkill_noSetSkill() {
		this.agent.$getSkill(Capacity1.class);
	}

	@Test
	public void getSkill_setSkill() {
		Skill1 so = new Skill1();
		this.agent.setSkill_Fake(so);
		//
		ClearableReference<Skill> ref0 = this.agent.$getSkill(Capacity1.class);
		assertNotNull(ref0);
		Skill s0 = ref0.get();
		assertSame(so, s0);
		//
		ClearableReference<Skill> ref1 = this.agent.$getSkill(Capacity1.class);
		assertNotNull(ref1);
		assertSame(ref0, ref1);
	}

	@Test
	public void getSkill_defaultskill() {
		ClearableReference<Skill> ref0 = this.agent.$getSkill(Capacity3.class);
		assertNotNull(ref0);
		Skill s0 = ref0.get();
		assertNotNull(s0);
		assertInstanceOf(Skill5.class, s0);
		//
		ClearableReference<Skill> ref1 = this.agent.$getSkill(Capacity3.class);
		assertNotNull(ref1);
		assertSame(ref0, ref1);
	}

	/** Only for making public several protected methods.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class AgentMock extends Agent {

		/**
		 * @param parentID
		 */
		public AgentMock(UUID parentID) {
			super(parentID, null);
		}

		public <S extends Skill> S setSkill_Fake(S skill, Class<? extends Capacity>... capacity) {
			return setSkill(skill, capacity);
		}

		@Override
		public ClearableReference<Skill> $getSkill(Class<? extends Capacity> capacity) {
			return super.$getSkill(capacity);
		}

		@Override
		public <S extends Capacity> S clearSkill(Class<S> capacity) {
			return super.clearSkill(capacity);
		}

		@Override
		public boolean hasSkill(Class<? extends Capacity> capacity) {
			return super.hasSkill(capacity);
		}

		@Override
		public <S extends Skill> void operator_mappedTo(
				Class<? extends Capacity> capacity, S skill) {
			super.operator_mappedTo(capacity, skill);
		}

		@Override
		public boolean isMe(Address address) {
			return super.isMe(address);
		}

		@Override
		public boolean isMe(UUID id) {
			return super.isMe(id);
		}

		@Override
		protected boolean isFromMe(Event event) {
			return super.isFromMe(event);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static interface Capacity1 extends Capacity {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static interface Capacity2 extends Capacity {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DefaultSkill(Skill5.class)
	private static interface Capacity3 extends Capacity {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill1 extends Skill implements Capacity1 {
		public Skill1() {
			//
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill2 extends Skill implements Capacity2 {
		public Skill2() {
			//
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill3 extends Skill {
		public Skill3() {
			//
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill4 extends Skill1 implements Capacity2 {
		private final AtomicInteger installCalls = new AtomicInteger();
		private final AtomicInteger uninstallPreCalls = new AtomicInteger();
		private final AtomicInteger uninstallPostCalls = new AtomicInteger();
		public Skill4() {
			//
		}
		@Override
		protected void install() {
			super.install();
			this.installCalls.incrementAndGet();
		}
		@Override
		protected void uninstall(UninstallationStage stage) {
			super.uninstall(stage);
			if (stage == UninstallationStage.PRE_DESTROY_EVENT) {
				this.uninstallPreCalls.incrementAndGet();
			} else {
				this.uninstallPostCalls.incrementAndGet();
			}
		}
		public int installCalls() {
			return this.installCalls.getAndSet(0);
		}
		public int uninstallPreCalls() {
			return this.uninstallPreCalls.getAndSet(0);
		}
		public int uninstallPostCalls() {
			return this.uninstallPostCalls.getAndSet(0);
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill5 extends Skill implements Capacity3 {
		public Skill5() {
			//
		}
	}

}
