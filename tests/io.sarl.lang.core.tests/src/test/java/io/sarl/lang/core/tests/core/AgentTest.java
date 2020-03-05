/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2020 the original authors or authors.
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
import static io.sarl.tests.api.tools.TestMockito.mock;
import static io.sarl.tests.api.tools.TestMockito.spy;
import static io.sarl.tests.api.tools.TestReflections.invokeFunc;
import static io.sarl.tests.api.tools.TestReflections.invokeProc;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.doReturn;

import java.security.InvalidParameterException;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.DefaultSkill;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.SREutils;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.UnimplementedCapacityException;
import io.sarl.lang.util.AtomicClearableReference;
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

	private void assertNoSkill(Class<? extends Capacity> c) throws Exception {
		assertException(UnimplementedCapacityException.class, () -> {
			invokeProc(this.agent.getClass(), this.agent, "getSkill", new Class[] {Class.class}, c);
			invokeProc(this.agent.getClass(), this.agent, "getSkill", new Class[] {Class.class}, c);
		});
	}

	private void assertSkill(Class<? extends Capacity> c, Skill expected) throws Exception {
		Object r = invokeFunc(this.agent.getClass(), this.agent, Skill.class,
				"getSkill", new Class[] {Class.class}, c);
		assertSame(expected, r);
	}

	private void assertSkill(Class<? extends Capacity> c) throws Exception {
		Object r = invokeFunc(this.agent.getClass(), this.agent, Skill.class,
				"getSkill", new Class[] {Class.class}, c);
		assertNotNull(r);
	}

	@BeforeEach
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
	public void setSkill() throws Exception {
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
	public void setSkill_withoutCapacity() throws Exception {
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
	public void clearSkill_multipleCapacityImplementation() throws Exception {
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
	public void clearSkill() throws Exception {
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
	public void hasSkill() throws Exception {
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
	public void operator_mappedTo() throws Exception {
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
	public void isMeAddress() throws Exception {
		Address adr;

		adr = mockAddress(this.agent.getID());
		assertTrue(this.agent.isMe(adr));

		adr = mockAddress(UUID.fromString(this.agent.getID().toString()));
		assertTrue(this.agent.isMe(adr));

		adr = mockAddress(UUID.randomUUID());
		assertFalse(this.agent.isMe(adr));
	}

	@Test
	public void isMeUUID() throws Exception {
		assertTrue(this.agent.isMe(this.agent.getID()));
		assertTrue(this.agent.isMe(UUID.fromString(this.agent.getID().toString())));
		assertFalse(this.agent.isMe(UUID.randomUUID()));
	}

	@Test
	public void isFromMeEvent() throws Exception {
		Event evt;

		evt = mockEvent(this.agent.getID());
		assertTrue(this.agent.isFromMe(evt));

		evt = mockEvent(UUID.fromString(this.agent.getID().toString()));
		assertTrue(this.agent.isFromMe(evt));

		evt = mockEvent(UUID.randomUUID());
		assertFalse(this.agent.isFromMe(evt));
	}

	@Test
	public void skillInstallation_withMultipleSetSkill() throws Exception {
		Skill4 s4 = new Skill4();
		this.agent.setSkill_Fake(s4, Capacity1.class);
		
		assertEquals(1, s4.installCalls());
		
		this.agent.setSkill_Fake(s4, Capacity1.class);

		assertEquals(0, s4.installCalls());
	}

	@Test
	public void skillInstallation_withSingleSetSkill() throws Exception {
		Skill4 s4 = new Skill4();
		this.agent.setSkill_Fake(s4);
		assertEquals(1, s4.installCalls());
	}

	@Test
	public void skillUninstallation_withClearSkill() throws Exception {
		Skill4 s4 = new Skill4();
		this.agent.setSkill_Fake(s4);

		this.agent.clearSkill(Capacity1.class);
		assertEquals(0, s4.uninstallPreCalls());
		assertEquals(0, s4.uninstallPostCalls());

		this.agent.clearSkill(Capacity2.class);
		assertEquals(1, s4.uninstallPreCalls());
		assertEquals(1, s4.uninstallPostCalls());
	}

	@Test
	public void getSkill_noSetSkill() throws Exception {
		assertException(UnimplementedCapacityException.class, () -> {
			this.agent.$getSkill(Capacity1.class);
		});
	}

	@Test
	public void getSkill_setSkill() throws Exception {
		Skill1 so = new Skill1();
		this.agent.setSkill_Fake(so);
		//
		AtomicClearableReference<Skill> ref0 = this.agent.$getSkill(Capacity1.class);
		assertNotNull(ref0);
		Skill s0 = ref0.get();
		assertSame(so, s0);
		//
		AtomicClearableReference<Skill> ref1 = this.agent.$getSkill(Capacity1.class);
		assertNotNull(ref1);
		assertSame(ref0, ref1);
	}

	@Test
	public void getSkill_defaultskill() throws Exception {
		AtomicClearableReference<Skill> ref0 = this.agent.$getSkill(Capacity3.class);
		assertNotNull(ref0);
		Skill s0 = ref0.get();
		assertNotNull(s0);
		assertInstanceOf(Skill5.class, s0);
		//
		AtomicClearableReference<Skill> ref1 = this.agent.$getSkill(Capacity3.class);
		assertNotNull(ref1);
		assertSame(ref0, ref1);
	}

	@Test
	public void getSkill_noRegistration() throws Exception {
		assertException(UnimplementedCapacityException.class, () -> {
			this.agent.$getSkill(Capacity4.class);
		});
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.agent.$getSkill(Capacity5.class);
		});
	}

	@Test
	public void getSkill_registrationSkill6() throws Exception {
		Skill expected = new Skill6();
		SREutils.setInternalSkill(this.agent, expected, new Class[0]);
		//
		AtomicClearableReference<Skill> actual = this.agent.$getSkill(Capacity4.class);
		assertNotNull(actual);
		assertSame(expected, actual.get());
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.agent.$getSkill(Capacity5.class);
		});
	}

	@Test
	public void getSkill_registrationSkill7() throws Exception {
		Skill expected = new Skill7();
		SREutils.setInternalSkill(this.agent, expected, new Class[0]);
		//
		AtomicClearableReference<Skill> actual0 = this.agent.$getSkill(Capacity4.class);
		assertNotNull(actual0);
		assertSame(expected, actual0.get());
		//
		AtomicClearableReference<Skill> actual1 = this.agent.$getSkill(Capacity5.class);
		assertNotNull(actual1);
		assertSame(expected, actual1.get());
	}

	@Test
	public void getSkill_registrationSkill6_clearReference() throws Exception {
		Skill expected = new Skill6();
		SREutils.setInternalSkill(this.agent, expected, new Class[0]).clear();
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.agent.$getSkill(Capacity4.class);
		});
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.agent.$getSkill(Capacity5.class);
		});
	}

	@Test
	public void getSkill_registrationSkill7_clearReference0() throws Exception {
		Skill expected = new Skill7();
		// The following line clear the Capacity5 reference only.
		SREutils.setInternalSkill(this.agent, expected, new Class[0]).clear();
		//
		AtomicClearableReference<Skill> actual = this.agent.$getSkill(Capacity4.class);
		assertNotNull(actual);
		assertSame(expected, actual.get());
		assertException(UnimplementedCapacityException.class, () -> {
			this.agent.$getSkill(Capacity5.class);
		});
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.agent.$getSkill(Capacity5.class);
		});
	}

	@Test
	public void getSkill_registrationSkill7_clearReference1() throws Exception {
		Skill expected = new Skill7();
		// The following line clear the Capacity5 reference only.
		SREutils.setInternalSkill(this.agent, expected, new Class[0]).clear();
		// The following line clear the Capacity4 reference only.
		SREutils.getInternalSkillReference(this.agent, Capacity4.class).clear();
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.agent.$getSkill(Capacity4.class);
		});
		//
		assertException(UnimplementedCapacityException.class, () -> {
			this.agent.$getSkill(Capacity5.class);
		});
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
		public AtomicClearableReference<Skill> $getSkill(Class<? extends Capacity> capacity) {
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
	private static interface Capacity4 extends Capacity {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static interface Capacity5 extends Capacity4 {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill6 extends Skill implements Capacity4 {
		public Skill6() {
			//
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class Skill7 extends Skill implements Capacity5 {
		public Skill7() {
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
