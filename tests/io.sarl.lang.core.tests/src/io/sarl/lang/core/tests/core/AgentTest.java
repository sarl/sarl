/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.*;

import java.security.InvalidParameterException;
import java.util.UUID;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.junit.Before;
import org.junit.Test;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.UnimplementedCapacityException;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class AgentTest extends AbstractSarlTest {

	@NonNullByDefault
	private UUID id;

	@NonNullByDefault
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

	private void assertNoSkill(Class<? extends Capacity> c) {
		try {
			this.agent.getSkill(c);
			fail("Expecting the exception UnimplementedCapacityException, but got no exception."); //$NON-NLS-1$
		} catch (UnimplementedCapacityException exception) {
			//
		}
	}

	private void assertSkill(Class<? extends Capacity> c, Skill expected) {
		Object r = this.agent.getSkill(c);
		assertSame(expected, r);
	}

	private void assertSkill(Class<? extends Capacity> c) {
		Object r = this.agent.getSkill(c);
		assertNotNull(r);
	}

	/**
	 */
	@Before
	public void setUp() {
		this.id = UUID.randomUUID();
		this.agent = spy(new AgentMock(this.id));
	}

	/**
	 */
	@Test
	public void getID() {
		UUID aid = this.agent.getID();
		assertNotNull(aid);
		assertNotEquals(this.id, aid);
	}

	/**
	 */
	@Test
	public void getParentID() {
		assertSame(this.id, this.agent.getParentID());
	}

	/**
	 */
	@Test
	public void setSkill() {
		Skill s1, s2, r;

		assertNoSkill(Capacity1.class);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		s1 = new Skill1();
		r = this.agent.setSkill(Capacity1.class, s1);
		assertSame(s1, r);
		assertSkill(Capacity1.class, s1);
		assertNoSkill(Capacity2.class);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		try {
			this.agent.setSkill(Capacity1.class, new Skill2());
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		}
		catch(InvalidParameterException exception) {
			//
		}

		try {
			this.agent.setSkill(Capacity1.class, new Skill3());
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		}
		catch(InvalidParameterException exception) {
			//
		}

		s2 = new Skill2();
		r = this.agent.setSkill(Capacity2.class, s2);
		assertSame(s2, r);
		assertSkill(Capacity1.class, s1);
		assertSkill(Capacity2.class, s2);
		assertNoSkill(Skill1.class);
		assertNoSkill(Skill2.class);

		try {
			this.agent.setSkill(Capacity2.class, new Skill1());
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		}
		catch(InvalidParameterException exception) {
			//
		}

		try {
			this.agent.setSkill(Capacity2.class, new Skill3());
			fail("Expecting the exception InvalidParameterException, but got no exception."); //$NON-NLS-1$
		}
		catch(InvalidParameterException exception) {
			//
		}
	}

	/**
	 */
	@Test
	public void clearSkill() {
		this.agent.setSkill(Capacity1.class, new Skill1());
		this.agent.setSkill(Capacity2.class, new Skill2());
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

	/**
	 */
	@Test
	public void hasSkill() {
		this.agent.setSkill(Capacity1.class, new Skill1());
		this.agent.setSkill(Capacity2.class, new Skill2());
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

	/**
	 */
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

	/**
	 */
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

	/**
	 */
	@Test
	public void isMeUUID() {
		assertTrue(this.agent.isMe(this.agent.getID()));
		assertTrue(this.agent.isMe(UUID.fromString(this.agent.getID().toString())));
		assertFalse(this.agent.isMe(UUID.randomUUID()));
	}

	/**
	 */
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
			super(mock(BuiltinCapacitiesProvider.class), parentID, null);
		}

		/** {@inheritDoc}
		 */
		@Override
		public <S extends Skill> S setSkill(Class<? extends Capacity> capacity, S skill) {
			return super.setSkill(capacity, skill);
		}

		/** {@inheritDoc}
		 */
		@Override
		public <S extends Capacity> S getSkill(Class<S> capacity) {
			return super.getSkill(capacity);
		}

		/** {@inheritDoc}
		 */
		@Override
		public <S extends Capacity> S clearSkill(Class<S> capacity) {
			return super.clearSkill(capacity);
		}

		/** {@inheritDoc}
		 */
		@Override
		public boolean hasSkill(Class<? extends Capacity> capacity) {
			return super.hasSkill(capacity);
		}

		/** {@inheritDoc}
		 */
		@Override
		public <S extends Skill> void operator_mappedTo(
				Class<? extends Capacity> capacity, S skill) {
			super.operator_mappedTo(capacity, skill);
		}

		/** {@inheritDoc}
		 */
		@Override
		public boolean isMe(Address address) {
			return super.isMe(address);
		}

		/** {@inheritDoc}
		 */
		@Override
		public boolean isMe(UUID id) {
			return super.isMe(id);
		}

		/** {@inheritDoc}
		 */
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

}
