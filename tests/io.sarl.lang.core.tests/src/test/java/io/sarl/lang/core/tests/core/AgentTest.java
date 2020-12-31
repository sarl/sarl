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

import static io.sarl.tests.api.tools.TestMockito.mock;
import static io.sarl.tests.api.tools.TestMockito.spy;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Event;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Agent")
@Tag("unit")
@Tag("core")
public class AgentTest extends AbstractSarlTest {

	private UUID id;

	private AgentMock agent;
	
	private static Address mockAddress(UUID agentID) {
		Address adr = mock(Address.class);
		doReturn(agentID).when(adr).getID();
		return adr;
	}

	private static Event mockEvent(UUID agentID) {
		Address adr = mockAddress(agentID);
		Event evt = mock(Event.class);
		doReturn(adr).when(evt).getSource();
		return evt;
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

}
