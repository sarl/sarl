/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Event;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Matchers;
import org.mockito.Mockito;

import static org.junit.Assert.*;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class EventTest {

	private Event event;
	
	private static Event mockEvent() {
		return new Event() {
			private static final long serialVersionUID = -1997616003372673851L;
			//
		};
	}
	
	/**
	 */
	@Before
	public void setUp() {
		this.event = mockEvent();
	}

	/**
	 */
	@Test
	public void getSource() {
		assertNull(this.event.getSource());
		Address adr = Mockito.mock(Address.class);
		this.event.setSource(adr);
		assertSame(adr, this.event.getSource());
	}

	/**
	 */
	@Test
	public void setSource() {
		assertNull(this.event.getSource());
		Address adr = Mockito.mock(Address.class);
		this.event.setSource(adr);
		assertSame(adr, this.event.getSource());
	}

	/**
	 */
	@Test
	public void equals() {
		Event e;
		Address adr1 = Mockito.mock(Address.class);
		Address adr2 = Mockito.mock(Address.class);
		Mockito.doReturn(Boolean.TRUE).when(adr1).equals(Matchers.eq(adr1));
		Mockito.doReturn(Boolean.FALSE).when(adr1).equals(Matchers.eq(adr2));
		Mockito.doReturn(Boolean.FALSE).when(adr2).equals(Matchers.eq(adr1));
		Mockito.doReturn(Boolean.TRUE).when(adr2).equals(Matchers.eq(adr2));
		this.event.setSource(adr1);
		//
		assertTrue(this.event.equals(this.event));
		//
		e = mockEvent();
		e.setSource(adr1);
		assertTrue(this.event.equals(e));
		//
		e = mockEvent();
		e.setSource(adr2);
		assertFalse(this.event.equals(e));
	}

	/**
	 */
	@Test
	public void testHashCode() {
		Event e;
		Address adr1 = Mockito.mock(Address.class);
		Address adr2 = Mockito.mock(Address.class);
		this.event.setSource(adr1);
		//
		assertEquals(this.event.hashCode(), this.event.hashCode());
		//
		e = mockEvent();
		e.setSource(adr1);
		assertEquals(this.event.hashCode(), e.hashCode());
		//
		e = mockEvent();
		e.setSource(adr2);
		assertNotEquals(this.event.hashCode(), e.hashCode());
	}

}
