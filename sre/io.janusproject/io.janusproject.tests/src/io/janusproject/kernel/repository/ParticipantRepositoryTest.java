/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.janusproject.kernel.repository;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import io.janusproject.testutils.AbstractJanusTest;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import io.sarl.lang.core.EventListener;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class ParticipantRepositoryTest extends AbstractJanusTest {

	@Nullable
	private ParticipantRepository<String> repository;

	@Nullable
	private Map<String, EventListener> listeners;

	@Before
	public void setUp() {
		this.repository = new ParticipantRepository<String>() {
			//
		};
		this.listeners = new TreeMap<>();
		this.listeners.put("a", Mockito.mock(EventListener.class)); //$NON-NLS-1$
		this.listeners.put("b", Mockito.mock(EventListener.class)); //$NON-NLS-1$
		this.listeners.put("c", Mockito.mock(EventListener.class)); //$NON-NLS-1$
		this.listeners.put("d", Mockito.mock(EventListener.class)); //$NON-NLS-1$
		this.listeners.put("e", Mockito.mock(EventListener.class)); //$NON-NLS-1$
	}

	private void addListenerMocks() {
		for (Entry<String, EventListener> l : this.listeners.entrySet()) {
			this.repository.addListener(l.getKey(), l.getValue());
		}
	}

	@Test
	public void listenerCount() {
		assertEquals(0, this.repository.listenerCount());
		//
		addListenerMocks();
		//
		assertEquals(5, this.repository.listenerCount());
	}

	@Test
	public void isListenerEmpty() {
		assertTrue(this.repository.isListenerEmpty());
		//
		addListenerMocks();
		//
		assertFalse(this.repository.isListenerEmpty());
	}

	@Test
	public void containsAddress() {
		for (String c : this.listeners.keySet()) {
			assertFalse(this.repository.containsAddress(c));
		}
		assertFalse(this.repository.containsAddress("f")); //$NON-NLS-1$
		//
		addListenerMocks();
		//
		for (String c : this.listeners.keySet()) {
			assertTrue(this.repository.containsAddress(c));
		}
		assertFalse(this.repository.containsAddress("f")); //$NON-NLS-1$
	}

	@Test
	public void containsListener() {
		for (EventListener l : this.listeners.values()) {
			assertFalse(this.repository.containsListener(l));
		}
		assertFalse(this.repository.containsListener(Mockito.mock(EventListener.class)));
		//
		addListenerMocks();
		//
		for (EventListener l : this.listeners.values()) {
			assertTrue(this.repository.containsListener(l));
		}
		assertFalse(this.repository.containsListener(Mockito.mock(EventListener.class)));
	}

	@Test
	public void getListener() {
		for (String k : this.listeners.keySet()) {
			assertNull(this.repository.getListener(k));
		}
		assertNull(this.repository.getListener("f")); //$NON-NLS-1$
		//
		addListenerMocks();
		//
		for (String k : this.listeners.keySet()) {
			assertSame(this.listeners.get(k), this.repository.getListener(k));
		}
		assertNull(this.repository.getListener("f")); //$NON-NLS-1$
	}

	@Test
	public void removeListener() {
		for (String k : this.listeners.keySet()) {
			assertNull(this.repository.removeListener(k));
		}
		assertNull(this.repository.removeListener("f")); //$NON-NLS-1$
		//
		addListenerMocks();
		//
		for (String k : this.listeners.keySet()) {
			assertSame(this.listeners.get(k), this.repository.removeListener(k));
		}
		assertNull(this.repository.removeListener("f")); //$NON-NLS-1$
		assertTrue(this.repository.isListenerEmpty());
	}

	@Test
	public void clearListeners() {
		assertTrue(this.repository.isListenerEmpty());
		//
		this.repository.clearListeners();
		assertTrue(this.repository.isListenerEmpty());
		//
		addListenerMocks();
		assertFalse(this.repository.isListenerEmpty());
		//
		this.repository.clearListeners();
		assertTrue(this.repository.isListenerEmpty());
	}

	@Test
	public void getAdresses() {
		Set<String> adrs = this.repository.getAdresses();
		assertNotNull(adrs);
		assertTrue(adrs.isEmpty());
		//
		addListenerMocks();
		//
		adrs = this.repository.getAdresses();
		assertNotNull(adrs);
		assertFalse(adrs.isEmpty());
		assertEquals(5, adrs.size());
		for (String k : this.listeners.keySet()) {
			assertTrue(adrs.contains(k));
		}
	}

	@Test
	public void getListeners() {
		Collection<EventListener> list = this.repository.getListeners();
		assertNotNull(list);
		assertTrue(list.isEmpty());
		//
		addListenerMocks();
		//
		list = this.repository.getListeners();
		assertNotNull(list);
		assertFalse(list.isEmpty());
		assertEquals(5, list.size());
		for (EventListener l : this.listeners.values()) {
			assertTrue(list.contains(l));
		}
	}

	@Test
	public void listenersEntrySet() {
		Set<Entry<String, EventListener>> list = this.repository.listenersEntrySet();
		assertNotNull(list);
		assertTrue(list.isEmpty());
		//
		addListenerMocks();
		//
		list = this.repository.listenersEntrySet();
		assertNotNull(list);
		assertFalse(list.isEmpty());
		assertEquals(5, list.size());
	}

}
