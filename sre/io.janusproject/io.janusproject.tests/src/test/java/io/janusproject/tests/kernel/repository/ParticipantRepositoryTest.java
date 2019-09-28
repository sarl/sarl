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
package io.janusproject.tests.kernel.repository;

import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.locks.ReadWriteLock;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import io.janusproject.kernel.repository.ParticipantRepository;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.lang.core.EventListener;
import io.sarl.tests.api.Nullable;
import io.sarl.util.concurrent.NoReadWriteLock;

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
			@Override
			public ReadWriteLock getLock() {
				return NoReadWriteLock.SINGLETON;
			}
		};
		this.listeners = new TreeMap<>();
		this.listeners.put("a", Mockito.mock(EventListener.class)); //$NON-NLS-1$
		this.listeners.put("b", Mockito.mock(EventListener.class)); //$NON-NLS-1$
		this.listeners.put("c", Mockito.mock(EventListener.class)); //$NON-NLS-1$
		this.listeners.put("d", Mockito.mock(EventListener.class)); //$NON-NLS-1$
		this.listeners.put("e", Mockito.mock(EventListener.class)); //$NON-NLS-1$
	}

	private void addListenerMocks() throws Exception {
		for (Entry<String, EventListener> l : this.listeners.entrySet()) {
			this.reflect.invoke(this.repository, "addListener", l.getKey(), l.getValue());
		}
	}

	@Test
	public void listenerCount() throws Exception {
		assertEquals(0, this.repository.listenerCount());
		//
		addListenerMocks();
		//
		assertEquals(5, this.repository.listenerCount());
	}

	@Test
	public void isListenerEmpty() throws Exception {
		assertTrue((Boolean) this.reflect.invoke(this.repository, "isListenerEmpty"));
		//
		addListenerMocks();
		//
		assertFalse((Boolean) this.reflect.invoke(this.repository, "isListenerEmpty"));
	}

	@Test
	public void containsAddress() throws Exception {
		for (String c : this.listeners.keySet()) {
			assertFalse((Boolean) this.reflect.invoke(this.repository, "containsAddress", c));
		}
		assertFalse((Boolean) this.reflect.invoke(this.repository, "containsAddress", "f")); //$NON-NLS-1$
		//
		addListenerMocks();
		//
		for (String c : this.listeners.keySet()) {
			assertTrue((Boolean) this.reflect.invoke(this.repository, "containsAddress", c));
		}
		assertFalse((Boolean) this.reflect.invoke(this.repository, "containsAddress", "f")); //$NON-NLS-1$
	}

	@Test
	public void containsListener() throws Exception {
		for (EventListener l : this.listeners.values()) {
			assertFalse((Boolean) this.reflect.invoke(this.repository, "containsListener", l));
		}
		assertFalse((Boolean) this.reflect.invoke(this.repository, "containsListener", Mockito.mock(EventListener.class)));
		//
		addListenerMocks();
		//
		for (EventListener l : this.listeners.values()) {
			assertTrue((Boolean) this.reflect.invoke(this.repository, "containsListener", l));
		}
		assertFalse((Boolean) this.reflect.invoke(this.repository, "containsListener", Mockito.mock(EventListener.class)));
	}

	@Test
	public void getListener() throws Exception {
		for (String k : this.listeners.keySet()) {
			assertNull(this.reflect.invoke(this.repository, "getListener", k));
		}
		assertNull(this.reflect.invoke(this.repository, "getListener", "f")); //$NON-NLS-1$
		//
		addListenerMocks();
		//
		for (String k : this.listeners.keySet()) {
			assertSame(this.listeners.get(k), this.reflect.invoke(this.repository, "getListener", k));
		}
		assertNull(this.reflect.invoke(this.repository, "getListener", "f")); //$NON-NLS-1$
	}

	@Test
	public void removeListener() throws Exception {
		for (String k : this.listeners.keySet()) {
			assertNull(this.reflect.invoke(this.repository, "removeListener", k));
		}
		assertNull(this.reflect.invoke(this.repository, "removeListener", "f")); //$NON-NLS-1$
		//
		addListenerMocks();
		//
		for (String k : this.listeners.keySet()) {
			assertSame(this.listeners.get(k), this.reflect.invoke(this.repository, "removeListener", k));
		}
		assertNull(this.reflect.invoke(this.repository, "removeListener", "f")); //$NON-NLS-1$
		assertTrue((Boolean) this.reflect.invoke(this.repository, "isListenerEmpty"));
	}

	@Test
	public void clearListeners() throws Exception {
		assertTrue((Boolean) this.reflect.invoke(this.repository, "isListenerEmpty"));
		//
		this.reflect.invoke(this.repository, "clearListeners");
		assertTrue((Boolean) this.reflect.invoke(this.repository, "isListenerEmpty"));
		//
		addListenerMocks();
		assertFalse((Boolean) this.reflect.invoke(this.repository, "isListenerEmpty"));
		//
		this.reflect.invoke(this.repository, "clearListeners");
		assertTrue((Boolean) this.reflect.invoke(this.repository, "isListenerEmpty"));
	}

	@Test
	public void getAdresses() throws Exception {
		Set<String> adrs = (Set<String>) this.reflect.invoke(this.repository, "getAdresses");
		assertNotNull(adrs);
		assertTrue(adrs.isEmpty());
		//
		addListenerMocks();
		//
		adrs = (Set<String>) this.reflect.invoke(this.repository, "getAdresses");
		assertNotNull(adrs);
		assertFalse(adrs.isEmpty());
		assertEquals(5, adrs.size());
		for (String k : this.listeners.keySet()) {
			assertTrue(adrs.contains(k));
		}
	}

	@Test
	public void getListeners() throws Exception {
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
	public void listenersEntrySet() throws Exception {
		Set<Entry<String, EventListener>> list = (Set<Entry<String, EventListener>>) this.reflect.invoke(this.repository, "listenersEntrySet");
		assertNotNull(list);
		assertTrue(list.isEmpty());
		//
		addListenerMocks();
		//
		list = (Set<Entry<String, EventListener>>) this.reflect.invoke(this.repository, "listenersEntrySet");
		assertNotNull(list);
		assertFalse(list.isEmpty());
		assertEquals(5, list.size());
	}

}
