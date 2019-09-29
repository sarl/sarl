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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.google.common.base.Supplier;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimaps;
import io.janusproject.kernel.repository.MultipleAddressParticipantRepository;
import io.janusproject.kernel.services.guava.DMultiMapView;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.tests.testutils.AbstractJanusTest;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

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
public class MultipleAddressParticipantRepositoryTest extends AbstractJanusTest {

	@Nullable
	private String distributedName;

	@Nullable
	private MultipleAddressParticipantRepository<String> repository;

	@Nullable
	private DistributedDataStructureService service;

	@Nullable
	private UUID id1;

	@Nullable
	private UUID id2;

	@Nullable
	private EventListener listener1;

	@Nullable
	private EventListener listener2;

	@Before
	public void setUp() {
		this.distributedName = getClass().getName() + UUID.randomUUID().toString();
		//
		this.service = Mockito.mock(DistributedDataStructureService.class);
		Map<Object, Collection<Object>> map = new HashMap<>();
		DMultiMapView<Object, Object> mapMock = new DMultiMapView<>(UUID.randomUUID().toString(),
				Multimaps.newMultimap(map, new Supplier<Collection<Object>>() {
					@Override
					public Collection<Object> get() {
						return Lists.newArrayList();
					}

				}));
		Mockito.when(this.service.getMultiMap(this.distributedName, null)).thenReturn(mapMock);
		Mockito.when(this.service.getMultiMap(this.distributedName)).thenReturn(mapMock);
		//
		this.repository = new MultipleAddressParticipantRepository<>(this.distributedName, this.service,
				() -> NoReadWriteLock.SINGLETON);
		//
		this.id1 = UUID.randomUUID();
		this.id2 = UUID.randomUUID();
		//
		this.listener1 = Mockito.mock(EventListener.class);
		Mockito.when(this.listener1.getID()).thenReturn(this.id1);
		this.listener2 = Mockito.mock(EventListener.class);
		Mockito.when(this.listener2.getID()).thenReturn(this.id2);
	}

	@Test
	public void registerParticipant() {
		Collection<String> col;
		col = this.repository.getAddresses(this.id1);
		assertTrue(col.isEmpty());
		col = this.repository.getAddresses(this.id2);
		assertTrue(col.isEmpty());
		this.repository.registerParticipant("a", this.listener1); //$NON-NLS-1$
		this.repository.registerParticipant("b", this.listener2); //$NON-NLS-1$
		this.repository.registerParticipant("c", this.listener1); //$NON-NLS-1$
		col = this.repository.getAddresses(this.id1);
		assertFalse(col.isEmpty());
		assertEquals(2, col.size());
		assertTrue(col.contains("a")); //$NON-NLS-1$
		assertTrue(col.contains("c")); //$NON-NLS-1$
		col = this.repository.getAddresses(this.id2);
		assertFalse(col.isEmpty());
		assertEquals(1, col.size());
		assertTrue(col.contains("b")); //$NON-NLS-1$
	}

	@Test
	public void unregisterParticipant() {
		this.repository.registerParticipant("a", this.listener1); //$NON-NLS-1$
		this.repository.registerParticipant("b", this.listener2); //$NON-NLS-1$
		this.repository.registerParticipant("c", this.listener1); //$NON-NLS-1$
		//
		this.repository.unregisterParticipant("c", this.listener1); //$NON-NLS-1$
		Collection<String> col = this.repository.getAddresses(this.id1);
		assertFalse(col.isEmpty());
		assertEquals(1, col.size());
		assertTrue(col.contains("a")); //$NON-NLS-1$
		col = this.repository.getAddresses(this.id2);
		assertFalse(col.isEmpty());
		assertEquals(1, col.size());
		assertTrue(col.contains("b")); //$NON-NLS-1$
		//
		this.repository.unregisterParticipant("b", this.listener1); //$NON-NLS-1$
		col = this.repository.getAddresses(this.id1);
		assertFalse(col.isEmpty());
		assertEquals(1, col.size());
		assertTrue(col.contains("a")); //$NON-NLS-1$
		col = this.repository.getAddresses(this.id2);
		assertFalse(col.isEmpty());
		assertEquals(1, col.size());
		assertTrue(col.contains("b")); //$NON-NLS-1$
		//
		this.repository.unregisterParticipant("b", this.listener2); //$NON-NLS-1$
		col = this.repository.getAddresses(this.id1);
		assertFalse(col.isEmpty());
		assertEquals(1, col.size());
		assertTrue(col.contains("a")); //$NON-NLS-1$
		col = this.repository.getAddresses(this.id2);
		assertTrue(col.isEmpty());
		//
		this.repository.unregisterParticipant("a", this.listener1); //$NON-NLS-1$
		col = this.repository.getAddresses(this.id1);
		assertTrue(col.isEmpty());
		col = this.repository.getAddresses(this.id2);
		assertTrue(col.isEmpty());
	}

	@Test
	public void getParticipantAddresses() {
		Collection<String> col;
		col = this.repository.getParticipantAddresses();
		assertTrue(col.isEmpty());
		this.repository.registerParticipant("a", this.listener1); //$NON-NLS-1$
		this.repository.registerParticipant("b", this.listener2); //$NON-NLS-1$
		this.repository.registerParticipant("c", this.listener1); //$NON-NLS-1$
		col = this.repository.getParticipantAddresses();
		assertFalse(col.isEmpty());
		assertEquals(3, col.size());
		assertTrue(col.contains("a")); //$NON-NLS-1$
		assertTrue(col.contains("b")); //$NON-NLS-1$
		assertTrue(col.contains("c")); //$NON-NLS-1$
	}

	@Test
	public void getParticipantIDs() {
		Collection<UUID> col;
		col = this.repository.getParticipantIDs();
		assertTrue(col.isEmpty());
		this.repository.registerParticipant("a", this.listener1); //$NON-NLS-1$
		this.repository.registerParticipant("b", this.listener2); //$NON-NLS-1$
		this.repository.registerParticipant("c", this.listener1); //$NON-NLS-1$
		col = this.repository.getParticipantIDs();
		assertFalse(col.isEmpty());
		assertEquals(2, col.size());
		assertTrue(col.contains(this.id1));
		assertTrue(col.contains(this.id2));
	}

}
