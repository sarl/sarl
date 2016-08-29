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
package io.janusproject.kernel.services.hazelcast;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Comparator;
import java.util.UUID;

import org.eclipse.jdt.annotation.Nullable;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.core.IMap;
import com.hazelcast.core.MultiMap;

import io.janusproject.services.distributeddata.DMap;
import io.janusproject.services.distributeddata.DMultiMap;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.testutils.AbstractDependentServiceTest;
import io.janusproject.testutils.StartServiceForTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@StartServiceForTest(startAfterSetUp = true)
public class HazelcastDistributedDataStructureFactoryTest
		extends AbstractDependentServiceTest<HazelcastDistributedDataStructureService> {

	@Nullable
	private HazelcastInstance hz;

	@Nullable
	private IMap<Object, Object> imap;

	@Nullable
	private MultiMap<Object, Object> multimap;

	@Nullable
	private HazelcastDistributedDataStructureService factory;

	public HazelcastDistributedDataStructureFactoryTest() {
		super(DistributedDataStructureService.class);
	}

	@Override
	public HazelcastDistributedDataStructureService newService() {
		return new HazelcastDistributedDataStructureService();
	}

	@Before
	public void setUp() {
		this.imap = Mockito.mock(IMap.class);
		this.multimap = Mockito.mock(com.hazelcast.core.MultiMap.class);
		this.hz = Mockito.mock(HazelcastInstance.class);
		Mockito.when(this.hz.getMap(Mockito.any(String.class))).thenReturn(this.imap);
		Mockito.when(this.hz.getMultiMap(Mockito.any(String.class))).thenReturn(this.multimap);
		this.factory = new HazelcastDistributedDataStructureService();
		this.factory.setHazelcastInstance(this.hz);
	}

	@Override
	public void getServiceDependencies() {
		assertContains(this.service.getServiceDependencies());
	}

	@Override
	public void getServiceWeakDependencies() {
		assertContains(this.service.getServiceWeakDependencies());
	}

	@Test
	public void getMap() {
		DMap<Object, Object> m = this.factory.getMap(UUID.randomUUID().toString());
		assertNotNull(m);
		m.put("a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Object> argument2 = ArgumentCaptor.forClass(Object.class);
		Mockito.verify(this.imap).put(argument1.capture(), argument2.capture());
		assertEquals("a", argument1.getValue()); //$NON-NLS-1$
		assertEquals("b", argument2.getValue()); //$NON-NLS-1$
	}

	@Test
	public void getMultiMap() {
		DMultiMap<Object, Object> m = this.factory.getMultiMap(UUID.randomUUID().toString());
		m.put("a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Object> argument2 = ArgumentCaptor.forClass(Object.class);
		Mockito.verify(this.multimap).put(argument1.capture(), argument2.capture());
		assertEquals("a", argument1.getValue()); //$NON-NLS-1$
		assertEquals("b", argument2.getValue()); //$NON-NLS-1$
	}

	@Test
	public void getMapComparator_null() {
		DMap<Object, Object> m = this.factory.getMap(UUID.randomUUID().toString(), null);
		assertNotNull(m);
		m.put("a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Object> argument2 = ArgumentCaptor.forClass(Object.class);
		Mockito.verify(this.imap).put(argument1.capture(), argument2.capture());
		assertEquals("a", argument1.getValue()); //$NON-NLS-1$
		assertEquals("b", argument2.getValue()); //$NON-NLS-1$
	}

	@Test
	public void getMultiMapComparator_null() {
		DMultiMap<Object, Object> m = this.factory.getMultiMap(UUID.randomUUID().toString(), null);
		m.put("a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Object> argument2 = ArgumentCaptor.forClass(Object.class);
		Mockito.verify(this.multimap).put(argument1.capture(), argument2.capture());
		assertEquals("a", argument1.getValue()); //$NON-NLS-1$
		assertEquals("b", argument2.getValue()); //$NON-NLS-1$
	}

	@Test
	public void getMapComparator_notNull() {
		DMap<Object, Object> m = this.factory.getMap(UUID.randomUUID().toString(), new ComparableComparator());
		assertNotNull(m);
		m.put("a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Object> argument2 = ArgumentCaptor.forClass(Object.class);
		Mockito.verify(this.imap).put(argument1.capture(), argument2.capture());
		assertEquals("a", argument1.getValue()); //$NON-NLS-1$
		assertEquals("b", argument2.getValue()); //$NON-NLS-1$
	}

	@Test
	public void getMultiMapComparator_notNull() {
		DMultiMap<Object, Object> m = this.factory.getMultiMap(UUID.randomUUID().toString(), new ComparableComparator());
		m.put("a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Object> argument2 = ArgumentCaptor.forClass(Object.class);
		Mockito.verify(this.multimap).put(argument1.capture(), argument2.capture());
		assertEquals("a", argument1.getValue()); //$NON-NLS-1$
		assertEquals("b", argument2.getValue()); //$NON-NLS-1$
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class ComparableComparator implements Comparator<Object> {

		public ComparableComparator() {
			//
		}

		@Override
		public int compare(Object o1, Object o2) {
			return System.identityHashCode(o1) - System.identityHashCode(o2);
		}

	}

}
