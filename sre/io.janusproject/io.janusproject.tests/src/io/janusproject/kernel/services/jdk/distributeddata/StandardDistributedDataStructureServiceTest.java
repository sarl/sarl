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
package io.janusproject.kernel.services.jdk.distributeddata;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.UUID;

import org.junit.Test;

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
@StartServiceForTest
@SuppressWarnings("all")
public final class StandardDistributedDataStructureServiceTest
		extends AbstractDependentServiceTest<StandardDistributedDataStructureService> {

	private static final int KEY_COUNT = 10;

	public StandardDistributedDataStructureServiceTest() {
		super(DistributedDataStructureService.class);
	}

	protected static List<String> randomKeys(int keyCount, Comparator<String> comparator) {
		Random random = new Random();
		List<String> list = new ArrayList<>(keyCount);
		for (int i = 0; i < keyCount; ++i) {
			list.add("v" + i);
		}
		if (comparator != null) {
			Collections.sort(list, comparator);
		}
		return list;
	}

	protected static void fillMap(DMap<String, String> map, Iterable<String> keys) {
		for (String key : keys) {
			map.put(key, UUID.randomUUID().toString());
		}
	}

	protected static void fillMap(DMultiMap<String, String> map, Iterable<String> keys) {
		for (String key : keys) {
			map.put(key, UUID.randomUUID().toString());
		}
	}

	@Override
	public StandardDistributedDataStructureService newService() {
		return new StandardDistributedDataStructureService();
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
	public void getMapString_getName() {
		String name = UUID.randomUUID().toString();
		DMap<String, String> map = this.service.getMap(name);
		assertNotNull(map);
		assertEquals(name, map.getName());
	}

	@Test
	public void getMapStringComparator_getName() {
		String name = UUID.randomUUID().toString();
		DMap<String, String> map = this.service.getMap(name, new Comparator<String>() {
			@Override
			public int compare(String o1, String o2) {
				return o1.compareTo(o2);
			}
		});
		assertNotNull(map);
		assertEquals(name, map.getName());
	}

	@Test
	public void getMultiMapString_getName() {
		String name = UUID.randomUUID().toString();
		DMultiMap<String, String> map = this.service.getMultiMap(name);
		assertNotNull(map);
		assertEquals(name, map.getName());
	}

	@Test
	public void getMultiMapStringComparator_getName() {
		String name = UUID.randomUUID().toString();
		DMultiMap<String, String> map = this.service.getMultiMap(name, new Comparator<String>() {
			@Override
			public int compare(String o1, String o2) {
				return o1.compareTo(o2);
			}
		});
		assertNotNull(map);
		assertEquals(name, map.getName());
	}

	@Test
	public void getMapStringComparator_keyOrder_nullComparator() {
		List<String> keys = randomKeys(KEY_COUNT, new NaturalComparator());
		DMap<String, String> map = this.service.getMap("testing-map", null);
		assertNotNull(map);
		fillMap(map, keys);
		assertContainsCollection(map.keySet(), keys);
	}

	@Test
	public void getMapStringComparator_keyOrder_notNullComparator() {
		Comparator<String> comparator = Collections.reverseOrder(new NaturalComparator());
		List<String> keys = randomKeys(KEY_COUNT, comparator);
		DMap<String, String> map = this.service.getMap("testing-map", comparator);
		assertNotNull(map);
		fillMap(map, keys);
		assertContainsCollection(map.keySet(), keys);
	}

	@Test
	public void getMultiMapStringComparator_keyOrder_nullComparator() {
		List<String> keys = randomKeys(KEY_COUNT, new NaturalComparator());
		DMultiMap<String, String> map = this.service.getMultiMap("testing-map", null);
		assertNotNull(map);
		fillMap(map, keys);
		assertContainsCollection(map.keySet(), keys);
	}

	@Test
	public void getMultiMapStringComparator_keyOrder_notNullComparator() {
		Comparator<String> comparator = Collections.reverseOrder(new NaturalComparator());
		List<String> keys = randomKeys(KEY_COUNT, comparator);
		DMultiMap<String, String> map = this.service.getMultiMap("testing-map", comparator);
		assertNotNull(map);
		fillMap(map, keys);
		assertContainsCollection(map.keySet(), keys);
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class NaturalComparator implements Comparator<String> {

		public NaturalComparator() {
			//
		}

		@Override
		public int compare(String o1, String o2) {
			return o1.compareTo(o2);
		}
	}

}
