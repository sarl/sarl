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
package io.janusproject.tests.kernel.services.hazelcast;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;

import com.google.common.base.Supplier;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.collect.Multiset;
import com.google.common.collect.testing.google.MultimapTestSuiteBuilder;
import com.hazelcast.config.Config;
import com.hazelcast.core.Hazelcast;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.core.MultiMap;
import io.janusproject.kernel.services.hazelcast.HazelcastDMultiMapView;
import io.janusproject.services.distributeddata.DMapListener;
import io.janusproject.tests.testutils.AbstractJanusTest;
import io.janusproject.tests.testutils.HzMultiMapMock;
import io.janusproject.util.DataViewDelegate.Delegator;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({ HazelcastDMultiMapViewTest.BackedCollectionTests.class,
		HazelcastDMultiMapViewTest.SimplifiedMultiMapOperationTests.class, HazelcastDMultiMapViewTest.ViewTests.class,
		HazelcastDMultiMapViewTest.SpecificDMultiMapFunctionTests.class, HazelcastDMultiMapViewTest.ListeningFeatureTests.class })
@SuppressWarnings("all")
public class HazelcastDMultiMapViewTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class BackedCollectionTests extends AbstractJanusTest {

		@Nullable
		private Config config;

		@Nullable
		private HazelcastInstance hazelcast;

		@Nullable
		private MultiMap<String, String> multimap;

		@Nullable
		private HazelcastDMultiMapView view;

		@Before
		public void setUp() {
			System.setProperty("hazelcast.logging.type", "none");
			this.config = new Config();
			this.hazelcast = Hazelcast.newHazelcastInstance(config);
			this.multimap = this.hazelcast.getMultiMap("janus-test-" + UUID.randomUUID().toString());
			this.view = new HazelcastDMultiMapView<>(this.multimap);
			this.view.put("a", "va");
			this.view.put("b", "vb");
		}

		@After
		public void tearDown() {
			this.hazelcast.shutdown();
		}

		@Test
		public void removeAll_replyList() {
			// This call should not fail: the internal cast to List is working.
			assertNotNull(this.view.removeAll("a"));
		}

		@Test
		public void replaceValues_replyList() {
			// This call should not fail: the internal cast to List is working.
			assertNotNull(this.view.replaceValues("a", Collections.singleton("vaa")));
		}

		@Test
		public void get_replyList() {
			// This call should not fail: the internal cast to List is working.
			assertNotNull(this.view.get("a"));
		}

		@Test
		public void entries_replyList() {
			// This call should not fail: the internal cast to List is working.
			assertTrue(((Delegator<?>) this.view.entries()).getDelegatedObject() instanceof Set<?>);
		}

		@Test
		public void isBackedCollection() {
			assertFalse(this.view.isBackedCollection());
		}

		@Test
		public void changesPropagation() {
			Assume.assumeTrue("The collection is not backing the changes to the underlying collection",
					this.view.isBackedCollection());
			assertTrue(this.view.keySet().remove("b"));
			assertEquals(1, this.multimap.size());
			assertTrue(this.multimap.containsKey("a"));
			assertTrue(this.multimap.containsValue("va"));
		}

		@Test
		public void noChangesPropagation() {
			Assume.assumeFalse("The collection is backing the changes to the underlying collection",
					this.view.isBackedCollection());
			assertTrue(this.view.keySet().remove("b"));
			assertEquals(2, this.multimap.size());
			assertTrue(this.multimap.containsKey("a"));
			assertTrue(this.multimap.containsValue("va"));
			assertTrue(this.multimap.containsKey("b"));
			assertTrue(this.multimap.containsValue("vb"));
		}

	}

	/**
	 * This unit test is partly inspired by {@link MultimapTestSuiteBuilder}, but this last builder is not used since it assumes
	 * that the replied collections are backed up to the source map. But this is not the case for the Hazelcast collection, see
	 * {@link BackedCollectionTests}.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class SimplifiedMultiMapOperationTests extends AbstractJanusTest {

		@Nullable
		private MultiMap<String, String> map;

		@Nullable
		private HazelcastDMultiMapView<String, String> view;

		@Before
		public void setUp() {
			this.map = new HzMultiMapMock<>(UUID.randomUUID().toString());
			this.map.put("a", "va1");
			this.map.put("a", "va2");
			this.map.put("b", "vb");
			this.view = new HazelcastDMultiMapView<>(this.map);
		}

		@Test
		public void size() {
			assertEquals(3, this.view.size());
			this.view.clear();
			assertEquals(0, this.view.size());
		}

		@Test
		public void isEmpty() {
			assertFalse(this.view.isEmpty());
			this.view.clear();
			assertTrue(this.view.isEmpty());
		}

		@Test
		public void containsKey() {
			assertTrue(this.view.containsKey("a"));
			assertTrue(this.view.containsKey("b"));
			assertFalse(this.view.containsKey("c"));
		}

		@Test
		public void containsValue() {
			assertTrue(this.view.containsValue("va1"));
			assertTrue(this.view.containsValue("va2"));
			assertTrue(this.view.containsValue("vb"));
			assertFalse(this.view.containsValue("vc"));
		}

		@Test
		public void containsEntry() {
			assertTrue(this.view.containsEntry("a", "va1"));
			assertTrue(this.view.containsEntry("a", "va2"));
			assertTrue(this.view.containsEntry("b", "vb"));
			assertFalse(this.view.containsEntry("b", "vc"));
			assertFalse(this.view.containsEntry("c", "va1"));
		}

		@Test
		public void put() {
			this.view.put("c", "vc");
			assertTrue(this.view.containsEntry("a", "va1"));
			assertTrue(this.view.containsEntry("a", "va2"));
			assertTrue(this.view.containsEntry("b", "vb"));
			assertTrue(this.view.containsEntry("c", "vc"));
			assertFalse(this.view.containsEntry("b", "vc"));
			assertFalse(this.view.containsEntry("c", "va1"));
		}

		@Test
		public void remove() {
			this.view.remove("a", "va2");
			assertTrue(this.view.containsEntry("a", "va1"));
			assertFalse(this.view.containsEntry("a", "va2"));
			assertTrue(this.view.containsEntry("b", "vb"));
			assertFalse(this.view.containsEntry("b", "vc"));
			assertFalse(this.view.containsEntry("c", "va1"));
		}

		@Test
		public void putAllKIterable() {
			this.view.putAll("c", Arrays.asList("vc1", "vc2"));
			assertTrue(this.view.containsEntry("a", "va1"));
			assertTrue(this.view.containsEntry("a", "va2"));
			assertTrue(this.view.containsEntry("b", "vb"));
			assertFalse(this.view.containsEntry("b", "vc"));
			assertTrue(this.view.containsEntry("c", "vc1"));
			assertTrue(this.view.containsEntry("c", "vc2"));
		}

		@Test
		public void putAllMultimap() {
			Supplier<List<String>> supplier = new Supplier<List<String>>() {
				@Override
				public List<String> get() {
					return Lists.newArrayList();
				}
			};
			Multimap<String, String> m = Multimaps.newListMultimap(new HashMap<String, Collection<String>>(), supplier);
			m.put("c", "vc1");
			m.put("c", "vc2");
			this.view.putAll(m);
			assertTrue(this.view.containsEntry("a", "va1"));
			assertTrue(this.view.containsEntry("a", "va2"));
			assertTrue(this.view.containsEntry("b", "vb"));
			assertFalse(this.view.containsEntry("b", "vc"));
			assertTrue(this.view.containsEntry("c", "vc1"));
			assertTrue(this.view.containsEntry("c", "vc2"));
		}

		@Test
		public void replaceValues() {
			this.view.replaceValues("b", Arrays.asList("n1", "n2"));
			assertTrue(this.view.containsEntry("a", "va1"));
			assertTrue(this.view.containsEntry("a", "va2"));
			assertFalse(this.view.containsEntry("b", "vb"));
			assertTrue(this.view.containsEntry("b", "n1"));
			assertTrue(this.view.containsEntry("b", "n2"));
		}

		@Test
		public void removeAll() {
			this.view.removeAll("a");
			assertFalse(this.view.containsEntry("a", "va1"));
			assertFalse(this.view.containsEntry("a", "va2"));
			assertTrue(this.view.containsEntry("b", "vb"));
			assertFalse(this.view.containsEntry("b", "vc"));
			assertFalse(this.view.containsEntry("c", "va1"));
		}

		@Test
		public void clear() {
			this.view.clear();
			assertFalse(this.view.containsEntry("a", "va1"));
			assertFalse(this.view.containsEntry("a", "va2"));
			assertFalse(this.view.containsEntry("b", "vb"));
			assertFalse(this.view.containsEntry("b", "vc"));
			assertFalse(this.view.containsEntry("c", "va1"));
		}

		@Test
		public void get() {
			assertContains(this.view.get("a"), "va1", "va2");
			assertContains(this.view.get("b"), "vb");
			assertContains(this.view.get("c"));
		}

		@Test
		public void keySet() {
			Set<String> keys = this.view.keySet();
			assertContains(keys, "a", "b");
		}

		@Test
		public void keys() {
			Multiset<String> keys = this.view.keys();
			assertContains(keys, "a", "a", "b");
		}

		@Test
		public void values() {
			Collection<String> values = this.view.values();
			assertContains(values, "va1", "va2", "vb");
		}

		@Test
		public void entries() {
			Collection<Entry<String, String>> entries = this.view.entries();
			assertContains(entries, Maps.immutableEntry("a", "va1"), Maps.immutableEntry("a", "va2"),
					Maps.immutableEntry("b", "vb"));
		}

		@Test
		public void asMap() {
			Map<String, Collection<String>> map = this.view.asMap();
			Collection<String> values;
			values = map.get("a");
			assertEquals(2, values.size());
			assertTrue(values.contains("va1"));
			assertTrue(values.contains("va2"));
			values = map.get("b");
			assertEquals(1, values.size());
			assertTrue(values.contains("vb"));
			values = map.get("c");
			assertNull(values);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ViewTests extends AbstractJanusTest {

		@Nullable
		private MultiMap<String, String> map;

		@Nullable
		private HazelcastDMultiMapView<String, String> view;

		@Before
		public void setUp() {
			this.map = new HzMultiMapMock<>(UUID.randomUUID().toString());
			this.view = new HazelcastDMultiMapView<>(this.map);
		}

		@Test
		public void put() {
			String newKey = UUID.randomUUID().toString();
			String newValue = UUID.randomUUID().toString();
			this.map.put(newKey, newValue);
			//
			assertEquals(1, this.view.size());
			Collection<String> col = this.view.get(newKey);
			assertNotNull(col);
			assertEquals(1, col.size());
			assertSame(newValue, col.iterator().next());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class SpecificDMultiMapFunctionTests extends AbstractJanusTest {

		@Nullable
		private MultiMap<String, String> map;

		@Nullable
		private HazelcastDMultiMapView<String, String> view;

		@Before
		public void setUp() {
			this.map = new HzMultiMapMock<>(UUID.randomUUID().toString());
			this.view = new HazelcastDMultiMapView<>(this.map);
		}

		@Test
		public void valueCount_empty() {
			assertEquals(0, this.view.valueCount("abc"));
		}

		@Test
		public void valueCount_oneKey_oneValue() {
			this.view.put("abc", "vABC");
			assertEquals(1, this.view.valueCount("abc"));
		}

		@Test
		public void valueCount_oneKey_twoValues() {
			this.view.put("abc", "vABC");
			this.view.put("abc", "vABC2");
			assertEquals(2, this.view.valueCount("abc"));
		}

		@Test
		public void valueCount_oneKey_threeValues() {
			this.view.put("abc", "vABC");
			this.view.put("abc", "vABC2");
			this.view.put("abc", "vABC3");
			assertEquals(3, this.view.valueCount("abc"));
		}

		@Test
		public void valueCount_twoKeys_oneValue() {
			this.view.put("abc", "vABC");
			this.view.put("def", "vDEF");
			assertEquals(1, this.view.valueCount("abc"));
		}

		@Test
		public void valueCount_twoKeys_twoValues() {
			this.view.put("abc", "vABC");
			this.view.put("abc", "vABC2");
			this.view.put("def", "vDEF");
			this.view.put("def", "vDEF2");
			assertEquals(2, this.view.valueCount("abc"));
		}

		@Test
		public void valueCount_twoKeys_threeValues() {
			this.view.put("abc", "vABC");
			this.view.put("abc", "vABC2");
			this.view.put("abc", "vABC2");
			this.view.put("def", "vDEF");
			this.view.put("def", "vDEF2");
			this.view.put("def", "vDEF2");
			assertEquals(2, this.view.valueCount("abc"));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ListeningFeatureTests extends AbstractJanusTest {

		@Nullable
		private DMapListener<String, String> listener;

		@Nullable
		private MultiMap<String, String> map;

		@Nullable
		private HazelcastDMultiMapView<String, String> view;

		@Before
		public void setUp() {
			this.listener = mock(DMapListener.class);
			this.map = new HzMultiMapMock<>(UUID.randomUUID().toString());
			this.view = new HazelcastDMultiMapView<>(this.map);
		}

		@Test
		public void entryAdded_withoutListener() {
			this.view.put("abc", "vABC");
			verifyZeroInteractions(this.listener);
		}

		@Test
		public void entryAdded_removeListener() {
			this.view.addDMapListener(this.listener);
			this.view.removeDMapListener(this.listener);
			this.view.put("abc", "vABC");
			verifyZeroInteractions(this.listener);
		}

		@Test
		public void entryAdded_withListener() {
			this.view.addDMapListener(this.listener);
			this.view.put("abc", "vABC");
			ArgumentCaptor<String> arg0 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<String> arg1 = ArgumentCaptor.forClass(String.class);
			verify(this.listener, times(1)).entryAdded(arg0.capture(), arg1.capture());
			assertEquals("abc", arg0.getValue());
			assertEquals("vABC", arg1.getValue());
		}

		@Test
		public void entryUpdated_withoutListener() {
			this.view.put("abc", "vABC");
			this.view.put("abc", "vABC2");
			verifyZeroInteractions(this.listener);
		}

		@Test
		public void entryUpdated_removeListener() {
			this.view.put("abc", "vABC");
			this.view.addDMapListener(this.listener);
			this.view.removeDMapListener(this.listener);
			this.view.put("abc", "vABC2");
			verifyZeroInteractions(this.listener);
		}

		@Test
		public void entryUpdated_withListener() {
			this.view.put("abc", "vABC");
			this.view.addDMapListener(this.listener);
			this.view.put("abc", "vABC2");
			ArgumentCaptor<String> arg0 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<String> arg1 = ArgumentCaptor.forClass(String.class);
			verify(this.listener, times(1)).entryAdded(arg0.capture(), arg1.capture());
			assertEquals("abc", arg0.getValue());
			assertEquals("vABC2", arg1.getValue());
		}

		@Test
		public void entryRemoved_withListener() {
			this.view.put("abc", "vABC");
			this.view.removeAll("abc");
			verifyZeroInteractions(this.listener);
		}

		@Test
		public void entryRemoved_removeListener() {
			this.view.put("abc", "vABC");
			this.view.addDMapListener(this.listener);
			this.view.removeDMapListener(this.listener);
			this.view.removeAll("abc");
			verifyZeroInteractions(this.listener);
		}

		@Test
		public void entryRemoved_withoutListener() {
			this.view.put("abc", "vABC");
			this.view.addDMapListener(this.listener);
			this.view.removeAll("abc");
			ArgumentCaptor<String> arg0 = ArgumentCaptor.forClass(String.class);
			ArgumentCaptor<String> arg1 = ArgumentCaptor.forClass(String.class);
			verify(this.listener, times(1)).entryRemoved(arg0.capture(), arg1.capture());
			assertEquals("abc", arg0.getValue());
			assertEquals("vABC", arg1.getValue());
		}

		@Test
		public void mapCleared_withListener() {
			this.view.put("abc", "vABC");
			this.view.clear();
			verifyZeroInteractions(this.listener);
		}

		@Test
		public void mapCleared_removeListener() {
			this.view.put("abc", "vABC");
			this.view.addDMapListener(this.listener);
			this.view.removeDMapListener(this.listener);
			this.view.clear();
			verifyZeroInteractions(this.listener);
		}

		@Test
		public void mapCleared_withoutListener() {
			this.view.put("abc", "vABC");
			this.view.addDMapListener(this.listener);
			this.view.clear();
			ArgumentCaptor<Boolean> arg0 = ArgumentCaptor.forClass(Boolean.class);
			verify(this.listener, times(1)).mapCleared(arg0.capture());
			assertFalse(arg0.getValue());
		}

	}

}
