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
package io.janusproject.tests.kernel.services.guava;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;

import java.io.Serializable;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
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
import com.google.common.collect.SetMultimap;
import com.google.common.collect.Sets;
import com.google.common.collect.testing.features.CollectionFeature;
import com.google.common.collect.testing.features.CollectionSize;
import com.google.common.collect.testing.features.MapFeature;
import com.google.common.collect.testing.google.MultimapTestSuiteBuilder;
import com.google.common.collect.testing.google.TestStringSetMultimapGenerator;
import io.janusproject.kernel.services.guava.DMultiMapView;
import io.janusproject.services.distributeddata.DMapListener;
import io.janusproject.tests.testutils.AbstractJanusTest;
import io.janusproject.util.DataViewDelegate;
import io.janusproject.util.DataViewDelegate.Delegator;
import junit.framework.TestSuite;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;

import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	DMultiMapViewTest.BackedCollectionTests.class,
	DMultiMapViewTest.ViewTests.class,
	DMultiMapViewTest.SpecificDMultiMapFunctionTests.class,
	DMultiMapViewTest.ListeningFeatureTests.class,
	//DMultiMapViewTest.GuavaMultiMapOperationTests.class,
})
@SuppressWarnings("all")
public class DMultiMapViewTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class BackedCollectionTests extends AbstractJanusTest {

		@Nullable
		private String name;

		@Nullable
		private Multimap<String, String> map;

		@Nullable
		private DMultiMapView<String, String> view;

		@Before
		public void setUp() {
			this.name = UUID.randomUUID().toString();
			Supplier<List<String>> supplier = new Supplier<List<String>>() {
				@Override
				public List<String> get() {
					return Lists.newArrayList();
				}
			};
			this.map = Multimaps.newListMultimap(Maps.<String, Collection<String>> newHashMap(), supplier);
			this.view = new DMultiMapView<>(this.name, this.map);
			this.view.put("a", "va");
			this.view.put("b", "vb");
		}

		@Test
		public void isBackedCollection() {
			assertTrue(this.view.isBackedCollection());
		}

		@Test
		public void changesPropagation() {
			Assume.assumeTrue("The collection is not backing the changes to the underlying collection",
					this.view.isBackedCollection());
			assertTrue(this.view.keySet().remove("b"));
			assertEquals(1, this.map.size());
			assertTrue(this.map.containsKey("a"));
			assertTrue(this.map.containsValue("va"));
		}

		@Test
		public void noChangesPropagation() {
			Assume.assumeFalse("The collection is backing the changes to the underlying collection",
					this.view.isBackedCollection());
			assertTrue(this.view.keySet().remove("b"));
			assertEquals(2, this.map.size());
			assertTrue(this.map.containsKey("a"));
			assertTrue(this.map.containsValue("va"));
			assertTrue(this.map.containsKey("b"));
			assertTrue(this.map.containsValue("vb"));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class GuavaMultiMapOperationTests {

		public static TestSuite suite() {
			return MultimapTestSuiteBuilder
					// The create method is called with an array of elements
					// that should populate the collection.
					.using(new TestStringSetMultimapGenerator() {
						@Override
						protected SetMultimap<String, String> create(Entry<String, String>[] entries) {
							Map<String, Collection<String>> jvmMap = Maps.newHashMap();
							Multimap<String, String> map = Multimaps.newSetMultimap(jvmMap, new Supplier<Set<String>>() {
								@Override
								public Set<String> get() {
									return Sets.newHashSet();
								}
							});
							for (Entry<String, String> entry : entries) {
								map.put(entry.getKey(), entry.getValue());
							}
							DMultiMapView<String, String> view = new DMultiMapView<>(UUID.randomUUID().toString(), map);
							return new GuavaTestFakeView(view);
						}
					}).named("Guava-based DMultiMap tests")
					.withFeatures(MapFeature.ALLOWS_NULL_KEYS, MapFeature.ALLOWS_NULL_VALUES, MapFeature.ALLOWS_ANY_NULL_QUERIES,
							MapFeature.GENERAL_PURPOSE, MapFeature.FAILS_FAST_ON_CONCURRENT_MODIFICATION,
							CollectionFeature.SUPPORTS_ITERATOR_REMOVE, CollectionSize.ANY)
					.createTestSuite();
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
		private Map<String, Collection<String>> jvmMap;

		@Nullable
		private Multimap<String, String> map;

		@Nullable
		private DMultiMapView<String, String> view;

		@Before
		public void setUp() {
			this.jvmMap = Maps.newHashMap();
			this.map = Multimaps.newListMultimap(this.jvmMap, new Supplier<List<String>>() {
				@Override
				public List<String> get() {
					return Lists.newArrayList();
				}
			});
			this.view = new DMultiMapView<>(UUID.randomUUID().toString(), this.map);
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
		private Map<String, Collection<String>> jvmMap;

		@Nullable
		private Multimap<String, String> map;

		@Nullable
		private DMultiMapView<String, String> view;

		@Before
		public void setUp() {
			this.jvmMap = Maps.newHashMap();
			this.map = Multimaps.newListMultimap(this.jvmMap, new Supplier<List<String>>() {
				@Override
				public List<String> get() {
					return Lists.newArrayList();
				}
			});
			this.view = new DMultiMapView<>(UUID.randomUUID().toString(), this.map);
		}

		@Test
		public void isBackedCollection() {
			assertTrue(this.view.isBackedCollection());
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
			assertEquals(3, this.view.valueCount("abc"));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ListeningFeatureTests extends AbstractJanusTest {

		private DMapListener<String, String> listener;
		private Map<String, Collection<String>> jvmMap;
		private Multimap<String, String> map;
		private DMultiMapView<String, String> view;

		@Before
		public void setUp() {
			this.listener = mock(DMapListener.class);
			this.jvmMap = Maps.newHashMap();
			this.map = Multimaps.newListMultimap(this.jvmMap, new Supplier<List<String>>() {
				@Override
				public List<String> get() {
					return Lists.newArrayList();
				}
			});
			this.view = new DMultiMapView<>(UUID.randomUUID().toString(), this.map);
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

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @param <K>
	 * @param <V>
	 */
	private static class GuavaTestFakeView
			implements SetMultimap<String, String>, Serializable, Delegator<DMultiMapView<String, String>> {

		private static final long serialVersionUID = -6970650402150118406L;

		private final DMultiMapView<String, String> dmultimap;

		public GuavaTestFakeView(DMultiMapView<String, String> dmultimap) {
			if (!(DataViewDelegate.undelegate(dmultimap) instanceof SetMultimap<?, ?>)) {
				throw new AssertionError("Incompatible type of multimap. It must be a SetMultimap.");
			}
			this.dmultimap = dmultimap;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public DMultiMapView<String, String> getDelegatedObject() {
			return this.dmultimap;
		}

		@Override
		public String toString() {
			return getDelegatedObject().toString();
		}

		@Override
		public boolean equals(Object obj) {
			return getDelegatedObject().equals(DataViewDelegate.undelegate(obj));
		}

		@Override
		public int hashCode() {
			return getDelegatedObject().hashCode();
		}

		@Override
		public int size() {
			return this.dmultimap.size();
		}

		@Override
		public boolean isEmpty() {
			return this.dmultimap.isEmpty();
		}

		@Override
		public boolean containsKey(Object key) {
			return this.dmultimap.containsKey(key);
		}

		@Override
		public boolean containsValue(Object value) {
			return this.dmultimap.containsValue(value);
		}

		@Override
		public boolean containsEntry(Object key, Object value) {
			return this.dmultimap.containsEntry(key, value);
		}

		@Override
		public boolean put(String key, String value) {
			return this.dmultimap.put(key, value);
		}

		@Override
		public boolean remove(Object key, Object value) {
			return this.dmultimap.remove(key, value);
		}

		@Override
		public boolean putAll(String key, Iterable<? extends String> values) {
			return this.dmultimap.putAll(key, values);
		}

		@Override
		public boolean putAll(Multimap<? extends String, ? extends String> multimap) {
			return this.dmultimap.putAll(multimap);
		}

		@Override
		public void clear() {
			this.dmultimap.clear();
		}

		@Override
		public Set<String> keySet() {
			return this.dmultimap.keySet();
		}

		@Override
		public Multiset<String> keys() {
			return this.dmultimap.keys();
		}

		@Override
		public Collection<String> values() {
			return this.dmultimap.values();
		}

		@Override
		public Map<String, Collection<String>> asMap() {
			return this.dmultimap.asMap();
		}

		@Override
		public Set<String> get(String key) {
			return new SetView(this.dmultimap.get(key));
		}

		@Override
		public Set<String> removeAll(Object key) {
			return new SetView(this.dmultimap.removeAll(key));
		}

		@Override
		public Set<String> replaceValues(String key, Iterable<? extends String> values) {
			return new SetView<>(this.dmultimap.replaceValues(key, values));
		}

		@Override
		public Set<Entry<String, String>> entries() {
			return new SetView(this.dmultimap.entries());
		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private static class SetView<E> extends AbstractSet<E> implements Serializable {

			private static final long serialVersionUID = -6970650402150118406L;

			private final Collection<E> collection;

			public SetView(Collection<E> collection) {
				this.collection = collection;
			}

			@Override
			public String toString() {
				return this.collection.toString();
			}

			@Override
			public boolean equals(Object o) {
				return this.collection.equals(o);
			}

			@Override
			public int hashCode() {
				return this.collection.hashCode();
			}

			@Override
			public boolean add(E e) {
				return this.collection.add(e);
			}

			@Override
			public void clear() {
				this.collection.clear();
			}

			@Override
			public Iterator<E> iterator() {
				return this.collection.iterator();
			}

			@Override
			public int size() {
				return this.collection.size();
			}

		}

	}

}
