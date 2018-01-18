/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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
package io.sarl.lang.core.tests.scoping.batch;

import static io.sarl.lang.scoping.batch.SARLMapExtensions.operator_add;
import static io.sarl.lang.scoping.batch.SARLMapExtensions.operator_minus;
import static io.sarl.lang.scoping.batch.SARLMapExtensions.operator_plus;
import static io.sarl.lang.scoping.batch.SARLMapExtensions.operator_remove;
import static org.junit.Assert.*;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.Map.Entry;

import com.google.common.collect.Maps;
import com.google.common.collect.testing.MapTestSuiteBuilder;
import com.google.common.collect.testing.TestStringMapGenerator;
import com.google.common.collect.testing.features.CollectionFeature;
import com.google.common.collect.testing.features.CollectionSize;
import com.google.common.collect.testing.features.MapFeature;
import junit.framework.TestSuite;
import org.eclipse.xtext.xbase.lib.Pair;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.scoping.batch.UnmodifiableMergingMapView;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@RunWith(Suite.class)
@SuiteClasses({
	SARLMapExtensionsTest.ManualTest.class,
	SARLMapExtensionsTest.GuavaTest.class,
})
public class SARLMapExtensionsTest {

	public static class GuavaTest {
		public static TestSuite suite() {
			return MapTestSuiteBuilder
					// The create method is called with an array of elements
					// that should populate the collection.
					.using(new TestStringMapGenerator() {
						@Override
						protected Map<String, String> create(Entry<String, String>[] source) {
							Map<String, String> left = Maps.newHashMap();
							Map<String, String> right = Maps.newHashMap();
							for(int i = 0; i < source.length; i++) {
								Entry<String, String> entry = source[i];
								if (right.containsKey(entry.getKey())) {
									left.put(entry.getKey(), right.get(entry.getKey()));
									right.put(entry.getKey(), entry.getValue());
								} else if (i % 2 != 0) {
									left.put(entry.getKey(), entry.getValue());	
								} else {
									right.put(entry.getKey(), entry.getValue());
									if (i % 4 != 0) {
										left.put(entry.getKey(), "will be ignored");
									}
								}
							}
							return new UnmodifiableMergingMapView(left, right);
						}
					}).named("Guava-based UnmodifiableMergingMapView tests")
					.withFeatures(
							MapFeature.ALLOWS_NULL_KEYS,
							MapFeature.ALLOWS_NULL_VALUES,
							MapFeature.ALLOWS_ANY_NULL_QUERIES,
							MapFeature.FAILS_FAST_ON_CONCURRENT_MODIFICATION,
							CollectionFeature.ALLOWS_NULL_QUERIES,
							CollectionFeature.FAILS_FAST_ON_CONCURRENT_MODIFICATION,
							CollectionSize.ANY)
					.createTestSuite();
		}
	}

	public static class ManualTest {

		private Map<String, String> map;
		private String key1;
		private String key2;
		private String value1;
		private String value2;
	
		@Before
		public void setUp() {
			this.map = new HashMap<String, String>();
			this.key1 = "k1"; //$NON-NLS-1$
			this.key2 = "k2"; //$NON-NLS-1$
			this.value1 = UUID.randomUUID().toString();
			this.value2 = UUID.randomUUID().toString();
			this.map.put(this.key1, this.value1);
			this.map.put(this.key2, this.value2);
		}
	
		@After
		public void tearDown() {
			this.key1 = this.value1 = null;
			this.key2 = this.value2 = null;
			this.map = null;
		}
	
		@Test
		public void operator_add_MapPair_0() {
			String v = UUID.randomUUID().toString();
			String o;
	
			o = operator_add(this.map, new Pair<String, String>("k3", v));
			assertNull(o);
			assertEquals(3, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertEquals(v, this.map.get("k3"));
		}
	
		@Test
		public void operator_add_MapPair_1() {
			String v = UUID.randomUUID().toString();
			String o;
	
			o = operator_add(this.map, new Pair<String, String>("k2", v));
			assertEquals(this.value2, o);
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(v, this.map.get("k2"));
		}
	
		@Test
		public void operator_remove_MapPair_0() {
			String v = UUID.randomUUID().toString();
			boolean o;
	
			o = operator_remove(this.map, new Pair<String, String>("k3", v));
			assertFalse(o);
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
		}
	
		@Test
		public void operator_remove_MapPair_1() {
			String v = UUID.randomUUID().toString();
			boolean o;
	
			o = operator_remove(this.map, new Pair<String, String>("k2", v));
			assertFalse(o);
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
		}
	
		@Test
		public void operator_remove_MapPair_2() {
			String v = UUID.randomUUID().toString();
			boolean o;
	
			o = operator_remove(this.map, new Pair<String, String>("k2", new String(this.value2)));
			assertTrue(o);
			assertEquals(1, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertNull(this.map.get("k2"));
		}
	
		@Test
		public void operator_plus_MapPair_0() {
			String v = UUID.randomUUID().toString();
			Map<String, String> o;
	
			o = operator_plus(this.map, new Pair<String, String>("k3", v));
			assertNotNull(o);
			assertNotSame(this.map, o);
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertEquals(3, o.size());
			assertEquals(this.value1, o.get("k1"));
			assertEquals(this.value2, o.get("k2"));
			assertEquals(v, o.get("k3"));
		}
	
		@Test
		public void operator_plus_MapPair_1() {
			String v = UUID.randomUUID().toString();
			Map<String, String> o = operator_plus(this.map, new Pair<String, String>("k2", v));
			assertNotNull(o);
			assertNotSame(this.map, o);
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertNull(this.map.get("k3"));
			assertEquals(2, o.size());
			assertEquals(this.value1, o.get("k1"));
			assertEquals(v, o.get("k2"));
			assertNull(o.get("k3"));
		}
	
		@Test
		public void operator_minus_MapPair_0() {
			String v = UUID.randomUUID().toString();
			Map<String, String> o;
	
			o = operator_minus(this.map, new Pair<String, String>("k3", v));
	
			assertNotNull(o);
			assertNotSame(this.map, o);
			
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertNull(this.map.get("k3"));
			
			assertEquals(2, o.size());
			assertEquals(this.value1, o.get("k1"));
			assertEquals(this.value2, o.get("k2"));
			assertNull(o.get("k3"));
		}
	
		@Test
		public void operator_minus_MapPair_1() {
			String v = UUID.randomUUID().toString();
			Map<String, String> o = operator_minus(this.map, new Pair<String, String>("k2", v));
			
			assertNotNull(o);
			assertNotSame(this.map, o);
			
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertNull(this.map.get("k3"));
			
			assertEquals(2, o.size());
			assertEquals(this.value1, o.get("k1"));
			assertEquals(this.value2, o.get("k2"));
			assertNull(o.get("k3"));
		}
	
		@Test
		public void operator_minus_MapPair_2() {
			String v = UUID.randomUUID().toString();
			Map<String, String> o = operator_minus(this.map, new Pair<String, String>("k2", new String(this.value2)));
			
			assertNotNull(o);
			assertNotSame(this.map, o);
			
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertNull(this.map.get("k3"));
			
			assertEquals(1, o.size());
			assertEquals(this.value1, o.get("k1"));
			assertNull(o.get("k2"));
			assertNull(o.get("k3"));
		}
	
		@Test
		public void operator_plus_MapMap_0() {
			String v1 = UUID.randomUUID().toString();
			String v2 = UUID.randomUUID().toString();
			String v3 = UUID.randomUUID().toString();
			Map<String, String> o;
	
			Map<String, String> tmp = new HashMap<String, String>();
			tmp.put("k3", v1);
			tmp.put("k4", v2);
			tmp.put("k5", v3);
	
			o = operator_plus(this.map, tmp);
			assertNotNull(o);
			assertNotSame(this.map, o);
			assertNotSame(tmp, o);
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertEquals(3, tmp.size());
			assertEquals(v1, tmp.get("k3"));
			assertEquals(v2, tmp.get("k4"));
			assertEquals(v3, tmp.get("k5"));
			assertEquals(5, o.size());
			assertEquals(this.value1, o.get("k1"));
			assertEquals(this.value2, o.get("k2"));
			assertEquals(v1, o.get("k3"));
			assertEquals(v2, o.get("k4"));
			assertEquals(v3, o.get("k5"));
		}
	
		@Test
		public void operator_plus_MapMap_1() {
			String v1 = UUID.randomUUID().toString();
			String v2 = UUID.randomUUID().toString();
			String v3 = UUID.randomUUID().toString();
			Map<String, String> o;
	
			Map<String, String> tmp = new HashMap<String, String>();
			tmp.put("k3", v1);
			tmp.put("k4", v2);
			tmp.put("k5", v3);
	
			o = operator_plus(tmp, this.map);
			assertNotNull(o);
			assertNotSame(this.map, o);
			assertNotSame(tmp, o);
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertEquals(3, tmp.size());
			assertEquals(v1, tmp.get("k3"));
			assertEquals(v2, tmp.get("k4"));
			assertEquals(v3, tmp.get("k5"));
			assertEquals(5, o.size());
			assertEquals(this.value1, o.get("k1"));
			assertEquals(this.value2, o.get("k2"));
			assertEquals(v1, o.get("k3"));
			assertEquals(v2, o.get("k4"));
			assertEquals(v3, o.get("k5"));
		}
	
		@Test
		public void operator_plus_MapMap_2() {
			String v1 = UUID.randomUUID().toString();
			String v2 = UUID.randomUUID().toString();
			String v3 = UUID.randomUUID().toString();
			Map<String, String> o;
	
			Map<String, String> tmp = new HashMap<String, String>();
			tmp.put("k3", v1);
			tmp.put("k1", v2);
			tmp.put("k5", v3);
	
			o = operator_plus(this.map, tmp);
			assertNotNull(o);
			assertEquals(4, o.size());
			assertEquals(v2, o.get("k1"));
			assertEquals(this.value2, o.get("k2"));
			assertEquals(v1, o.get("k3"));
			assertNull(o.get("k4"));
			assertEquals(v3, o.get("k5"));
		}
	
		@Test
		public void operator_plus_MapMap_3() {
			String v1 = UUID.randomUUID().toString();
			String v2 = UUID.randomUUID().toString();
			String v3 = UUID.randomUUID().toString();
			Map<String, String> o;
	
			Map<String, String> tmp = new HashMap<String, String>();
			tmp.put("k3", v1);
			tmp.put("k1", v2);
			tmp.put("k5", v3);
	
			o = operator_plus(tmp, this.map);
			assertNotNull(o);
			assertEquals(4, o.size());
			assertEquals(this.value1, o.get("k1"));
			assertEquals(this.value2, o.get("k2"));
			assertEquals(v1, o.get("k3"));
			assertNull(o.get("k4"));
			assertEquals(v3, o.get("k5"));
		}
	
		@Test
		public void operator_remove_MapK_0() {
			String o = operator_remove(this.map, "k0");
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertNull(o);
		}
	
		@Test
		public void operator_remove_MapK_1() {
			String o = operator_remove(this.map, "k1");
			assertEquals(1, this.map.size());
			assertNull(this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertEquals(this.value1, o);
		}
	
		@Test
		public void operator_remove_MapK_2() {
			String o = operator_remove(this.map, "k2");
			assertEquals(1, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertNull(this.map.get("k2"));
			assertEquals(this.value2, o);
		}
	
		@Test
		public void operator_remove_MapK_3() {
			String o1 = operator_remove(this.map, "k2");
			String o2 = operator_remove(this.map, "k1");
			assertEquals(0, this.map.size());
			assertNull(this.map.get("k1"));
			assertNull(this.map.get("k2"));
			assertEquals(this.value2, o1);
			assertEquals(this.value1, o2);
		}
	
		@Test
		public void operator_remove_MapIterable() {
			List<String> m = new ArrayList<String>();
			m.add("k0");
			m.add("k1");
			operator_remove(this.map, m);
			assertEquals(1, this.map.size());
			assertNull(this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
		}
	
		@Test
		public void operator_minus_MapK_0() {
			Map<String, String> o = operator_minus(this.map, "k0");
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertNotNull(o);
			assertNotSame(this.map, o);
			assertEquals(2, o.size());
			assertEquals(this.value1, o.get("k1"));
			assertEquals(this.value2, o.get("k2"));
		}
	
		@Test
		public void operator_minus_MapK_1() {
			Map<String, String> o = operator_minus(this.map, "k2");
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertNotNull(o);
			assertNotSame(this.map, o);
			assertEquals(1, o.size());
			assertEquals(this.value1, o.get("k1"));
			assertNull(o.get("k2"));
		}
	
		@Test
		public void operator_minus_MapK_2() {
			Map<String, String> o1 = operator_minus(this.map, "k2");
			Map<String, String> o2 = operator_minus(o1, "k1");
			assertEquals(2, this.map.size());
			assertEquals(this.value1, this.map.get("k1"));
			assertEquals(this.value2, this.map.get("k2"));
			assertNotNull(o1);
			assertNotSame(this.map, o1);
			assertEquals(1, o1.size());
			assertEquals(this.value1, o1.get("k1"));
			assertNull(o1.get("k2"));
			assertNotNull(o2);
			assertNotSame(this.map, o2);
			assertEquals(0, o2.size());
			assertNull(o2.get("k1"));
			assertNull(o2.get("k2"));
		}

	}

}
