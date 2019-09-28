/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.util.tests.util.concurrent;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.lang.util.SynchronizedSet;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.util.concurrent.Collections3;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	Collections3Test.SynchronizedCollectionTest.class,
	Collections3Test.SynchronizedSetTest.class
})
@SuppressWarnings("all")
public class Collections3Test {

	@RunWith(Suite.class)
	@SuiteClasses({
		Collections3Test.SynchronizedCollectionTest.CollectionTest.class,
		Collections3Test.SynchronizedCollectionTest.SyncTest.class
	})
	public static class SynchronizedCollectionTest {

		public static class CollectionTest extends AbstractSarlTest {

			private ReadWriteLock lock;

			private List<String> original;

			private SynchronizedCollection<String> collection;

			@Before
			public void setUp() {
				this.lock = new ReentrantReadWriteLock();
				this.original = new ArrayList<>();
				for(int i=0; i<50; ++i) {
					this.original.add("0x"+Double.toHexString(Math.random())); //$NON-NLS-1$
				}
				this.collection = Collections3.synchronizedCollection(this.original, this.lock);
			}

			@Test
			public void size() {
				assertEquals(this.original.size(), this.collection.size());
			}

			@Test
			public void isEmpty() {
				assertFalse(this.collection.isEmpty());
			}

			@Test
			public void contains() {
				for(String s : this.original) {
					assertTrue(this.collection.contains(s));
				}
				for(int i=0; i<50; ++i) {
					assertFalse(this.collection.contains(Integer.toString(i)));
				}
			}

			@Test
			public void toArray() {
				Object[] t = this.collection.toArray();
				assertEquals(this.original.size(), t.length);
				for(int i=0; i<t.length; ++i) {
					assertSame(this.original.get(i), t[i]);
				}
			}

			@Test
			public void toArray_ObjectArray() {
				String[] t = new String[this.original.size()/2];
				String[] t2 = this.collection.toArray(t);
				assertNotSame(t, t2);
				assertEquals(this.original.size(), t2.length);
				for(int i=0; i<t2.length; ++i) {
					assertSame(this.original.get(i), t2[i]);
				}
			}

			@Test
			public void add() {
				this.collection.add(Integer.toString(5));
				assertTrue(this.collection.contains(Integer.toString(5)));
				assertTrue(this.original.contains(Integer.toString(5)));
			}

			@Test
			public void remove() {
				assertFalse(this.collection.remove(Integer.toString(5)));
				Random r = new Random();
				while (!this.original.isEmpty()) {
					String s = this.original.get(r.nextInt(this.original.size()));
					assertTrue(this.collection.remove(s));
					assertFalse(this.collection.contains(s));
					assertFalse(this.original.contains(s));
				}
				assertTrue(this.collection.isEmpty());
				assertFalse(this.collection.remove(Integer.toString(5)));
			}

			@Test
			public void clear() {
				this.collection.clear();
				assertTrue(this.collection.isEmpty());
				assertTrue(this.original.isEmpty());
			}

			@Test
			public void testEquals() {
				assertTrue(this.collection.equals(this.collection));
				assertTrue(this.collection.equals(this.original));
				Collection<String> c = new ArrayList<>(this.original);
				assertTrue(this.collection.equals(c));
				assertFalse(this.collection.equals(Collections.singleton(5)));
			}

			@Test
			public void testHashCode() {
				assertEquals(this.collection.hashCode(), this.collection.hashCode());
				assertEquals(this.original.hashCode(), this.collection.hashCode());
				Collection<String> c = new ArrayList<>(this.original);
				assertEquals(c.hashCode(), this.collection.hashCode());
				assertNotEquals(Collections.singleton(5), this.collection.hashCode());
			}

			@Test
			public void iterator() {
				Iterator<String> it = this.collection.iterator();
				int i=0;
				while (it.hasNext() && i<this.original.size()) {
					String e = it.next();
					assertSame(this.original.get(i), e);
					++i;
				}
				assertFalse(it.hasNext());
				assertEquals(this.original.size(), i);
			}

			@Test
			public void getLock() {
				assertSame(this.lock, this.collection.getLock());
			}

		}

		public static class SyncTest extends AbstractSarlTest {

			private ExecutorService executors;
			private ReadWriteLock lock;
			private List<String> original;
			private Collection<String> collection;

			@Before
			public void setUp() {
				this.executors = Executors.newFixedThreadPool(5);
				this.lock = new ReentrantReadWriteLock();
				this.original = new ArrayList<>();
				for(int i=0; i<50; ++i) {
					this.original.add("0x"+Double.toHexString(Math.random())); //$NON-NLS-1$
				}
				this.collection = Collections3.synchronizedCollection(this.original, this.lock);
			}

			@After
			public void tearDown() throws Exception {
				this.executors.shutdownNow();
				this.executors.awaitTermination(30, TimeUnit.SECONDS);
			}

			@Test
			public void iteratorHasMutex() throws Exception {
				this.lock.writeLock().lock();
				try {
					this.executors.submit(new Runnable() {
						@Override
						public void run() {
							for(int i=0; i<10; ++i) {
								SyncTest.this.collection.add(Integer.toString(i));
							}
						}
					});
					Iterator<String> it = this.collection.iterator();
					int i = 0;
					while(it.hasNext()) {
						String s = it.next();
						assertSame(this.original.get(i), s);
						++i;
					}
				} finally {
					this.lock.writeLock().unlock();
				}
				this.executors.shutdown();
				this.executors.awaitTermination(30, TimeUnit.SECONDS);
				for(int i=0; i<10; ++i) {
					assertTrue(this.collection.contains(Integer.toString(i)));
				}
			}

		}

	}

	@RunWith(Suite.class)
	@SuiteClasses({
		Collections3Test.SynchronizedSetTest.CollectionTest.class,
		Collections3Test.SynchronizedSetTest.SyncTest.class
	})
	public static class SynchronizedSetTest {

		public static class CollectionTest extends AbstractSarlTest {

			private ReadWriteLock lock;
			private TreeSet<String> original;
			private SynchronizedSet<String> collection;

			@Before
			public void setUp() {
				this.lock = new ReentrantReadWriteLock();
				this.original = new TreeSet<>();
				for(int i=0; i<50; ++i) {
					this.original.add("0x"+Double.toHexString(Math.random())); //$NON-NLS-1$
				}
				this.collection = Collections3.synchronizedSet(this.original, this.lock);
			}

			@Test
			public void size() {
				assertEquals(this.original.size(), this.collection.size());
			}

			@Test
			public void isEmpty() {
				assertFalse(this.collection.isEmpty());
			}

			@Test
			public void contains() {
				for(String s : this.original) {
					assertTrue(this.collection.contains(s));
				}
				for(int i=0; i<50; ++i) {
					assertFalse(this.collection.contains(Integer.toString(i)));
				}
			}

			@Test
			public void toArray() {
				Object[] t = this.collection.toArray();
				assertEquals(this.original.size(), t.length);
				Iterator<String> it = this.original.iterator();
				for(int i=0; i<t.length; ++i) {
					assertSame(it.next(), t[i]);
				}
			}

			@Test
			public void toArray_ObjectArray() {
				String[] t = new String[this.original.size()/2];
				String[] t2 = this.collection.toArray(t);
				assertNotSame(t, t2);
				assertEquals(this.original.size(), t2.length);
				Iterator<String> it = this.original.iterator();
				for(int i=0; i<t2.length; ++i) {
					assertSame(it.next(), t2[i]);
				}
			}

			@Test
			public void add() {
				this.collection.add(Integer.toString(5));
				assertTrue(this.collection.contains(Integer.toString(5)));
				assertTrue(this.original.contains(Integer.toString(5)));
			}

			private <S> S get(Set<S> c, int index) {
				Iterator<S> it = c.iterator();
				for(int i=0; i<index; ++i) {
					it.next();
				}
				return it.next();
			}

			@Test
			public void remove() {
				assertFalse(this.collection.remove(Integer.toString(5)));
				Random r = new Random();
				while (!this.original.isEmpty()) {
					String s = get(this.original, r.nextInt(this.original.size()));
					assertTrue(this.collection.remove(s));
					assertFalse(this.collection.contains(s));
					assertFalse(this.original.contains(s));
				}
				assertTrue(this.collection.isEmpty());
				assertFalse(this.collection.remove(Integer.toString(5)));
			}

			@Test
			public void clear() {
				this.collection.clear();
				assertTrue(this.collection.isEmpty());
				assertTrue(this.original.isEmpty());
			}

			@Test
			public void testEquals() {
				assertTrue(this.collection.equals(this.collection));
				assertTrue(this.collection.equals(this.original));
				Collection<String> c = new ArrayList<>(this.original);
				assertFalse(this.collection.equals(c));
				c = new TreeSet<>(this.original);
				assertTrue(this.collection.equals(c));
				assertFalse(this.collection.equals(Collections.singleton(5)));
			}

			@Test
			public void testHashCode() {
				assertEquals(this.collection.hashCode(), this.collection.hashCode());
				assertEquals(this.original.hashCode(), this.collection.hashCode());
				Collection<String> c = new ArrayList<>(this.original);
				assertNotEquals(c.hashCode(), this.collection.hashCode());
				c = new TreeSet<>(this.original);
				assertEquals(c.hashCode(), this.collection.hashCode());
				assertNotEquals(Collections.singleton(5), this.collection.hashCode());
			}

			@Test
			public void iterator() {
				Iterator<String> it = this.collection.iterator();
				Iterator<String> oit = this.original.iterator();
				while (it.hasNext() && oit.hasNext()) {
					String e = it.next();
					assertSame(oit.next(), e);
				}
				assertFalse(it.hasNext());
				assertFalse(oit.hasNext());
			}

			@Test
			public void getLock() {
				assertSame(this.lock, this.collection.getLock());
			}

		}

		public static class SyncTest extends AbstractSarlTest {

			private <S> S get(Set<S> c, int index) {
				Iterator<S> it = c.iterator();
				for(int i=0; i<index; ++i) {
					it.next();
				}
				return it.next();
			}

			private ExecutorService executors;
			private ReadWriteLock lock;
			private TreeSet<String> original;
			private Set<String> collection;

			@Before
			public void setUp() {
				this.executors = Executors.newFixedThreadPool(5);
				this.lock = new ReentrantReadWriteLock();
				this.original = new TreeSet<>();
				for(int i=0; i<50; ++i) {
					this.original.add("0x"+Double.toHexString(Math.random())); //$NON-NLS-1$
				}
				this.collection = Collections3.synchronizedSet(this.original, this.lock);
			}

			@After
			public void tearDown() throws Exception {
				this.executors.shutdownNow();
				this.executors.awaitTermination(30, TimeUnit.SECONDS);
			}

			@Test
			public void iteratorHasMutex() throws Exception {
				this.lock.writeLock().lock();
				try {
					this.executors.submit(new Runnable() {
						@Override
						public void run() {
							for(int i=0; i<10; ++i) {
								SyncTest.this.collection.add(Integer.toString(i));
							}
						}
					});
					Iterator<String> it = this.collection.iterator();
					int i = 0;
					while(it.hasNext()) {
						String s = it.next();
						assertSame(get(this.original, i), s);
						++i;
					}
				} finally {
					this.lock.writeLock().unlock();
				}
				this.executors.shutdown();
				this.executors.awaitTermination(30, TimeUnit.SECONDS);
				for(int i=0; i<10; ++i) {
					assertTrue(this.collection.contains(Integer.toString(i)));
				}
			}
		}

	}

}
