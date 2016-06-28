/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.util;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;

import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.lang.util.SynchronizedSet;

/** Utilities on collections.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class Collections3 {

	/**
	 * The empty set (immutable).  This set is serializable.
	 *
	 * @see #emptySynchronizedSet()
	 */
	public static final SynchronizedSet<Object> EMPTY_SET = new EmptySet();

	private Collections3() {
		//
	}

	/** Copied from Collections.
	 *
	 * @param <E> - type of the elements in the collection.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class SynchronizedCollectionWrapper<E> implements Serializable, SynchronizedCollection<E> {

		private static final long serialVersionUID = -7369575596119505391L;

		/** Backing Collection.
		 */
		final Collection<E> collection;

		/** Object on which to synchronize.
		 */
		final Object mutex;

		SynchronizedCollectionWrapper(Collection<E> collection, Object mutex) {
			this.collection = collection;
			this.mutex = mutex;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj) {
				return true;
			}
			synchronized (this.mutex) {
				return this.collection.equals(obj);
			}
		}

		@Override
		public int hashCode() {
			synchronized (this.mutex) {
				return this.collection.hashCode();
			}
		}

		@Override
		public int size() {
			synchronized (this.mutex) {
				return this.collection.size();
			}
		}

		@Override
		public boolean isEmpty() {
			synchronized (this.mutex) {
				return this.collection.isEmpty();
			}
		}

		@Override
		public boolean contains(Object object) {
			synchronized (this.mutex) {
				return this.collection.contains(object);
			}
		}

		@Override
		public Object[] toArray() {
			synchronized (this.mutex) {
				return this.collection.toArray();
			}
		}

		@Override
		public <T> T[] toArray(T[] output) {
			synchronized (this.mutex) {
				return this.collection.toArray(output);
			}
		}

		@Override
		public Iterator<E> iterator() {
			// Must be manually synched by user!
			return this.collection.iterator();
		}

		@Override
		public boolean add(E element) {
			synchronized (this.mutex) {
				return this.collection.add(element);
			}
		}

		@Override
		public boolean remove(Object element) {
			synchronized (this.mutex) {
				return this.collection.remove(element);
			}
		}

		@Override
		public boolean containsAll(Collection<?> coll) {
			synchronized (this.mutex) {
				return this.collection.containsAll(coll);
			}
		}

		@Override
		public boolean addAll(Collection<? extends E> coll) {
			synchronized (this.mutex) {
				return this.collection.addAll(coll);
			}
		}

		@Override
		public boolean removeAll(Collection<?> coll) {
			synchronized (this.mutex) {
				return this.collection.removeAll(coll);
			}
		}

		@Override
		public boolean retainAll(Collection<?> coll) {
			synchronized (this.mutex) {
				return this.collection.retainAll(coll);
			}
		}

		@Override
		public void clear() {
			synchronized (this.mutex) {
				this.collection.clear();
			}
		}

		@Override
		public String toString() {
			synchronized (this.mutex) {
				return this.collection.toString();
			}
		}

		private void writeObject(ObjectOutputStream stream) throws IOException {
			synchronized (this.mutex) {
				stream.defaultWriteObject();
			}
		}

		@Override
		public Object mutex() {
			return this.mutex;
		}
	}

	/** Iterator that disable modifications on the collection.
	 *
	 * @param <E> the type of the elements in the collection.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class UnmodifiableIterator<E> implements Iterator<E> {

		private final Iterator<E> iterator;

		UnmodifiableIterator(Iterator<E> iterator) {
			this.iterator = iterator;
		}

		@Override
		public boolean hasNext() {
			return this.iterator.hasNext();
		}

		@Override
		public E next() {
			return this.iterator.next();
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

	/** Implementation of a set wrapper that disable modifications.
	 *
	 * @param <E> the type of the elements in the collection.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class UnmodifiableSetWrapper<E>
			implements Serializable, SynchronizedSet<E> {

		private static final long serialVersionUID = -6836341328672221159L;

		private final SynchronizedSet<E> collection;

		UnmodifiableSetWrapper(SynchronizedSet<E> set) {
			this.collection = set;
		}

		@Override
		public int size() {
			return this.collection.size();
		}

		@Override
		public boolean isEmpty() {
			return this.collection.isEmpty();
		}

		@Override
		public boolean contains(Object object) {
			return this.collection.contains(object);
		}

		@Override
		public Iterator<E> iterator() {
			return new UnmodifiableIterator<>(this.collection.iterator());
		}

		@Override
		public Object[] toArray() {
			return this.collection.toArray();
		}

		@Override
		public <T> T[] toArray(T[] output) {
			return this.collection.toArray(output);
		}

		@Override
		public boolean add(E element) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean remove(Object element) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean containsAll(Collection<?> collection) {
			return this.collection.containsAll(collection);
		}

		@Override
		public boolean addAll(Collection<? extends E> collection) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean removeAll(Collection<?> collection) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean retainAll(Collection<?> collection) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void clear() {
			throw new UnsupportedOperationException();
		}

		@Override
		public Object mutex() {
			return this.collection.mutex();
		}
	}

	/** Implementation of a synchronized set wrapper that disable modifications.
	 *
	 * @param <E> the type of the elements in the collection.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class UnmodifiableSynchronizedSetWrapper<E>
			extends SynchronizedSetWrapper<E> {

		private static final long serialVersionUID = 4528746726889467656L;

		UnmodifiableSynchronizedSetWrapper(Set<E> set, Object mutex) {
			super(set, mutex);
		}

		@Override
		public boolean add(E element) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean remove(Object object) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean addAll(Collection<? extends E> collection) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean removeAll(Collection<?> collection) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean retainAll(Collection<?> collection) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void clear() {
			throw new UnsupportedOperationException();
		}

		@Override
		public Iterator<E> iterator() {
			return new UnmodifiableIterator<>(super.iterator());
		}

	}

	/** Copied from Collections.
	 *
	 * @param <E> the type of the elements in the collection.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class SynchronizedSetWrapper<E>
			extends SynchronizedCollectionWrapper<E>
			implements SynchronizedSet<E> {

		private static final long serialVersionUID = -4653222127490655349L;

		SynchronizedSetWrapper(Set<E> set, Object mutex) {
			super(set, mutex);
		}

	}

	/** Implementation of an empty set.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class EmptySet extends AbstractSet<Object> implements SynchronizedSet<Object>, Serializable {

		private static final long serialVersionUID = -3127287187730958288L;

		EmptySet() {
			//
		}

		@Override
		public Object mutex() {
			return new Object();
		}

		@Override
		public Iterator<Object> iterator() {
			return Collections.emptyIterator();
		}

		@Override
		public int size() {
			return 0;
		}

	}

	/** Implementation of a set with a single element.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class SingletonSet<T> extends AbstractSet<T> implements SynchronizedSet<T>, Serializable {

		private static final long serialVersionUID = -143726600219970259L;

		private final T singleton;

		/**
		 * @param singleton - the singleton.
		 */
		SingletonSet(T singleton) {
			this.singleton = singleton;
		}

		@Override
		public Object mutex() {
			return this;
		}

		@Override
		public Iterator<T> iterator() {
			return Collections.singleton(this.singleton).iterator();
		}

		@Override
		public int size() {
			return 1;
		}

	}

	/**
	 * Returns a synchronized (thread-safe) set backed by the specified
	 * set.  In order to guarantee serial access, it is critical that
	 * <strong>all</strong> access to the backing set is accomplished
	 * through the returned set.
	 *
	 * <p>It is imperative that the user manually synchronize on the returned
	 * set when iterating over it:
	 * <pre>
	 *  Set s = Collections.synchronizedSet(new HashSet());
	 *      ...
	 *  synchronized (s) {
	 *      Iterator i = s.iterator(); // Must be in the synchronized block
	 *      while (i.hasNext())
	 *          foo(i.next());
	 *  }
	 * </pre>
	 * Failure to follow this advice may result in non-deterministic behavior.
	 *
	 * <p>The returned set will be serializable if the specified set is
	 * serializable.
	 *
	 * @param <T> - type of the set element.
	 * @param  set the set to be "wrapped" in a synchronized set.
	 * @param mutex is the mutex to use for synchronizing.
	 * @return a synchronized view of the specified set.
	 */
	public static <T> SynchronizedSet<T> synchronizedSet(Set<T> set, Object mutex) {
		return new SynchronizedSetWrapper<>(set, mutex);
	}

	/**
	 * Returns a synchronized (thread-safe) collection backed by the specified
	 * collection.  In order to guarantee serial access, it is critical that
	 * <strong>all</strong> access to the backing collection is accomplished
	 * through the returned collection.
	 *
	 * <p>It is imperative that the user manually synchronize on the returned
	 * collection when iterating over it:
	 * <pre>
	 *  Collection c = Collections.synchronizedCollection(myCollection);
	 *     ...
	 *  synchronized (c) {
	 *      Iterator i = c.iterator(); // Must be in the synchronized block
	 *      while (i.hasNext())
	 *         foo(i.next());
	 *  }
	 * </pre>
	 * Failure to follow this advice may result in non-deterministic behavior.
	 *
	 * <p>The returned collection does <i>not</i> pass the <tt>hashCode</tt>
	 * and <tt>equals</tt> operations through to the backing collection, but
	 * relies on <tt>Object</tt>'s equals and hashCode methods.  This is
	 * necessary to preserve the contracts of these operations in the case
	 * that the backing collection is a set or a list.
	 *
	 * <p>The returned collection will be serializable if the specified collection
	 * is serializable.
	 *
	 * @param <T> - type of the set elements.
	 * @param  collection the collection to be "wrapped" in a synchronized collection.
	 * @param mutex is the mutex to use for synchronizing.
	 * @return a synchronized view of the specified collection.
	 */
	public static <T> SynchronizedCollection<T> synchronizedCollection(Collection<T> collection, Object mutex) {
		return new SynchronizedCollectionWrapper<>(collection, mutex);
	}

	/** Replies an empty synchronized set.
	 *
	 * @param <T> - type of the set element.
	 * @return an empty synchronized set.
	 */
	@SuppressWarnings("unchecked")
	public static <T> SynchronizedSet<T> emptySynchronizedSet() {
		return (SynchronizedSet<T>) EMPTY_SET;
	}

	/** Replies an empty synchronized set.
	 *
	 * @param <T> - type of the element.
	 * @param element - the element to put in the singleton.
	 * @return an empty synchronized set.
	 */
	public static <T> SynchronizedSet<T> synchronizedSingleton(T element) {
		return new SingletonSet<>(element);
	}

	/** Replies an unmodifiable synchronized set.
	 *
	 * @param <T> - type of the elements in the set.
	 * @param elements - the set to transform as unmodifiable set.
	 * @return an unmodifiable synchronized set.
	 */
	public static <T> SynchronizedSet<T> unmodifiableSynchronizedSet(SynchronizedSet<T> elements) {
		return new UnmodifiableSetWrapper<>(elements);
	}

	/**
	 * Returns an immutable synchronized (thread-safe) set backed by the specified
	 * set.  In order to guarantee serial access, it is critical that
	 * <strong>all</strong> access to the backing set is accomplished
	 * through the returned set.
	 *
	 * <p>It is imperative that the user manually synchronize on the returned
	 * set when iterating over it:
	 * <pre>
	 *  Set s = Collections.synchronizedSet(new HashSet());
	 *      ...
	 *  synchronized (s) {
	 *      Iterator i = s.iterator(); // Must be in the synchronized block
	 *      while (i.hasNext())
	 *          foo(i.next());
	 *  }
	 * </pre>
	 * Failure to follow this advice may result in non-deterministic behavior.
	 *
	 * <p>The returned set will be serializable if the specified set is
	 * serializable.
	 *
	 * @param <T> - type of the elements in the set.
	 * @param elements - the set to transform as unmodifiable set.
	 * @param mutex is the mutex to use for synchronizing.
	 * @return an unmodifiable synchronized set.
	 */
	public static <T> SynchronizedSet<T> unmodifiableSynchronizedSet(Set<T> elements, Object mutex) {
		return new UnmodifiableSynchronizedSetWrapper<>(elements, mutex);
	}

}
