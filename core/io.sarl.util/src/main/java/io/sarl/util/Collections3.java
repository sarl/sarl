/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.util;

import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.lang.util.SynchronizedSet;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;



/** Utilities on collections.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class Collections3 {

	/**
	 * The empty set (immutable).  This set is serializable.
	 *
	 * @see #emptySynchronizedSet()
	 */
	public static final SynchronizedSet<Object> EMPTY_SET = new EmptySet();

	/** Copied from Collections.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @param <E>
	 */
	private static class SynchronizedCollectionWrapper<E> implements Serializable, SynchronizedCollection<E> {

		private static final long serialVersionUID = -7369575596119505391L;

		final Collection<E> c;  // Backing Collection
		final Object mutex;     // Object on which to synchronize

		SynchronizedCollectionWrapper(Collection<E> c, Object mutex) {
			this.c = c;
			this.mutex = mutex;
		}

		/** {@inheritDoc}
		 */
		@Override
		public boolean equals(Object obj) {
			if (this == obj) return true;
			synchronized (this.mutex) {return this.c.equals(obj);}
		}

		/** {@inheritDoc}
		 */
		@Override
		public int hashCode() {
			synchronized (this.mutex) {return this.c.hashCode();}
		}

		@Override
		public int size() {
			synchronized (this.mutex) {return this.c.size();}
		}
		@Override
		public boolean isEmpty() {
			synchronized (this.mutex) {return this.c.isEmpty();}
		}
		@Override
		public boolean contains(Object o) {
			synchronized (this.mutex) {return this.c.contains(o);}
		}
		@Override
		public Object[] toArray() {
			synchronized (this.mutex) {return this.c.toArray();}
		}
		@Override
		public <T> T[] toArray(T[] a) {
			synchronized (this.mutex) {return this.c.toArray(a);}
		}
		@Override
		public Iterator<E> iterator() {
			return this.c.iterator(); // Must be manually synched by user!
		}
		@Override
		public boolean add(E e) {
			synchronized (this.mutex) {return this.c.add(e);}
		}
		@Override
		public boolean remove(Object o) {
			synchronized (this.mutex) {return this.c.remove(o);}
		}

		@Override
		public boolean containsAll(Collection<?> coll) {
			synchronized (this.mutex) {return this.c.containsAll(coll);}
		}
		@Override
		public boolean addAll(Collection<? extends E> coll) {
			synchronized (this.mutex) {return this.c.addAll(coll);}
		}
		@Override
		public boolean removeAll(Collection<?> coll) {
			synchronized (this.mutex) {return this.c.removeAll(coll);}
		}
		@Override
		public boolean retainAll(Collection<?> coll) {
			synchronized (this.mutex) {return this.c.retainAll(coll);}
		}
		@Override
		public void clear() {
			synchronized (this.mutex) {this.c.clear();}
		}
		@Override
		public String toString() {
			synchronized (this.mutex) {return this.c.toString();}
		}
		private void writeObject(ObjectOutputStream s) throws IOException {
			synchronized (this.mutex) {s.defaultWriteObject();}
		}
		@Override
		public Object mutex() {

			return this.mutex;
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @param <E>
	 */
	private static class UnmodifiableIterator<E> implements Iterator<E> {
		private final Iterator<E> i;
		public UnmodifiableIterator(Iterator<E> i) {
			this.i = i;
		}
		@Override
		public boolean hasNext() {
			return this.i.hasNext();
		}
		@Override
		public E next() {
			return this.i.next();
		}
		/** {@inheritDoc}
		 */
		 @Override
		 public void remove() {
			 throw new UnsupportedOperationException();
		 }
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @param <E>
	 */
	private static class UnmodifiableSetWrapper<E>
	implements Serializable, SynchronizedSet<E> {
		private static final long serialVersionUID = -6836341328672221159L;
		private final SynchronizedSet<E> c;
		UnmodifiableSetWrapper(SynchronizedSet<E> s) {
			this.c = s;
		}
		@Override
		public int size() {
			return this.c.size();
		}
		@Override
		public boolean isEmpty() {
			return this.c.isEmpty();
		}
		@Override
		public boolean contains(Object o) {
			return this.c.contains(o);
		}
		@Override
		public Iterator<E> iterator() {
			return new UnmodifiableIterator<>(this.c.iterator());
		}
		@Override
		public Object[] toArray() {
			return this.c.toArray();
		}
		@Override
		public <T> T[] toArray(T[] a) {
			return this.c.toArray(a);
		}
		@Override
		public boolean add(E e) {
			throw new UnsupportedOperationException();
		}
		@Override
		public boolean remove(Object o) {
			throw new UnsupportedOperationException();
		}
		@Override
		public boolean containsAll(Collection<?> c) {
			return this.c.containsAll(c);
		}
		@Override
		public boolean addAll(Collection<? extends E> c) {
			throw new UnsupportedOperationException();
		}
		@Override
		public boolean removeAll(Collection<?> c) {
			throw new UnsupportedOperationException();
		}
		@Override
		public boolean retainAll(Collection<?> c) {
			throw new UnsupportedOperationException();
		}
		@Override
		public void clear() {
			throw new UnsupportedOperationException();
		}
		@Override
		public Object mutex() {
			return this.c.mutex();
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @param <E>
	 */
	private static class UnmodifiableSynchronizedSetWrapper<E>
	extends SynchronizedSetWrapper<E> {
		private static final long serialVersionUID = 4528746726889467656L;
		UnmodifiableSynchronizedSetWrapper(Set<E> s, Object mutex) {
			super(s, mutex);
		}
		@Override
		public boolean add(E e) {
			throw new UnsupportedOperationException();
		}
		@Override
		public boolean remove(Object o) {
			throw new UnsupportedOperationException();
		}
		@Override
		public boolean addAll(Collection<? extends E> c) {
			throw new UnsupportedOperationException();
		}
		@Override
		public boolean removeAll(Collection<?> c) {
			throw new UnsupportedOperationException();
		}
		@Override
		public boolean retainAll(Collection<?> c) {
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
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @param <E>
	 */
	private static class SynchronizedSetWrapper<E>
	extends SynchronizedCollectionWrapper<E>
	implements SynchronizedSet<E> {
		private static final long serialVersionUID = -4653222127490655349L;
		SynchronizedSetWrapper(Set<E> s, Object mutex) {
			super(s, mutex);
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class EmptySet extends AbstractSet<Object> implements SynchronizedSet<Object>, Serializable {

		private static final long serialVersionUID = -3127287187730958288L;

		/**
		 */
		public EmptySet() {
			//
		}

		/** {@inheritDoc}
		 */
		@Override
		public Object mutex() {
			return new Object();
		}

		/** {@inheritDoc}
		 */
		@Override
		public Iterator<Object> iterator() {
			return Collections.emptyIterator();
		}

		/** {@inheritDoc}
		 */
		@Override
		public int size() {
			return 0;
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class SingletonSet<T> extends AbstractSet<T> implements SynchronizedSet<T>, Serializable {

		private static final long serialVersionUID = -143726600219970259L;

		private final T singleton; 

		/**
		 * @param singleton
		 */
		public SingletonSet(T singleton) {
			this.singleton = singleton;
		}

		/** {@inheritDoc}
		 */
		@Override
		public Object mutex() {
			return this;
		}

		/** {@inheritDoc}
		 */
		@Override
		public Iterator<T> iterator() {
			return Collections.singleton(this.singleton).iterator();
		}

		/** {@inheritDoc}
		 */
		@Override
		public int size() {
			return 1;
		}

	}

	/**
	 * Returns a synchronized (thread-safe) set backed by the specified
	 * set.  In order to guarantee serial access, it is critical that
	 * <strong>all</strong> access to the backing set is accomplished
	 * through the returned set.<p>
	 *
	 * It is imperative that the user manually synchronize on the returned
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
	 * @param  s the set to be "wrapped" in a synchronized set.
	 * @param mutex is the mutex to use for synchronizing.
	 * @return a synchronized view of the specified set.
	 */
	public static <T> SynchronizedSet<T> synchronizedSet(Set<T> s, Object mutex) {
		return new SynchronizedSetWrapper<>(s, mutex);
	}

	/**
	 * Returns a synchronized (thread-safe) collection backed by the specified
	 * collection.  In order to guarantee serial access, it is critical that
	 * <strong>all</strong> access to the backing collection is accomplished
	 * through the returned collection.<p>
	 *
	 * It is imperative that the user manually synchronize on the returned
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
	 * that the backing collection is a set or a list.<p>
	 *
	 * The returned collection will be serializable if the specified collection
	 * is serializable.
	 *
	 * @param  c the collection to be "wrapped" in a synchronized collection.
	 * @param mutex is the mutex to use for synchronizing.
	 * @return a synchronized view of the specified collection.
	 */
	public static <T> SynchronizedCollection<T> synchronizedCollection(Collection<T> c, Object mutex) {
		return new SynchronizedCollectionWrapper<>(c, mutex);
	}

	/** Replies an empty synchronized set.
	 * 
	 * @return an empty synchronized set.
	 */
	@SuppressWarnings("unchecked")
	public static <T> SynchronizedSet<T> emptySynchronizedSet() {
		return (SynchronizedSet<T>)EMPTY_SET;
	}

	/** Replies an empty synchronized set.
	 * 
	 * @param element
	 * @return an empty synchronized set.
	 */
	public static <T> SynchronizedSet<T> synchronizedSingleton(T element) {
		return new SingletonSet<>(element);
	}

	/** Replies an unmodifiable synchronized set.
	 * 
	 * @param elements
	 * @return an unmodifiable synchronized set.
	 */
	public static <T> SynchronizedSet<T> unmodifiableSynchronizedSet(SynchronizedSet<T> elements) {
		return new UnmodifiableSetWrapper<>(elements); 
	}

	/**
	 * Returns an immutable synchronized (thread-safe) set backed by the specified
	 * set.  In order to guarantee serial access, it is critical that
	 * <strong>all</strong> access to the backing set is accomplished
	 * through the returned set.<p>
	 *
	 * It is imperative that the user manually synchronize on the returned
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
	 * @param elements
	 * @param mutex is the mutex to use for synchronizing.
	 * @return an unmodifiable synchronized set.
	 */
	public static <T> SynchronizedSet<T> unmodifiableSynchronizedSet(Set<T> elements, Object mutex) {
		return new UnmodifiableSynchronizedSetWrapper<>(elements, mutex);
	}

}
