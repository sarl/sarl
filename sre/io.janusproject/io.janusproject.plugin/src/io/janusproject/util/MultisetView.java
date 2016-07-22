/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.janusproject.util;

import static com.google.common.base.Preconditions.checkNotNull;

import java.io.Serializable;
import java.util.AbstractCollection;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

import com.google.common.base.Objects;
import com.google.common.collect.Iterators;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multiset;
import io.janusproject.util.DataViewDelegate.Delegator;

/**
 * A view if the multiset of the keys in a {@link AbstractDMultiMapView}.
 *
 * @param <K> - the keys.
 * @param <V> - the values.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class MultisetView<K, V> extends AbstractCollection<K> implements Multiset<K>, Serializable, Delegator<Multimap<K, V>> {

	private static final long serialVersionUID = -4540240956327121021L;

	private final Multimap<K, V> multimap;

	private transient ElementSet elementSet;

	private transient EntrySet entrySet;

	/**
	 * @param map - the backed map.
	 */
	public MultisetView(Multimap<K, V> map) {
		this.multimap = map;
	}

	@Override
	public Multimap<K, V> getDelegatedObject() {
		return this.multimap;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		}
		if (obj instanceof Multiset) {
			Multiset<?> that = (Multiset<?>) obj;

			// We can't simply check whether the entry sets are equal, since that
			// approach fails when a TreeMultiset has a comparator that returns 0
			// when passed unequal elements.
			if (size() != that.size() || entrySet().size() != that.entrySet().size()) {
				return false;
			}
			for (Entry<?> entry : that.entrySet()) {
				if (count(entry.getElement()) != entry.getCount()) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	@Override
	public int hashCode() {
		return entrySet().hashCode();
	}

	@Override
	public String toString() {
		return entrySet().toString();
	}

	@Override
	public int size() {
		return getDelegatedObject().size();
	}

	@Override
	public boolean isEmpty() {
		return getDelegatedObject().isEmpty();
	}

	@Override
	public void clear() {
		getDelegatedObject().clear();
	}

	@SuppressWarnings("unchecked")
	@Override
	public int count(Object element) {
		try {
			Collection<V> values = getDelegatedObject().get((K) element);
			return values.size();
		} catch (ClassCastException exception) {
			return 0;
		}
	}

	@Override
	public boolean remove(Object element) {
		return remove(element, 1) > 0;
	}

	@SuppressWarnings("unchecked")
	@Override
	public int remove(Object element, int occurrences) {
		if (occurrences < 0) {
			throw new IllegalArgumentException();
		}
		if (occurrences > 0) {
			try {
				K key = (K) element;
				Collection<V> values = getDelegatedObject().get(key);
				if (values.isEmpty()) {
					return 0;
				}

				int oldCount = values.size();

				int numberRemoved;
				if (oldCount > occurrences) {
					numberRemoved = occurrences;
				} else {
					numberRemoved = oldCount;
				}

				Iterator<V> valueIterator = values.iterator();
				for (int i = 0; i < numberRemoved && valueIterator.hasNext(); ++i) {
					valueIterator.next();
					valueIterator.remove();
				}

				return oldCount;
			} catch (ClassCastException exception) {
				//
			}
		}
		return count(element);
	}

	@Override
	public int setCount(K element, int count) {
		if (count < 0) {
			throw new IllegalArgumentException();
		}
		int currentCount = count(element);
		return setCountImpl(element, currentCount, count);
	}

	@Override
	public boolean setCount(K element, int oldCount, int newCount) {
		if (oldCount < 0 || newCount < 0) {
			throw new IllegalArgumentException();
		}
		int count = count(element);
		if (count == oldCount) {
			setCountImpl(element, count, newCount);
			return true;
		}
		return false;
	}

	private int setCountImpl(K element, int oldCount, int newCount) {
		int delta = newCount - oldCount;
		if (delta > 0) {
			add(element, delta);
		} else if (delta < 0) {
			remove(element, -delta);
		}
		return oldCount;
	}

	@Override
	public boolean addAll(Collection<? extends K> collection) {
		if (collection.isEmpty()) {
			return false;
		}
		if (collection instanceof Multiset) {
			Multiset<? extends K> that = (Multiset<? extends K>) collection;
			for (Entry<? extends K> entry : that.entrySet()) {
				add(entry.getElement(), entry.getCount());
			}
		} else {
			Iterators.addAll(this, collection.iterator());
		}
		return true;
	}

	@Override
	public Set<K> elementSet() {
		if (this.elementSet == null) {
			this.elementSet = new ElementSet();
		}
		return this.elementSet;
	}

	@Override
	public Set<Entry<K>> entrySet() {
		if (this.entrySet == null) {
			this.entrySet = new EntrySet();
		}
		return this.entrySet;
	}

	@Override
	public Iterator<K> iterator() {
		return new MultisetIterator(entrySet().iterator());
	}

	@Override
	public boolean contains(Object element) {
		return count(element) > 0;
	}

	@Override
	public int add(K element, int occurrences) {
		throw new UnsupportedOperationException();
	}

	@Override
	public boolean add(K element) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Set of entries in a Multiset.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class EntrySet extends AbstractSet<Entry<K>> {

		EntrySet() {
			//
		}

		/**
		 * Replies the associated multiset.
		 *
		 * @return the associated multiset.
		 */
		protected Multiset<K> multiset() {
			return MultisetView.this;
		}

		@Override
		public boolean contains(Object obj) {
			if (obj instanceof Entry) {
				Entry<?> entry = (Entry<?>) obj;
				if (entry.getCount() <= 0) {
					return false;
				}
				int count = multiset().count(entry.getElement());
				return count == entry.getCount();

			}
			return false;
		}

		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public boolean remove(Object object) {
			if (object instanceof Multiset.Entry) {
				Entry<?> entry = (Entry<?>) object;
				Object element = entry.getElement();
				int entryCount = entry.getCount();
				if (entryCount != 0) {
					// Safe as long as we never add a new entry, which we won't.
					Multiset<Object> multiset = (Multiset) multiset();
					return multiset.setCount(element, entryCount, 0);
				}
			}
			return false;
		}

		@Override
		public void clear() {
			multiset().clear();
		}

		@Override
		public Iterator<Entry<K>> iterator() {
			final Iterator<K> backingEntries = getDelegatedObject().keySet().iterator();
			return new Iterator<Entry<K>>() {
				private boolean canRemove;

				@Override
				public boolean hasNext() {
					return backingEntries.hasNext();
				}

				@Override
				public Multiset.Entry<K> next() {
					final K entryKey = backingEntries.next();
					final Collection<V> entryValues = getDelegatedObject().get(entryKey);
					this.canRemove = true;
					return new Entry<K>() {
						@Override
						public boolean equals(Object object) {
							if (object instanceof Multiset.Entry) {
								Multiset.Entry<?> that = (Multiset.Entry<?>) object;
								return this.getCount() == that.getCount() && Objects.equal(getElement(), that.getElement());
							}
							return false;
						}

						@Override
						public int hashCode() {
							K element = getElement();
							return ((element == null) ? 0 : element.hashCode()) ^ getCount();
						}

						@Override
						public String toString() {
							StringBuilder b = new StringBuilder();
							b.append(String.valueOf(getElement()));
							int elementCount = getCount();
							if (elementCount != 1) {
								b.append(" x "); //$NON-NLS-1$
								b.append(elementCount);
							}
							return b.toString();
						}

						@Override
						public K getElement() {
							return entryKey;
						}

						@Override
						public int getCount() {
							Collection<V> values = entryValues;
							if (values == null || values.size() == 0) {
								values = getDelegatedObject().get(getElement());
							}
							return (values == null) ? 0 : values.size();
						}
					};
				}

				@Override
				public void remove() {
					if (!this.canRemove) {
						throw new IllegalStateException();
					}
					backingEntries.remove();
					this.canRemove = false;
				}
			};
		}

		@Override
		public int size() {
			return getDelegatedObject().keySet().size();
		}
	}

	/**
	 * Set of elements in a MultisetView.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class ElementSet extends AbstractSet<K> {

		ElementSet() {
			//
		}

		/**
		 * Replies the associated multiset.
		 *
		 * @return the associated multiset.
		 */
		protected Multiset<K> multiset() {
			return MultisetView.this;
		}

		@Override
		public void clear() {
			multiset().clear();
		}

		@Override
		public boolean contains(Object obj) {
			return multiset().contains(obj);
		}

		@Override
		public boolean containsAll(Collection<?> col) {
			return multiset().containsAll(col);
		}

		@Override
		public boolean isEmpty() {
			return multiset().isEmpty();
		}

		@Override
		public Iterator<K> iterator() {
			return new TransformedIterator(multiset().entrySet().iterator()) {
				@Override
				protected K transform(Entry<K> entry) {
					return entry.getElement();
				}
			};
		}

		@Override
		public boolean remove(Object obj) {
			int count = multiset().count(obj);
			if (count > 0) {
				multiset().remove(obj, count);
				return true;
			}
			return false;
		}

		@Override
		public int size() {
			return multiset().entrySet().size();
		}

	}

	/**
	 * Iterator on Multiset keys.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class MultisetIterator implements Iterator<K> {

		private final Iterator<Entry<K>> entryIterator;

		private Entry<K> currentEntry;

		/**
		 * Count of subsequent elements equal to current element.
		 */
		private int laterCount;

		/**
		 * Count of all elements equal to current element.
		 */
		private int totalCount;

		private boolean canRemove;

		MultisetIterator(Iterator<Entry<K>> entryIterator) {
			this.entryIterator = entryIterator;
		}

		@Override
		public boolean hasNext() {
			return this.laterCount > 0 || this.entryIterator.hasNext();
		}

		@Override
		public K next() {
			if (!hasNext()) {
				throw new NoSuchElementException();
			}
			if (this.laterCount == 0) {
				this.currentEntry = this.entryIterator.next();
				this.laterCount = this.currentEntry.getCount();
				this.totalCount = this.laterCount;
			}
			this.laterCount--;
			this.canRemove = true;
			return this.currentEntry.getElement();
		}

		@Override
		public void remove() {
			if (!this.canRemove) {
				throw new IllegalStateException();
			}
			if (this.totalCount == 1) {
				this.entryIterator.remove();
			} else {
				MultisetView.this.remove(this.currentEntry.getElement());
			}
			this.totalCount--;
			this.canRemove = false;
		}
	}

	/**
	 * Iterator on transformed MultisetView.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private abstract class TransformedIterator implements Iterator<K> {
		final Iterator<? extends Entry<K>> backingIterator;

		/**
		 * @param backingIterator - the iterator.
		 */
		TransformedIterator(Iterator<? extends Entry<K>> backingIterator) {
			this.backingIterator = checkNotNull(backingIterator);
		}

		protected abstract K transform(Entry<K> from);

		@Override
		public final boolean hasNext() {
			return this.backingIterator.hasNext();
		}

		@Override
		public final K next() {
			return transform(this.backingIterator.next());
		}

		@Override
		public final void remove() {
			this.backingIterator.remove();
		}
	}

}
