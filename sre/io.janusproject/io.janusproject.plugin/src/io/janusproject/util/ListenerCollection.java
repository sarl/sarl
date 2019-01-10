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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.EventListener;

/**
 * A collection of listeners.
 *
 * <p>This collection is thread-safe.
 *
 * <p>This class is inspirated by <code>EventListenerList</code>.
 *
 * <p>This class is copied from the <a href="http://www.arakhne.org/afc">Arakhn&ecirc; Foundation Classes</a>.
 *
 * @param <L> is the type of listeners.
 * @author $Author: galland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ListenerCollection<L extends EventListener> {

	private static final Object[] NULL = new Object[0];

	/**
	 * Listeners.
	 */
	protected transient Object[] listeners = NULL;

	/**
	 * Construct.
	 */
	public ListenerCollection() {
		//
	}

	/**
	 * Replies if this collection is empty.
	 *
	 * @return <code>true</code> if this collection does not contains any listener, otherwise <code>false</code>
	 */
	public boolean isEmpty() {
		return this.listeners == NULL;
	}

	/**
	 * Clear this collection.
	 */
	public void clear() {
		this.listeners = NULL;
	}

	/**
	 * Passes back the event listener list as an array of ListenerType-listener pairs. Note that for performance reasons, this
	 * implementation passes back the actual data structure in which the listener data is stored internally! This method is
	 * guaranteed to pass back a non-null array, so that no null-checking is required in fire methods. A zero-length array of
	 * Object should be returned if there are currently no listeners.
	 *
	 * <p><b>WARNING!!!</b> Absolutely NO modification of the data contained in this array should be made -- if any such manipulation is
	 * necessary, it should be done on a copy of the array returned rather than the array itself.
	 *
	 * @return the listeners.
	 */
	public Object[] getListenerList() {
		return this.listeners;
	}

	/**
	 * Return an array of all the listeners of the given type.
	 *
	 * @param <T> is the type of expected listeners.
	 * @param type is the type of expected listeners.
	 * @return all of the listeners of the specified type.
	 */
	@SuppressWarnings("unchecked")
	public <T extends EventListener> T[] getListeners(Class<T> type) {
		final Object[] iListeners = this.listeners;
		final int listenerCount = getListenerCount(iListeners, type);
		final T[] result = (T[]) Array.newInstance(type, listenerCount);
		int j = 0;
		for (int i = iListeners.length - 2; i >= 0; i -= 2) {
			if (iListeners[i] == type) {
				result[j++] = type.cast(iListeners[i + 1]);
			}
		}
		return result;
	}

	/**
	 * Returns the total number of listeners for this listener list.
	 *
	 * @return the total number of listeners for this listener list.
	 */
	public int size() {
		return this.listeners.length / 2;
	}

	/**
	 * Returns the total number of listeners of the supplied type for this listener list.
	 *
	 * @param type type of the listeners to consider.
	 * @return the total number of listeners of the supplied type for this listener list.
	 */
	public synchronized int getListenerCount(Class<?> type) {
		return getListenerCount(this.listeners, type);
	}

	private static int getListenerCount(Object[] list, Class<?> type) {
		int count = 0;
		for (int i = 0; i < list.length; i += 2) {
			if (type == (Class<?>) list[i]) {
				++count;
			}
		}
		return count;
	}

	/**
	 * Adds the listener as a listener of the specified type.
	 *
	 * @param <T> the type of the listener to be added
	 * @param type the type of the listener to be added
	 * @param listener the listener to be added
	 */
	public <T extends EventListener> void add(Class<T> type, T listener) {
		assert listener != null;
		synchronized (this) {
			Object[] ilisteners = this.listeners;
			if (ilisteners == NULL) {
				// if this is the first listener added,
				// initialize the lists
				ilisteners = new Object[] {type, listener};
			} else {
				// Otherwise copy the array and add the new listener
				final int i = ilisteners.length;
				final Object[] tmp = new Object[i + 2];
				System.arraycopy(ilisteners, 0, tmp, 0, i);

				tmp[i] = type;
				tmp[i + 1] = listener;

				ilisteners = tmp;
			}
			this.listeners = ilisteners;
		}
	}

	/**
	 * Removes the listener as a listener of the specified type.
	 *
	 * @param <T> the type of the listener to be removed
	 * @param type the type of the listener to be removed
	 * @param listener the listener to be removed
	 */
	public <T extends EventListener> void remove(Class<T> type, T listener) {
		assert listener != null;
		synchronized (this) {
			Object[] ilisteners = this.listeners;

			// Is l on the list?
			int index = -1;
			for (int i = ilisteners.length - 2; i >= 0; i -= 2) {
				if ((ilisteners[i] == type) && (ilisteners[i + 1].equals(listener))) {
					index = i;
					break;
				}
			}

			// If so, remove it
			if (index != -1) {
				final Object[] tmp = new Object[ilisteners.length - 2];
				// Copy the list up to index
				System.arraycopy(ilisteners, 0, tmp, 0, index);
				// Copy from two past the index, up to
				// the end of tmp (which is two elements
				// shorter than the old list)
				if (index < tmp.length) {
					System.arraycopy(ilisteners, index + 2, tmp, index, tmp.length - index);
				}
				// set the listener array to the new array or null
				ilisteners = (tmp.length == 0) ? NULL : tmp;
			}

			this.listeners = ilisteners;
		}
	}

	// Serialization support.
	private void writeObject(ObjectOutputStream stream) throws IOException {
		final Object[] lList = this.listeners;
		stream.defaultWriteObject();

		// Save the non-null event listeners:
		for (int i = 0; i < lList.length; i += 2) {
			final Class<?> t = (Class<?>) lList[i];
			final EventListener listener = (EventListener) lList[i + 1];
			if ((listener != null) && (listener instanceof Serializable)) {
				stream.writeObject(t.getName());
				stream.writeObject(listener);
			}
		}

		stream.writeObject(null);
	}

	@SuppressWarnings("unchecked")
	private void readObject(ObjectInputStream stream) throws IOException, ClassNotFoundException {
		this.listeners = NULL;
		stream.defaultReadObject();
		Object listenerTypeOrNull;

		while (null != (listenerTypeOrNull = stream.readObject())) {
			final ClassLoader cl = Thread.currentThread().getContextClassLoader();
			final EventListener listener = (EventListener) stream.readObject();
			add((Class<EventListener>) Class.forName((String) listenerTypeOrNull, true, cl), listener);
		}
	}

	@Override
	public String toString() {
		final Object[] lList = this.listeners;
		String txt = "EventListenerList: "; //$NON-NLS-1$
		txt += lList.length / 2 + " listeners: "; //$NON-NLS-1$
		for (int i = 0; i <= lList.length - 2; i += 2) {
			txt += " type " + ((Class<?>) lList[i]).getName(); //$NON-NLS-1$
			txt += " listener " + lList[i + 1]; //$NON-NLS-1$
		}
		return txt;
	}

}
