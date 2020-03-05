/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2020 the original authors or authors.
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

package io.sarl.lang.util;

import java.io.Serializable;

/** A reference to an object that could be clear dynamically.
 *
 * <p>This class is thread-safe.
 *
 * <p>This type does not extend the {@code java.lang.reflect.Reference} because of
 * the private constructor of this later.
 *
 * <p>This type does not extend the {@code java.util.concurrent.atomic.AtomicReference} because
 * we don't want to exhibit several of its public functions.
 *
 * @param <T> the type of the referenced object.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class AtomicClearableReference<T> implements Serializable, Cloneable {

	private static final long serialVersionUID = -2985132547428365532L;

	private volatile T reference;

	/** Constructor.
	 *
	 * @param object the object to reference to.
	 */
	public AtomicClearableReference(T object) {
		this.reference = object;
	}

	@SuppressWarnings("unchecked")
	@Override
	public AtomicClearableReference<T> clone() {
		try {
			return (AtomicClearableReference<T>) super.clone();
		} catch (CloneNotSupportedException exception) {
			throw new Error(exception);
		}
	}

	/** Returns this reference object's referent.
	 *
	 * @return the object to which this reference refers, or
	 *           {@code null} if this reference object has been cleared.
	 */
	public T get() {
		return this.reference;
	}

	/**
	 * Clears this reference object.
	 *
	 * @return the old reference.
	 */
	public T clear() {
		final T ref = this.reference;
		this.reference = null;
		return ref;
	}

	@Override
	public String toString() {
		final T ref = this.reference;
		if (ref != null) {
			return ref.toString();
		}
		return "null"; //$NON-NLS-1$
	}

}
