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

package io.sarl.lang.util;

/** A reference to an object that could be clear dynamically.
 *
 * <p>This type does not extends the {@code java.lang.reflect.Reference} because of
 * the private constructor of this later.
 *
 * @param <T> the type of the referenced object.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ClearableReference<T> {

	private T reference;

	/** Constructor.
	 *
	 * @param object the object to reference to.
	 */
	public ClearableReference(T object) {
		this.reference = object;
	}

	/** Returns this reference object's referent.
	 *
	 * @return the object to which this reference refers, or
	 *           <code>null</code> if this reference object has been cleared.
	 */
	public T get() {
		return this.reference;
	}

	/**
	 * Clears this reference object.
	 *
	 * @return the cleared reference.
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
