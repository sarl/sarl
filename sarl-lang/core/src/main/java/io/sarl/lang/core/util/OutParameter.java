/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.core.util;

import java.util.Objects;

/**
 * An out parameter for a function.
 * This class is not thread-safe. See {@link MutableOptional} for a thread-safe
 * implementation.
 *
 * @param <T> the type of the value.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 * @see {@link MutableOptional}
 */
public class OutParameter<T> implements Cloneable {

	private T value;

	/** Construct an out parameter with {@code null} value.
	 */
	public OutParameter() {
		//
	}

	/** Construct an out parameter with the given value.
	 *
	 * @param value the initial value.
	 */
	public OutParameter(T value) {
		this.value = value;
	}

	@SuppressWarnings("unchecked")
	@Override
	public OutParameter<T> clone() {
		try {
			return (OutParameter<T>) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new Error(e);
		}
	}

	@Override
	public boolean equals(Object obj) {
		return Objects.equals(this.value, obj);
	}

	@Override
	public int hashCode() {
		final var val = this.value;
		return val == null ? 0 : val.hashCode();
	}

	@Override
	public String toString() {
		return Objects.toString(this.value);
	}

	/** Replies the value.
	 *
	 * @return the value.
	 */
	public T get() {
		return this.value;
	}

	/** Change the value.
	 *
	 * @param value the value.
	 */
	public void set(T value) {
		this.value = value;
	}

	/** Clear the value.
	 */
	public void clear() {
		this.value = null;
	}

}
