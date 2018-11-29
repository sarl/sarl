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

package io.sarl.lang.scoping.extensions.cast;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static functions related to the casting of primitives that are not numbers.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public final class PrimitiveCastExtensions {

	private PrimitiveCastExtensions() {
		//
	}

	/** Convert the given value to {@code String}.
	 *
	 * @param value a value of {@code boolean} type.
	 * @return the equivalent value to {@code value} of {@code String} type.
	 */
	@Pure
	@Inline(value = "$2.toString($1)", imported = Boolean.class)
	public static String toString(boolean value) {
		return Boolean.toString(value);
	}

	/** Convert the given value to {@code String}.
	 *
	 * @param value a value of {@code char} type.
	 * @return the equivalent value to {@code value} of {@code String} type.
	 */
	@Pure
	@Inline(value = "$2.toString($1)", imported = Character.class)
	public static String toString(char value) {
		return Character.toString(value);
	}

}
