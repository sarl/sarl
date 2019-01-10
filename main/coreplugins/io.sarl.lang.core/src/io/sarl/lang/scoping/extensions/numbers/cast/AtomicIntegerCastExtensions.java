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

package io.sarl.lang.scoping.extensions.numbers.cast;

import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static functions related to the casting of numbers of type {@code AtomicInteger}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
public final class AtomicIntegerCastExtensions {

	private AtomicIntegerCastExtensions() {
		//
	}

	/** Convert the given value to {@code float}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code float} type.
	 */
	@Pure
	@Inline(value = "$1.floatValue()")
	public static float toFloat(AtomicInteger number) {
		return number.floatValue();
	}

	/** Convert the given value to {@code byte}. This function is not null-safe
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code byte} type.
	 */
	@Pure
	@Inline(value = "$1.byteValue()")
	public static byte toByte(AtomicInteger number) {
		return number.byteValue();
	}

	/** Convert the given value to {@code long}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code long} type.
	 */
	@Pure
	@Inline(value = "$1.longValue()")
	public static long toLong(AtomicInteger number) {
		return number.longValue();
	}

	/** Convert the given value to {@code double}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code double} type.
	 */
	@Pure
	@Inline(value = "$1.doubleValue()")
	public static double toDouble(AtomicInteger number) {
		return number.doubleValue();
	}

	/** Convert the given value to {@code short}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code short} type.
	 */
	@Pure
	@Inline(value = "$1.shortValue()")
	public static short toShort(AtomicInteger number) {
		return number.shortValue();
	}

	/** Convert the given value to {@code int}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code int} type.
	 */
	@Pure
	@Inline(value = "$1.intValue()")
	public static int toInt(AtomicInteger number) {
		return number.intValue();
	}

	/** Convert the given value to {@code Integer}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code Integer} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.intValue())", imported = {Integer.class})
	public static Integer toInteger(AtomicInteger number) {
		return Integer.valueOf(number.intValue());
	}

}
