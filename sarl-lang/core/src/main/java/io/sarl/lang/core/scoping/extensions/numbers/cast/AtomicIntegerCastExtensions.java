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

package io.sarl.lang.core.scoping.extensions.numbers.cast;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import com.google.common.util.concurrent.AtomicDouble;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static functions related to the casting of numbers of type {@code AtomicInteger}.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
public final class AtomicIntegerCastExtensions {

	private AtomicIntegerCastExtensions() {
		//
	}

	/** Convert the given value to {@code byte}. This function is not null-safe
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code byte} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.byteValue())", imported = Byte.class)
	public static Byte toByte(AtomicInteger number) {
		return Byte.valueOf(number.byteValue());
	}

	/** Convert the given value to {@code short}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code short} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.shortValue())", imported = Short.class)
	public static Short toShort(AtomicInteger number) {
		return Short.valueOf(number.shortValue());
	}

	/** Convert the given value to {@code Integer}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code Integer} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.intValue())", imported = Integer.class)
	public static Integer toInteger(AtomicInteger number) {
		return Integer.valueOf(number.intValue());
	}

	/** Convert the given value to {@code long}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code long} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.longValue())", imported = Long.class)
	public static Long toLong(AtomicInteger number) {
		return Long.valueOf(number.longValue());
	}

	/** Convert the given value to {@code AtomicLong}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code AtomicLong} type.
	 */
	@Pure
	@Inline(value = "new $2($1.longValue())", imported = AtomicLong.class)
	public static AtomicLong toAtomicLong(AtomicInteger number) {
		return new AtomicLong(number.longValue());
	}

	/** Convert the given value to {@code Float}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code float} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.floatValue())", imported = Float.class)
	public static Float toFloat(AtomicInteger number) {
		return Float.valueOf(number.floatValue());
	}

	/** Convert the given value to {@code Double}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code Double} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.doubleValue())", imported = Double.class)
	public static Double toDouble(AtomicInteger number) {
		return Double.valueOf(number.doubleValue());
	}

	/** Convert the given value to {@code AtomicDouble}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code AtomicDouble} type.
	 */
	@Pure
	@Inline(value = "new $2($1.doubleValue())", imported = AtomicDouble.class)
	public static AtomicDouble toAtomicDouble(AtomicInteger number) {
		return new AtomicDouble(number.doubleValue());
	}

	/** Convert the given value to {@code BigInteger}.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code BigInteger} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.longValue())", imported = {BigInteger.class})
	public static BigInteger toBigInteger(AtomicInteger number) {
		return BigInteger.valueOf(number.longValue());
	}

	/** Convert the given value to {@code BigDecimal}.
	 *
	 * @param number a number of {@code AtomicInteger} type.
	 * @return the equivalent value to {@code number} of {@code BigDecimal} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.doubleValue())", imported = BigDecimal.class)
	public static BigDecimal toBigDecimal(AtomicInteger number) {
		return BigDecimal.valueOf(number.doubleValue());
	}

}
