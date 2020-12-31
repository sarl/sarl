/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import com.google.common.util.concurrent.AtomicDouble;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static functions related to the casting of numbers of type {@code long}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
public final class PrimitiveLongCastExtensions {

	private PrimitiveLongCastExtensions() {
		//
	}

	/** Convert the given value to {@code String}.
	 *
	 * @param number a number of {@code long} type.
	 * @return the equivalent value to {@code number} of {@code String} type.
	 */
	@Pure
	@Inline(value = "$2.toString($1)", imported = Long.class)
	public static String toString(long number) {
		return Long.toString(number);
	}

	/** Convert the given value to {@code AtomicLong}.
	 *
	 * @param number a number of {@code long} type.
	 * @return the equivalent value to {@code number} of {@code AtomicLong} type.
	 */
	@Pure
	@Inline(value = "new $2($1)", imported = AtomicLong.class)
	public static AtomicLong toAtomicLong(long number) {
		return new AtomicLong(number);
	}

	/** Convert the given value to {@code AtomicInteger}.
	 *
	 * @param number a number of {@code long} type.
	 * @return the equivalent value to {@code number} of {@code AtomicInteger} type.
	 */
	@Pure
	@Inline(value = "new $2(($3) $1)", imported = {AtomicInteger.class, int.class})
	public static AtomicInteger toAtomicInteger(long number) {
		return new AtomicInteger((int) number);
	}

	/** Convert the given value to {@code AtomicDouble}.
	 *
	 * @param number a number of {@code long} type.
	 * @return the equivalent value to {@code number} of {@code AtomicDouble} type.
	 */
	@Pure
	@Inline(value = "new $2($1)", imported = AtomicDouble.class)
	public static AtomicDouble toAtomicDouble(long number) {
		return new AtomicDouble(number);
	}

	/** Convert the given value to {@code BigInteger}.
	 *
	 * @param number a number of {@code long} type.
	 * @return the equivalent value to {@code number} of {@code BigInteger} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1)", imported = {BigInteger.class})
	public static BigInteger toBigInteger(long number) {
		return BigInteger.valueOf(number);
	}

	/** Convert the given value to {@code BigDecimal}.
	 *
	 * @param number a number of {@code long} type.
	 * @return the equivalent value to {@code number} of {@code BigDecimal} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1)", imported = BigDecimal.class)
	public static BigDecimal toBigDecimal(long number) {
		return BigDecimal.valueOf(number);
	}

	/** Convert the given value to {@code Byte}.
	 *
	 * @param number a number of {@code long} type.
	 * @return the equivalent value to {@code number} of {@code Byte} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf(($3) $1)", imported = {Byte.class, byte.class})
	public static Byte toByte(long number) {
		return Byte.valueOf((byte) number);
	}

	/** Convert the given value to {@code Integer}.
	 *
	 * @param number a number of {@code long} type.
	 * @return the equivalent value to {@code number} of {@code Integer} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf(($3) $1)", imported = {Short.class, short.class})
	public static Short toShort(long number) {
		return Short.valueOf((short) number);
	}

	/** Convert the given value to {@code Integer}.
	 *
	 * @param number a number of {@code long} type.
	 * @return the equivalent value to {@code number} of {@code Integer} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf(($3) $1)", imported = {Integer.class, int.class})
	public static Integer toInteger(long number) {
		return Integer.valueOf((int) number);
	}

	/** Convert the given value to {@code Float}.
	 *
	 * @param number a number of {@code long} type.
	 * @return the equivalent value to {@code number} of {@code Float} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1)", imported = Float.class)
	public static Float toFloat(long number) {
		return Float.valueOf(number);
	}

	/** Convert the given value to {@code Double}.
	 *
	 * @param number a number of {@code long} type.
	 * @return the equivalent value to {@code number} of {@code Double} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1)", imported = Double.class)
	public static Double toDouble(long number) {
		return Double.valueOf(number);
	}

}
