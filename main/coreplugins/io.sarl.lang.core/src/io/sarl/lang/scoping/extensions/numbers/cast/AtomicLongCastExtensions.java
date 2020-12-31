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

/** Provide static functions related to the casting of numbers of type {@code AtomicLong}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
public final class AtomicLongCastExtensions {

	private AtomicLongCastExtensions() {
		//
	}

	/** Convert the given value to {@code Byte}. This function is not null-safe
	 *
	 * @param number a number of {@code AtomicLong} type.
	 * @return the equivalent value to {@code number} of {@code Byte} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.byteValue())", imported = Byte.class)
	public static Byte toByte(AtomicLong number) {
		return Byte.valueOf(number.byteValue());
	}

	/** Convert the given value to {@code Short}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicLong} type.
	 * @return the equivalent value to {@code number} of {@code Short} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.shortValue())", imported = Short.class)
	public static Short toShort(AtomicLong number) {
		return Short.valueOf(number.shortValue());
	}

	/** Convert the given value to {@code int}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicLong} type.
	 * @return the equivalent value to {@code number} of {@code int} type.
	 * @deprecated See {@link #toInteger(AtomicLong)}.
	 */
	@Pure
	@Inline(value = "$1.intValue()")
	@Deprecated
	public static int toInt(AtomicLong number) {
		return number.intValue();
	}

	/** Convert the given value to {@code Integer}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicLong} type.
	 * @return the equivalent value to {@code number} of {@code Integer} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.intValue())", imported = Integer.class)
	public static Integer toInteger(AtomicLong number) {
		return Integer.valueOf(number.intValue());
	}

	/** Convert the given value to {@code AtomicInteger}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicLong} type.
	 * @return the equivalent value to {@code number} of {@code AtomicInteger} type.
	 */
	@Pure
	@Inline(value = "new $2($1.intValue())", imported = AtomicInteger.class)
	public static AtomicInteger toAtomicInteger(AtomicLong number) {
		return new AtomicInteger(number.intValue());
	}

	/** Convert the given value to {@code Long}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicLong} type.
	 * @return the equivalent value to {@code number} of {@code Long} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.longValue())", imported = Long.class)
	public static Long toLong(AtomicLong number) {
		return number.longValue();
	}

	/** Convert the given value to {@code Long}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicLong} type.
	 * @return the equivalent value to {@code number} of {@code Long} type.
	 * @deprecated see {@link #toLong(AtomicLong)}
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.longValue())", imported = Long.class)
	@Deprecated
	public static Long toLongInteger(AtomicLong number) {
		return Long.valueOf(number.longValue());
	}

	/** Convert the given value to {@code Float}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicLong} type.
	 * @return the equivalent value to {@code number} of {@code Float} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.floatValue())", imported = Float.class)
	public static Float toFloat(AtomicLong number) {
		return Float.valueOf(number.floatValue());
	}

	/** Convert the given value to {@code Double}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicLong} type.
	 * @return the equivalent value to {@code number} of {@code Double} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.doubleValue())", imported = Double.class)
	public static Double toDouble(AtomicLong number) {
		return Double.valueOf(number.doubleValue());
	}

	/** Convert the given value to {@code AtomicDouble}. This function is not null-safe.
	 *
	 * @param number a number of {@code AtomicLong} type.
	 * @return the equivalent value to {@code number} of {@code AtomicDouble} type.
	 */
	@Pure
	@Inline(value = "new $2($1.doubleValue())", imported = AtomicDouble.class)
	public static AtomicDouble toAtomicDouble(AtomicLong number) {
		return new AtomicDouble(number.doubleValue());
	}

	/** Convert the given value to {@code BigInteger}.
	 *
	 * @param number a number of {@code AtomicLong} type.
	 * @return the equivalent value to {@code number} of {@code BigInteger} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.longValue())", imported = {BigInteger.class})
	public static BigInteger toBigInteger(AtomicLong number) {
		return BigInteger.valueOf(number.longValue());
	}

	/** Convert the given value to {@code BigDecimal}.
	 *
	 * @param number a number of {@code AtomicLong} type.
	 * @return the equivalent value to {@code number} of {@code BigDecimal} type.
	 */
	@Pure
	@Inline(value = "$2.valueOf($1.doubleValue())", imported = BigDecimal.class)
	public static BigDecimal toBigDecimal(AtomicLong number) {
		return BigDecimal.valueOf(number.doubleValue());
	}

}
