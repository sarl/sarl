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

package io.sarl.lang.scoping.extensions.numbers.arithmetic;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static operators for numbers of type {@code byte}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("checkstyle:methodcount")
public final class PrimitiveByteArithmeticExtensions {

	private PrimitiveByteArithmeticExtensions() {
		//
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1 - $2.doubleValue())", constantExpression = true)
	public static double operator_minus(byte left, Number right) {
		return left - right.doubleValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1 - $2.longValue())", constantExpression = true)
	public static long operator_minus(byte left, Long right) {
		return left - right.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1 - $2.byteValue())", constantExpression = true)
	public static int operator_minus(byte left, Byte right) {
		return left - right.byteValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1 - $2.floatValue())", constantExpression = true)
	public static float operator_minus(byte left, Float right) {
		return left - right.floatValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1 - $2.intValue())", constantExpression = true)
	public static int operator_minus(byte left, Integer right) {
		return left - right.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1 - $2.shortValue())", constantExpression = true)
	public static int operator_minus(byte left, Short right) {
		return left - right.shortValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1 - $2.intValue())", constantExpression = true)
	public static int operator_minus(byte left, AtomicInteger right) {
		return left - right.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1 - $2.longValue())", constantExpression = true)
	public static long operator_minus(byte left, AtomicLong right) {
		return left - right.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1 + $2.longValue())", constantExpression = true)
	public static long operator_plus(byte left, Long right) {
		return left + right.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1 + $2.byteValue())", constantExpression = true)
	public static int operator_plus(byte left, Byte right) {
		return left + right.byteValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1 + $2.floatValue())", constantExpression = true)
	public static float operator_plus(byte left, Float right) {
		return left + right.floatValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1 + $2.intValue())", constantExpression = true)
	public static int operator_plus(byte left, Integer right) {
		return left + right.intValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1 + $2.shortValue())", constantExpression = true)
	public static int operator_plus(byte left, Short right) {
		return left + right.shortValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1 + $2.intValue())", constantExpression = true)
	public static int operator_plus(byte left, AtomicInteger right) {
		return left + right.intValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1 + $2.longValue())", constantExpression = true)
	public static long operator_plus(byte left, AtomicLong right) {
		return left + right.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1 + $2.doubleValue())", constantExpression = true)
	public static double operator_plus(byte left, Number right) {
		return left + right.doubleValue();
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1, $2.doubleValue())", imported = Math.class)
	public static double operator_power(byte left, Number right) {
		return Math.pow(left, right.doubleValue());
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1 / $2.longValue())", constantExpression = true)
	public static long operator_divide(byte left, Long right) {
		return left / right.longValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1 / $2.byteValue())", constantExpression = true)
	public static int operator_divide(byte left, Byte right) {
		return left / right.byteValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1 / $2.floatValue())", constantExpression = true)
	public static float operator_divide(byte left, Float right) {
		return left / right.floatValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1 / $2.intValue())", constantExpression = true)
	public static int operator_divide(byte left, Integer right) {
		return left / right.intValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1 / $2.doubleValue())", constantExpression = true)
	public static double operator_divide(byte left, Number right) {
		return left / right.doubleValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1 / $2.shortValue())", constantExpression = true)
	public static int operator_divide(byte left, Short right) {
		return left / right.shortValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1 / $2.intValue())", constantExpression = true)
	public static int operator_divide(byte left, AtomicInteger right) {
		return left / right.intValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1 / $2.longValue())", constantExpression = true)
	public static long operator_divide(byte left, AtomicLong right) {
		return left / right.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1 * $2.longValue())", constantExpression = true)
	public static long operator_multiply(byte left, Long right) {
		return left * right.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1 * $2.byteValue())", constantExpression = true)
	public static int operator_multiply(byte left, Byte right) {
		return left * right.byteValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1 * $2.floatValue())", constantExpression = true)
	public static float operator_multiply(byte left, Float right) {
		return left * right.floatValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1 * $2.intValue())", constantExpression = true)
	public static int operator_multiply(byte left, Integer right) {
		return left * right.intValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1 * $2.doubleValue())", constantExpression = true)
	public static double operator_multiply(byte left, Number right) {
		return left * right.doubleValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1 * $2.shortValue())", constantExpression = true)
	public static int operator_multiply(byte left, Short right) {
		return left * right.shortValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1 * $2.intValue())", constantExpression = true)
	public static int operator_multiply(byte left, AtomicInteger right) {
		return left * right.intValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1 * $2.longValue())", constantExpression = true)
	public static long operator_multiply(byte left, AtomicLong right) {
		return left * right.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1 % $2.longValue())", constantExpression = true)
	public static long operator_modulo(byte left, Long right) {
		return left % right.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1 % $2.byteValue())", constantExpression = true)
	public static int operator_modulo(byte left, Byte right) {
		return left % right.byteValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1 % $2.floatValue())", constantExpression = true)
	public static float operator_modulo(byte left, Float right) {
		return left % right.floatValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1 % $2.intValue())", constantExpression = true)
	public static int operator_modulo(byte left, Integer right) {
		return left % right.intValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1 % $2.doubleValue())", constantExpression = true)
	public static double operator_modulo(byte left, Number right) {
		return left % right.doubleValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1 % $2.shortValue())", constantExpression = true)
	public static int operator_modulo(byte left, Short right) {
		return left % right.shortValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1 % $2.intValue())", constantExpression = true)
	public static int operator_modulo(byte left, AtomicInteger right) {
		return left % right.intValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1 % $2.longValue())", constantExpression = true)
	public static long operator_modulo(byte left, AtomicLong right) {
		return left % right.longValue();
	}

}
