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

/** Provide static operators for numbers of type {@code Byte}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("checkstyle:methodcount")
public final class ByteArithmeticExtensions {

	private ByteArithmeticExtensions() {
		//
	}

	/** The unary {@code minus} operator. This is the equivalent to
	 * the Java's {@code -} function. This function is not null-safe.
	 *
	 * @param number a number.
	 * @return {@code -number}
	 */
	@Pure
	@Inline(value = "(-($1.byteValue()))", constantExpression = true)
	public static int operator_minus(Byte number) {
		return -number.byteValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2)", constantExpression = true)
	public static long operator_minus(Byte left, long right) {
		return left.byteValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2)", constantExpression = true)
	public static int operator_minus(Byte left, byte right) {
		return left.byteValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2)", constantExpression = true)
	public static int operator_minus(Byte left, int right) {
		return left.byteValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2)", constantExpression = true)
	public static int operator_minus(Byte left, short right) {
		return left.byteValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2)", constantExpression = true)
	public static double operator_minus(Byte left, double right) {
		return left.byteValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2)", constantExpression = true)
	public static float operator_minus(Byte left, float right) {
		return left.byteValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2.doubleValue())", constantExpression = true)
	public static double operator_minus(Byte left, Number right) {
		return left.byteValue() - right.doubleValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2.longValue())", constantExpression = true)
	public static long operator_minus(Byte left, Long right) {
		return left.byteValue() - right.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2.byteValue())", constantExpression = true)
	public static int operator_minus(Byte left, Byte right) {
		return left.byteValue() - right.byteValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2.floatValue())", constantExpression = true)
	public static float operator_minus(Byte left, Float right) {
		return left.byteValue() - right.floatValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2.intValue())", constantExpression = true)
	public static int operator_minus(Byte left, Integer right) {
		return left.byteValue() - right.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2.shortValue())", constantExpression = true)
	public static int operator_minus(Byte left, Short right) {
		return left.byteValue() - right.shortValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2.intValue())", constantExpression = true)
	public static int operator_minus(Byte left, AtomicInteger right) {
		return left.byteValue() - right.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() - $2.longValue())", constantExpression = true)
	public static long operator_minus(Byte left, AtomicLong right) {
		return left.byteValue() - right.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2)", constantExpression = true)
	public static long operator_plus(Byte left, long right) {
		return left.byteValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2)", constantExpression = true)
	public static int operator_plus(Byte left, byte right) {
		return left.byteValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2)", constantExpression = true)
	public static int operator_plus(Byte left, int right) {
		return left.byteValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2)", constantExpression = true)
	public static int operator_plus(Byte left, short right) {
		return left.byteValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2)", constantExpression = true)
	public static double operator_plus(Byte left, double right) {
		return left.byteValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2)", constantExpression = true)
	public static float operator_plus(Byte left, float right) {
		return left.byteValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2.longValue())", constantExpression = true)
	public static long operator_plus(Byte left, Long right) {
		return left.byteValue() + right.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2.byteValue())", constantExpression = true)
	public static int operator_plus(Byte left, Byte right) {
		return left.byteValue() + right.byteValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2.floatValue())", constantExpression = true)
	public static float operator_plus(Byte left, Float right) {
		return left.byteValue() + right.floatValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2.intValue())", constantExpression = true)
	public static int operator_plus(Byte left, Integer right) {
		return left.byteValue() + right.intValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2.shortValue())", constantExpression = true)
	public static int operator_plus(Byte left, Short right) {
		return left.byteValue() + right.shortValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2.intValue())", constantExpression = true)
	public static int operator_plus(Byte left, AtomicInteger right) {
		return left.byteValue() + right.intValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2.longValue())", constantExpression = true)
	public static long operator_plus(Byte left, AtomicLong right) {
		return left.byteValue() + right.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() + $2.doubleValue())", constantExpression = true)
	public static double operator_plus(Byte left, Number right) {
		return left.byteValue() + right.doubleValue();
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.byteValue(), $2)", imported = Math.class)
	public static double operator_power(Byte left, long right) {
		return Math.pow(left.byteValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.byteValue(), $2)", imported = Math.class)
	public static double operator_power(Byte left, byte right) {
		return Math.pow(left.byteValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.byteValue(), $2)", imported = Math.class)
	public static double operator_power(Byte left, int right) {
		return Math.pow(left.byteValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.byteValue(), $2)", imported = Math.class)
	public static double operator_power(Byte left, short right) {
		return Math.pow(left.byteValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.byteValue(), $2)", imported = Math.class)
	public static double operator_power(Byte left, double right) {
		return Math.pow(left.byteValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.byteValue(), $2)", imported = Math.class)
	public static double operator_power(Byte left, float right) {
		return Math.pow(left.byteValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.byteValue(), $2.doubleValue())", imported = Math.class)
	public static double operator_power(Byte left, Number right) {
		return Math.pow(left.byteValue(), right.doubleValue());
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2)", constantExpression = true)
	public static long operator_divide(Byte left, long right) {
		return left.byteValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2)", constantExpression = true)
	public static int operator_divide(Byte left, byte right) {
		return left.byteValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2)", constantExpression = true)
	public static int operator_divide(Byte left, int right) {
		return left.byteValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2)", constantExpression = true)
	public static int operator_divide(Byte left, short right) {
		return left.byteValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2)", constantExpression = true)
	public static double operator_divide(Byte left, double right) {
		return left.byteValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2)", constantExpression = true)
	public static float operator_divide(Byte left, float right) {
		return left.byteValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2.longValue())", constantExpression = true)
	public static long operator_divide(Byte left, Long right) {
		return left.byteValue() / right.longValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2.byteValue())", constantExpression = true)
	public static int operator_divide(Byte left, Byte right) {
		return left.byteValue() / right.byteValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2.floatValue())", constantExpression = true)
	public static float operator_divide(Byte left, Float right) {
		return left.byteValue() / right.floatValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2.intValue())", constantExpression = true)
	public static int operator_divide(Byte left, Integer right) {
		return left.byteValue() / right.intValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2.doubleValue())", constantExpression = true)
	public static double operator_divide(Byte left, Number right) {
		return left.byteValue() / right.doubleValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2.shortValue())", constantExpression = true)
	public static int operator_divide(Byte left, Short right) {
		return left.byteValue() / right.shortValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2.intValue())", constantExpression = true)
	public static int operator_divide(Byte left, AtomicInteger right) {
		return left.byteValue() / right.intValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() / $2.longValue())", constantExpression = true)
	public static long operator_divide(Byte left, AtomicLong right) {
		return left.byteValue() / right.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2)", constantExpression = true)
	public static long operator_multiply(Byte left, long right) {
		return left.byteValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2)", constantExpression = true)
	public static int operator_multiply(Byte left, byte right) {
		return left.byteValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2)", constantExpression = true)
	public static int operator_multiply(Byte left, int right) {
		return left.byteValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2)", constantExpression = true)
	public static int operator_multiply(Byte left, short right) {
		return left.byteValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2)", constantExpression = true)
	public static double operator_multiply(Byte left, double right) {
		return left.byteValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2)", constantExpression = true)
	public static float operator_multiply(Byte left, float right) {
		return left.byteValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2.longValue())", constantExpression = true)
	public static long operator_multiply(Byte left, Long right) {
		return left.byteValue() * right.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2.byteValue())", constantExpression = true)
	public static int operator_multiply(Byte left, Byte right) {
		return left.byteValue() * right.byteValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2.floatValue())", constantExpression = true)
	public static float operator_multiply(Byte left, Float right) {
		return left.byteValue() * right.floatValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2.intValue())", constantExpression = true)
	public static int operator_multiply(Byte left, Integer right) {
		return left.byteValue() * right.intValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2.doubleValue())", constantExpression = true)
	public static double operator_multiply(Byte left, Number right) {
		return left.byteValue() * right.doubleValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2.shortValue())", constantExpression = true)
	public static int operator_multiply(Byte left, Short right) {
		return left.byteValue() * right.shortValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2.intValue())", constantExpression = true)
	public static int operator_multiply(Byte left, AtomicInteger right) {
		return left.byteValue() * right.intValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() * $2.longValue())", constantExpression = true)
	public static long operator_multiply(Byte left, AtomicLong right) {
		return left.byteValue() * right.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2)", constantExpression = true)
	public static long operator_modulo(Byte left, long right) {
		return left.byteValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2)", constantExpression = true)
	public static int operator_modulo(Byte left, byte right) {
		return left.byteValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2)", constantExpression = true)
	public static int operator_modulo(Byte left, int right) {
		return left.byteValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2)", constantExpression = true)
	public static int operator_modulo(Byte left, short right) {
		return left.byteValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2)", constantExpression = true)
	public static double operator_modulo(Byte left, double right) {
		return left.byteValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2)", constantExpression = true)
	public static float operator_modulo(Byte left, float right) {
		return left.byteValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2.longValue())", constantExpression = true)
	public static long operator_modulo(Byte left, Long right) {
		return left.byteValue() % right.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2.byteValue())", constantExpression = true)
	public static int operator_modulo(Byte left, Byte right) {
		return left.byteValue() % right.byteValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2.floatValue())", constantExpression = true)
	public static float operator_modulo(Byte left, Float right) {
		return left.byteValue() % right.floatValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2.intValue())", constantExpression = true)
	public static int operator_modulo(Byte left, Integer right) {
		return left.byteValue() % right.intValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2.doubleValue())", constantExpression = true)
	public static double operator_modulo(Byte left, Number right) {
		return left.byteValue() % right.doubleValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2.shortValue())", constantExpression = true)
	public static int operator_modulo(Byte left, Short right) {
		return left.byteValue() % right.shortValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2.intValue())", constantExpression = true)
	public static int operator_modulo(Byte left, AtomicInteger right) {
		return left.byteValue() % right.intValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() % $2.longValue())", constantExpression = true)
	public static long operator_modulo(Byte left, AtomicLong right) {
		return left.byteValue() % right.longValue();
	}

}
