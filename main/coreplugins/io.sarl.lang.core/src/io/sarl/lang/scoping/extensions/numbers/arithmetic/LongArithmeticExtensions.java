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

/** Provide static operators for numbers of type {@code Long}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("checkstyle:methodcount")
public final class LongArithmeticExtensions {

	private LongArithmeticExtensions() {
		//
	}

	/** The unary {@code minus} operator. This is the equivalent to
	 * the Java's {@code -} function. This function is not null-safe.
	 *
	 * @param number a number.
	 * @return {@code -number}
	 */
	@Pure
	@Inline(value = "(-($1.longValue()))", constantExpression = true)
	public static long operator_minus(Long number) {
		return -number.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2)", constantExpression = true)
	public static long operator_minus(Long left, long right) {
		return left.longValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2)", constantExpression = true)
	public static long operator_minus(Long left, byte right) {
		return left.longValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2)", constantExpression = true)
	public static long operator_minus(Long left, int right) {
		return left.longValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2)", constantExpression = true)
	public static long operator_minus(Long left, short right) {
		return left.longValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2)", constantExpression = true)
	public static double operator_minus(Long left, double right) {
		return left.longValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2)", constantExpression = true)
	public static float operator_minus(Long left, float right) {
		return left.longValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2.doubleValue())", constantExpression = true)
	public static double operator_minus(Long left, Number right) {
		return left.longValue() - right.doubleValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2.longValue())", constantExpression = true)
	public static long operator_minus(Long left, Long right) {
		return left.longValue() - right.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2.byteValue())", constantExpression = true)
	public static long operator_minus(Long left, Byte right) {
		return left.longValue() - right.byteValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2.floatValue())", constantExpression = true)
	public static float operator_minus(Long left, Float right) {
		return left.longValue() - right.floatValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2.intValue())", constantExpression = true)
	public static long operator_minus(Long left, Integer right) {
		return left.longValue() - right.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2.shortValue())", constantExpression = true)
	public static long operator_minus(Long left, Short right) {
		return left.longValue() - right.shortValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2.intValue())", constantExpression = true)
	public static long operator_minus(Long left, AtomicInteger right) {
		return left.longValue() - right.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.longValue() - $2.longValue())", constantExpression = true)
	public static long operator_minus(Long left, AtomicLong right) {
		return left.longValue() - right.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2)", constantExpression = true)
	public static long operator_plus(Long left, long right) {
		return left.longValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2)", constantExpression = true)
	public static long operator_plus(Long left, byte right) {
		return left.longValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2)", constantExpression = true)
	public static long operator_plus(Long left, int right) {
		return left.longValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2)", constantExpression = true)
	public static long operator_plus(Long left, short right) {
		return left.longValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2)", constantExpression = true)
	public static double operator_plus(Long left, double right) {
		return left.longValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2)", constantExpression = true)
	public static float operator_plus(Long left, float right) {
		return left.longValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2.longValue())", constantExpression = true)
	public static long operator_plus(Long left, Long right) {
		return left.longValue() + right.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2.byteValue())", constantExpression = true)
	public static long operator_plus(Long left, Byte right) {
		return left.longValue() + right.byteValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2.floatValue())", constantExpression = true)
	public static float operator_plus(Long left, Float right) {
		return left.longValue() + right.floatValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2.intValue())", constantExpression = true)
	public static long operator_plus(Long left, Integer right) {
		return left.longValue() + right.intValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2.shortValue())", constantExpression = true)
	public static long operator_plus(Long left, Short right) {
		return left.longValue() + right.shortValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2.intValue())", constantExpression = true)
	public static long operator_plus(Long left, AtomicInteger right) {
		return left.longValue() + right.intValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2.longValue())", constantExpression = true)
	public static long operator_plus(Long left, AtomicLong right) {
		return left.longValue() + right.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.longValue() + $2.doubleValue())", constantExpression = true)
	public static double operator_plus(Long left, Number right) {
		return left.longValue() + right.doubleValue();
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.longValue(), $2)", imported = Math.class)
	public static double operator_power(Long left, long right) {
		return Math.pow(left.longValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.longValue(), $2)", imported = Math.class)
	public static double operator_power(Long left, byte right) {
		return Math.pow(left.longValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.longValue(), $2)", imported = Math.class)
	public static double operator_power(Long left, int right) {
		return Math.pow(left.longValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.longValue(), $2)", imported = Math.class)
	public static double operator_power(Long left, short right) {
		return Math.pow(left.longValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.longValue(), $2)", imported = Math.class)
	public static double operator_power(Long left, double right) {
		return Math.pow(left.longValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.longValue(), $2)", imported = Math.class)
	public static double operator_power(Long left, float right) {
		return Math.pow(left.longValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.longValue(), $2.doubleValue())", imported = Math.class)
	public static double operator_power(Long left, Number right) {
		return Math.pow(left.longValue(), right.doubleValue());
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2)", constantExpression = true)
	public static long operator_divide(Long left, long right) {
		return left.longValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2)", constantExpression = true)
	public static long operator_divide(Long left, byte right) {
		return left.longValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2)", constantExpression = true)
	public static long operator_divide(Long left, int right) {
		return left.longValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2)", constantExpression = true)
	public static long operator_divide(Long left, short right) {
		return left.longValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2)", constantExpression = true)
	public static double operator_divide(Long left, double right) {
		return left.longValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2)", constantExpression = true)
	public static float operator_divide(Long left, float right) {
		return left.longValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2.longValue())", constantExpression = true)
	public static long operator_divide(Long left, Long right) {
		return left.longValue() / right.longValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2.byteValue())", constantExpression = true)
	public static long operator_divide(Long left, Byte right) {
		return left.longValue() / right.byteValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2.floatValue())", constantExpression = true)
	public static float operator_divide(Long left, Float right) {
		return left.longValue() / right.floatValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2.intValue())", constantExpression = true)
	public static long operator_divide(Long left, Integer right) {
		return left.longValue() / right.intValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2.doubleValue())", constantExpression = true)
	public static double operator_divide(Long left, Number right) {
		return left.longValue() / right.doubleValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2.shortValue())", constantExpression = true)
	public static long operator_divide(Long left, Short right) {
		return left.longValue() / right.shortValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2.intValue())", constantExpression = true)
	public static long operator_divide(Long left, AtomicInteger right) {
		return left.longValue() / right.intValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.longValue() / $2.longValue())", constantExpression = true)
	public static long operator_divide(Long left, AtomicLong right) {
		return left.longValue() / right.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2)", constantExpression = true)
	public static long operator_multiply(Long left, long right) {
		return left.longValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2)", constantExpression = true)
	public static long operator_multiply(Long left, byte right) {
		return left.longValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2)", constantExpression = true)
	public static long operator_multiply(Long left, int right) {
		return left.longValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2)", constantExpression = true)
	public static long operator_multiply(Long left, short right) {
		return left.longValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2)", constantExpression = true)
	public static double operator_multiply(Long left, double right) {
		return left.longValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2)", constantExpression = true)
	public static float operator_multiply(Long left, float right) {
		return left.longValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2.longValue())", constantExpression = true)
	public static long operator_multiply(Long left, Long right) {
		return left.longValue() * right.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2.byteValue())", constantExpression = true)
	public static long operator_multiply(Long left, Byte right) {
		return left.longValue() * right.byteValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2.floatValue())", constantExpression = true)
	public static float operator_multiply(Long left, Float right) {
		return left.longValue() * right.floatValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2.intValue())", constantExpression = true)
	public static long operator_multiply(Long left, Integer right) {
		return left.longValue() * right.intValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2.doubleValue())", constantExpression = true)
	public static double operator_multiply(Long left, Number right) {
		return left.longValue() * right.doubleValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2.shortValue())", constantExpression = true)
	public static long operator_multiply(Long left, Short right) {
		return left.longValue() * right.shortValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2.intValue())", constantExpression = true)
	public static long operator_multiply(Long left, AtomicInteger right) {
		return left.longValue() * right.intValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.longValue() * $2.longValue())", constantExpression = true)
	public static long operator_multiply(Long left, AtomicLong right) {
		return left.longValue() * right.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2)", constantExpression = true)
	public static long operator_modulo(Long left, long right) {
		return left.longValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2)", constantExpression = true)
	public static long operator_modulo(Long left, byte right) {
		return left.longValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2)", constantExpression = true)
	public static long operator_modulo(Long left, int right) {
		return left.longValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2)", constantExpression = true)
	public static long operator_modulo(Long left, short right) {
		return left.longValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2)", constantExpression = true)
	public static double operator_modulo(Long left, double right) {
		return left.longValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2)", constantExpression = true)
	public static float operator_modulo(Long left, float right) {
		return left.longValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2.longValue())", constantExpression = true)
	public static long operator_modulo(Long left, Long right) {
		return left.longValue() % right.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2.byteValue())", constantExpression = true)
	public static long operator_modulo(Long left, Byte right) {
		return left.longValue() % right.byteValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2.floatValue())", constantExpression = true)
	public static float operator_modulo(Long left, Float right) {
		return left.longValue() % right.floatValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2.intValue())", constantExpression = true)
	public static long operator_modulo(Long left, Integer right) {
		return left.longValue() % right.intValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2.doubleValue())", constantExpression = true)
	public static double operator_modulo(Long left, Number right) {
		return left.longValue() % right.doubleValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2.shortValue())", constantExpression = true)
	public static long operator_modulo(Long left, Short right) {
		return left.longValue() % right.shortValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2.intValue())", constantExpression = true)
	public static long operator_modulo(Long left, AtomicInteger right) {
		return left.longValue() % right.intValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.longValue() % $2.longValue())", constantExpression = true)
	public static long operator_modulo(Long left, AtomicLong right) {
		return left.longValue() % right.longValue();
	}

}
