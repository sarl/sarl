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

/** Provide static operators for numbers of type {@code Integer}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("checkstyle:methodcount")
public final class IntegerArithmeticExtensions {

	private IntegerArithmeticExtensions() {
		//
	}

	/** The unary {@code minus} operator. This is the equivalent to
	 * the Java's {@code -} function. This function is not null-safe.
	 *
	 * @param number a number.
	 * @return {@code -number}
	 */
	@Pure
	@Inline(value = "(-($1.intValue()))", constantExpression = true)
	public static int operator_minus(Integer number) {
		return -number.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2)", constantExpression = true)
	public static long operator_minus(Integer left, long right) {
		return left.intValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2)", constantExpression = true)
	public static int operator_minus(Integer left, byte right) {
		return left.intValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2)", constantExpression = true)
	public static int operator_minus(Integer left, int right) {
		return left.intValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2)", constantExpression = true)
	public static int operator_minus(Integer left, short right) {
		return left.intValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2)", constantExpression = true)
	public static double operator_minus(Integer left, double right) {
		return left.intValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2)", constantExpression = true)
	public static float operator_minus(Integer left, float right) {
		return left.intValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2.doubleValue())", constantExpression = true)
	public static double operator_minus(Integer left, Number right) {
		return left.intValue() - right.doubleValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2.longValue())", constantExpression = true)
	public static long operator_minus(Integer left, Long right) {
		return left.intValue() - right.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2.byteValue())", constantExpression = true)
	public static int operator_minus(Integer left, Byte right) {
		return left.intValue() - right.byteValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2.floatValue())", constantExpression = true)
	public static float operator_minus(Integer left, Float right) {
		return left.intValue() - right.floatValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2.intValue())", constantExpression = true)
	public static int operator_minus(Integer left, Integer right) {
		return left.intValue() - right.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2.shortValue())", constantExpression = true)
	public static int operator_minus(Integer left, Short right) {
		return left.shortValue() - right.shortValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2.intValue())", constantExpression = true)
	public static int operator_minus(Integer left, AtomicInteger right) {
		return left.intValue() - right.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.intValue() - $2.longValue())", constantExpression = true)
	public static long operator_minus(Integer left, AtomicLong right) {
		return left.intValue() - right.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2)", constantExpression = true)
	public static long operator_plus(Integer left, long right) {
		return left.intValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2)", constantExpression = true)
	public static int operator_plus(Integer left, byte right) {
		return left.intValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2)", constantExpression = true)
	public static int operator_plus(Integer left, int right) {
		return left.intValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2)", constantExpression = true)
	public static int operator_plus(Integer left, short right) {
		return left.intValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2)", constantExpression = true)
	public static double operator_plus(Integer left, double right) {
		return left.intValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2)", constantExpression = true)
	public static float operator_plus(Integer left, float right) {
		return left.intValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2.longValue())", constantExpression = true)
	public static long operator_plus(Integer left, Long right) {
		return left.intValue() + right.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2.byteValue())", constantExpression = true)
	public static int operator_plus(Integer left, Byte right) {
		return left.intValue() + right.byteValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2.floatValue())", constantExpression = true)
	public static float operator_plus(Integer left, Float right) {
		return left.intValue() + right.floatValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2.intValue())", constantExpression = true)
	public static int operator_plus(Integer left, Integer right) {
		return left.intValue() + right.intValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2.shortValue())", constantExpression = true)
	public static int operator_plus(Integer left, Short right) {
		return left.shortValue() + right.shortValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2.intValue())", constantExpression = true)
	public static int operator_plus(Integer left, AtomicInteger right) {
		return left.intValue() + right.intValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2.longValue())", constantExpression = true)
	public static long operator_plus(Integer left, AtomicLong right) {
		return left.intValue() + right.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.intValue() + $2.doubleValue())", constantExpression = true)
	public static double operator_plus(Integer left, Number right) {
		return left.intValue() + right.doubleValue();
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.intValue(), $2)", imported = Math.class)
	public static double operator_power(Integer left, long right) {
		return Math.pow(left.intValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.intValue(), $2)", imported = Math.class)
	public static double operator_power(Integer left, byte right) {
		return Math.pow(left.intValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.intValue(), $2)", imported = Math.class)
	public static double operator_power(Integer left, int right) {
		return Math.pow(left.intValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.intValue(), $2)", imported = Math.class)
	public static double operator_power(Integer left, short right) {
		return Math.pow(left.intValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.intValue(), $2)", imported = Math.class)
	public static double operator_power(Integer left, double right) {
		return Math.pow(left.intValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.intValue(), $2)", imported = Math.class)
	public static double operator_power(Integer left, float right) {
		return Math.pow(left.intValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.intValue(), $2.doubleValue())", imported = Math.class)
	public static double operator_power(Integer left, Number right) {
		return Math.pow(left.intValue(), right.doubleValue());
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2)", constantExpression = true)
	public static long operator_divide(Integer left, long right) {
		return left.intValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2)", constantExpression = true)
	public static int operator_divide(Integer left, byte right) {
		return left.intValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2)", constantExpression = true)
	public static int operator_divide(Integer left, int right) {
		return left.intValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2)", constantExpression = true)
	public static int operator_divide(Integer left, short right) {
		return left.intValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2)", constantExpression = true)
	public static double operator_divide(Integer left, double right) {
		return left.intValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2)", constantExpression = true)
	public static float operator_divide(Integer left, float right) {
		return left.intValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2.longValue())", constantExpression = true)
	public static long operator_divide(Integer left, Long right) {
		return left.intValue() / right.longValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2.byteValue())", constantExpression = true)
	public static int operator_divide(Integer left, Byte right) {
		return left.intValue() / right.byteValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2.floatValue())", constantExpression = true)
	public static float operator_divide(Integer left, Float right) {
		return left.intValue() / right.floatValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2.intValue())", constantExpression = true)
	public static int operator_divide(Integer left, Integer right) {
		return left.intValue() / right.intValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2.doubleValue())", constantExpression = true)
	public static double operator_divide(Integer left, Number right) {
		return left.intValue() / right.doubleValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2.shortValue())", constantExpression = true)
	public static int operator_divide(Integer left, Short right) {
		return left.intValue() / right.shortValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2.intValue())", constantExpression = true)
	public static int operator_divide(Integer left, AtomicInteger right) {
		return left.intValue() / right.intValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.intValue() / $2.longValue())", constantExpression = true)
	public static long operator_divide(Integer left, AtomicLong right) {
		return left.intValue() / right.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2)", constantExpression = true)
	public static long operator_multiply(Integer left, long right) {
		return left.intValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2)", constantExpression = true)
	public static int operator_multiply(Integer left, byte right) {
		return left.intValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2)", constantExpression = true)
	public static int operator_multiply(Integer left, int right) {
		return left.intValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2)", constantExpression = true)
	public static int operator_multiply(Integer left, short right) {
		return left.intValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2)", constantExpression = true)
	public static double operator_multiply(Integer left, double right) {
		return left.intValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2)", constantExpression = true)
	public static float operator_multiply(Integer left, float right) {
		return left.intValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2.longValue())", constantExpression = true)
	public static long operator_multiply(Integer left, Long right) {
		return left.intValue() * right.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2.byteValue())", constantExpression = true)
	public static int operator_multiply(Integer left, Byte right) {
		return left.intValue() * right.byteValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2.floatValue())", constantExpression = true)
	public static float operator_multiply(Integer left, Float right) {
		return left.intValue() * right.floatValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2.intValue())", constantExpression = true)
	public static int operator_multiply(Integer left, Integer right) {
		return left.intValue() * right.intValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2.doubleValue())", constantExpression = true)
	public static double operator_multiply(Integer left, Number right) {
		return left.intValue() * right.doubleValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2.shortValue())", constantExpression = true)
	public static int operator_multiply(Integer left, Short right) {
		return left.intValue() * right.shortValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2.intValue())", constantExpression = true)
	public static int operator_multiply(Integer left, AtomicInteger right) {
		return left.intValue() * right.intValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.intValue() * $2.longValue())", constantExpression = true)
	public static long operator_multiply(Integer left, AtomicLong right) {
		return left.intValue() * right.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2)", constantExpression = true)
	public static long operator_modulo(Integer left, long right) {
		return left.intValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2)", constantExpression = true)
	public static int operator_modulo(Integer left, byte right) {
		return left.intValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2)", constantExpression = true)
	public static int operator_modulo(Integer left, int right) {
		return left.intValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2)", constantExpression = true)
	public static int operator_modulo(Integer left, short right) {
		return left.intValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2)", constantExpression = true)
	public static double operator_modulo(Integer left, double right) {
		return left.intValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2)", constantExpression = true)
	public static float operator_modulo(Integer left, float right) {
		return left.intValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2.longValue())", constantExpression = true)
	public static long operator_modulo(Integer left, Long right) {
		return left.intValue() % right.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2.byteValue())", constantExpression = true)
	public static int operator_modulo(Integer left, Byte right) {
		return left.intValue() % right.byteValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2.floatValue())", constantExpression = true)
	public static float operator_modulo(Integer left, Float right) {
		return left.intValue() % right.floatValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2.intValue())", constantExpression = true)
	public static int operator_modulo(Integer left, Integer right) {
		return left.intValue() % right.intValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2.doubleValue())", constantExpression = true)
	public static double operator_modulo(Integer left, Number right) {
		return left.intValue() % right.doubleValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2.shortValue())", constantExpression = true)
	public static int operator_modulo(Integer left, Short right) {
		return left.shortValue() % right.shortValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2.intValue())", constantExpression = true)
	public static int operator_modulo(Integer left, AtomicInteger right) {
		return left.intValue() % right.intValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.intValue() % $2.longValue())", constantExpression = true)
	public static long operator_modulo(Integer left, AtomicLong right) {
		return left.intValue() % right.longValue();
	}

}
