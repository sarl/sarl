/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

// THIS FILE IS AUTO-GENERATED. DO NOT CHANGE MANUALLY

package io.sarl.lang.scoping.numbers;

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
@SuppressWarnings("all")
public final class LongOperatorExtensions {

	private LongOperatorExtensions() {
		//
	}

	// BEGIN GENERATED BLOCK

	/** The unary {@code minus} operator. This is the equivalent to
	 * the Java's {@code -} function.
	 *
	 * @param i a number.
	 * @return {@code -i}
	 */
	@Pure
	@Inline(value = "(-($1).longValue())", constantExpression=true)
	public static long operator_minus(Long i) {
		return -i.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() + $2)", constantExpression=true)
	public static long operator_plus(Long a, long b) {
		return a.longValue() + b;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() + $2)", constantExpression=true)
	public static long operator_plus(Long a, byte b) {
		return a.longValue() + b;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() + $2)", constantExpression=true)
	public static long operator_plus(Long a, int b) {
		return a.longValue() + b;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() + $2)", constantExpression=true)
	public static long operator_plus(Long a, short b) {
		return a.longValue() + b;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() + $2)", constantExpression=true)
	public static double operator_plus(Long a, double b) {
		return a.doubleValue() + b;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() + $2)", constantExpression=true)
	public static float operator_plus(Long a, float b) {
		return a.floatValue() + b;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() + ($2).longValue())", constantExpression=true)
	public static long operator_plus(Long a, Long b) {
		return a.longValue() + b.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() + ($2).longValue())", constantExpression=true)
	public static long operator_plus(Long a, Byte b) {
		return a.longValue() + b.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() + ($2).floatValue())", constantExpression=true)
	public static float operator_plus(Long a, Float b) {
		return a.floatValue() + b.floatValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() + ($2).longValue())", constantExpression=true)
	public static long operator_plus(Long a, Integer b) {
		return a.longValue() + b.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(Long a, Double b) {
		return a.doubleValue() + b.doubleValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() + ($2).longValue())", constantExpression=true)
	public static long operator_plus(Long a, Short b) {
		return a.longValue() + b.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() + ($2).longValue())", constantExpression=true)
	public static long operator_plus(Long a, AtomicInteger b) {
		return a.longValue() + b.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() + ($2).longValue())", constantExpression=true)
	public static long operator_plus(Long a, AtomicLong b) {
		return a.longValue() + b.longValue();
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), $2)", imported = Math.class)
	public static double operator_power(Long a, long b) {
		return Math.pow(a.longValue(), b);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), $2)", imported = Math.class)
	public static double operator_power(Long a, byte b) {
		return Math.pow(a.longValue(), b);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), $2)", imported = Math.class)
	public static double operator_power(Long a, int b) {
		return Math.pow(a.longValue(), b);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), $2)", imported = Math.class)
	public static double operator_power(Long a, short b) {
		return Math.pow(a.longValue(), b);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).doubleValue(), $2)", imported = Math.class)
	public static double operator_power(Long a, double b) {
		return Math.pow(a.doubleValue(), b);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).floatValue(), $2)", imported = Math.class)
	public static double operator_power(Long a, float b) {
		return Math.pow(a.floatValue(), b);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), ($2).longValue())", imported = Math.class)
	public static double operator_power(Long a, Long b) {
		return Math.pow(a.longValue(), b.longValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), ($2).longValue())", imported = Math.class)
	public static double operator_power(Long a, Byte b) {
		return Math.pow(a.longValue(), b.longValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).floatValue(), ($2).floatValue())", imported = Math.class)
	public static double operator_power(Long a, Float b) {
		return Math.pow(a.floatValue(), b.floatValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), ($2).longValue())", imported = Math.class)
	public static double operator_power(Long a, Integer b) {
		return Math.pow(a.longValue(), b.longValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).doubleValue(), ($2).doubleValue())", imported = Math.class)
	public static double operator_power(Long a, Double b) {
		return Math.pow(a.doubleValue(), b.doubleValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), ($2).longValue())", imported = Math.class)
	public static double operator_power(Long a, Short b) {
		return Math.pow(a.longValue(), b.longValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), ($2).longValue())", imported = Math.class)
	public static double operator_power(Long a, AtomicInteger b) {
		return Math.pow(a.longValue(), b.longValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), ($2).longValue())", imported = Math.class)
	public static double operator_power(Long a, AtomicLong b) {
		return Math.pow(a.longValue(), b.longValue());
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, long b) {
		return a.longValue() >= b;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, byte b) {
		return a.longValue() >= b;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, int b) {
		return a.longValue() >= b;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, short b) {
		return a.longValue() >= b;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, double b) {
		return a.doubleValue() >= b;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, float b) {
		return a.floatValue() >= b;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, Long b) {
		return a.longValue() >= b.longValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, Byte b) {
		return a.longValue() >= b.longValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() >= ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, Float b) {
		return a.floatValue() >= b.floatValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, Integer b) {
		return a.longValue() >= b.longValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() >= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, Double b) {
		return a.doubleValue() >= b.doubleValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, Short b) {
		return a.longValue() >= b.longValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, AtomicInteger b) {
		return a.longValue() >= b.longValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Long a, AtomicLong b) {
		return a.longValue() >= b.longValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(Long a, long b) {
		return a.longValue() != b;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(Long a, byte b) {
		return a.longValue() != b;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(Long a, int b) {
		return a.longValue() != b;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(Long a, short b) {
		return a.longValue() != b;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(Long a, double b) {
		return a.doubleValue() != b;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(Long a, float b) {
		return a.floatValue() != b;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(Long a, Long b) {
		return a.longValue() != b.longValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(Long a, Byte b) {
		return a.longValue() != b.longValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() != ($2).floatValue())", constantExpression=true)
	public static boolean operator_notEquals(Long a, Float b) {
		return a.floatValue() != b.floatValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(Long a, Integer b) {
		return a.longValue() != b.longValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() != ($2).doubleValue())", constantExpression=true)
	public static boolean operator_notEquals(Long a, Double b) {
		return a.doubleValue() != b.doubleValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(Long a, Short b) {
		return a.longValue() != b.longValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(Long a, AtomicInteger b) {
		return a.longValue() != b.longValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(Long a, AtomicLong b) {
		return a.longValue() != b.longValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() / $2)", constantExpression=true)
	public static long operator_divide(Long a, long b) {
		return a.longValue() / b;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() / $2)", constantExpression=true)
	public static long operator_divide(Long a, byte b) {
		return a.longValue() / b;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() / $2)", constantExpression=true)
	public static long operator_divide(Long a, int b) {
		return a.longValue() / b;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() / $2)", constantExpression=true)
	public static long operator_divide(Long a, short b) {
		return a.longValue() / b;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() / $2)", constantExpression=true)
	public static double operator_divide(Long a, double b) {
		return a.doubleValue() / b;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() / $2)", constantExpression=true)
	public static float operator_divide(Long a, float b) {
		return a.floatValue() / b;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() / ($2).longValue())", constantExpression=true)
	public static long operator_divide(Long a, Long b) {
		return a.longValue() / b.longValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() / ($2).longValue())", constantExpression=true)
	public static long operator_divide(Long a, Byte b) {
		return a.longValue() / b.longValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() / ($2).floatValue())", constantExpression=true)
	public static float operator_divide(Long a, Float b) {
		return a.floatValue() / b.floatValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() / ($2).longValue())", constantExpression=true)
	public static long operator_divide(Long a, Integer b) {
		return a.longValue() / b.longValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() / ($2).doubleValue())", constantExpression=true)
	public static double operator_divide(Long a, Double b) {
		return a.doubleValue() / b.doubleValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() / ($2).longValue())", constantExpression=true)
	public static long operator_divide(Long a, Short b) {
		return a.longValue() / b.longValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() / ($2).longValue())", constantExpression=true)
	public static long operator_divide(Long a, AtomicInteger b) {
		return a.longValue() / b.longValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() / ($2).longValue())", constantExpression=true)
	public static long operator_divide(Long a, AtomicLong b) {
		return a.longValue() / b.longValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, long b) {
		return a.longValue() <= b;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, byte b) {
		return a.longValue() <= b;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, int b) {
		return a.longValue() <= b;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, short b) {
		return a.longValue() <= b;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, double b) {
		return a.doubleValue() <= b;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, float b) {
		return a.floatValue() <= b;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, Long b) {
		return a.longValue() <= b.longValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, Byte b) {
		return a.longValue() <= b.longValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() <= ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, Float b) {
		return a.floatValue() <= b.floatValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, Integer b) {
		return a.longValue() <= b.longValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() <= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, Double b) {
		return a.doubleValue() <= b.doubleValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, Short b) {
		return a.longValue() <= b.longValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, AtomicInteger b) {
		return a.longValue() <= b.longValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(Long a, AtomicLong b) {
		return a.longValue() <= b.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() * $2)", constantExpression=true)
	public static long operator_multiply(Long a, long b) {
		return a.longValue() * b;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() * $2)", constantExpression=true)
	public static long operator_multiply(Long a, byte b) {
		return a.longValue() * b;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() * $2)", constantExpression=true)
	public static long operator_multiply(Long a, int b) {
		return a.longValue() * b;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() * $2)", constantExpression=true)
	public static long operator_multiply(Long a, short b) {
		return a.longValue() * b;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() * $2)", constantExpression=true)
	public static double operator_multiply(Long a, double b) {
		return a.doubleValue() * b;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() * $2)", constantExpression=true)
	public static float operator_multiply(Long a, float b) {
		return a.floatValue() * b;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(Long a, Long b) {
		return a.longValue() * b.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(Long a, Byte b) {
		return a.longValue() * b.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() * ($2).floatValue())", constantExpression=true)
	public static float operator_multiply(Long a, Float b) {
		return a.floatValue() * b.floatValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(Long a, Integer b) {
		return a.longValue() * b.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() * ($2).doubleValue())", constantExpression=true)
	public static double operator_multiply(Long a, Double b) {
		return a.doubleValue() * b.doubleValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(Long a, Short b) {
		return a.longValue() * b.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(Long a, AtomicInteger b) {
		return a.longValue() * b.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(Long a, AtomicLong b) {
		return a.longValue() * b.longValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() == $2)", constantExpression=true)
	public static boolean operator_equals(Long a, long b) {
		return a.longValue() == b;
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() == $2)", constantExpression=true)
	public static boolean operator_equals(Long a, byte b) {
		return a.longValue() == b;
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() == $2)", constantExpression=true)
	public static boolean operator_equals(Long a, int b) {
		return a.longValue() == b;
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() == $2)", constantExpression=true)
	public static boolean operator_equals(Long a, short b) {
		return a.longValue() == b;
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() == $2)", constantExpression=true)
	public static boolean operator_equals(Long a, double b) {
		return a.doubleValue() == b;
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() == $2)", constantExpression=true)
	public static boolean operator_equals(Long a, float b) {
		return a.floatValue() == b;
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(Long a, Long b) {
		return a.longValue() == b.longValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(Long a, Byte b) {
		return a.longValue() == b.longValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() == ($2).floatValue())", constantExpression=true)
	public static boolean operator_equals(Long a, Float b) {
		return a.floatValue() == b.floatValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(Long a, Integer b) {
		return a.longValue() == b.longValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() == ($2).doubleValue())", constantExpression=true)
	public static boolean operator_equals(Long a, Double b) {
		return a.doubleValue() == b.doubleValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(Long a, Short b) {
		return a.longValue() == b.longValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(Long a, AtomicInteger b) {
		return a.longValue() == b.longValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(Long a, AtomicLong b) {
		return a.longValue() == b.longValue();
	}

	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(Long a, long b) {
		return a.longValue() > b;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(Long a, byte b) {
		return a.longValue() > b;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(Long a, int b) {
		return a.longValue() > b;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(Long a, short b) {
		return a.longValue() > b;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(Long a, double b) {
		return a.doubleValue() > b;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(Long a, float b) {
		return a.floatValue() > b;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(Long a, Long b) {
		return a.longValue() > b.longValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(Long a, Byte b) {
		return a.longValue() > b.longValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() > ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterThan(Long a, Float b) {
		return a.floatValue() > b.floatValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(Long a, Integer b) {
		return a.longValue() > b.longValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() > ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterThan(Long a, Double b) {
		return a.doubleValue() > b.doubleValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(Long a, Short b) {
		return a.longValue() > b.longValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(Long a, AtomicInteger b) {
		return a.longValue() > b.longValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(Long a, AtomicLong b) {
		return a.longValue() > b.longValue();
	}


	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() % $2)", constantExpression=true)
	public static long operator_modulo(Long a, long b) {
		return a.longValue() % b;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() % $2)", constantExpression=true)
	public static long operator_modulo(Long a, byte b) {
		return a.longValue() % b;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() % $2)", constantExpression=true)
	public static long operator_modulo(Long a, int b) {
		return a.longValue() % b;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() % $2)", constantExpression=true)
	public static long operator_modulo(Long a, short b) {
		return a.longValue() % b;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() % $2)", constantExpression=true)
	public static double operator_modulo(Long a, double b) {
		return a.doubleValue() % b;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() % $2)", constantExpression=true)
	public static float operator_modulo(Long a, float b) {
		return a.floatValue() % b;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(Long a, Long b) {
		return a.longValue() % b.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(Long a, Byte b) {
		return a.longValue() % b.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() % ($2).floatValue())", constantExpression=true)
	public static float operator_modulo(Long a, Float b) {
		return a.floatValue() % b.floatValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(Long a, Integer b) {
		return a.longValue() % b.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() % ($2).doubleValue())", constantExpression=true)
	public static double operator_modulo(Long a, Double b) {
		return a.doubleValue() % b.doubleValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(Long a, Short b) {
		return a.longValue() % b.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(Long a, AtomicInteger b) {
		return a.longValue() % b.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(Long a, AtomicLong b) {
		return a.longValue() % b.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() - $2)", constantExpression=true)
	public static long operator_minus(Long a, long b) {
		return a.longValue() - b;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() - $2)", constantExpression=true)
	public static long operator_minus(Long a, byte b) {
		return a.longValue() - b;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() - $2)", constantExpression=true)
	public static long operator_minus(Long a, int b) {
		return a.longValue() - b;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() - $2)", constantExpression=true)
	public static long operator_minus(Long a, short b) {
		return a.longValue() - b;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() - $2)", constantExpression=true)
	public static double operator_minus(Long a, double b) {
		return a.doubleValue() - b;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() - $2)", constantExpression=true)
	public static float operator_minus(Long a, float b) {
		return a.floatValue() - b;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() - ($2).longValue())", constantExpression=true)
	public static long operator_minus(Long a, Long b) {
		return a.longValue() - b.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() - ($2).longValue())", constantExpression=true)
	public static long operator_minus(Long a, Byte b) {
		return a.longValue() - b.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() - ($2).floatValue())", constantExpression=true)
	public static float operator_minus(Long a, Float b) {
		return a.floatValue() - b.floatValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() - ($2).longValue())", constantExpression=true)
	public static long operator_minus(Long a, Integer b) {
		return a.longValue() - b.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() - ($2).doubleValue())", constantExpression=true)
	public static double operator_minus(Long a, Double b) {
		return a.doubleValue() - b.doubleValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() - ($2).longValue())", constantExpression=true)
	public static long operator_minus(Long a, Short b) {
		return a.longValue() - b.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() - ($2).longValue())", constantExpression=true)
	public static long operator_minus(Long a, AtomicInteger b) {
		return a.longValue() - b.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() - ($2).longValue())", constantExpression=true)
	public static long operator_minus(Long a, AtomicLong b) {
		return a.longValue() - b.longValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(Long a, long b) {
		return a.longValue() < b;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(Long a, byte b) {
		return a.longValue() < b;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(Long a, int b) {
		return a.longValue() < b;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(Long a, short b) {
		return a.longValue() < b;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(Long a, double b) {
		return a.doubleValue() < b;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(Long a, float b) {
		return a.floatValue() < b;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(Long a, Long b) {
		return a.longValue() < b.longValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(Long a, Byte b) {
		return a.longValue() < b.longValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).floatValue() < ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessThan(Long a, Float b) {
		return a.floatValue() < b.floatValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(Long a, Integer b) {
		return a.longValue() < b.longValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() < ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessThan(Long a, Double b) {
		return a.doubleValue() < b.doubleValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(Long a, Short b) {
		return a.longValue() < b.longValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(Long a, AtomicInteger b) {
		return a.longValue() < b.longValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).longValue() < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(Long a, AtomicLong b) {
		return a.longValue() < b.longValue();
	}

	// END GENERATED BLOCK

}
