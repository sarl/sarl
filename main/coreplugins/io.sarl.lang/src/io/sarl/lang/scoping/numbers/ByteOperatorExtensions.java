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

/** Provide static operators for numbers of type {@code Byte}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
public final class ByteOperatorExtensions {

	private ByteOperatorExtensions() {
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
	@Inline(value = "(-($1).byteValue())", constantExpression=true)
	public static int operator_minus(Byte i) {
		return -i.byteValue();
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
	public static long operator_plus(Byte a, long b) {
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
	@Inline(value = "(($1).byteValue() + $2)", constantExpression=true)
	public static int operator_plus(Byte a, byte b) {
		return a.byteValue() + b;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() + $2)", constantExpression=true)
	public static int operator_plus(Byte a, int b) {
		return a.intValue() + b;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).shortValue() + $2)", constantExpression=true)
	public static int operator_plus(Byte a, short b) {
		return a.shortValue() + b;
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
	public static double operator_plus(Byte a, double b) {
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
	public static float operator_plus(Byte a, float b) {
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
	public static long operator_plus(Byte a, Long b) {
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
	@Inline(value = "(($1).byteValue() + ($2).byteValue())", constantExpression=true)
	public static int operator_plus(Byte a, Byte b) {
		return a.byteValue() + b.byteValue();
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
	public static float operator_plus(Byte a, Float b) {
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
	@Inline(value = "(($1).intValue() + ($2).intValue())", constantExpression=true)
	public static int operator_plus(Byte a, Integer b) {
		return a.intValue() + b.intValue();
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
	public static double operator_plus(Byte a, Double b) {
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
	@Inline(value = "(($1).shortValue() + ($2).shortValue())", constantExpression=true)
	public static int operator_plus(Byte a, Short b) {
		return a.shortValue() + b.shortValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() + ($2).intValue())", constantExpression=true)
	public static int operator_plus(Byte a, AtomicInteger b) {
		return a.intValue() + b.intValue();
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
	public static long operator_plus(Byte a, AtomicLong b) {
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
	public static double operator_power(Byte a, long b) {
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
	@Inline(value = "$3.pow(($1).byteValue(), $2)", imported = Math.class)
	public static double operator_power(Byte a, byte b) {
		return Math.pow(a.byteValue(), b);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).intValue(), $2)", imported = Math.class)
	public static double operator_power(Byte a, int b) {
		return Math.pow(a.intValue(), b);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).shortValue(), $2)", imported = Math.class)
	public static double operator_power(Byte a, short b) {
		return Math.pow(a.shortValue(), b);
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
	public static double operator_power(Byte a, double b) {
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
	public static double operator_power(Byte a, float b) {
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
	public static double operator_power(Byte a, Long b) {
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
	@Inline(value = "$3.pow(($1).byteValue(), ($2).byteValue())", imported = Math.class)
	public static double operator_power(Byte a, Byte b) {
		return Math.pow(a.byteValue(), b.byteValue());
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
	public static double operator_power(Byte a, Float b) {
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
	@Inline(value = "$3.pow(($1).intValue(), ($2).intValue())", imported = Math.class)
	public static double operator_power(Byte a, Integer b) {
		return Math.pow(a.intValue(), b.intValue());
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
	public static double operator_power(Byte a, Double b) {
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
	@Inline(value = "$3.pow(($1).shortValue(), ($2).shortValue())", imported = Math.class)
	public static double operator_power(Byte a, Short b) {
		return Math.pow(a.shortValue(), b.shortValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(($1).intValue(), ($2).intValue())", imported = Math.class)
	public static double operator_power(Byte a, AtomicInteger b) {
		return Math.pow(a.intValue(), b.intValue());
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
	public static double operator_power(Byte a, AtomicLong b) {
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
	public static boolean operator_greaterEqualsThan(Byte a, long b) {
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
	@Inline(value = "(($1).byteValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Byte a, byte b) {
		return a.byteValue() >= b;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Byte a, int b) {
		return a.intValue() >= b;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).shortValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Byte a, short b) {
		return a.shortValue() >= b;
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
	public static boolean operator_greaterEqualsThan(Byte a, double b) {
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
	public static boolean operator_greaterEqualsThan(Byte a, float b) {
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
	public static boolean operator_greaterEqualsThan(Byte a, Long b) {
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
	@Inline(value = "(($1).byteValue() >= ($2).byteValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Byte a, Byte b) {
		return a.byteValue() >= b.byteValue();
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
	public static boolean operator_greaterEqualsThan(Byte a, Float b) {
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
	@Inline(value = "(($1).intValue() >= ($2).intValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Byte a, Integer b) {
		return a.intValue() >= b.intValue();
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
	public static boolean operator_greaterEqualsThan(Byte a, Double b) {
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
	@Inline(value = "(($1).shortValue() >= ($2).shortValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Byte a, Short b) {
		return a.shortValue() >= b.shortValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() >= ($2).intValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(Byte a, AtomicInteger b) {
		return a.intValue() >= b.intValue();
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
	public static boolean operator_greaterEqualsThan(Byte a, AtomicLong b) {
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
	public static boolean operator_notEquals(Byte a, long b) {
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
	@Inline(value = "(($1).byteValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(Byte a, byte b) {
		return a.byteValue() != b;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(Byte a, int b) {
		return a.intValue() != b;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).shortValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(Byte a, short b) {
		return a.shortValue() != b;
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
	public static boolean operator_notEquals(Byte a, double b) {
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
	public static boolean operator_notEquals(Byte a, float b) {
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
	public static boolean operator_notEquals(Byte a, Long b) {
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
	@Inline(value = "(($1).byteValue() != ($2).byteValue())", constantExpression=true)
	public static boolean operator_notEquals(Byte a, Byte b) {
		return a.byteValue() != b.byteValue();
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
	public static boolean operator_notEquals(Byte a, Float b) {
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
	@Inline(value = "(($1).intValue() != ($2).intValue())", constantExpression=true)
	public static boolean operator_notEquals(Byte a, Integer b) {
		return a.intValue() != b.intValue();
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
	public static boolean operator_notEquals(Byte a, Double b) {
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
	@Inline(value = "(($1).shortValue() != ($2).shortValue())", constantExpression=true)
	public static boolean operator_notEquals(Byte a, Short b) {
		return a.shortValue() != b.shortValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() != ($2).intValue())", constantExpression=true)
	public static boolean operator_notEquals(Byte a, AtomicInteger b) {
		return a.intValue() != b.intValue();
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
	public static boolean operator_notEquals(Byte a, AtomicLong b) {
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
	public static long operator_divide(Byte a, long b) {
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
	@Inline(value = "(($1).byteValue() / $2)", constantExpression=true)
	public static int operator_divide(Byte a, byte b) {
		return a.byteValue() / b;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() / $2)", constantExpression=true)
	public static int operator_divide(Byte a, int b) {
		return a.intValue() / b;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).shortValue() / $2)", constantExpression=true)
	public static int operator_divide(Byte a, short b) {
		return a.shortValue() / b;
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
	public static double operator_divide(Byte a, double b) {
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
	public static float operator_divide(Byte a, float b) {
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
	public static long operator_divide(Byte a, Long b) {
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
	@Inline(value = "(($1).byteValue() / ($2).byteValue())", constantExpression=true)
	public static int operator_divide(Byte a, Byte b) {
		return a.byteValue() / b.byteValue();
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
	public static float operator_divide(Byte a, Float b) {
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
	@Inline(value = "(($1).intValue() / ($2).intValue())", constantExpression=true)
	public static int operator_divide(Byte a, Integer b) {
		return a.intValue() / b.intValue();
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
	public static double operator_divide(Byte a, Double b) {
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
	@Inline(value = "(($1).shortValue() / ($2).shortValue())", constantExpression=true)
	public static int operator_divide(Byte a, Short b) {
		return a.shortValue() / b.shortValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() / ($2).intValue())", constantExpression=true)
	public static int operator_divide(Byte a, AtomicInteger b) {
		return a.intValue() / b.intValue();
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
	public static long operator_divide(Byte a, AtomicLong b) {
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
	public static boolean operator_lessEqualsThan(Byte a, long b) {
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
	@Inline(value = "(($1).byteValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(Byte a, byte b) {
		return a.byteValue() <= b;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(Byte a, int b) {
		return a.intValue() <= b;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).shortValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(Byte a, short b) {
		return a.shortValue() <= b;
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
	public static boolean operator_lessEqualsThan(Byte a, double b) {
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
	public static boolean operator_lessEqualsThan(Byte a, float b) {
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
	public static boolean operator_lessEqualsThan(Byte a, Long b) {
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
	@Inline(value = "(($1).byteValue() <= ($2).byteValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(Byte a, Byte b) {
		return a.byteValue() <= b.byteValue();
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
	public static boolean operator_lessEqualsThan(Byte a, Float b) {
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
	@Inline(value = "(($1).intValue() <= ($2).intValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(Byte a, Integer b) {
		return a.intValue() <= b.intValue();
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
	public static boolean operator_lessEqualsThan(Byte a, Double b) {
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
	@Inline(value = "(($1).shortValue() <= ($2).shortValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(Byte a, Short b) {
		return a.shortValue() <= b.shortValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() <= ($2).intValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(Byte a, AtomicInteger b) {
		return a.intValue() <= b.intValue();
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
	public static boolean operator_lessEqualsThan(Byte a, AtomicLong b) {
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
	public static long operator_multiply(Byte a, long b) {
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
	@Inline(value = "(($1).byteValue() * $2)", constantExpression=true)
	public static int operator_multiply(Byte a, byte b) {
		return a.byteValue() * b;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() * $2)", constantExpression=true)
	public static int operator_multiply(Byte a, int b) {
		return a.intValue() * b;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).shortValue() * $2)", constantExpression=true)
	public static int operator_multiply(Byte a, short b) {
		return a.shortValue() * b;
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
	public static double operator_multiply(Byte a, double b) {
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
	public static float operator_multiply(Byte a, float b) {
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
	public static long operator_multiply(Byte a, Long b) {
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
	@Inline(value = "(($1).byteValue() * ($2).byteValue())", constantExpression=true)
	public static int operator_multiply(Byte a, Byte b) {
		return a.byteValue() * b.byteValue();
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
	public static float operator_multiply(Byte a, Float b) {
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
	@Inline(value = "(($1).intValue() * ($2).intValue())", constantExpression=true)
	public static int operator_multiply(Byte a, Integer b) {
		return a.intValue() * b.intValue();
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
	public static double operator_multiply(Byte a, Double b) {
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
	@Inline(value = "(($1).shortValue() * ($2).shortValue())", constantExpression=true)
	public static int operator_multiply(Byte a, Short b) {
		return a.shortValue() * b.shortValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() * ($2).intValue())", constantExpression=true)
	public static int operator_multiply(Byte a, AtomicInteger b) {
		return a.intValue() * b.intValue();
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
	public static long operator_multiply(Byte a, AtomicLong b) {
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
	public static boolean operator_equals(Byte a, long b) {
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
	@Inline(value = "(($1).byteValue() == $2)", constantExpression=true)
	public static boolean operator_equals(Byte a, byte b) {
		return a.byteValue() == b;
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() == $2)", constantExpression=true)
	public static boolean operator_equals(Byte a, int b) {
		return a.intValue() == b;
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).shortValue() == $2)", constantExpression=true)
	public static boolean operator_equals(Byte a, short b) {
		return a.shortValue() == b;
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
	public static boolean operator_equals(Byte a, double b) {
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
	public static boolean operator_equals(Byte a, float b) {
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
	public static boolean operator_equals(Byte a, Long b) {
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
	@Inline(value = "(($1).byteValue() == ($2).byteValue())", constantExpression=true)
	public static boolean operator_equals(Byte a, Byte b) {
		return a.byteValue() == b.byteValue();
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
	public static boolean operator_equals(Byte a, Float b) {
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
	@Inline(value = "(($1).intValue() == ($2).intValue())", constantExpression=true)
	public static boolean operator_equals(Byte a, Integer b) {
		return a.intValue() == b.intValue();
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
	public static boolean operator_equals(Byte a, Double b) {
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
	@Inline(value = "(($1).shortValue() == ($2).shortValue())", constantExpression=true)
	public static boolean operator_equals(Byte a, Short b) {
		return a.shortValue() == b.shortValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() == ($2).intValue())", constantExpression=true)
	public static boolean operator_equals(Byte a, AtomicInteger b) {
		return a.intValue() == b.intValue();
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
	public static boolean operator_equals(Byte a, AtomicLong b) {
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
	public static boolean operator_greaterThan(Byte a, long b) {
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
	@Inline(value = "(($1).byteValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(Byte a, byte b) {
		return a.byteValue() > b;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(Byte a, int b) {
		return a.intValue() > b;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).shortValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(Byte a, short b) {
		return a.shortValue() > b;
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
	public static boolean operator_greaterThan(Byte a, double b) {
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
	public static boolean operator_greaterThan(Byte a, float b) {
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
	public static boolean operator_greaterThan(Byte a, Long b) {
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
	@Inline(value = "(($1).byteValue() > ($2).byteValue())", constantExpression=true)
	public static boolean operator_greaterThan(Byte a, Byte b) {
		return a.byteValue() > b.byteValue();
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
	public static boolean operator_greaterThan(Byte a, Float b) {
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
	@Inline(value = "(($1).intValue() > ($2).intValue())", constantExpression=true)
	public static boolean operator_greaterThan(Byte a, Integer b) {
		return a.intValue() > b.intValue();
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
	public static boolean operator_greaterThan(Byte a, Double b) {
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
	@Inline(value = "(($1).shortValue() > ($2).shortValue())", constantExpression=true)
	public static boolean operator_greaterThan(Byte a, Short b) {
		return a.shortValue() > b.shortValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() > ($2).intValue())", constantExpression=true)
	public static boolean operator_greaterThan(Byte a, AtomicInteger b) {
		return a.intValue() > b.intValue();
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
	public static boolean operator_greaterThan(Byte a, AtomicLong b) {
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
	public static long operator_modulo(Byte a, long b) {
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
	@Inline(value = "(($1).byteValue() % $2)", constantExpression=true)
	public static int operator_modulo(Byte a, byte b) {
		return a.byteValue() % b;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() % $2)", constantExpression=true)
	public static int operator_modulo(Byte a, int b) {
		return a.intValue() % b;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).shortValue() % $2)", constantExpression=true)
	public static int operator_modulo(Byte a, short b) {
		return a.shortValue() % b;
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
	public static double operator_modulo(Byte a, double b) {
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
	public static float operator_modulo(Byte a, float b) {
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
	public static long operator_modulo(Byte a, Long b) {
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
	@Inline(value = "(($1).byteValue() % ($2).byteValue())", constantExpression=true)
	public static int operator_modulo(Byte a, Byte b) {
		return a.byteValue() % b.byteValue();
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
	public static float operator_modulo(Byte a, Float b) {
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
	@Inline(value = "(($1).intValue() % ($2).intValue())", constantExpression=true)
	public static int operator_modulo(Byte a, Integer b) {
		return a.intValue() % b.intValue();
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
	public static double operator_modulo(Byte a, Double b) {
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
	@Inline(value = "(($1).shortValue() % ($2).shortValue())", constantExpression=true)
	public static int operator_modulo(Byte a, Short b) {
		return a.shortValue() % b.shortValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() % ($2).intValue())", constantExpression=true)
	public static int operator_modulo(Byte a, AtomicInteger b) {
		return a.intValue() % b.intValue();
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
	public static long operator_modulo(Byte a, AtomicLong b) {
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
	public static long operator_minus(Byte a, long b) {
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
	@Inline(value = "(($1).byteValue() - $2)", constantExpression=true)
	public static int operator_minus(Byte a, byte b) {
		return a.byteValue() - b;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() - $2)", constantExpression=true)
	public static int operator_minus(Byte a, int b) {
		return a.intValue() - b;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).shortValue() - $2)", constantExpression=true)
	public static int operator_minus(Byte a, short b) {
		return a.shortValue() - b;
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
	public static double operator_minus(Byte a, double b) {
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
	public static float operator_minus(Byte a, float b) {
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
	public static long operator_minus(Byte a, Long b) {
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
	@Inline(value = "(($1).byteValue() - ($2).byteValue())", constantExpression=true)
	public static int operator_minus(Byte a, Byte b) {
		return a.byteValue() - b.byteValue();
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
	public static float operator_minus(Byte a, Float b) {
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
	@Inline(value = "(($1).intValue() - ($2).intValue())", constantExpression=true)
	public static int operator_minus(Byte a, Integer b) {
		return a.intValue() - b.intValue();
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
	public static double operator_minus(Byte a, Double b) {
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
	@Inline(value = "(($1).shortValue() - ($2).shortValue())", constantExpression=true)
	public static int operator_minus(Byte a, Short b) {
		return a.shortValue() - b.shortValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() - ($2).intValue())", constantExpression=true)
	public static int operator_minus(Byte a, AtomicInteger b) {
		return a.intValue() - b.intValue();
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
	public static long operator_minus(Byte a, AtomicLong b) {
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
	public static boolean operator_lessThan(Byte a, long b) {
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
	@Inline(value = "(($1).byteValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(Byte a, byte b) {
		return a.byteValue() < b;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(Byte a, int b) {
		return a.intValue() < b;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).shortValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(Byte a, short b) {
		return a.shortValue() < b;
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
	public static boolean operator_lessThan(Byte a, double b) {
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
	public static boolean operator_lessThan(Byte a, float b) {
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
	public static boolean operator_lessThan(Byte a, Long b) {
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
	@Inline(value = "(($1).byteValue() < ($2).byteValue())", constantExpression=true)
	public static boolean operator_lessThan(Byte a, Byte b) {
		return a.byteValue() < b.byteValue();
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
	public static boolean operator_lessThan(Byte a, Float b) {
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
	@Inline(value = "(($1).intValue() < ($2).intValue())", constantExpression=true)
	public static boolean operator_lessThan(Byte a, Integer b) {
		return a.intValue() < b.intValue();
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
	public static boolean operator_lessThan(Byte a, Double b) {
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
	@Inline(value = "(($1).shortValue() < ($2).shortValue())", constantExpression=true)
	public static boolean operator_lessThan(Byte a, Short b) {
		return a.shortValue() < b.shortValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(($1).intValue() < ($2).intValue())", constantExpression=true)
	public static boolean operator_lessThan(Byte a, AtomicInteger b) {
		return a.intValue() < b.intValue();
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
	public static boolean operator_lessThan(Byte a, AtomicLong b) {
		return a.longValue() < b.longValue();
	}

	// END GENERATED BLOCK

}
