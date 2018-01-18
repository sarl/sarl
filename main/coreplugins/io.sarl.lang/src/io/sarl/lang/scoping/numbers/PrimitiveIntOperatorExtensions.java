/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

/** Provide static operators for numbers of type {@code int}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
public final class PrimitiveIntOperatorExtensions {

	private PrimitiveIntOperatorExtensions() {
		//
	}

	// BEGIN GENERATED BLOCK

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "($1 + ($2).longValue())", constantExpression=true)
	public static long operator_plus(int a, Long b) {
		return a + b.longValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "($1 + ($2).intValue())", constantExpression=true)
	public static int operator_plus(int a, Byte b) {
		return a + b.intValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "($1 + ($2).floatValue())", constantExpression=true)
	public static float operator_plus(int a, Float b) {
		return a + b.floatValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "($1 + ($2).intValue())", constantExpression=true)
	public static int operator_plus(int a, Integer b) {
		return a + b.intValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "($1 + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(int a, Double b) {
		return a + b.doubleValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "($1 + ($2).intValue())", constantExpression=true)
	public static int operator_plus(int a, Short b) {
		return a + b.intValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "($1 + ($2).intValue())", constantExpression=true)
	public static int operator_plus(int a, AtomicInteger b) {
		return a + b.intValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "($1 + ($2).longValue())", constantExpression=true)
	public static long operator_plus(int a, AtomicLong b) {
		return a + b.longValue();
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).longValue())", imported = Math.class)
	public static double operator_power(int a, Long b) {
		return Math.pow(a, b.longValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).intValue())", imported = Math.class)
	public static double operator_power(int a, Byte b) {
		return Math.pow(a, b.intValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).floatValue())", imported = Math.class)
	public static double operator_power(int a, Float b) {
		return Math.pow(a, b.floatValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).intValue())", imported = Math.class)
	public static double operator_power(int a, Integer b) {
		return Math.pow(a, b.intValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).doubleValue())", imported = Math.class)
	public static double operator_power(int a, Double b) {
		return Math.pow(a, b.doubleValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).intValue())", imported = Math.class)
	public static double operator_power(int a, Short b) {
		return Math.pow(a, b.intValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).intValue())", imported = Math.class)
	public static double operator_power(int a, AtomicInteger b) {
		return Math.pow(a, b.intValue());
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).longValue())", imported = Math.class)
	public static double operator_power(int a, AtomicLong b) {
		return Math.pow(a, b.longValue());
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "($1 >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(int a, Long b) {
		return a >= b.longValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "($1 >= ($2).intValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(int a, Byte b) {
		return a >= b.intValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "($1 >= ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(int a, Float b) {
		return a >= b.floatValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "($1 >= ($2).intValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(int a, Integer b) {
		return a >= b.intValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "($1 >= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(int a, Double b) {
		return a >= b.doubleValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "($1 >= ($2).intValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(int a, Short b) {
		return a >= b.intValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "($1 >= ($2).intValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(int a, AtomicInteger b) {
		return a >= b.intValue();
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "($1 >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(int a, AtomicLong b) {
		return a >= b.longValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "($1 != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(int a, Long b) {
		return a != b.longValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "($1 != ($2).intValue())", constantExpression=true)
	public static boolean operator_notEquals(int a, Byte b) {
		return a != b.intValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "($1 != ($2).floatValue())", constantExpression=true)
	public static boolean operator_notEquals(int a, Float b) {
		return a != b.floatValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "($1 != ($2).intValue())", constantExpression=true)
	public static boolean operator_notEquals(int a, Integer b) {
		return a != b.intValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "($1 != ($2).doubleValue())", constantExpression=true)
	public static boolean operator_notEquals(int a, Double b) {
		return a != b.doubleValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "($1 != ($2).intValue())", constantExpression=true)
	public static boolean operator_notEquals(int a, Short b) {
		return a != b.intValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "($1 != ($2).intValue())", constantExpression=true)
	public static boolean operator_notEquals(int a, AtomicInteger b) {
		return a != b.intValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "($1 != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(int a, AtomicLong b) {
		return a != b.longValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "($1 / ($2).longValue())", constantExpression=true)
	public static long operator_divide(int a, Long b) {
		return a / b.longValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "($1 / ($2).intValue())", constantExpression=true)
	public static int operator_divide(int a, Byte b) {
		return a / b.intValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "($1 / ($2).floatValue())", constantExpression=true)
	public static float operator_divide(int a, Float b) {
		return a / b.floatValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "($1 / ($2).intValue())", constantExpression=true)
	public static int operator_divide(int a, Integer b) {
		return a / b.intValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "($1 / ($2).doubleValue())", constantExpression=true)
	public static double operator_divide(int a, Double b) {
		return a / b.doubleValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "($1 / ($2).intValue())", constantExpression=true)
	public static int operator_divide(int a, Short b) {
		return a / b.intValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "($1 / ($2).intValue())", constantExpression=true)
	public static int operator_divide(int a, AtomicInteger b) {
		return a / b.intValue();
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "($1 / ($2).longValue())", constantExpression=true)
	public static long operator_divide(int a, AtomicLong b) {
		return a / b.longValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "($1 <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(int a, Long b) {
		return a <= b.longValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "($1 <= ($2).intValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(int a, Byte b) {
		return a <= b.intValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "($1 <= ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(int a, Float b) {
		return a <= b.floatValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "($1 <= ($2).intValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(int a, Integer b) {
		return a <= b.intValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "($1 <= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(int a, Double b) {
		return a <= b.doubleValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "($1 <= ($2).intValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(int a, Short b) {
		return a <= b.intValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "($1 <= ($2).intValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(int a, AtomicInteger b) {
		return a <= b.intValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "($1 <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(int a, AtomicLong b) {
		return a <= b.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "($1 * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(int a, Long b) {
		return a * b.longValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "($1 * ($2).intValue())", constantExpression=true)
	public static int operator_multiply(int a, Byte b) {
		return a * b.intValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "($1 * ($2).floatValue())", constantExpression=true)
	public static float operator_multiply(int a, Float b) {
		return a * b.floatValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "($1 * ($2).intValue())", constantExpression=true)
	public static int operator_multiply(int a, Integer b) {
		return a * b.intValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "($1 * ($2).doubleValue())", constantExpression=true)
	public static double operator_multiply(int a, Double b) {
		return a * b.doubleValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "($1 * ($2).intValue())", constantExpression=true)
	public static int operator_multiply(int a, Short b) {
		return a * b.intValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "($1 * ($2).intValue())", constantExpression=true)
	public static int operator_multiply(int a, AtomicInteger b) {
		return a * b.intValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "($1 * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(int a, AtomicLong b) {
		return a * b.longValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "($1 == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(int a, Long b) {
		return a == b.longValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "($1 == ($2).intValue())", constantExpression=true)
	public static boolean operator_equals(int a, Byte b) {
		return a == b.intValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "($1 == ($2).floatValue())", constantExpression=true)
	public static boolean operator_equals(int a, Float b) {
		return a == b.floatValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "($1 == ($2).intValue())", constantExpression=true)
	public static boolean operator_equals(int a, Integer b) {
		return a == b.intValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "($1 == ($2).doubleValue())", constantExpression=true)
	public static boolean operator_equals(int a, Double b) {
		return a == b.doubleValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "($1 == ($2).intValue())", constantExpression=true)
	public static boolean operator_equals(int a, Short b) {
		return a == b.intValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "($1 == ($2).intValue())", constantExpression=true)
	public static boolean operator_equals(int a, AtomicInteger b) {
		return a == b.intValue();
	}

	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "($1 == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(int a, AtomicLong b) {
		return a == b.longValue();
	}

	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "($1 > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(int a, Long b) {
		return a > b.longValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "($1 > ($2).intValue())", constantExpression=true)
	public static boolean operator_greaterThan(int a, Byte b) {
		return a > b.intValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "($1 > ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterThan(int a, Float b) {
		return a > b.floatValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "($1 > ($2).intValue())", constantExpression=true)
	public static boolean operator_greaterThan(int a, Integer b) {
		return a > b.intValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "($1 > ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterThan(int a, Double b) {
		return a > b.doubleValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "($1 > ($2).intValue())", constantExpression=true)
	public static boolean operator_greaterThan(int a, Short b) {
		return a > b.intValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "($1 > ($2).intValue())", constantExpression=true)
	public static boolean operator_greaterThan(int a, AtomicInteger b) {
		return a > b.intValue();
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "($1 > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(int a, AtomicLong b) {
		return a > b.longValue();
	}


	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "($1 % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(int a, Long b) {
		return a % b.longValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "($1 % ($2).intValue())", constantExpression=true)
	public static int operator_modulo(int a, Byte b) {
		return a % b.intValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "($1 % ($2).floatValue())", constantExpression=true)
	public static float operator_modulo(int a, Float b) {
		return a % b.floatValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "($1 % ($2).intValue())", constantExpression=true)
	public static int operator_modulo(int a, Integer b) {
		return a % b.intValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "($1 % ($2).doubleValue())", constantExpression=true)
	public static double operator_modulo(int a, Double b) {
		return a % b.doubleValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "($1 % ($2).intValue())", constantExpression=true)
	public static int operator_modulo(int a, Short b) {
		return a % b.intValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "($1 % ($2).intValue())", constantExpression=true)
	public static int operator_modulo(int a, AtomicInteger b) {
		return a % b.intValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "($1 % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(int a, AtomicLong b) {
		return a % b.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "($1 - ($2).longValue())", constantExpression=true)
	public static long operator_minus(int a, Long b) {
		return a - b.longValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "($1 - ($2).intValue())", constantExpression=true)
	public static int operator_minus(int a, Byte b) {
		return a - b.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "($1 - ($2).floatValue())", constantExpression=true)
	public static float operator_minus(int a, Float b) {
		return a - b.floatValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "($1 - ($2).intValue())", constantExpression=true)
	public static int operator_minus(int a, Integer b) {
		return a - b.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "($1 - ($2).doubleValue())", constantExpression=true)
	public static double operator_minus(int a, Double b) {
		return a - b.doubleValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "($1 - ($2).intValue())", constantExpression=true)
	public static int operator_minus(int a, Short b) {
		return a - b.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "($1 - ($2).intValue())", constantExpression=true)
	public static int operator_minus(int a, AtomicInteger b) {
		return a - b.intValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "($1 - ($2).longValue())", constantExpression=true)
	public static long operator_minus(int a, AtomicLong b) {
		return a - b.longValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "($1 < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(int a, Long b) {
		return a < b.longValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "($1 < ($2).intValue())", constantExpression=true)
	public static boolean operator_lessThan(int a, Byte b) {
		return a < b.intValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "($1 < ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessThan(int a, Float b) {
		return a < b.floatValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "($1 < ($2).intValue())", constantExpression=true)
	public static boolean operator_lessThan(int a, Integer b) {
		return a < b.intValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "($1 < ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessThan(int a, Double b) {
		return a < b.doubleValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "($1 < ($2).intValue())", constantExpression=true)
	public static boolean operator_lessThan(int a, Short b) {
		return a < b.intValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "($1 < ($2).intValue())", constantExpression=true)
	public static boolean operator_lessThan(int a, AtomicInteger b) {
		return a < b.intValue();
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "($1 < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(int a, AtomicLong b) {
		return a < b.longValue();
	}

	// END GENERATED BLOCK

}
