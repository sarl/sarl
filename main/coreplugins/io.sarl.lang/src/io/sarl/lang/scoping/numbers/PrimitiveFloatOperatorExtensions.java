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

/** Provide static operators for numbers of type {@code float}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
public final class PrimitiveFloatOperatorExtensions {

	private PrimitiveFloatOperatorExtensions() {
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
	@Inline(value = "($1 + ($2).floatValue())", constantExpression=true)
	public static float operator_plus(float a, Long b) {
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
	@Inline(value = "($1 + ($2).floatValue())", constantExpression=true)
	public static float operator_plus(float a, Byte b) {
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
	@Inline(value = "($1 + ($2).floatValue())", constantExpression=true)
	public static float operator_plus(float a, Float b) {
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
	@Inline(value = "($1 + ($2).floatValue())", constantExpression=true)
	public static float operator_plus(float a, Integer b) {
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
	@Inline(value = "($1 + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(float a, Double b) {
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
	@Inline(value = "($1 + ($2).floatValue())", constantExpression=true)
	public static float operator_plus(float a, Short b) {
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
	@Inline(value = "($1 + ($2).floatValue())", constantExpression=true)
	public static float operator_plus(float a, AtomicInteger b) {
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
	@Inline(value = "($1 + ($2).floatValue())", constantExpression=true)
	public static float operator_plus(float a, AtomicLong b) {
		return a + b.floatValue();
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
	public static double operator_power(float a, Long b) {
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
	@Inline(value = "$3.pow($1, ($2).floatValue())", imported = Math.class)
	public static double operator_power(float a, Byte b) {
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
	@Inline(value = "$3.pow($1, ($2).floatValue())", imported = Math.class)
	public static double operator_power(float a, Float b) {
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
	@Inline(value = "$3.pow($1, ($2).floatValue())", imported = Math.class)
	public static double operator_power(float a, Integer b) {
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
	@Inline(value = "$3.pow($1, ($2).doubleValue())", imported = Math.class)
	public static double operator_power(float a, Double b) {
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
	@Inline(value = "$3.pow($1, ($2).floatValue())", imported = Math.class)
	public static double operator_power(float a, Short b) {
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
	@Inline(value = "$3.pow($1, ($2).floatValue())", imported = Math.class)
	public static double operator_power(float a, AtomicInteger b) {
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
	@Inline(value = "$3.pow($1, ($2).floatValue())", imported = Math.class)
	public static double operator_power(float a, AtomicLong b) {
		return Math.pow(a, b.floatValue());
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
	public static boolean operator_greaterEqualsThan(float a, Long b) {
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
	@Inline(value = "($1 >= ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(float a, Byte b) {
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
	@Inline(value = "($1 >= ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(float a, Float b) {
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
	@Inline(value = "($1 >= ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(float a, Integer b) {
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
	@Inline(value = "($1 >= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(float a, Double b) {
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
	@Inline(value = "($1 >= ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(float a, Short b) {
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
	@Inline(value = "($1 >= ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(float a, AtomicInteger b) {
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
	@Inline(value = "($1 >= ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(float a, AtomicLong b) {
		return a >= b.floatValue();
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
	public static boolean operator_notEquals(float a, Long b) {
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
	@Inline(value = "($1 != ($2).floatValue())", constantExpression=true)
	public static boolean operator_notEquals(float a, Byte b) {
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
	@Inline(value = "($1 != ($2).floatValue())", constantExpression=true)
	public static boolean operator_notEquals(float a, Float b) {
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
	@Inline(value = "($1 != ($2).floatValue())", constantExpression=true)
	public static boolean operator_notEquals(float a, Integer b) {
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
	@Inline(value = "($1 != ($2).doubleValue())", constantExpression=true)
	public static boolean operator_notEquals(float a, Double b) {
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
	@Inline(value = "($1 != ($2).floatValue())", constantExpression=true)
	public static boolean operator_notEquals(float a, Short b) {
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
	@Inline(value = "($1 != ($2).floatValue())", constantExpression=true)
	public static boolean operator_notEquals(float a, AtomicInteger b) {
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
	@Inline(value = "($1 != ($2).floatValue())", constantExpression=true)
	public static boolean operator_notEquals(float a, AtomicLong b) {
		return a != b.floatValue();
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
	public static float operator_divide(float a, Long b) {
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
	@Inline(value = "($1 / ($2).floatValue())", constantExpression=true)
	public static float operator_divide(float a, Byte b) {
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
	@Inline(value = "($1 / ($2).floatValue())", constantExpression=true)
	public static float operator_divide(float a, Float b) {
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
	@Inline(value = "($1 / ($2).floatValue())", constantExpression=true)
	public static float operator_divide(float a, Integer b) {
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
	@Inline(value = "($1 / ($2).doubleValue())", constantExpression=true)
	public static double operator_divide(float a, Double b) {
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
	@Inline(value = "($1 / ($2).floatValue())", constantExpression=true)
	public static float operator_divide(float a, Short b) {
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
	@Inline(value = "($1 / ($2).floatValue())", constantExpression=true)
	public static float operator_divide(float a, AtomicInteger b) {
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
	@Inline(value = "($1 / ($2).floatValue())", constantExpression=true)
	public static float operator_divide(float a, AtomicLong b) {
		return a / b.floatValue();
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
	public static boolean operator_lessEqualsThan(float a, Long b) {
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
	@Inline(value = "($1 <= ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(float a, Byte b) {
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
	@Inline(value = "($1 <= ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(float a, Float b) {
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
	@Inline(value = "($1 <= ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(float a, Integer b) {
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
	@Inline(value = "($1 <= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(float a, Double b) {
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
	@Inline(value = "($1 <= ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(float a, Short b) {
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
	@Inline(value = "($1 <= ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(float a, AtomicInteger b) {
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
	@Inline(value = "($1 <= ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(float a, AtomicLong b) {
		return a <= b.floatValue();
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
	public static float operator_multiply(float a, Long b) {
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
	@Inline(value = "($1 * ($2).floatValue())", constantExpression=true)
	public static float operator_multiply(float a, Byte b) {
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
	@Inline(value = "($1 * ($2).floatValue())", constantExpression=true)
	public static float operator_multiply(float a, Float b) {
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
	@Inline(value = "($1 * ($2).floatValue())", constantExpression=true)
	public static float operator_multiply(float a, Integer b) {
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
	@Inline(value = "($1 * ($2).doubleValue())", constantExpression=true)
	public static double operator_multiply(float a, Double b) {
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
	@Inline(value = "($1 * ($2).floatValue())", constantExpression=true)
	public static float operator_multiply(float a, Short b) {
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
	@Inline(value = "($1 * ($2).floatValue())", constantExpression=true)
	public static float operator_multiply(float a, AtomicInteger b) {
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
	@Inline(value = "($1 * ($2).floatValue())", constantExpression=true)
	public static float operator_multiply(float a, AtomicLong b) {
		return a * b.floatValue();
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
	public static boolean operator_equals(float a, Long b) {
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
	@Inline(value = "($1 == ($2).floatValue())", constantExpression=true)
	public static boolean operator_equals(float a, Byte b) {
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
	@Inline(value = "($1 == ($2).floatValue())", constantExpression=true)
	public static boolean operator_equals(float a, Float b) {
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
	@Inline(value = "($1 == ($2).floatValue())", constantExpression=true)
	public static boolean operator_equals(float a, Integer b) {
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
	@Inline(value = "($1 == ($2).doubleValue())", constantExpression=true)
	public static boolean operator_equals(float a, Double b) {
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
	@Inline(value = "($1 == ($2).floatValue())", constantExpression=true)
	public static boolean operator_equals(float a, Short b) {
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
	@Inline(value = "($1 == ($2).floatValue())", constantExpression=true)
	public static boolean operator_equals(float a, AtomicInteger b) {
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
	@Inline(value = "($1 == ($2).floatValue())", constantExpression=true)
	public static boolean operator_equals(float a, AtomicLong b) {
		return a == b.floatValue();
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
	public static boolean operator_greaterThan(float a, Long b) {
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
	@Inline(value = "($1 > ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterThan(float a, Byte b) {
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
	@Inline(value = "($1 > ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterThan(float a, Float b) {
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
	@Inline(value = "($1 > ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterThan(float a, Integer b) {
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
	@Inline(value = "($1 > ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterThan(float a, Double b) {
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
	@Inline(value = "($1 > ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterThan(float a, Short b) {
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
	@Inline(value = "($1 > ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterThan(float a, AtomicInteger b) {
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
	@Inline(value = "($1 > ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterThan(float a, AtomicLong b) {
		return a > b.floatValue();
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
	public static float operator_modulo(float a, Long b) {
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
	@Inline(value = "($1 % ($2).floatValue())", constantExpression=true)
	public static float operator_modulo(float a, Byte b) {
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
	@Inline(value = "($1 % ($2).floatValue())", constantExpression=true)
	public static float operator_modulo(float a, Float b) {
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
	@Inline(value = "($1 % ($2).floatValue())", constantExpression=true)
	public static float operator_modulo(float a, Integer b) {
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
	@Inline(value = "($1 % ($2).doubleValue())", constantExpression=true)
	public static double operator_modulo(float a, Double b) {
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
	@Inline(value = "($1 % ($2).floatValue())", constantExpression=true)
	public static float operator_modulo(float a, Short b) {
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
	@Inline(value = "($1 % ($2).floatValue())", constantExpression=true)
	public static float operator_modulo(float a, AtomicInteger b) {
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
	@Inline(value = "($1 % ($2).floatValue())", constantExpression=true)
	public static float operator_modulo(float a, AtomicLong b) {
		return a % b.floatValue();
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
	public static float operator_minus(float a, Long b) {
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
	@Inline(value = "($1 - ($2).floatValue())", constantExpression=true)
	public static float operator_minus(float a, Byte b) {
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
	@Inline(value = "($1 - ($2).floatValue())", constantExpression=true)
	public static float operator_minus(float a, Float b) {
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
	@Inline(value = "($1 - ($2).floatValue())", constantExpression=true)
	public static float operator_minus(float a, Integer b) {
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
	@Inline(value = "($1 - ($2).doubleValue())", constantExpression=true)
	public static double operator_minus(float a, Double b) {
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
	@Inline(value = "($1 - ($2).floatValue())", constantExpression=true)
	public static float operator_minus(float a, Short b) {
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
	@Inline(value = "($1 - ($2).floatValue())", constantExpression=true)
	public static float operator_minus(float a, AtomicInteger b) {
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
	@Inline(value = "($1 - ($2).floatValue())", constantExpression=true)
	public static float operator_minus(float a, AtomicLong b) {
		return a - b.floatValue();
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
	public static boolean operator_lessThan(float a, Long b) {
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
	@Inline(value = "($1 < ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessThan(float a, Byte b) {
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
	@Inline(value = "($1 < ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessThan(float a, Float b) {
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
	@Inline(value = "($1 < ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessThan(float a, Integer b) {
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
	@Inline(value = "($1 < ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessThan(float a, Double b) {
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
	@Inline(value = "($1 < ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessThan(float a, Short b) {
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
	@Inline(value = "($1 < ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessThan(float a, AtomicInteger b) {
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
	@Inline(value = "($1 < ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessThan(float a, AtomicLong b) {
		return a < b.floatValue();
	}

	// END GENERATED BLOCK

}
