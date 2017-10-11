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

/** Provide static operators for numbers of type {@code double}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
public final class PrimitiveDoubleOperatorExtensions {

	private PrimitiveDoubleOperatorExtensions() {
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
	@Inline(value = "($1 + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(double a, Long b) {
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
	@Inline(value = "($1 + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(double a, Byte b) {
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
	@Inline(value = "($1 + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(double a, Float b) {
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
	@Inline(value = "($1 + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(double a, Integer b) {
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
	@Inline(value = "($1 + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(double a, Double b) {
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
	@Inline(value = "($1 + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(double a, Short b) {
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
	@Inline(value = "($1 + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(double a, AtomicInteger b) {
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
	@Inline(value = "($1 + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(double a, AtomicLong b) {
		return a + b.doubleValue();
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
	public static double operator_power(double a, Long b) {
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
	@Inline(value = "$3.pow($1, ($2).doubleValue())", imported = Math.class)
	public static double operator_power(double a, Byte b) {
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
	@Inline(value = "$3.pow($1, ($2).doubleValue())", imported = Math.class)
	public static double operator_power(double a, Float b) {
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
	@Inline(value = "$3.pow($1, ($2).doubleValue())", imported = Math.class)
	public static double operator_power(double a, Integer b) {
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
	@Inline(value = "$3.pow($1, ($2).doubleValue())", imported = Math.class)
	public static double operator_power(double a, Double b) {
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
	@Inline(value = "$3.pow($1, ($2).doubleValue())", imported = Math.class)
	public static double operator_power(double a, Short b) {
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
	@Inline(value = "$3.pow($1, ($2).doubleValue())", imported = Math.class)
	public static double operator_power(double a, AtomicInteger b) {
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
	@Inline(value = "$3.pow($1, ($2).doubleValue())", imported = Math.class)
	public static double operator_power(double a, AtomicLong b) {
		return Math.pow(a, b.doubleValue());
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
	public static boolean operator_greaterEqualsThan(double a, Long b) {
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
	@Inline(value = "($1 >= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(double a, Byte b) {
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
	@Inline(value = "($1 >= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(double a, Float b) {
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
	@Inline(value = "($1 >= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(double a, Integer b) {
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
	@Inline(value = "($1 >= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(double a, Double b) {
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
	@Inline(value = "($1 >= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(double a, Short b) {
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
	@Inline(value = "($1 >= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(double a, AtomicInteger b) {
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
	@Inline(value = "($1 >= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(double a, AtomicLong b) {
		return a >= b.doubleValue();
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
	public static boolean operator_notEquals(double a, Long b) {
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
	@Inline(value = "($1 != ($2).doubleValue())", constantExpression=true)
	public static boolean operator_notEquals(double a, Byte b) {
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
	@Inline(value = "($1 != ($2).doubleValue())", constantExpression=true)
	public static boolean operator_notEquals(double a, Float b) {
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
	@Inline(value = "($1 != ($2).doubleValue())", constantExpression=true)
	public static boolean operator_notEquals(double a, Integer b) {
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
	@Inline(value = "($1 != ($2).doubleValue())", constantExpression=true)
	public static boolean operator_notEquals(double a, Double b) {
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
	@Inline(value = "($1 != ($2).doubleValue())", constantExpression=true)
	public static boolean operator_notEquals(double a, Short b) {
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
	@Inline(value = "($1 != ($2).doubleValue())", constantExpression=true)
	public static boolean operator_notEquals(double a, AtomicInteger b) {
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
	@Inline(value = "($1 != ($2).doubleValue())", constantExpression=true)
	public static boolean operator_notEquals(double a, AtomicLong b) {
		return a != b.doubleValue();
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
	public static double operator_divide(double a, Long b) {
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
	@Inline(value = "($1 / ($2).doubleValue())", constantExpression=true)
	public static double operator_divide(double a, Byte b) {
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
	@Inline(value = "($1 / ($2).doubleValue())", constantExpression=true)
	public static double operator_divide(double a, Float b) {
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
	@Inline(value = "($1 / ($2).doubleValue())", constantExpression=true)
	public static double operator_divide(double a, Integer b) {
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
	@Inline(value = "($1 / ($2).doubleValue())", constantExpression=true)
	public static double operator_divide(double a, Double b) {
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
	@Inline(value = "($1 / ($2).doubleValue())", constantExpression=true)
	public static double operator_divide(double a, Short b) {
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
	@Inline(value = "($1 / ($2).doubleValue())", constantExpression=true)
	public static double operator_divide(double a, AtomicInteger b) {
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
	@Inline(value = "($1 / ($2).doubleValue())", constantExpression=true)
	public static double operator_divide(double a, AtomicLong b) {
		return a / b.doubleValue();
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
	public static boolean operator_lessEqualsThan(double a, Long b) {
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
	@Inline(value = "($1 <= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(double a, Byte b) {
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
	@Inline(value = "($1 <= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(double a, Float b) {
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
	@Inline(value = "($1 <= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(double a, Integer b) {
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
	@Inline(value = "($1 <= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(double a, Double b) {
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
	@Inline(value = "($1 <= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(double a, Short b) {
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
	@Inline(value = "($1 <= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(double a, AtomicInteger b) {
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
	@Inline(value = "($1 <= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(double a, AtomicLong b) {
		return a <= b.doubleValue();
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
	public static double operator_multiply(double a, Long b) {
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
	@Inline(value = "($1 * ($2).doubleValue())", constantExpression=true)
	public static double operator_multiply(double a, Byte b) {
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
	@Inline(value = "($1 * ($2).doubleValue())", constantExpression=true)
	public static double operator_multiply(double a, Float b) {
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
	@Inline(value = "($1 * ($2).doubleValue())", constantExpression=true)
	public static double operator_multiply(double a, Integer b) {
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
	@Inline(value = "($1 * ($2).doubleValue())", constantExpression=true)
	public static double operator_multiply(double a, Double b) {
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
	@Inline(value = "($1 * ($2).doubleValue())", constantExpression=true)
	public static double operator_multiply(double a, Short b) {
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
	@Inline(value = "($1 * ($2).doubleValue())", constantExpression=true)
	public static double operator_multiply(double a, AtomicInteger b) {
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
	@Inline(value = "($1 * ($2).doubleValue())", constantExpression=true)
	public static double operator_multiply(double a, AtomicLong b) {
		return a * b.doubleValue();
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
	public static boolean operator_equals(double a, Long b) {
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
	@Inline(value = "($1 == ($2).doubleValue())", constantExpression=true)
	public static boolean operator_equals(double a, Byte b) {
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
	@Inline(value = "($1 == ($2).doubleValue())", constantExpression=true)
	public static boolean operator_equals(double a, Float b) {
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
	@Inline(value = "($1 == ($2).doubleValue())", constantExpression=true)
	public static boolean operator_equals(double a, Integer b) {
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
	@Inline(value = "($1 == ($2).doubleValue())", constantExpression=true)
	public static boolean operator_equals(double a, Double b) {
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
	@Inline(value = "($1 == ($2).doubleValue())", constantExpression=true)
	public static boolean operator_equals(double a, Short b) {
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
	@Inline(value = "($1 == ($2).doubleValue())", constantExpression=true)
	public static boolean operator_equals(double a, AtomicInteger b) {
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
	@Inline(value = "($1 == ($2).doubleValue())", constantExpression=true)
	public static boolean operator_equals(double a, AtomicLong b) {
		return a == b.doubleValue();
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
	public static boolean operator_greaterThan(double a, Long b) {
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
	@Inline(value = "($1 > ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterThan(double a, Byte b) {
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
	@Inline(value = "($1 > ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterThan(double a, Float b) {
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
	@Inline(value = "($1 > ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterThan(double a, Integer b) {
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
	@Inline(value = "($1 > ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterThan(double a, Double b) {
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
	@Inline(value = "($1 > ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterThan(double a, Short b) {
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
	@Inline(value = "($1 > ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterThan(double a, AtomicInteger b) {
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
	@Inline(value = "($1 > ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterThan(double a, AtomicLong b) {
		return a > b.doubleValue();
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
	public static double operator_modulo(double a, Long b) {
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
	@Inline(value = "($1 % ($2).doubleValue())", constantExpression=true)
	public static double operator_modulo(double a, Byte b) {
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
	@Inline(value = "($1 % ($2).doubleValue())", constantExpression=true)
	public static double operator_modulo(double a, Float b) {
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
	@Inline(value = "($1 % ($2).doubleValue())", constantExpression=true)
	public static double operator_modulo(double a, Integer b) {
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
	@Inline(value = "($1 % ($2).doubleValue())", constantExpression=true)
	public static double operator_modulo(double a, Double b) {
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
	@Inline(value = "($1 % ($2).doubleValue())", constantExpression=true)
	public static double operator_modulo(double a, Short b) {
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
	@Inline(value = "($1 % ($2).doubleValue())", constantExpression=true)
	public static double operator_modulo(double a, AtomicInteger b) {
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
	@Inline(value = "($1 % ($2).doubleValue())", constantExpression=true)
	public static double operator_modulo(double a, AtomicLong b) {
		return a % b.doubleValue();
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
	public static double operator_minus(double a, Long b) {
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
	@Inline(value = "($1 - ($2).doubleValue())", constantExpression=true)
	public static double operator_minus(double a, Byte b) {
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
	@Inline(value = "($1 - ($2).doubleValue())", constantExpression=true)
	public static double operator_minus(double a, Float b) {
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
	@Inline(value = "($1 - ($2).doubleValue())", constantExpression=true)
	public static double operator_minus(double a, Integer b) {
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
	@Inline(value = "($1 - ($2).doubleValue())", constantExpression=true)
	public static double operator_minus(double a, Double b) {
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
	@Inline(value = "($1 - ($2).doubleValue())", constantExpression=true)
	public static double operator_minus(double a, Short b) {
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
	@Inline(value = "($1 - ($2).doubleValue())", constantExpression=true)
	public static double operator_minus(double a, AtomicInteger b) {
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
	@Inline(value = "($1 - ($2).doubleValue())", constantExpression=true)
	public static double operator_minus(double a, AtomicLong b) {
		return a - b.doubleValue();
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
	public static boolean operator_lessThan(double a, Long b) {
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
	@Inline(value = "($1 < ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessThan(double a, Byte b) {
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
	@Inline(value = "($1 < ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessThan(double a, Float b) {
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
	@Inline(value = "($1 < ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessThan(double a, Integer b) {
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
	@Inline(value = "($1 < ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessThan(double a, Double b) {
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
	@Inline(value = "($1 < ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessThan(double a, Short b) {
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
	@Inline(value = "($1 < ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessThan(double a, AtomicInteger b) {
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
	@Inline(value = "($1 < ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessThan(double a, AtomicLong b) {
		return a < b.doubleValue();
	}

	// END GENERATED BLOCK

}
