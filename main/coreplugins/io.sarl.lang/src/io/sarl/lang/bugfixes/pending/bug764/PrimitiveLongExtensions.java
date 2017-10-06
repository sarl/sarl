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

package io.sarl.lang.bugfixes.pending.bug764;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static operators for atomic numbers.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
public final class PrimitiveLongExtensions {

	private PrimitiveLongExtensions() {
		//
	}

	// BEGIN GENERATED BLOCK

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "($1 != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(long a, Short b) {
		return a != b.longValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "($1 != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(long a, Integer b) {
		return a != b.longValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "($1 != ($2).floatValue())", constantExpression=true)
	public static boolean operator_notEquals(long a, Float b) {
		return a != b.floatValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "($1 != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(long a, Long b) {
		return a != b.longValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "($1 != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(long a, Byte b) {
		return a != b.longValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "($1 != ($2).doubleValue())", constantExpression=true)
	public static boolean operator_notEquals(long a, Double b) {
		return a != b.doubleValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "($1 != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(long a, AtomicInteger b) {
		return a != b.longValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "($1 != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(long a, AtomicLong b) {
		return a != b.longValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "($1 / ($2).longValue())", constantExpression=true)
	public static long operator_divide(long a, Short b) {
		return a / b.longValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "($1 / ($2).longValue())", constantExpression=true)
	public static long operator_divide(long a, Integer b) {
		return a / b.longValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "($1 / ($2).floatValue())", constantExpression=true)
	public static float operator_divide(long a, Float b) {
		return a / b.floatValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "($1 / ($2).longValue())", constantExpression=true)
	public static long operator_divide(long a, Long b) {
		return a / b.longValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "($1 / ($2).longValue())", constantExpression=true)
	public static long operator_divide(long a, Byte b) {
		return a / b.longValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "($1 / ($2).doubleValue())", constantExpression=true)
	public static double operator_divide(long a, Double b) {
		return a / b.doubleValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "($1 / ($2).longValue())", constantExpression=true)
	public static long operator_divide(long a, AtomicInteger b) {
		return a / b.longValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "($1 / ($2).longValue())", constantExpression=true)
	public static long operator_divide(long a, AtomicLong b) {
		return a / b.longValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "($1 == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(long a, Short b) {
		return a == b.longValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "($1 == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(long a, Integer b) {
		return a == b.longValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "($1 == ($2).floatValue())", constantExpression=true)
	public static boolean operator_equals(long a, Float b) {
		return a == b.floatValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "($1 == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(long a, Long b) {
		return a == b.longValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "($1 == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(long a, Byte b) {
		return a == b.longValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "($1 == ($2).doubleValue())", constantExpression=true)
	public static boolean operator_equals(long a, Double b) {
		return a == b.doubleValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "($1 == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(long a, AtomicInteger b) {
		return a == b.longValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "($1 == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(long a, AtomicLong b) {
		return a == b.longValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "($1 * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(long a, Short b) {
		return a * b.longValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "($1 * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(long a, Integer b) {
		return a * b.longValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "($1 * ($2).floatValue())", constantExpression=true)
	public static float operator_multiply(long a, Float b) {
		return a * b.floatValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "($1 * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(long a, Long b) {
		return a * b.longValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "($1 * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(long a, Byte b) {
		return a * b.longValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "($1 * ($2).doubleValue())", constantExpression=true)
	public static double operator_multiply(long a, Double b) {
		return a * b.doubleValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "($1 * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(long a, AtomicInteger b) {
		return a * b.longValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "($1 * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(long a, AtomicLong b) {
		return a * b.longValue();
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).longValue())", imported = Math.class)
	public static double operator_power(long a, Short b) {
		return Math.pow(a, b.longValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).longValue())", imported = Math.class)
	public static double operator_power(long a, Integer b) {
		return Math.pow(a, b.longValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).floatValue())", imported = Math.class)
	public static double operator_power(long a, Float b) {
		return Math.pow(a, b.floatValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).longValue())", imported = Math.class)
	public static double operator_power(long a, Long b) {
		return Math.pow(a, b.longValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).longValue())", imported = Math.class)
	public static double operator_power(long a, Byte b) {
		return Math.pow(a, b.longValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).doubleValue())", imported = Math.class)
	public static double operator_power(long a, Double b) {
		return Math.pow(a, b.doubleValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).longValue())", imported = Math.class)
	public static double operator_power(long a, AtomicInteger b) {
		return Math.pow(a, b.longValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow($1, ($2).longValue())", imported = Math.class)
	public static double operator_power(long a, AtomicLong b) {
		return Math.pow(a, b.longValue());
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "($1 < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(long a, Short b) {
		return a < b.longValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "($1 < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(long a, Integer b) {
		return a < b.longValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "($1 < ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessThan(long a, Float b) {
		return a < b.floatValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "($1 < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(long a, Long b) {
		return a < b.longValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "($1 < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(long a, Byte b) {
		return a < b.longValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "($1 < ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessThan(long a, Double b) {
		return a < b.doubleValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "($1 < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(long a, AtomicInteger b) {
		return a < b.longValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "($1 < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(long a, AtomicLong b) {
		return a < b.longValue();
	}

	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "($1 > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(long a, Short b) {
		return a > b.longValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "($1 > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(long a, Integer b) {
		return a > b.longValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "($1 > ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterThan(long a, Float b) {
		return a > b.floatValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "($1 > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(long a, Long b) {
		return a > b.longValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "($1 > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(long a, Byte b) {
		return a > b.longValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "($1 > ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterThan(long a, Double b) {
		return a > b.doubleValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "($1 > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(long a, AtomicInteger b) {
		return a > b.longValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "($1 > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(long a, AtomicLong b) {
		return a > b.longValue();
	}


	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(long a, Short b) {
		return a <= b.longValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(long a, Integer b) {
		return a <= b.longValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 <= ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(long a, Float b) {
		return a <= b.floatValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(long a, Long b) {
		return a <= b.longValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(long a, Byte b) {
		return a <= b.longValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 <= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(long a, Double b) {
		return a <= b.doubleValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(long a, AtomicInteger b) {
		return a <= b.longValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(long a, AtomicLong b) {
		return a <= b.longValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "($1 % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(long a, Short b) {
		return a % b.longValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "($1 % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(long a, Integer b) {
		return a % b.longValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "($1 % ($2).floatValue())", constantExpression=true)
	public static float operator_modulo(long a, Float b) {
		return a % b.floatValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "($1 % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(long a, Long b) {
		return a % b.longValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "($1 % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(long a, Byte b) {
		return a % b.longValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "($1 % ($2).doubleValue())", constantExpression=true)
	public static double operator_modulo(long a, Double b) {
		return a % b.doubleValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "($1 % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(long a, AtomicInteger b) {
		return a % b.longValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "($1 % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(long a, AtomicLong b) {
		return a % b.longValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(long a, Short b) {
		return a >= b.longValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(long a, Integer b) {
		return a >= b.longValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 >= ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(long a, Float b) {
		return a >= b.floatValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(long a, Long b) {
		return a >= b.longValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(long a, Byte b) {
		return a >= b.longValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 >= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(long a, Double b) {
		return a >= b.doubleValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(long a, AtomicInteger b) {
		return a >= b.longValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "($1 >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(long a, AtomicLong b) {
		return a >= b.longValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "($1 + ($2).longValue())", constantExpression=true)
	public static long operator_plus(long a, Short b) {
		return a + b.longValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "($1 + ($2).longValue())", constantExpression=true)
	public static long operator_plus(long a, Integer b) {
		return a + b.longValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "($1 + ($2).floatValue())", constantExpression=true)
	public static float operator_plus(long a, Float b) {
		return a + b.floatValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "($1 + ($2).longValue())", constantExpression=true)
	public static long operator_plus(long a, Long b) {
		return a + b.longValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "($1 + ($2).longValue())", constantExpression=true)
	public static long operator_plus(long a, Byte b) {
		return a + b.longValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "($1 + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(long a, Double b) {
		return a + b.doubleValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "($1 + ($2).longValue())", constantExpression=true)
	public static long operator_plus(long a, AtomicInteger b) {
		return a + b.longValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "($1 + ($2).longValue())", constantExpression=true)
	public static long operator_plus(long a, AtomicLong b) {
		return a + b.longValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "($1 - ($2).longValue())", constantExpression=true)
	public static long operator_minus(long a, Short b) {
		return a - b.longValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "($1 - ($2).longValue())", constantExpression=true)
	public static long operator_minus(long a, Integer b) {
		return a - b.longValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "($1 - ($2).floatValue())", constantExpression=true)
	public static float operator_minus(long a, Float b) {
		return a - b.floatValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "($1 - ($2).longValue())", constantExpression=true)
	public static long operator_minus(long a, Long b) {
		return a - b.longValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "($1 - ($2).longValue())", constantExpression=true)
	public static long operator_minus(long a, Byte b) {
		return a - b.longValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "($1 - ($2).doubleValue())", constantExpression=true)
	public static double operator_minus(long a, Double b) {
		return a - b.doubleValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "($1 - ($2).longValue())", constantExpression=true)
	public static long operator_minus(long a, AtomicInteger b) {
		return a - b.longValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "($1 - ($2).longValue())", constantExpression=true)
	public static long operator_minus(long a, AtomicLong b) {
		return a - b.longValue();
	}

	// END GENERATED BLOCK

}
