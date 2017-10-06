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
public final class AtomicLongExtensions {

	private AtomicLongExtensions() {
		//
	}

	// BEGIN GENERATED BLOCK

	/**
	 *The unary <code>minus</code> operator. This is the equivalent to the Java's <code>-</code> function.
	 *
	 * @param i a number.
	 * @return   <code>-i</code>
	 */
	@Pure
	@Inline(value = "(-($1).longValue())", constantExpression=true)
	public static long operator_minus(AtomicLong i) {
		return -i.longValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, int b) {
		return a.longValue() != b;
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, long b) {
		return a.longValue() != b;
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, short b) {
		return a.longValue() != b;
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, byte b) {
		return a.longValue() != b;
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, double b) {
		return a.doubleValue() != b;
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() != $2)", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, float b) {
		return a.floatValue() != b;
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, Short b) {
		return a.longValue() != b.longValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, Integer b) {
		return a.longValue() != b.longValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() != ($2).floatValue())", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, Float b) {
		return a.floatValue() != b.floatValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, Long b) {
		return a.longValue() != b.longValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, Byte b) {
		return a.longValue() != b.longValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() != ($2).doubleValue())", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, Double b) {
		return a.doubleValue() != b.doubleValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, AtomicInteger b) {
		return a.longValue() != b.longValue();
	}

	/**
	 *The binary <code>notEquals</code> operator. This is the equivalent to the Java <code>!=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a!=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() != ($2).longValue())", constantExpression=true)
	public static boolean operator_notEquals(AtomicLong a, AtomicLong b) {
		return a.longValue() != b.longValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() / $2)", constantExpression=true)
	public static long operator_divide(AtomicLong a, int b) {
		return a.longValue() / b;
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() / $2)", constantExpression=true)
	public static long operator_divide(AtomicLong a, long b) {
		return a.longValue() / b;
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() / $2)", constantExpression=true)
	public static long operator_divide(AtomicLong a, short b) {
		return a.longValue() / b;
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() / $2)", constantExpression=true)
	public static long operator_divide(AtomicLong a, byte b) {
		return a.longValue() / b;
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() / $2)", constantExpression=true)
	public static double operator_divide(AtomicLong a, double b) {
		return a.doubleValue() / b;
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() / $2)", constantExpression=true)
	public static float operator_divide(AtomicLong a, float b) {
		return a.floatValue() / b;
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() / ($2).longValue())", constantExpression=true)
	public static long operator_divide(AtomicLong a, Short b) {
		return a.longValue() / b.longValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() / ($2).longValue())", constantExpression=true)
	public static long operator_divide(AtomicLong a, Integer b) {
		return a.longValue() / b.longValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() / ($2).floatValue())", constantExpression=true)
	public static float operator_divide(AtomicLong a, Float b) {
		return a.floatValue() / b.floatValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() / ($2).longValue())", constantExpression=true)
	public static long operator_divide(AtomicLong a, Long b) {
		return a.longValue() / b.longValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() / ($2).longValue())", constantExpression=true)
	public static long operator_divide(AtomicLong a, Byte b) {
		return a.longValue() / b.longValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() / ($2).doubleValue())", constantExpression=true)
	public static double operator_divide(AtomicLong a, Double b) {
		return a.doubleValue() / b.doubleValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() / ($2).longValue())", constantExpression=true)
	public static long operator_divide(AtomicLong a, AtomicInteger b) {
		return a.longValue() / b.longValue();
	}

	/**
	 *The binary <code>divide</code> operator. This is the equivalent to the Java <code>/</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a/b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() / ($2).longValue())", constantExpression=true)
	public static long operator_divide(AtomicLong a, AtomicLong b) {
		return a.longValue() / b.longValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() == $2)", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, int b) {
		return a.longValue() == b;
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() == $2)", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, long b) {
		return a.longValue() == b;
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() == $2)", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, short b) {
		return a.longValue() == b;
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() == $2)", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, byte b) {
		return a.longValue() == b;
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() == $2)", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, double b) {
		return a.doubleValue() == b;
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() == $2)", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, float b) {
		return a.floatValue() == b;
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, Short b) {
		return a.longValue() == b.longValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, Integer b) {
		return a.longValue() == b.longValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() == ($2).floatValue())", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, Float b) {
		return a.floatValue() == b.floatValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, Long b) {
		return a.longValue() == b.longValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, Byte b) {
		return a.longValue() == b.longValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() == ($2).doubleValue())", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, Double b) {
		return a.doubleValue() == b.doubleValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, AtomicInteger b) {
		return a.longValue() == b.longValue();
	}

	/**
	 *The binary <code>equals</code> operator. This is the equivalent to the Java <code>==</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a==b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() == ($2).longValue())", constantExpression=true)
	public static boolean operator_equals(AtomicLong a, AtomicLong b) {
		return a.longValue() == b.longValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() * $2)", constantExpression=true)
	public static long operator_multiply(AtomicLong a, int b) {
		return a.longValue() * b;
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() * $2)", constantExpression=true)
	public static long operator_multiply(AtomicLong a, long b) {
		return a.longValue() * b;
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() * $2)", constantExpression=true)
	public static long operator_multiply(AtomicLong a, short b) {
		return a.longValue() * b;
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() * $2)", constantExpression=true)
	public static long operator_multiply(AtomicLong a, byte b) {
		return a.longValue() * b;
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() * $2)", constantExpression=true)
	public static double operator_multiply(AtomicLong a, double b) {
		return a.doubleValue() * b;
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() * $2)", constantExpression=true)
	public static float operator_multiply(AtomicLong a, float b) {
		return a.floatValue() * b;
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(AtomicLong a, Short b) {
		return a.longValue() * b.longValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(AtomicLong a, Integer b) {
		return a.longValue() * b.longValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() * ($2).floatValue())", constantExpression=true)
	public static float operator_multiply(AtomicLong a, Float b) {
		return a.floatValue() * b.floatValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(AtomicLong a, Long b) {
		return a.longValue() * b.longValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(AtomicLong a, Byte b) {
		return a.longValue() * b.longValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() * ($2).doubleValue())", constantExpression=true)
	public static double operator_multiply(AtomicLong a, Double b) {
		return a.doubleValue() * b.doubleValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(AtomicLong a, AtomicInteger b) {
		return a.longValue() * b.longValue();
	}

	/**
	 *The binary <code>multiply</code> operator. This is the equivalent to the Java <code>*</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a*b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() * ($2).longValue())", constantExpression=true)
	public static long operator_multiply(AtomicLong a, AtomicLong b) {
		return a.longValue() * b.longValue();
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), $2)", imported = Math.class)
	public static double operator_power(AtomicLong a, int b) {
		return Math.pow(a.longValue(), b);
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), $2)", imported = Math.class)
	public static double operator_power(AtomicLong a, long b) {
		return Math.pow(a.longValue(), b);
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), $2)", imported = Math.class)
	public static double operator_power(AtomicLong a, short b) {
		return Math.pow(a.longValue(), b);
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), $2)", imported = Math.class)
	public static double operator_power(AtomicLong a, byte b) {
		return Math.pow(a.longValue(), b);
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).doubleValue(), $2)", imported = Math.class)
	public static double operator_power(AtomicLong a, double b) {
		return Math.pow(a.doubleValue(), b);
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).floatValue(), $2)", imported = Math.class)
	public static double operator_power(AtomicLong a, float b) {
		return Math.pow(a.floatValue(), b);
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), ($2).longValue())", imported = Math.class)
	public static double operator_power(AtomicLong a, Short b) {
		return Math.pow(a.longValue(), b.longValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), ($2).longValue())", imported = Math.class)
	public static double operator_power(AtomicLong a, Integer b) {
		return Math.pow(a.longValue(), b.longValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).floatValue(), ($2).floatValue())", imported = Math.class)
	public static double operator_power(AtomicLong a, Float b) {
		return Math.pow(a.floatValue(), b.floatValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), ($2).longValue())", imported = Math.class)
	public static double operator_power(AtomicLong a, Long b) {
		return Math.pow(a.longValue(), b.longValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), ($2).longValue())", imported = Math.class)
	public static double operator_power(AtomicLong a, Byte b) {
		return Math.pow(a.longValue(), b.longValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).doubleValue(), ($2).doubleValue())", imported = Math.class)
	public static double operator_power(AtomicLong a, Double b) {
		return Math.pow(a.doubleValue(), b.doubleValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), ($2).longValue())", imported = Math.class)
	public static double operator_power(AtomicLong a, AtomicInteger b) {
		return Math.pow(a.longValue(), b.longValue());
	}

	/**
	 *The binary <code>power</code> operator. This is the equivalent to the Java's <code>Math.pow()</code> function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>Math.pow(a, b)</code>
	 */
	@Pure
	@Inline(value = "$3.pow(($1).longValue(), ($2).longValue())", imported = Math.class)
	public static double operator_power(AtomicLong a, AtomicLong b) {
		return Math.pow(a.longValue(), b.longValue());
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, int b) {
		return a.longValue() < b;
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, long b) {
		return a.longValue() < b;
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, short b) {
		return a.longValue() < b;
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, byte b) {
		return a.longValue() < b;
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, double b) {
		return a.doubleValue() < b;
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() < $2)", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, float b) {
		return a.floatValue() < b;
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, Short b) {
		return a.longValue() < b.longValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, Integer b) {
		return a.longValue() < b.longValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() < ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, Float b) {
		return a.floatValue() < b.floatValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, Long b) {
		return a.longValue() < b.longValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, Byte b) {
		return a.longValue() < b.longValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() < ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, Double b) {
		return a.doubleValue() < b.doubleValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, AtomicInteger b) {
		return a.longValue() < b.longValue();
	}

	/**
	 *The binary <code>lessThan</code> operator. This is the equivalent to the Java <code>&lt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() < ($2).longValue())", constantExpression=true)
	public static boolean operator_lessThan(AtomicLong a, AtomicLong b) {
		return a.longValue() < b.longValue();
	}

	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, int b) {
		return a.longValue() > b;
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, long b) {
		return a.longValue() > b;
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, short b) {
		return a.longValue() > b;
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, byte b) {
		return a.longValue() > b;
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, double b) {
		return a.doubleValue() > b;
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() > $2)", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, float b) {
		return a.floatValue() > b;
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, Short b) {
		return a.longValue() > b.longValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, Integer b) {
		return a.longValue() > b.longValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() > ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, Float b) {
		return a.floatValue() > b.floatValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, Long b) {
		return a.longValue() > b.longValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, Byte b) {
		return a.longValue() > b.longValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() > ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, Double b) {
		return a.doubleValue() > b.doubleValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, AtomicInteger b) {
		return a.longValue() > b.longValue();
	}


	/**
	 *The binary <code>greaterThan</code> operator. This is the equivalent to the Java <code>&gt;</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() > ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterThan(AtomicLong a, AtomicLong b) {
		return a.longValue() > b.longValue();
	}


	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, int b) {
		return a.longValue() <= b;
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, long b) {
		return a.longValue() <= b;
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, short b) {
		return a.longValue() <= b;
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, byte b) {
		return a.longValue() <= b;
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, double b) {
		return a.doubleValue() <= b;
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() <= $2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, float b) {
		return a.floatValue() <= b;
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, Short b) {
		return a.longValue() <= b.longValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, Integer b) {
		return a.longValue() <= b.longValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() <= ($2).floatValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, Float b) {
		return a.floatValue() <= b.floatValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, Long b) {
		return a.longValue() <= b.longValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, Byte b) {
		return a.longValue() <= b.longValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() <= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, Double b) {
		return a.doubleValue() <= b.doubleValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, AtomicInteger b) {
		return a.longValue() <= b.longValue();
	}

	/**
	 *The binary <code>lessEqualsThan</code> operator. This is the equivalent to the Java <code>&lt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&lt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() <= ($2).longValue())", constantExpression=true)
	public static boolean operator_lessEqualsThan(AtomicLong a, AtomicLong b) {
		return a.longValue() <= b.longValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() % $2)", constantExpression=true)
	public static long operator_modulo(AtomicLong a, int b) {
		return a.longValue() % b;
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() % $2)", constantExpression=true)
	public static long operator_modulo(AtomicLong a, long b) {
		return a.longValue() % b;
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() % $2)", constantExpression=true)
	public static long operator_modulo(AtomicLong a, short b) {
		return a.longValue() % b;
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() % $2)", constantExpression=true)
	public static long operator_modulo(AtomicLong a, byte b) {
		return a.longValue() % b;
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() % $2)", constantExpression=true)
	public static double operator_modulo(AtomicLong a, double b) {
		return a.doubleValue() % b;
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() % $2)", constantExpression=true)
	public static float operator_modulo(AtomicLong a, float b) {
		return a.floatValue() % b;
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(AtomicLong a, Short b) {
		return a.longValue() % b.longValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(AtomicLong a, Integer b) {
		return a.longValue() % b.longValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() % ($2).floatValue())", constantExpression=true)
	public static float operator_modulo(AtomicLong a, Float b) {
		return a.floatValue() % b.floatValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(AtomicLong a, Long b) {
		return a.longValue() % b.longValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(AtomicLong a, Byte b) {
		return a.longValue() % b.longValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() % ($2).doubleValue())", constantExpression=true)
	public static double operator_modulo(AtomicLong a, Double b) {
		return a.doubleValue() % b.doubleValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(AtomicLong a, AtomicInteger b) {
		return a.longValue() % b.longValue();
	}

	/**
	 *The binary <code>modulo</code> operator. This is the equivalent to the Java <code>%</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a%b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() % ($2).longValue())", constantExpression=true)
	public static long operator_modulo(AtomicLong a, AtomicLong b) {
		return a.longValue() % b.longValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, int b) {
		return a.longValue() >= b;
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, long b) {
		return a.longValue() >= b;
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, short b) {
		return a.longValue() >= b;
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, byte b) {
		return a.longValue() >= b;
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, double b) {
		return a.doubleValue() >= b;
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() >= $2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, float b) {
		return a.floatValue() >= b;
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, Short b) {
		return a.longValue() >= b.longValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, Integer b) {
		return a.longValue() >= b.longValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() >= ($2).floatValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, Float b) {
		return a.floatValue() >= b.floatValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, Long b) {
		return a.longValue() >= b.longValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, Byte b) {
		return a.longValue() >= b.longValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() >= ($2).doubleValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, Double b) {
		return a.doubleValue() >= b.doubleValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, AtomicInteger b) {
		return a.longValue() >= b.longValue();
	}

	/**
	 *The binary <code>greaterEqualsThan</code> operator. This is the equivalent to the Java <code>&gt;=</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a&gt;=b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() >= ($2).longValue())", constantExpression=true)
	public static boolean operator_greaterEqualsThan(AtomicLong a, AtomicLong b) {
		return a.longValue() >= b.longValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() + $2)", constantExpression=true)
	public static long operator_plus(AtomicLong a, int b) {
		return a.longValue() + b;
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() + $2)", constantExpression=true)
	public static long operator_plus(AtomicLong a, long b) {
		return a.longValue() + b;
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() + $2)", constantExpression=true)
	public static long operator_plus(AtomicLong a, short b) {
		return a.longValue() + b;
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() + $2)", constantExpression=true)
	public static long operator_plus(AtomicLong a, byte b) {
		return a.longValue() + b;
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() + $2)", constantExpression=true)
	public static double operator_plus(AtomicLong a, double b) {
		return a.doubleValue() + b;
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() + $2)", constantExpression=true)
	public static float operator_plus(AtomicLong a, float b) {
		return a.floatValue() + b;
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() + ($2).longValue())", constantExpression=true)
	public static long operator_plus(AtomicLong a, Short b) {
		return a.longValue() + b.longValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() + ($2).longValue())", constantExpression=true)
	public static long operator_plus(AtomicLong a, Integer b) {
		return a.longValue() + b.longValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() + ($2).floatValue())", constantExpression=true)
	public static float operator_plus(AtomicLong a, Float b) {
		return a.floatValue() + b.floatValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() + ($2).longValue())", constantExpression=true)
	public static long operator_plus(AtomicLong a, Long b) {
		return a.longValue() + b.longValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() + ($2).longValue())", constantExpression=true)
	public static long operator_plus(AtomicLong a, Byte b) {
		return a.longValue() + b.longValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() + ($2).doubleValue())", constantExpression=true)
	public static double operator_plus(AtomicLong a, Double b) {
		return a.doubleValue() + b.doubleValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() + ($2).longValue())", constantExpression=true)
	public static long operator_plus(AtomicLong a, AtomicInteger b) {
		return a.longValue() + b.longValue();
	}

	/**
	 *The binary <code>plus</code> operator. This is the equivalent to the Java <code>+</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a+b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() + ($2).longValue())", constantExpression=true)
	public static long operator_plus(AtomicLong a, AtomicLong b) {
		return a.longValue() + b.longValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() - $2)", constantExpression=true)
	public static long operator_minus(AtomicLong a, int b) {
		return a.longValue() - b;
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() - $2)", constantExpression=true)
	public static long operator_minus(AtomicLong a, long b) {
		return a.longValue() - b;
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() - $2)", constantExpression=true)
	public static long operator_minus(AtomicLong a, short b) {
		return a.longValue() - b;
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() - $2)", constantExpression=true)
	public static long operator_minus(AtomicLong a, byte b) {
		return a.longValue() - b;
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() - $2)", constantExpression=true)
	public static double operator_minus(AtomicLong a, double b) {
		return a.doubleValue() - b;
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() - $2)", constantExpression=true)
	public static float operator_minus(AtomicLong a, float b) {
		return a.floatValue() - b;
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() - ($2).longValue())", constantExpression=true)
	public static long operator_minus(AtomicLong a, Short b) {
		return a.longValue() - b.longValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() - ($2).longValue())", constantExpression=true)
	public static long operator_minus(AtomicLong a, Integer b) {
		return a.longValue() - b.longValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).floatValue() - ($2).floatValue())", constantExpression=true)
	public static float operator_minus(AtomicLong a, Float b) {
		return a.floatValue() - b.floatValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() - ($2).longValue())", constantExpression=true)
	public static long operator_minus(AtomicLong a, Long b) {
		return a.longValue() - b.longValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() - ($2).longValue())", constantExpression=true)
	public static long operator_minus(AtomicLong a, Byte b) {
		return a.longValue() - b.longValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).doubleValue() - ($2).doubleValue())", constantExpression=true)
	public static double operator_minus(AtomicLong a, Double b) {
		return a.doubleValue() - b.doubleValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() - ($2).longValue())", constantExpression=true)
	public static long operator_minus(AtomicLong a, AtomicInteger b) {
		return a.longValue() - b.longValue();
	}

/**
	 *The binary <code>minus</code> operator. This is the equivalent to the Java <code>-</code> operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return   <code>a-b</code>
	 */
	@Pure
	@Inline(value = "(($1).longValue() - ($2).longValue())", constantExpression=true)
	public static long operator_minus(AtomicLong a, AtomicLong b) {
		return a.longValue() - b.longValue();
	}

	// END GENERATED BLOCK

}
