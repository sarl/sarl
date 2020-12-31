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

package io.sarl.lang.scoping.extensions.numbers.comparison;

import java.util.concurrent.atomic.AtomicLong;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static comparison operators for numbers of type {@code AtomicLong}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("checkstyle:methodcount")
public final class AtomicLongComparisonExtensions {

	private AtomicLongComparisonExtensions() {
		//
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.longValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(AtomicLong left, long right) {
		return left.longValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.longValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(AtomicLong left, byte right) {
		return left.longValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.longValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(AtomicLong left, int right) {
		return left.longValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.longValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(AtomicLong left, short right) {
		return left.longValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(AtomicLong left, double right) {
		return left.doubleValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(AtomicLong left, float right) {
		return left.doubleValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() >= $2.doubleValue())", constantExpression = true)
	public static boolean operator_greaterEqualsThan(AtomicLong left, Number right) {
		return left.doubleValue() >= right.doubleValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.longValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(AtomicLong left, long right) {
		return left.longValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.longValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(AtomicLong left, byte right) {
		return left.longValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.longValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(AtomicLong left, int right) {
		return left.longValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.longValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(AtomicLong left, short right) {
		return left.longValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(AtomicLong left, double right) {
		return left.doubleValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(AtomicLong left, float right) {
		return left.doubleValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() <= $2.doubleValue())", constantExpression = true)
	public static boolean operator_lessEqualsThan(AtomicLong left, Number right) {
		return left.doubleValue() <= right.doubleValue();
	}

	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.longValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(AtomicLong left, long right) {
		return left.longValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.longValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(AtomicLong left, byte right) {
		return left.longValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.longValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(AtomicLong left, int right) {
		return left.longValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.longValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(AtomicLong left, short right) {
		return left.longValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(AtomicLong left, double right) {
		return left.doubleValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(AtomicLong left, float right) {
		return left.doubleValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() > $2.doubleValue())", constantExpression = true)
	public static boolean operator_greaterThan(AtomicLong left, Number right) {
		return left.doubleValue() > right.doubleValue();
	}


	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.longValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(AtomicLong left, long right) {
		return left.longValue() < right;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.longValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(AtomicLong left, byte right) {
		return left.longValue() < right;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.longValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(AtomicLong left, int right) {
		return left.longValue() < right;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.shortValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(AtomicLong left, short right) {
		return left.shortValue() < right;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(AtomicLong left, double right) {
		return left.doubleValue() < right;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(AtomicLong left, float right) {
		return left.doubleValue() < right;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() < $2.doubleValue())", constantExpression = true)
	public static boolean operator_lessThan(AtomicLong left, Number right) {
		return left.doubleValue() < right.doubleValue();
	}

	/**
	 * The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 * This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left==right}
	 */
	@Pure
	@Inline(value = "($1 != null ? ($1.longValue() == $2) : false)", constantExpression = true)
	public static boolean operator_equals(AtomicLong left, long right) {
		return left != null ? left.longValue() == right : false;
	}

	/**
	 * The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 * This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left==right}
	 */
	@Pure
	@Inline(value = "($1 != null ? ($1.longValue() == $2) : false)", constantExpression = true)
	public static boolean operator_equals(AtomicLong left, byte right) {
		return left != null ? left.longValue() == right : false;
	}

	/**
	 * The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 * This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left==right}
	 */
	@Pure
	@Inline(value = "($1 != null ? ($1.longValue() == $2) : false)", constantExpression = true)
	public static boolean operator_equals(AtomicLong left, int right) {
		return left != null ? left.longValue() == right : false;
	}

	/**
	 * The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 * This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left==right}
	 */
	@Pure
	@Inline(value = "($1 != null ? ($1.longValue() == $2) : false)", constantExpression = true)
	public static boolean operator_equals(AtomicLong left, short right) {
		return left != null ? left.longValue() == right : false;
	}

	/**
	 * The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 * This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left==right}
	 */
	@Pure
	@Inline(value = "($1 != null ? ($1.doubleValue() == $2) : false)", constantExpression = true)
	public static boolean operator_equals(AtomicLong left, double right) {
		return left != null ? left.doubleValue() == right : false;
	}

	/**
	 * The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 * This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left==right}
	 */
	@Pure
	@Inline(value = "($1 != null ? ($1.doubleValue() == $2) : false)", constantExpression = true)
	public static boolean operator_equals(AtomicLong left, float right) {
		return left != null ? left.doubleValue() == right : false;
	}

	/**
	 * The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 * This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left==right}
	 */
	@Pure
	@Inline(value = "($1 == null ? ($2 == null) : ($2 != null && $1.doubleValue() == $2.doubleValue()))", constantExpression = true)
	public static boolean operator_equals(AtomicLong left, Number right) {
		return left == null ? right == null : right != null && left.doubleValue() == right.doubleValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null ? false : ($1.longValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(AtomicLong left, long right) {
		return left == null ? false : left.longValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null ? false : ($1.longValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(AtomicLong left, byte right) {
		return left == null ? false : left.longValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null ? false : ($1.longValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(AtomicLong left, int right) {
		return left == null ? false : left.longValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null ? false : ($1.longValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(AtomicLong left, short right) {
		return left == null ? false : left.longValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null ? false : ($1.doubleValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(AtomicLong left, double right) {
		return left == null ? false : left.doubleValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null ? false : ($1.doubleValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(AtomicLong left, float right) {
		return left == null ? false : left.doubleValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null ? ($2 != null) : ($2 == null || $1.doubleValue() != $2.doubleValue()))", constantExpression = true)
	public static boolean operator_notEquals(AtomicLong left, Number right) {
		return left == null ? right != null : right == null || left.doubleValue() != right.doubleValue();
	}

}
