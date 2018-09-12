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

package io.sarl.lang.scoping.numbers.comparison;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static comparison operators for numbers of type {@code Short}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("checkstyle:methodcount")
public final class ShortComparisonExtensions {

	private ShortComparisonExtensions() {
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
	public static boolean operator_greaterEqualsThan(Short left, long right) {
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
	@Inline(value = "($1.shortValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Short left, byte right) {
		return left.shortValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.intValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Short left, int right) {
		return left.intValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.shortValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Short left, short right) {
		return left.shortValue() >= right;
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
	public static boolean operator_greaterEqualsThan(Short left, double right) {
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
	@Inline(value = "($1.floatValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Short left, float right) {
		return left.floatValue() >= right;
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
	public static boolean operator_greaterEqualsThan(Short left, Number right) {
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
	public static boolean operator_lessEqualsThan(Short left, long right) {
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
	@Inline(value = "($1.shortValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Short left, byte right) {
		return left.shortValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.intValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Short left, int right) {
		return left.intValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.shortValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Short left, short right) {
		return left.shortValue() <= right;
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
	public static boolean operator_lessEqualsThan(Short left, double right) {
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
	@Inline(value = "($1.floatValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Short left, float right) {
		return left.floatValue() <= right;
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
	public static boolean operator_lessEqualsThan(Short left, Number right) {
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
	public static boolean operator_greaterThan(Short left, long right) {
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
	@Inline(value = "($1.shortValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Short left, byte right) {
		return left.shortValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.intValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Short left, int right) {
		return left.intValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.shortValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Short left, short right) {
		return left.shortValue() > right;
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
	public static boolean operator_greaterThan(Short left, double right) {
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
	@Inline(value = "($1.floatValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Short left, float right) {
		return left.floatValue() > right;
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
	public static boolean operator_greaterThan(Short left, Number right) {
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
	public static boolean operator_lessThan(Short left, long right) {
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
	public static boolean operator_lessThan(Short left, byte right) {
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
	@Inline(value = "($1.intValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(Short left, int right) {
		return left.intValue() < right;
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
	public static boolean operator_lessThan(Short left, short right) {
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
	public static boolean operator_lessThan(Short left, double right) {
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
	@Inline(value = "($1.floatValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(Short left, float right) {
		return left.floatValue() < right;
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
	public static boolean operator_lessThan(Short left, Number right) {
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
	public static boolean operator_equals(Short left, long right) {
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
	@Inline(value = "($1 != null ? ($1.shortValue() == $2) : false)", constantExpression = true)
	public static boolean operator_equals(Short left, byte right) {
		return left != null ? left.shortValue() == right : false;
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
	@Inline(value = "($1 != null ? ($1.intValue() == $2) : false)", constantExpression = true)
	public static boolean operator_equals(Short left, int right) {
		return left != null ? left.intValue() == right : false;
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
	@Inline(value = "($1 != null ? ($1.shortValue() == $2) : false)", constantExpression = true)
	public static boolean operator_equals(Short left, short right) {
		return left != null ? left.shortValue() == right : false;
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
	public static boolean operator_equals(Short left, double right) {
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
	@Inline(value = "($1 != null ? ($1.floatValue() == $2) : false)", constantExpression = true)
	public static boolean operator_equals(Short left, float right) {
		return left != null ? left.floatValue() == right : false;
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
	public static boolean operator_equals(Short left, Number right) {
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
	@Inline(value = "$1 == null ? false : ($1.longValue() != $2)", constantExpression = true)
	public static boolean operator_notEquals(Short left, long right) {
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
	@Inline(value = " $1 == null ? false : ($1.shortValue() != $2)", constantExpression = true)
	public static boolean operator_notEquals(Short left, byte right) {
		return left == null ? false : left.shortValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "$1 == null ? false : ($1.intValue() != $2)", constantExpression = true)
	public static boolean operator_notEquals(Short left, int right) {
		return left == null ? false : left.intValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "$1 == null ? false : ($1.shortValue() != $2)", constantExpression = true)
	public static boolean operator_notEquals(Short left, short right) {
		return left == null ? false : left.shortValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "$1 == null ? false : ($1.doubleValue() != $2)", constantExpression = true)
	public static boolean operator_notEquals(Short left, double right) {
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
	@Inline(value = "$1 == null ? false : ($1.floatValue() != $2)", constantExpression = true)
	public static boolean operator_notEquals(Short left, float right) {
		return left == null ? false : left.floatValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "$1 == null ? ($2 != null) : ($2 == null || $1.doubleValue() != $2.doubleValue())", constantExpression = true)
	public static boolean operator_notEquals(Short left, Number right) {
		return left == null ? right != null : right == null || left.doubleValue() != right.doubleValue();
	}

}
