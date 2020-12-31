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

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

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
	@Inline(value = "($1.shortValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Short left, long right) {
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
	@Inline(value = "($1.shortValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Short left, int right) {
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
	@Inline(value = "($1.shortValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Short left, double right) {
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
	@Inline(value = "($1.shortValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Short left, float right) {
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
	@Inline(value = "($1.shortValue() >= $2.doubleValue())", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Short left, Number right) {
		return left.shortValue() >= right.doubleValue();
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
	public static boolean operator_lessEqualsThan(Short left, long right) {
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
	@Inline(value = "($1.shortValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Short left, int right) {
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
	@Inline(value = "($1.shortValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Short left, double right) {
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
	@Inline(value = "($1.shortValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Short left, float right) {
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
	@Inline(value = "($1.shortValue() <= $2.doubleValue())", constantExpression = true)
	public static boolean operator_lessEqualsThan(Short left, Number right) {
		return left.shortValue() <= right.doubleValue();
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
	public static boolean operator_greaterThan(Short left, long right) {
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
	@Inline(value = "($1.shortValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Short left, int right) {
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
	@Inline(value = "($1.shortValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Short left, double right) {
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
	@Inline(value = "($1.shortValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Short left, float right) {
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
	@Inline(value = "($1.shortValue() > $2.doubleValue())", constantExpression = true)
	public static boolean operator_greaterThan(Short left, Number right) {
		return left.shortValue() > right.doubleValue();
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
	public static boolean operator_lessThan(Short left, long right) {
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
	@Inline(value = "($1.shortValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(Short left, int right) {
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
	@Inline(value = "($1.shortValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(Short left, double right) {
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
	@Inline(value = "($1.shortValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(Short left, float right) {
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
	@Inline(value = "($1.shortValue() < $2.doubleValue())", constantExpression = true)
	public static boolean operator_lessThan(Short left, Number right) {
		return left.shortValue() < right.doubleValue();
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
	@Inline(value = "($1 != null && ($1.shortValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Short left, long right) {
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
	@Inline(value = "($1 != null && ($1.shortValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Short left, byte right) {
		return left != null && left.shortValue() == right;
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
	@Inline(value = "($1 != null && ($1.shortValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Short left, int right) {
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
	@Inline(value = "($1 != null && ($1.shortValue() == $2))", constantExpression = true)
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
	@Inline(value = "($1 != null && ($1.shortValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Short left, double right) {
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
	@Inline(value = "($1 != null && ($1.shortValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Short left, float right) {
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
	@Inline(value = "($1 == null ? ($2 == null) : ($2 != null && $1.shortValue() == $2.doubleValue()))", constantExpression = true)
	public static boolean operator_equals(Short left, Number right) {
		return left == null ? right == null : right != null && left.shortValue() == right.doubleValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.shortValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Short left, long right) {
		return left != null && left.shortValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.shortValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Short left, byte right) {
		return left != null && left.shortValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.shortValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Short left, int right) {
		return left != null && left.shortValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.shortValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Short left, short right) {
		return left != null && left.shortValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.shortValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Short left, double right) {
		return left != null && left.shortValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.shortValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Short left, float right) {
		return left != null && left.shortValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null ? ($2 != null) : ($2 == null || $1.shortValue() != $2.doubleValue()))", constantExpression = true)
	public static boolean operator_notEquals(Short left, Number right) {
		return left == null ? right != null : right == null || left.shortValue() != right.doubleValue();
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2)", constantExpression = true, imported = Short.class)
	public static int operator_spaceship(Short left, byte right) {
		return Short.compare(left.shortValue(),  right);
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2)", constantExpression = true, imported = Short.class)
	public static int operator_spaceship(Short left, short right) {
		return Short.compare(left.shortValue(),  right);
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2)", constantExpression = true, imported = Integer.class)
	public static int operator_spaceship(Short left, int right) {
		return Integer.compare(left.shortValue(),  right);
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2)", constantExpression = true, imported = Long.class)
	public static int operator_spaceship(Short left, long right) {
		return Long.compare(left.shortValue(),  right);
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2)", constantExpression = true, imported = Float.class)
	public static int operator_spaceship(Short left, float right) {
		return Float.compare(left.shortValue(),  right);
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2)", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Short left, double right) {
		return Double.compare(left.shortValue(),  right);
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2.byteValue())", constantExpression = true, imported = Short.class)
	public static int operator_spaceship(Short left, Byte right) {
		return Short.compare(left.shortValue(),  right.byteValue());
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2.shortValue())", constantExpression = true, imported = Short.class)
	public static int operator_spaceship(Short left, Short right) {
		return Short.compare(left.shortValue(),  right.shortValue());
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2.intValue())", constantExpression = true, imported = Integer.class)
	public static int operator_spaceship(Short left, Integer right) {
		return Integer.compare(left.shortValue(),  right.intValue());
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2.longValue())", constantExpression = true, imported = Long.class)
	public static int operator_spaceship(Short left, Long right) {
		return Long.compare(left.shortValue(),  right.longValue());
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2.floatValue())", constantExpression = true, imported = Float.class)
	public static int operator_spaceship(Short left, Float right) {
		return Float.compare(left.shortValue(),  right.floatValue());
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2.doubleValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Short left, Double right) {
		return Double.compare(left.shortValue(),  right.doubleValue());
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2.intValue())", constantExpression = true, imported = Integer.class)
	public static int operator_spaceship(Short left, AtomicInteger right) {
		return Integer.compare(left.shortValue(),  right.intValue());
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2.longValue())", constantExpression = true, imported = Long.class)
	public static int operator_spaceship(Short left, AtomicLong right) {
		return Long.compare(left.shortValue(),  right.longValue());
	}

	/** The number comparison operator. This is equivalent to the Java
	 * {@code compareTo} function on numbers. This function is null-safe.
	 *
	 * @param left a number
	 * @param right a number.
	 * @return the value {@code 0} if {@code left == right};
     *         a value less than {@code 0} if {@code left < right}; and
     *         a value greater than {@code 0} if {@code left > right}.
	 */
	@Pure
	@Inline(value = "$3.compare($1.shortValue(), $2.doubleValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Short left, Number right) {
		return Double.compare(left.shortValue(),  right.doubleValue());
	}

}
