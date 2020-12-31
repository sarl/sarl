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

/** Provide static comparison operators for numbers of type {@code Number}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("checkstyle:methodcount")
public final class NumberComparisonExtensions {

	private NumberComparisonExtensions() {
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
	@Inline(value = "($1.doubleValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Number left, long right) {
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
	public static boolean operator_greaterEqualsThan(Number left, byte right) {
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
	public static boolean operator_greaterEqualsThan(Number left, int right) {
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
	public static boolean operator_greaterEqualsThan(Number left, short right) {
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
	public static boolean operator_greaterEqualsThan(Number left, double right) {
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
	public static boolean operator_greaterEqualsThan(Number left, float right) {
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
	public static boolean operator_greaterEqualsThan(Number left, Number right) {
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
	@Inline(value = "($1.doubleValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Number left, long right) {
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
	public static boolean operator_lessEqualsThan(Number left, byte right) {
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
	public static boolean operator_lessEqualsThan(Number left, int right) {
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
	public static boolean operator_lessEqualsThan(Number left, short right) {
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
	public static boolean operator_lessEqualsThan(Number left, double right) {
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
	public static boolean operator_lessEqualsThan(Number left, float right) {
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
	public static boolean operator_lessEqualsThan(Number left, Number right) {
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
	@Inline(value = "($1.doubleValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Number left, long right) {
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
	public static boolean operator_greaterThan(Number left, byte right) {
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
	public static boolean operator_greaterThan(Number left, int right) {
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
	public static boolean operator_greaterThan(Number left, short right) {
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
	public static boolean operator_greaterThan(Number left, double right) {
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
	public static boolean operator_greaterThan(Number left, float right) {
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
	public static boolean operator_greaterThan(Number left, Number right) {
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
	@Inline(value = "($1.doubleValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(Number left, long right) {
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
	public static boolean operator_lessThan(Number left, byte right) {
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
	public static boolean operator_lessThan(Number left, int right) {
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
	public static boolean operator_lessThan(Number left, short right) {
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
	public static boolean operator_lessThan(Number left, double right) {
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
	public static boolean operator_lessThan(Number left, float right) {
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
	public static boolean operator_lessThan(Number left, Number right) {
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
	@Inline(value = "($1 != null && ($1.doubleValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Number left, long right) {
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
	@Inline(value = "($1 != null && ($1.doubleValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Number left, byte right) {
		return left != null && left.doubleValue() == right;
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
	@Inline(value = "($1 != null && ($1.doubleValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Number left, int right) {
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
	@Inline(value = "($1 != null && ($1.doubleValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Number left, short right) {
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
	@Inline(value = "($1 != null && ($1.doubleValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Number left, double right) {
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
	@Inline(value = "($1 != null && ($1.doubleValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Number left, float right) {
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
	public static boolean operator_equals(Number left, Number right) {
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
	@Inline(value = "($1 == null || ($1.doubleValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Number left, long right) {
		return left != null && left.doubleValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.doubleValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Number left, byte right) {
		return left != null && left.doubleValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.doubleValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Number left, int right) {
		return left != null && left.doubleValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.doubleValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Number left, short right) {
		return left != null && left.doubleValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.doubleValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Number left, double right) {
		return left != null && left.doubleValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.doubleValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Number left, float right) {
		return left != null && left.doubleValue() != right;
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
	public static boolean operator_notEquals(Number left, Number right) {
		return left == null ? right != null : right == null || left.doubleValue() != right.doubleValue();
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
	@Inline(value = "$3.compare($1.doubleValue(), $2)", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, byte right) {
		return Double.compare(left.doubleValue(),  right);
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
	@Inline(value = "$3.compare($1.doubleValue(), $2)", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, short right) {
		return Double.compare(left.doubleValue(),  right);
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
	@Inline(value = "$3.compare($1.doubleValue(), $2)", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, int right) {
		return Double.compare(left.doubleValue(),  right);
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
	@Inline(value = "$3.compare($1.doubleValue(), $2)", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, long right) {
		return Double.compare(left.doubleValue(),  right);
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
	@Inline(value = "$3.compare($1.doubleValue(), $2)", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, float right) {
		return Double.compare(left.doubleValue(),  right);
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
	@Inline(value = "$3.compare($1.doubleValue(), $2)", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, double right) {
		return Double.compare(left.doubleValue(),  right);
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
	@Inline(value = "$3.compare($1.doubleValue(), $2.byteValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, Byte right) {
		return Double.compare(left.doubleValue(),  right.byteValue());
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
	@Inline(value = "$3.compare($1.doubleValue(), $2.shortValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, Short right) {
		return Double.compare(left.doubleValue(),  right.shortValue());
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
	@Inline(value = "$3.compare($1.doubleValue(), $2.intValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, Integer right) {
		return Double.compare(left.doubleValue(),  right.intValue());
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
	@Inline(value = "$3.compare($1.doubleValue(), $2.longValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, Long right) {
		return Double.compare(left.doubleValue(),  right.longValue());
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
	@Inline(value = "$3.compare($1.doubleValue(), $2.floatValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, Float right) {
		return Double.compare(left.doubleValue(),  right.floatValue());
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
	@Inline(value = "$3.compare($1.doubleValue(), $2.doubleValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, Double right) {
		return Double.compare(left.doubleValue(),  right.doubleValue());
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
	@Inline(value = "$3.compare($1.doubleValue(), $2.intValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, AtomicInteger right) {
		return Double.compare(left.doubleValue(),  right.intValue());
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
	@Inline(value = "$3.compare($1.doubleValue(), $2.longValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, AtomicLong right) {
		return Double.compare(left.doubleValue(),  right.longValue());
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
	@Inline(value = "$3.compare($1.doubleValue(), $2.doubleValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Number left, Number right) {
		return Double.compare(left.doubleValue(),  right.doubleValue());
	}

}
