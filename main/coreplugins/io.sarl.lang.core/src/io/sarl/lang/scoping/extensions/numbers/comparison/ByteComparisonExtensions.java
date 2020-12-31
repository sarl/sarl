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

/** Provide static comparison operators for numbers of type {@code Byte}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("checkstyle:methodcount")
public final class ByteComparisonExtensions {

	private ByteComparisonExtensions() {
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
	@Inline(value = "($1.byteValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Byte left, long right) {
		return left.byteValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Byte left, byte right) {
		return left.byteValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Byte left, int right) {
		return left.byteValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Byte left, short right) {
		return left.byteValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Byte left, double right) {
		return left.byteValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() >= $2)", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Byte left, float right) {
		return left.byteValue() >= right;
	}

	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() >= $2.doubleValue())", constantExpression = true)
	public static boolean operator_greaterEqualsThan(Byte left, Number right) {
		return left.byteValue() >= right.doubleValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Byte left, long right) {
		return left.byteValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Byte left, byte right) {
		return left.byteValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Byte left, int right) {
		return left.byteValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Byte left, short right) {
		return left.byteValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Byte left, double right) {
		return left.byteValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() <= $2)", constantExpression = true)
	public static boolean operator_lessEqualsThan(Byte left, float right) {
		return left.byteValue() <= right;
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() <= $2.doubleValue())", constantExpression = true)
	public static boolean operator_lessEqualsThan(Byte left, Number right) {
		return left.byteValue() <= right.doubleValue();
	}

	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Byte left, long right) {
		return left.byteValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Byte left, byte right) {
		return left.byteValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Byte left, int right) {
		return left.byteValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Byte left, short right) {
		return left.byteValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Byte left, double right) {
		return left.byteValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() > $2)", constantExpression = true)
	public static boolean operator_greaterThan(Byte left, float right) {
		return left.byteValue() > right;
	}


	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() > $2.doubleValue())", constantExpression = true)
	public static boolean operator_greaterThan(Byte left, Number right) {
		return left.byteValue() > right.doubleValue();
	}


	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(Byte left, long right) {
		return left.byteValue() < right;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(Byte left, byte right) {
		return left.byteValue() < right;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(Byte left, int right) {
		return left.byteValue() < right;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(Byte left, short right) {
		return left.byteValue() < right;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(Byte left, double right) {
		return left.byteValue() < right;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() < $2)", constantExpression = true)
	public static boolean operator_lessThan(Byte left, float right) {
		return left.byteValue() < right;
	}

	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1.byteValue() < $2.doubleValue())", constantExpression = true)
	public static boolean operator_lessThan(Byte left, Number right) {
		return left.byteValue() < right.doubleValue();
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
	@Inline(value = "($1 != null && ($1.byteValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Byte left, long right) {
		return left != null ? left.byteValue() == right : false;
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
	@Inline(value = "($1 != null && ($1.byteValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Byte left, byte right) {
		return left != null && left.byteValue() == right;
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
	@Inline(value = "($1 != null && ($1.byteValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Byte left, int right) {
		return left != null ? left.byteValue() == right : false;
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
	@Inline(value = "($1 != null && ($1.byteValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Byte left, short right) {
		return left != null ? left.byteValue() == right : false;
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
	@Inline(value = "($1 != null && ($1.byteValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Byte left, double right) {
		return left != null ? left.byteValue() == right : false;
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
	@Inline(value = "($1 != null && ($1.byteValue() == $2))", constantExpression = true)
	public static boolean operator_equals(Byte left, float right) {
		return left != null ? left.byteValue() == right : false;
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
	@Inline(value = "($1 == null ? ($2 == null) : ($2 != null && $1.byteValue() == $2.doubleValue()))", constantExpression = true)
	public static boolean operator_equals(Byte left, Number right) {
		return left == null ? right == null : right != null && left.byteValue() == right.doubleValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.byteValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Byte left, long right) {
		return left != null && left.byteValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.byteValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Byte left, byte right) {
		return left != null && left.byteValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.byteValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Byte left, int right) {
		return left != null && left.byteValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.byteValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Byte left, short right) {
		return left != null && left.byteValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.byteValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Byte left, double right) {
		return left != null && left.byteValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null || ($1.byteValue() != $2))", constantExpression = true)
	public static boolean operator_notEquals(Byte left, float right) {
		return left != null && left.byteValue() != right;
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($1 == null ? ($2 != null) : ($2 == null || $1.byteValue() != $2.doubleValue()))", constantExpression = true)
	public static boolean operator_notEquals(Byte left, Number right) {
		return left == null ? right != null : right == null || left.byteValue() != right.doubleValue();
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
	@Inline(value = "$3.compare($1.byteValue(), $2)", constantExpression = true, imported = Byte.class)
	public static int operator_spaceship(Byte left, byte right) {
		return Byte.compare(left.byteValue(),  right);
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
	@Inline(value = "$3.compare($1.byteValue(), $2)", constantExpression = true, imported = Short.class)
	public static int operator_spaceship(Byte left, short right) {
		return Short.compare(left.byteValue(),  right);
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
	@Inline(value = "$3.compare($1.byteValue(), $2)", constantExpression = true, imported = Integer.class)
	public static int operator_spaceship(Byte left, int right) {
		return Integer.compare(left.byteValue(),  right);
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
	@Inline(value = "$3.compare($1.byteValue(), $2)", constantExpression = true, imported = Long.class)
	public static int operator_spaceship(Byte left, long right) {
		return Long.compare(left.byteValue(),  right);
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
	@Inline(value = "$3.compare($1.byteValue(), $2)", constantExpression = true, imported = Float.class)
	public static int operator_spaceship(Byte left, float right) {
		return Float.compare(left.byteValue(),  right);
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
	@Inline(value = "$3.compare($1.byteValue(), $2)", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Byte left, double right) {
		return Double.compare(left.byteValue(),  right);
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
	@Inline(value = "$3.compare($1.byteValue(), $2.byteValue())", constantExpression = true, imported = Byte.class)
	public static int operator_spaceship(Byte left, Byte right) {
		return Byte.compare(left.byteValue(),  right.byteValue());
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
	@Inline(value = "$3.compare($1.byteValue(), $2.shortValue())", constantExpression = true, imported = Short.class)
	public static int operator_spaceship(Byte left, Short right) {
		return Short.compare(left.byteValue(),  right.shortValue());
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
	@Inline(value = "$3.compare($1.byteValue(), $2.intValue())", constantExpression = true, imported = Integer.class)
	public static int operator_spaceship(Byte left, Integer right) {
		return Integer.compare(left.byteValue(),  right.intValue());
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
	@Inline(value = "$3.compare($1.byteValue(), $2.longValue())", constantExpression = true, imported = Long.class)
	public static int operator_spaceship(Byte left, Long right) {
		return Long.compare(left.byteValue(),  right.longValue());
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
	@Inline(value = "$3.compare($1.byteValue(), $2.floatValue())", constantExpression = true, imported = Float.class)
	public static int operator_spaceship(Byte left, Float right) {
		return Float.compare(left.byteValue(),  right.floatValue());
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
	@Inline(value = "$3.compare($1.byteValue(), $2.doubleValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Byte left, Double right) {
		return Double.compare(left.byteValue(),  right.doubleValue());
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
	@Inline(value = "$3.compare($1.byteValue(), $2.intValue())", constantExpression = true, imported = Integer.class)
	public static int operator_spaceship(Byte left, AtomicInteger right) {
		return Integer.compare(left.byteValue(),  right.intValue());
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
	@Inline(value = "$3.compare($1.byteValue(), $2.longValue())", constantExpression = true, imported = Long.class)
	public static int operator_spaceship(Byte left, AtomicLong right) {
		return Long.compare(left.byteValue(),  right.longValue());
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
	@Inline(value = "$3.compare($1.byteValue(), $2.doubleValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(Byte left, Number right) {
		return Double.compare(left.byteValue(),  right.doubleValue());
	}

}
