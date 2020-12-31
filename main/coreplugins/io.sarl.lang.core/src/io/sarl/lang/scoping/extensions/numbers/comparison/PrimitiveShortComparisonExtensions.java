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

/** Provide static comparison operators for numbers of type {@code short}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("checkstyle:methodcount")
public final class PrimitiveShortComparisonExtensions {

	private PrimitiveShortComparisonExtensions() {
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
	@Inline(value = "($1 >= $2.doubleValue())", constantExpression = true)
	public static boolean operator_greaterEqualsThan(short left, Number right) {
		return left >= right.doubleValue();
	}

	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;=right}
	 */
	@Pure
	@Inline(value = "($1 <= $2.doubleValue())", constantExpression = true)
	public static boolean operator_lessEqualsThan(short left, Number right) {
		return left <= right.doubleValue();
	}

	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&gt;right}
	 */
	@Pure
	@Inline(value = "($1 > $2.doubleValue())", constantExpression = true)
	public static boolean operator_greaterThan(short left, Number right) {
		return left > right.doubleValue();
	}


	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left&lt;right}
	 */
	@Pure
	@Inline(value = "($1 < $2.doubleValue())", constantExpression = true)
	public static boolean operator_lessThan(short left, Number right) {
		return left < right.doubleValue();
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
	@Inline(value = "($2 != null && $1 == $2.doubleValue())", constantExpression = true)
	public static boolean operator_equals(short left, Number right) {
		return right != null && left == right.doubleValue();
	}

	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator. This function is null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left!=right}
	 */
	@Pure
	@Inline(value = "($2 == null || $1 != $2.doubleValue())", constantExpression = true)
	public static boolean operator_notEquals(short left, Number right) {
		return right == null || left != right.doubleValue();
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
	@Inline(value = "$3.compare($1, $2)", constantExpression = true, imported = Short.class)
	public static int operator_spaceship(short left, byte right) {
		return Short.compare(left,  right);
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
	@Inline(value = "$3.compare($1, $2)", constantExpression = true, imported = Short.class)
	public static int operator_spaceship(short left, short right) {
		return Short.compare(left,  right);
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
	@Inline(value = "$3.compare($1, $2)", constantExpression = true, imported = Integer.class)
	public static int operator_spaceship(short left, int right) {
		return Integer.compare(left,  right);
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
	@Inline(value = "$3.compare($1, $2)", constantExpression = true, imported = Long.class)
	public static int operator_spaceship(short left, long right) {
		return Long.compare(left,  right);
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
	@Inline(value = "$3.compare($1, $2)", constantExpression = true, imported = Float.class)
	public static int operator_spaceship(short left, float right) {
		return Float.compare(left,  right);
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
	@Inline(value = "$3.compare($1, $2)", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(short left, double right) {
		return Double.compare(left,  right);
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
	@Inline(value = "$3.compare($1, $2.byteValue())", constantExpression = true, imported = Short.class)
	public static int operator_spaceship(short left, Byte right) {
		return Short.compare(left,  right.byteValue());
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
	@Inline(value = "$3.compare($1, $2.shortValue())", constantExpression = true, imported = Short.class)
	public static int operator_spaceship(short left, Short right) {
		return Short.compare(left,  right.shortValue());
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
	@Inline(value = "$3.compare($1, $2.intValue())", constantExpression = true, imported = Integer.class)
	public static int operator_spaceship(short left, Integer right) {
		return Integer.compare(left,  right.intValue());
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
	@Inline(value = "$3.compare($1, $2.longValue())", constantExpression = true, imported = Long.class)
	public static int operator_spaceship(short left, Long right) {
		return Long.compare(left,  right.longValue());
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
	@Inline(value = "$3.compare($1, $2.floatValue())", constantExpression = true, imported = Float.class)
	public static int operator_spaceship(short left, Float right) {
		return Float.compare(left,  right.floatValue());
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
	@Inline(value = "$3.compare($1, $2.doubleValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(short left, Double right) {
		return Double.compare(left,  right.doubleValue());
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
	@Inline(value = "$3.compare($1, $2.intValue())", constantExpression = true, imported = Integer.class)
	public static int operator_spaceship(short left, AtomicInteger right) {
		return Integer.compare(left,  right.intValue());
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
	@Inline(value = "$3.compare($1, $2.longValue())", constantExpression = true, imported = Long.class)
	public static int operator_spaceship(short left, AtomicLong right) {
		return Long.compare(left,  right.longValue());
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
	@Inline(value = "$3.compare($1, $2.doubleValue())", constantExpression = true, imported = Double.class)
	public static int operator_spaceship(short left, Number right) {
		return Double.compare(left,  right.doubleValue());
	}

}
