/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.core.scoping.extensions.numbers.arithmetic;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static operators for numbers of type {@code Number}.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
public final class NumberArithmeticExtensions {

	private NumberArithmeticExtensions() {
		//
	}

	/** The unary {@code minus} operator. This is the equivalent to
	 * the Java's {@code -} function. This function is not null-safe.
	 *
	 * @param number a number.
	 * @return {@code -number}
	 */
	@Pure
	@Inline(value = "(-($1.doubleValue()))", constantExpression = true)
	public static double operator_minus(Number number) {
		return -number.doubleValue();
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() - $2)", constantExpression = true)
	public static double operator_minus(Number left, long right) {
		return left.doubleValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() - $2)", constantExpression = true)
	public static double operator_minus(Number left, byte right) {
		return left.doubleValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() - $2)", constantExpression = true)
	public static double operator_minus(Number left, int right) {
		return left.doubleValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() - $2)", constantExpression = true)
	public static double operator_minus(Number left, short right) {
		return left.doubleValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() - $2)", constantExpression = true)
	public static double operator_minus(Number left, double right) {
		return left.doubleValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() - $2)", constantExpression = true)
	public static double operator_minus(Number left, float right) {
		return left.doubleValue() - right;
	}

	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left-right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() - $2.doubleValue())", constantExpression = true)
	public static double operator_minus(Number left, Number right) {
		return left.doubleValue() - right.doubleValue();
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() + $2)", constantExpression = true)
	public static double operator_plus(Number left, long right) {
		return left.doubleValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() + $2)", constantExpression = true)
	public static double operator_plus(Number left, byte right) {
		return left.doubleValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() + $2)", constantExpression = true)
	public static double operator_plus(Number left, int right) {
		return left.doubleValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() + $2)", constantExpression = true)
	public static double operator_plus(Number left, short right) {
		return left.doubleValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() + $2)", constantExpression = true)
	public static double operator_plus(Number left, double right) {
		return left.doubleValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() + $2)", constantExpression = true)
	public static double operator_plus(Number left, float right) {
		return left.doubleValue() + right;
	}

	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left+right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() + $2.doubleValue())", constantExpression = true)
	public static double operator_plus(Number left, Number right) {
		return left.doubleValue() + right.doubleValue();
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.doubleValue(), $2)", imported = Math.class)
	public static double operator_power(Number left, long right) {
		return Math.pow(left.doubleValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.doubleValue(), $2)", imported = Math.class)
	public static double operator_power(Number left, byte right) {
		return Math.pow(left.doubleValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.doubleValue(), $2)", imported = Math.class)
	public static double operator_power(Number left, int right) {
		return Math.pow(left.doubleValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.doubleValue(), $2)", imported = Math.class)
	public static double operator_power(Number left, short right) {
		return Math.pow(left.doubleValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.doubleValue(), $2)", imported = Math.class)
	public static double operator_power(Number left, double right) {
		return Math.pow(left.doubleValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.doubleValue(), $2)", imported = Math.class)
	public static double operator_power(Number left, float right) {
		return Math.pow(left.doubleValue(), right);
	}

	/** The binary {@code power} operator. This is the equivalent to
	 * the Java's {@code Math.pow()} function.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code Math::pow(left, right)}
	 */
	@Pure
	@Inline(value = "$3.pow($1.doubleValue(), $2.doubleValue())", imported = Math.class)
	public static double operator_power(Number left, Number right) {
		return Math.pow(left.doubleValue(), right.doubleValue());
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() / $2)", constantExpression = true)
	public static double operator_divide(Number left, long right) {
		return left.doubleValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() / $2)", constantExpression = true)
	public static double operator_divide(Number left, byte right) {
		return left.doubleValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() / $2)", constantExpression = true)
	public static double operator_divide(Number left, int right) {
		return left.doubleValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() / $2)", constantExpression = true)
	public static double operator_divide(Number left, short right) {
		return left.doubleValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() / $2)", constantExpression = true)
	public static double operator_divide(Number left, double right) {
		return left.doubleValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() / $2)", constantExpression = true)
	public static double operator_divide(Number left, float right) {
		return left.doubleValue() / right;
	}

	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left/right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() / $2.doubleValue())", constantExpression = true)
	public static double operator_divide(Number left, Number right) {
		return left.doubleValue() / right.doubleValue();
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() * $2)", constantExpression = true)
	public static double operator_multiply(Number left, long right) {
		return left.doubleValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() * $2)", constantExpression = true)
	public static double operator_multiply(Number left, byte right) {
		return left.doubleValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() * $2)", constantExpression = true)
	public static double operator_multiply(Number left, int right) {
		return left.doubleValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() * $2)", constantExpression = true)
	public static double operator_multiply(Number left, short right) {
		return left.doubleValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() * $2)", constantExpression = true)
	public static double operator_multiply(Number left, double right) {
		return left.doubleValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() * $2)", constantExpression = true)
	public static double operator_multiply(Number left, float right) {
		return left.doubleValue() * right;
	}

	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left*right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() * $2.doubleValue())", constantExpression = true)
	public static double operator_multiply(Number left, Number right) {
		return left.doubleValue() * right.doubleValue();
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() % $2)", constantExpression = true)
	public static double operator_modulo(Number left, long right) {
		return left.doubleValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() % $2)", constantExpression = true)
	public static double operator_modulo(Number left, byte right) {
		return left.doubleValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() % $2)", constantExpression = true)
	public static double operator_modulo(Number left, int right) {
		return left.doubleValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() % $2)", constantExpression = true)
	public static double operator_modulo(Number left, short right) {
		return left.doubleValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() % $2)", constantExpression = true)
	public static double operator_modulo(Number left, double right) {
		return left.doubleValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() % $2)", constantExpression = true)
	public static double operator_modulo(Number left, float right) {
		return left.doubleValue() % right;
	}

	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator. This function is not null-safe.
	 *
	 * @param left a number.
	 * @param right a number.
	 * @return {@code left%right}
	 */
	@Pure
	@Inline(value = "($1.doubleValue() % $2.doubleValue())", constantExpression = true)
	public static double operator_modulo(Number left, Number right) {
		return left.doubleValue() % right.doubleValue();
	}

}
