/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.core.tests.scoping.extensions.numbers.arithmetic.atomicinteger;

import static io.sarl.lang.core.scoping.extensions.numbers.arithmetic.AtomicIntegerArithmeticExtensions.operator_divide;
import static io.sarl.lang.core.scoping.extensions.numbers.arithmetic.AtomicIntegerArithmeticExtensions.operator_minus;
import static io.sarl.lang.core.scoping.extensions.numbers.arithmetic.AtomicIntegerArithmeticExtensions.operator_modulo;
import static io.sarl.lang.core.scoping.extensions.numbers.arithmetic.AtomicIntegerArithmeticExtensions.operator_multiply;
import static io.sarl.lang.core.scoping.extensions.numbers.arithmetic.AtomicIntegerArithmeticExtensions.operator_plus;
import static io.sarl.lang.core.scoping.extensions.numbers.arithmetic.AtomicIntegerArithmeticExtensions.operator_power;
import static io.sarl.tests.api.tools.TestAssertions.assertEpsilonEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
@DisplayName("Calling atomic integer arithmetic operators")
@Tag("core")
@Tag("unit")
public class CodeTest {

	private static AtomicInteger left = new AtomicInteger(4);

	private static int right = 3;

	@Test
	public void operator_minus_AtomicInteger() throws Exception {
		assertEquals(-4, operator_minus(left));
	}

	@Test
	public void operator_minus_AtomicInteger_Number() throws Exception {
		assertEpsilonEquals(1., operator_minus(left, (Number) Double.valueOf(right)));
	}

	@Test
	public void operator_minus_AtomicInteger_long() throws Exception {
		assertEquals(1, operator_minus(left, (long) right));
	}

	@Test
	public void operator_minus_AtomicInteger_Long() throws Exception {
		assertEquals(1, operator_minus(left, Long.valueOf(right)));
	}

	@Test
	public void operator_minus_AtomicInteger_byte() throws Exception {
		assertEquals(1, operator_minus(left, (byte)right));
	}

	@Test
	public void operator_minus_AtomicInteger_Byte() throws Exception {
		assertEquals(1, operator_minus(left, Byte.valueOf((byte)right)));
	}

	@Test
	public void operator_minus_AtomicInteger_float() throws Exception {
		assertEpsilonEquals(1f, operator_minus(left, (float) right));
	}

	@Test
	public void operator_minus_AtomicInteger_Float() throws Exception {
		assertEpsilonEquals(1f, operator_minus(left, Float.valueOf(right)));
	}

	@Test
	public void operator_minus_AtomicInteger_int() throws Exception {
		assertEquals(1, operator_minus(left, (int) right));
	}

	@Test
	public void operator_minus_AtomicInteger_Integer() throws Exception {
		assertEquals(1, operator_minus(left, Integer.valueOf(right)));
	}

	@Test
	public void operator_minus_AtomicInteger_short() throws Exception {
		assertEquals(1, operator_minus(left, (short) right));
	}

	@Test
	public void operator_minus_AtomicInteger_Short() throws Exception {
		assertEquals(1, operator_minus(left, Short.valueOf((short) right)));
	}

	@Test
	public void operator_minus_AtomicInteger_AtomicInteger() throws Exception {
		assertEquals(1, operator_minus(left, new AtomicInteger(right)));
	}

	@Test
	public void operator_minus_AtomicInteger_AtomicLong() throws Exception {
		assertEquals(1l, operator_minus(left, new AtomicLong(right)));
	}

	@Test
	public void operator_plus_AtomicInteger_long() throws Exception {
		assertEquals(7l, operator_plus(left, (long) right));
	}

	@Test
	public void operator_plus_AtomicInteger_Long() throws Exception {
		assertEquals(7l, operator_plus(left, Long.valueOf(right)));
	}

	@Test
	public void operator_plus_AtomicInteger_byte() throws Exception {
		assertEquals(7, operator_plus(left, (byte) right));
	}

	@Test
	public void operator_plus_AtomicInteger_Byte() throws Exception {
		assertEquals(7, operator_plus(left, Byte.valueOf((byte) right)));
	}

	@Test
	public void operator_plus_AtomicInteger_float() throws Exception {
		assertEpsilonEquals(7f, operator_plus(left, (float) right));
	}

	@Test
	public void operator_plus_AtomicInteger_Float() throws Exception {
		assertEpsilonEquals(7f, operator_plus(left, Float.valueOf(right)));
	}

	@Test
	public void operator_plus_AtomicInteger_int() throws Exception {
		assertEquals(7, operator_plus(left, (int) right));
	}

	@Test
	public void operator_plus_AtomicInteger_Integer() throws Exception {
		assertEquals(7, operator_plus(left, Integer.valueOf(right)));
	}

	@Test
	public void operator_plus_AtomicInteger_short() throws Exception {
		assertEquals(7, operator_plus(left, (short) right));
	}

	@Test
	public void operator_plus_AtomicInteger_Short() throws Exception {
		assertEquals(7, operator_plus(left, Short.valueOf((short) right)));
	}

	@Test
	public void operator_plus_AtomicInteger_AtomicInteger() throws Exception {
		assertEquals(7, operator_plus(left, new AtomicInteger(right)));
	}

	@Test
	public void operator_plus_AtomicInteger_AtomicLong() throws Exception {
		assertEquals(7l, operator_plus(left, new AtomicLong(right)));
	}

	@Test
	public void operator_plus_AtomicInteger_Number() throws Exception {
		assertEpsilonEquals(7., operator_plus(left, (Number) Double.valueOf(right)));
	}

	@Test
	public void operator_power_AtomicInteger_Number() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (Number) Integer.valueOf(right)));
	}


	@Test
	public void operator_power_AtomicInteger_byte() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (byte) right));
	}

	@Test
	public void operator_power_AtomicInteger_short() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (short) right));
	}

	@Test
	public void operator_power_AtomicInteger_int() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (int) right));
	}

	@Test
	public void operator_power_AtomicInteger_long() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (long) right));
	}

	@Test
	public void operator_power_AtomicInteger_float() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (float) right));
	}

	@Test
	public void operator_power_AtomicInteger_double() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (double) right));
	}

	@Test
	public void operator_divide_AtomicInteger_long() throws Exception {
		assertEquals(1l, operator_divide(left, (long) right));
	}

	@Test
	public void operator_divide_AtomicInteger_Long() throws Exception {
		assertEquals(1l, operator_divide(left, Long.valueOf(right)));
	}

	@Test
	public void operator_divide_AtomicInteger_byte() throws Exception {
		assertEquals(1l, operator_divide(left, (byte) right));
	}

	@Test
	public void operator_divide_AtomicInteger_Byte() throws Exception {
		assertEquals(1l, operator_divide(left, Byte.valueOf((byte) right)));
	}

	@Test
	public void operator_divide_AtomicInteger_float() throws Exception {
		assertEpsilonEquals(1.33333333333333333f, operator_divide(left, (float) right));
	}

	@Test
	public void operator_divide_AtomicInteger_Float() throws Exception {
		assertEpsilonEquals(1.33333333333333333f, operator_divide(left, Float.valueOf(right)));
	}

	@Test
	public void operator_divide_AtomicInteger_int() throws Exception {
		assertEquals(1, operator_divide(left, (int) right));
	}

	@Test
	public void operator_divide_AtomicInteger_Integer() throws Exception {
		assertEquals(1, operator_divide(left, Integer.valueOf(right)));
	}

	@Test
	public void operator_divide_AtomicInteger_Number() throws Exception {
		assertEpsilonEquals(1.333333333333333333, operator_divide(left, (Number) Long.valueOf(right)));
	}

	@Test
	public void operator_divide_AtomicInteger_short() throws Exception {
		assertEquals(1, operator_divide(left, (short) right));
	}

	@Test
	public void operator_divide_AtomicInteger_Short() throws Exception {
		assertEquals(1, operator_divide(left, Short.valueOf((short) right)));
	}

	@Test
	public void operator_divide_AtomicInteger_AtomicInteger() throws Exception {
		assertEquals(1, operator_divide(left, new AtomicInteger(right)));
	}

	@Test
	public void operator_divide_AtomicInteger_AtomicLong() throws Exception {
		assertEquals(1l, operator_divide(left, new AtomicLong(right)));
	}

	@Test
	public void operator_multiply_AtomicInteger_long() throws Exception {
		assertEquals(12l, operator_multiply(left, (long) right));
	}

	@Test
	public void operator_multiply_AtomicInteger_Long() throws Exception {
		assertEquals(12l, operator_multiply(left, Long.valueOf(right)));
	}

	@Test
	public void operator_multiply_AtomicInteger_byte() throws Exception {
		assertEquals(12, operator_multiply(left, (byte) right));
	}

	@Test
	public void operator_multiply_AtomicInteger_Byte() throws Exception {
		assertEquals(12, operator_multiply(left, Byte.valueOf((byte) right)));
	}

	@Test
	public void operator_multiply_AtomicInteger_float() throws Exception {
		assertEpsilonEquals(12f, operator_multiply(left, (float) right));
	}

	@Test
	public void operator_multiply_AtomicInteger_Float() throws Exception {
		assertEpsilonEquals(12f, operator_multiply(left, Float.valueOf(right)));
	}

	@Test
	public void operator_multiply_AtomicInteger_int() throws Exception {
		assertEquals(12, operator_multiply(left, (int) right));
	}

	@Test
	public void operator_multiply_AtomicInteger_Integer() throws Exception {
		assertEquals(12, operator_multiply(left, Integer.valueOf(right)));
	}

	@Test
	public void operator_multiply_AtomicInteger_Number() throws Exception {
		assertEpsilonEquals(12., operator_multiply(left, (Number) Long.valueOf(right)));
	}

	@Test
	public void operator_multiply_AtomicInteger_short() throws Exception {
		assertEquals(12, operator_multiply(left, (short) right));
	}

	@Test
	public void operator_multiply_AtomicInteger_Short() throws Exception {
		assertEquals(12, operator_multiply(left, Short.valueOf((short) right)));
	}

	@Test
	public void operator_multiply_AtomicInteger_AtomicInteger() throws Exception {
		assertEquals(12, operator_multiply(left, new AtomicInteger(right)));
	}

	@Test
	public void operator_multiply_AtomicInteger_AtomicLong() throws Exception {
		assertEquals(12l, operator_multiply(left, new AtomicLong(right)));
	}

	@Test
	public void operator_modulo_AtomicInteger_long() throws Exception {
		assertEquals(1l, operator_modulo(left, (long) right));
	}

	@Test
	public void operator_modulo_AtomicInteger_Long() throws Exception {
		assertEquals(1l, operator_modulo(left, Long.valueOf(right)));
	}

	@Test
	public void operator_modulo_AtomicInteger_byte() throws Exception {
		assertEquals(1l, operator_modulo(left, (byte) right));
	}

	@Test
	public void operator_modulo_AtomicInteger_Byte() throws Exception {
		assertEquals(1l, operator_modulo(left, Byte.valueOf((byte) right)));
	}

	@Test
	public void operator_modulo_AtomicInteger_float() throws Exception {
		assertEpsilonEquals(1f, operator_modulo(left, (float) right));
	}

	@Test
	public void operator_modulo_AtomicInteger_Float() throws Exception {
		assertEpsilonEquals(1f, operator_modulo(left, Float.valueOf(right)));
	}

	@Test
	public void operator_modulo_AtomicInteger_int() throws Exception {
		assertEquals(1, operator_modulo(left, (int) right));
	}

	@Test
	public void operator_modulo_AtomicInteger_Integer() throws Exception {
		assertEquals(1, operator_modulo(left, Integer.valueOf(right)));
	}

	@Test
	public void operator_modulo_AtomicInteger_Number() throws Exception {
		assertEpsilonEquals(1., operator_modulo(left, (Number) Integer.valueOf(right)));
	}

	@Test
	public void operator_modulo_AtomicInteger_short() throws Exception {
		assertEquals(1, operator_modulo(left, (short) right));
	}

	@Test
	public void operator_modulo_AtomicInteger_Short() throws Exception {
		assertEquals(1, operator_modulo(left, Short.valueOf((short) right)));
	}

	@Test
	public void operator_modulo_AtomicInteger_AtomicInteger() throws Exception {
		assertEquals(1, operator_modulo(left, new AtomicInteger(right)));
	}

	@Test
	public void operator_modulo_AtomicInteger_AtomicLong() throws Exception {
		assertEquals(1l, operator_modulo(left, new AtomicLong(right)));
	}

}
