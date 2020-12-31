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

package io.sarl.lang.core.tests.scoping.extensions.numbers.arithmetic.byteprimitive;

import static io.sarl.lang.scoping.extensions.numbers.arithmetic.PrimitiveByteArithmeticExtensions.operator_divide;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.PrimitiveByteArithmeticExtensions.operator_minus;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.PrimitiveByteArithmeticExtensions.operator_modulo;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.PrimitiveByteArithmeticExtensions.operator_multiply;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.PrimitiveByteArithmeticExtensions.operator_plus;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.PrimitiveByteArithmeticExtensions.operator_power;
import static io.sarl.tests.api.tools.TestAssertions.assertEpsilonEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
@DisplayName("Calling byte arithmetic operators")
@Tag("core")
@Tag("unit")
public class CodeTest extends AbstractSarlTest {

	private static byte left = 4;

	private static int right = 3;

	@Test
	public void operator_minus_byte_Number() throws Exception {
		assertEpsilonEquals(1., operator_minus(left, (Number) Double.valueOf(right)));
	}

	@Test
	public void operator_minus_byte_Long() throws Exception {
		assertEquals(1, operator_minus(left, Long.valueOf(right)));
	}

	@Test
	public void operator_minus_byte_Byte() throws Exception {
		assertEquals(1, operator_minus(left, Byte.valueOf((byte)right)));
	}

	@Test
	public void operator_minus_byte_Float() throws Exception {
		assertEpsilonEquals(1f, operator_minus(left, Float.valueOf(right)));
	}

	@Test
	public void operator_minus_byte_Integer() throws Exception {
		assertEquals(1, operator_minus(left, Integer.valueOf(right)));
	}

	@Test
	public void operator_minus_byte_Short() throws Exception {
		assertEquals(1, operator_minus(left, Short.valueOf((short) right)));
	}

	@Test
	public void operator_minus_byte_AtomicInteger() throws Exception {
		assertEquals(1, operator_minus(left, new AtomicInteger(right)));
	}

	@Test
	public void operator_minus_byte_AtomicLong() throws Exception {
		assertEquals(1l, operator_minus(left, new AtomicLong(right)));
	}

	@Test
	public void operator_plus_byte_Long() throws Exception {
		assertEquals(7l, operator_plus(left, Long.valueOf(right)));
	}

	@Test
	public void operator_plus_byte_Byte() throws Exception {
		assertEquals(7, operator_plus(left, Byte.valueOf((byte) right)));
	}

	@Test
	public void operator_plus_byte_Float() throws Exception {
		assertEpsilonEquals(7f, operator_plus(left, Float.valueOf(right)));
	}

	@Test
	public void operator_plus_byte_Integer() throws Exception {
		assertEquals(7, operator_plus(left, Integer.valueOf(right)));
	}

	@Test
	public void operator_plus_byte_Short() throws Exception {
		assertEquals(7, operator_plus(left, Short.valueOf((short) right)));
	}

	@Test
	public void operator_plus_byte_AtomicInteger() throws Exception {
		assertEquals(7, operator_plus(left, new AtomicInteger(right)));
	}

	@Test
	public void operator_plus_byte_AtomicLong() throws Exception {
		assertEquals(7l, operator_plus(left, new AtomicLong(right)));
	}

	@Test
	public void operator_plus_byte_Number() throws Exception {
		assertEpsilonEquals(7., operator_plus(left, (Number) Double.valueOf(right)));
	}

	@Test
	public void operator_power_byte_Number() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (Number) Integer.valueOf(right)));
	}

	@Test
	public void operator_divide_byte_Long() throws Exception {
		assertEquals(1l, operator_divide(left, Long.valueOf(right)));
	}

	@Test
	public void operator_divide_byte_Byte() throws Exception {
		assertEquals(1l, operator_divide(left, Byte.valueOf((byte) right)));
	}

	@Test
	public void operator_divide_byte_Float() throws Exception {
		assertEpsilonEquals(1.33333333333333333f, operator_divide(left, Float.valueOf(right)));
	}

	@Test
	public void operator_divide_byte_Integer() throws Exception {
		assertEquals(1, operator_divide(left, Integer.valueOf(right)));
	}

	@Test
	public void operator_divide_byte_Number() throws Exception {
		assertEpsilonEquals(1.333333333333333333, operator_divide(left, (Number) Long.valueOf(right)));
	}

	@Test
	public void operator_divide_byte_Short() throws Exception {
		assertEquals(1, operator_divide(left, Short.valueOf((short) right)));
	}

	@Test
	public void operator_divide_byte_AtomicInteger() throws Exception {
		assertEquals(1, operator_divide(left, new AtomicInteger(right)));
	}

	@Test
	public void operator_divide_byte_AtomicLong() throws Exception {
		assertEquals(1l, operator_divide(left, new AtomicLong(right)));
	}

	@Test
	public void operator_multiply_byte_Long() throws Exception {
		assertEquals(12l, operator_multiply(left, Long.valueOf(right)));
	}

	@Test
	public void operator_multiply_byte_Byte() throws Exception {
		assertEquals(12, operator_multiply(left, Byte.valueOf((byte) right)));
	}

	@Test
	public void operator_multiply_byte_Float() throws Exception {
		assertEpsilonEquals(12f, operator_multiply(left, Float.valueOf(right)));
	}

	@Test
	public void operator_multiply_byte_Integer() throws Exception {
		assertEquals(12, operator_multiply(left, Integer.valueOf(right)));
	}

	@Test
	public void operator_multiply_byte_Number() throws Exception {
		assertEpsilonEquals(12., operator_multiply(left, (Number) Long.valueOf(right)));
	}

	@Test
	public void operator_multiply_byte_Short() throws Exception {
		assertEquals(12, operator_multiply(left, Short.valueOf((short) right)));
	}

	@Test
	public void operator_multiply_byte_AtomicInteger() throws Exception {
		assertEquals(12, operator_multiply(left, new AtomicInteger(right)));
	}

	@Test
	public void operator_multiply_byte_AtomicLong() throws Exception {
		assertEquals(12l, operator_multiply(left, new AtomicLong(right)));
	}

	@Test
	public void operator_modulo_byte_Long() throws Exception {
		assertEquals(1l, operator_modulo(left, Long.valueOf(right)));
	}

	@Test
	public void operator_modulo_byte_Byte() throws Exception {
		assertEquals(1l, operator_modulo(left, Byte.valueOf((byte) right)));
	}

	@Test
	public void operator_modulo_byte_Float() throws Exception {
		assertEpsilonEquals(1f, operator_modulo(left, Float.valueOf(right)));
	}

	@Test
	public void operator_modulo_byte_Integer() throws Exception {
		assertEquals(1, operator_modulo(left, Integer.valueOf(right)));
	}

	@Test
	public void operator_modulo_byte_Number() throws Exception {
		assertEpsilonEquals(1., operator_modulo(left, (Number) Integer.valueOf(right)));
	}

	@Test
	public void operator_modulo_byte_Short() throws Exception {
		assertEquals(1, operator_modulo(left, Short.valueOf((short) right)));
	}

	@Test
	public void operator_modulo_byte_AtomicInteger() throws Exception {
		assertEquals(1, operator_modulo(left, new AtomicInteger(right)));
	}

	@Test
	public void operator_modulo_byte_AtomicLong() throws Exception {
		assertEquals(1l, operator_modulo(left, new AtomicLong(right)));
	}

}
