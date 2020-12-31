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

package io.sarl.lang.core.tests.scoping.extensions.numbers.arithmetic.doubleobject;

import static io.sarl.lang.scoping.extensions.numbers.arithmetic.DoubleArithmeticExtensions.operator_divide;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.DoubleArithmeticExtensions.operator_minus;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.DoubleArithmeticExtensions.operator_modulo;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.DoubleArithmeticExtensions.operator_multiply;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.DoubleArithmeticExtensions.operator_plus;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.DoubleArithmeticExtensions.operator_power;
import static io.sarl.tests.api.tools.TestAssertions.assertEpsilonEquals;

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
@DisplayName("Calling Double arithmetic operators")
@Tag("core")
@Tag("unit")
public class CodeTest extends AbstractSarlTest {

	private static Double left = Double.valueOf(4);

	private static int right = 3;

	@Test
	public void operator_minus_Double_long() throws Exception {
		assertEpsilonEquals(1, operator_minus(left, (long) right));
	}

	@Test
	public void operator_minus_Double_byte() throws Exception {
		assertEpsilonEquals(1, operator_minus(left, (byte)right));
	}

	@Test
	public void operator_minus_Double_float() throws Exception {
		assertEpsilonEquals(1f, operator_minus(left, (float) right));
	}

	@Test
	public void operator_minus_Double_int() throws Exception {
		assertEpsilonEquals(1, operator_minus(left, (int) right));
	}

	@Test
	public void operator_minus_Double_short() throws Exception {
		assertEpsilonEquals(1, operator_minus(left, (short) right));
	}

	@Test
	public void operator_plus_Double_long() throws Exception {
		assertEpsilonEquals(7l, operator_plus(left, (long) right));
	}

	@Test
	public void operator_plus_Double_byte() throws Exception {
		assertEpsilonEquals(7, operator_plus(left, (byte) right));
	}

	@Test
	public void operator_plus_Double_float() throws Exception {
		assertEpsilonEquals(7f, operator_plus(left, (float) right));
	}

	@Test
	public void operator_plus_Double_int() throws Exception {
		assertEpsilonEquals(7, operator_plus(left, (int) right));
	}

	@Test
	public void operator_plus_Double_short() throws Exception {
		assertEpsilonEquals(7, operator_plus(left, (short) right));
	}

	@Test
	public void operator_divide_Double_long() throws Exception {
		assertEpsilonEquals(1.333333333333333333333, operator_divide(left, (long) right));
	}

	@Test
	public void operator_divide_Double_byte() throws Exception {
		assertEpsilonEquals(1.333333333333333333333, operator_divide(left, (byte) right));
	}

	@Test
	public void operator_divide_Double_float() throws Exception {
		assertEpsilonEquals(1.333333333333333333333, operator_divide(left, (float) right));
	}

	@Test
	public void operator_divide_Double_int() throws Exception {
		assertEpsilonEquals(1.333333333333333333333, operator_divide(left, (int) right));
	}

	@Test
	public void operator_divide_Double_short() throws Exception {
		assertEpsilonEquals(1.333333333333333333333, operator_divide(left, (short) right));
	}

	@Test
	public void operator_multiply_Double_long() throws Exception {
		assertEpsilonEquals(12l, operator_multiply(left, (long) right));
	}

	@Test
	public void operator_multiply_Double_byte() throws Exception {
		assertEpsilonEquals(12, operator_multiply(left, (byte) right));
	}

	@Test
	public void operator_multiply_Double_float() throws Exception {
		assertEpsilonEquals(12f, operator_multiply(left, (float) right));
	}

	@Test
	public void operator_multiply_Double_int() throws Exception {
		assertEpsilonEquals(12, operator_multiply(left, (int) right));
	}

	@Test
	public void operator_multiply_Double_short() throws Exception {
		assertEpsilonEquals(12, operator_multiply(left, (short) right));
	}

	@Test
	public void operator_modulo_Double_long() throws Exception {
		assertEpsilonEquals(1l, operator_modulo(left, (long) right));
	}

	@Test
	public void operator_modulo_Double_byte() throws Exception {
		assertEpsilonEquals(1l, operator_modulo(left, (byte) right));
	}

	@Test
	public void operator_modulo_Double_float() throws Exception {
		assertEpsilonEquals(1f, operator_modulo(left, (float) right));
	}

	@Test
	public void operator_modulo_Double_int() throws Exception {
		assertEpsilonEquals(1, operator_modulo(left, (int) right));
	}

	@Test
	public void operator_modulo_Double_short() throws Exception {
		assertEpsilonEquals(1, operator_modulo(left, (short) right));
	}

	@Test
	public void operator_modulo_Double_Number() throws Exception {
		assertEpsilonEquals(1, operator_modulo(left, (Number) Double.valueOf(right)));
	}

	@Test
	public void operator_power_Double_byte() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (byte) right));
	}

	@Test
	public void operator_power_Double_short() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (short) right));
	}

	@Test
	public void operator_power_Double_int() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (int) right));
	}

	@Test
	public void operator_power_Double_long() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (long) right));
	}

	@Test
	public void operator_power_Double_float() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (float) right));
	}

	@Test
	public void operator_power_Double_double() throws Exception {
		assertEpsilonEquals(64., operator_power(left, (double) right));
	}

}
