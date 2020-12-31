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

package io.sarl.lang.core.tests.scoping.extensions.numbers.arithmetic.doubleprimitive;

import static io.sarl.lang.scoping.extensions.numbers.arithmetic.PrimitiveDoubleArithmeticExtensions.operator_divide;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.PrimitiveDoubleArithmeticExtensions.operator_minus;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.PrimitiveDoubleArithmeticExtensions.operator_modulo;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.PrimitiveDoubleArithmeticExtensions.operator_multiply;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.PrimitiveDoubleArithmeticExtensions.operator_plus;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.PrimitiveDoubleArithmeticExtensions.operator_power;
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
@DisplayName("Calling double arithmetic operators")
@Tag("core")
@Tag("unit")
public class CodeTest extends AbstractSarlTest {

	private static double left = 4.;

	private static int right = 3;

	@Test
	public void operator_minus_double_Number() {
		assertEpsilonEquals(1., operator_minus(left, Double.valueOf(right)));
	}

	@Test
	public void operator_plus_double_Number() {
		assertEpsilonEquals(7., operator_plus(left, Double.valueOf(right)));
	}

	@Test
	public void operator_power_double_Number() {
		assertEpsilonEquals(64., operator_power(left, Double.valueOf(right)));
	}

	@Test
	public void operator_divide_double_Number() {
		assertEpsilonEquals(1.33333333333333333333, operator_divide(left, Double.valueOf(right)));
	}

	@Test
	public void operator_multiply_double_Number() {
		assertEpsilonEquals(12., operator_multiply(left, Double.valueOf(right)));
	}

	@Test
	public void operator_modulo_double_Number() {
		assertEpsilonEquals(1., operator_modulo(left, Double.valueOf(right)));
	}

}
