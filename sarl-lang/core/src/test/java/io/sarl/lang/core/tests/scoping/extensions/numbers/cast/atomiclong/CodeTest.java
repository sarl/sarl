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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast.atomiclong;

import static io.sarl.lang.core.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toAtomicDouble;
import static io.sarl.lang.core.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toAtomicInteger;
import static io.sarl.lang.core.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toBigDecimal;
import static io.sarl.lang.core.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toBigInteger;
import static io.sarl.lang.core.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toByte;
import static io.sarl.lang.core.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toDouble;
import static io.sarl.lang.core.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toFloat;
import static io.sarl.lang.core.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toInteger;
import static io.sarl.lang.core.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toLong;
import static io.sarl.lang.core.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toShort;
import static io.sarl.tests.api.tools.TestAssertions.assertEpsilonEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
@DisplayName("Calling atomic long cast operator")
@Tag("core")
@Tag("unit")
public class CodeTest {

	private static AtomicLong left = new AtomicLong(4);

	@Test
	public void toByte_AtomicLong() {
		assertEquals(Byte.valueOf((byte) 4), toByte(left));
	}

	@Test
	public void toShort_AtomicLong() {
		assertEquals(Short.valueOf((short) 4), toShort(left));
	}

	@Test
	public void toInteger_AtomicLong() {
		assertEquals(Integer.valueOf(4), toInteger(left));
	}

	@Test
	public void toAtomicInteger_AtomicLong() {
		assertEquals(4, toAtomicInteger(left).intValue());
	}

	@Test
	public void toLong_AtomicLong() {
		assertEquals(Long.valueOf(4), toLong(left));
	}

	@Test
	public void toFloat_AtomicLong() {
		assertEpsilonEquals(Float.valueOf(4), toFloat(left));
	}

	@Test
	public void toDouble_AtomicLong() {
		assertEpsilonEquals(Double.valueOf(4), toDouble(left));
	}

	@Test
	public void toAtomicDouble_AtomicLong() {
		assertEpsilonEquals(4., toAtomicDouble(left).doubleValue());
	}

	@Test
	public void toBigInteger_AtomicLong() {
		BigInteger value = toBigInteger(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

	@Test
	public void toBigDecimal_AtomicLong() {
		BigDecimal value = toBigDecimal(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

}
