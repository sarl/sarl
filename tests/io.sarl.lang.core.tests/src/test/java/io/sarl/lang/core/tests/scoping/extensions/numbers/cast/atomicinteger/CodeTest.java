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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast.atomicinteger;

import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicIntegerCastExtensions.toAtomicDouble;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicIntegerCastExtensions.toAtomicLong;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicIntegerCastExtensions.toBigDecimal;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicIntegerCastExtensions.toBigInteger;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicIntegerCastExtensions.toByte;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicIntegerCastExtensions.toDouble;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicIntegerCastExtensions.toFloat;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicIntegerCastExtensions.toInteger;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicIntegerCastExtensions.toLong;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicIntegerCastExtensions.toShort;
import static io.sarl.tests.api.tools.TestAssertions.assertEpsilonEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;

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
@DisplayName("Calling atomic integer cast operator")
@Tag("core")
@Tag("unit")
public class CodeTest extends AbstractSarlTest {

	private static AtomicInteger left = new AtomicInteger(4);

	@Test
	public void toByte_AtomicInteger() {
		assertEquals(Byte.valueOf((byte) 4), toByte(left));
	}

	@Test
	public void toShort_AtomicInteger() {
		assertEquals(Short.valueOf((short) 4), toShort(left));
	}

	@Test
	public void toInteger_AtomicInteger() {
		assertEquals(4, toInteger(left).intValue());
	}

	@Test
	public void toLong_AtomicInteger() {
		assertEquals(Long.valueOf(4), toLong(left));
	}

	@Test
	public void toAtomicLong_AtomicInteger() {
		assertEquals(4l, toAtomicLong(left).longValue());
	}

	@Test
	public void toFloat_AtomicInteger() {
		assertEpsilonEquals(Float.valueOf(4), toFloat(left));
	}

	@Test
	public void toDouble_AtomicInteger() {
		assertEpsilonEquals(Double.valueOf(4), toDouble(left));
	}

	@Test
	public void toAtomicDouble_AtomicInteger() {
		assertEpsilonEquals(4., toAtomicDouble(left).doubleValue());
	}

	@Test
	public void toBigInteger_AtomicInteger() {
		BigInteger value = toBigInteger(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

	@Test
	public void toBigDecimal_AtomicInteger() {
		BigDecimal value = toBigDecimal(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

}
