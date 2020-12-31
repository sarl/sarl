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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast.number;

import static io.sarl.lang.scoping.extensions.numbers.cast.NumberCastExtensions.toAtomicDouble;
import static io.sarl.lang.scoping.extensions.numbers.cast.NumberCastExtensions.toAtomicInteger;
import static io.sarl.lang.scoping.extensions.numbers.cast.NumberCastExtensions.toAtomicLong;
import static io.sarl.lang.scoping.extensions.numbers.cast.NumberCastExtensions.toBigDecimal;
import static io.sarl.lang.scoping.extensions.numbers.cast.NumberCastExtensions.toBigInteger;
import static io.sarl.lang.scoping.extensions.numbers.cast.NumberCastExtensions.toByte;
import static io.sarl.lang.scoping.extensions.numbers.cast.NumberCastExtensions.toDouble;
import static io.sarl.lang.scoping.extensions.numbers.cast.NumberCastExtensions.toFloat;
import static io.sarl.lang.scoping.extensions.numbers.cast.NumberCastExtensions.toInteger;
import static io.sarl.lang.scoping.extensions.numbers.cast.NumberCastExtensions.toLong;
import static io.sarl.lang.scoping.extensions.numbers.cast.NumberCastExtensions.toShort;
import static io.sarl.tests.api.tools.TestAssertions.assertEpsilonEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import com.google.common.util.concurrent.AtomicDouble;
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
@DisplayName("Calling Number cast operator")
@Tag("core")
@Tag("unit")
public class CodeTest extends AbstractSarlTest {

	private static Number left = new AtomicDouble(4);

	@Test
	public void toByte_Number() {
		Byte value = toByte(left);
		assertNotNull(value);
		assertEquals((byte) 4, value.byteValue());
	}

	@Test
	public void toShort_Number() {
		Short value = toShort(left);
		assertNotNull(value);
		assertEquals((short) 4, value.shortValue());
	}

	@Test
	public void toInteger_Number() {
		Integer value = toInteger(left);
		assertNotNull(value);
		assertEquals(4, value.intValue());
	}

	@Test
	public void toAtomicInteger_Number() {
		AtomicInteger value = toAtomicInteger(left);
		assertNotNull(value);
		assertEquals(4, value.intValue());
	}

	@Test
	public void toLong_Number() {
		Long value = toLong(left);
		assertNotNull(value);
		assertEquals(4l, value.longValue());
	}

	@Test
	public void toAtomicLong_Number() {
		AtomicLong value = toAtomicLong(left);
		assertNotNull(value);
		assertEquals(4l, value.longValue());
	}

	@Test
	public void toFloat_Number() {
		Float value = toFloat(left);
		assertNotNull(value);
		assertEpsilonEquals(4f, value.floatValue());
	}

	@Test
	public void toDouble_Number() {
		Double value = toDouble(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

	@Test
	public void toAtomicDouble_Number() {
		AtomicDouble value = toAtomicDouble(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

	@Test
	public void toBigInteger_Number() {
		BigInteger value = toBigInteger(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

	@Test
	public void toBigDecimal_Number() {
		BigDecimal value = toBigDecimal(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

}
