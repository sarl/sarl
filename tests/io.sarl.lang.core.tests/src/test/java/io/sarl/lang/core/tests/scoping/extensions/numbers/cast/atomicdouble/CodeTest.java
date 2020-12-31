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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast.atomicdouble;

import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toAtomicInteger;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toAtomicLong;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toBigDecimal;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toBigInteger;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toByte;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toDouble;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toFloat;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toInteger;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toLong;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toShort;
import static io.sarl.tests.api.tools.TestAssertions.assertEpsilonEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.math.BigDecimal;
import java.math.BigInteger;

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
@DisplayName("Calling atomic double cast operator")
@Tag("core")
@Tag("unit")
public class CodeTest extends AbstractSarlTest {

	private static AtomicDouble left = new AtomicDouble(4);

	@Test
	public void toByte_AtomicDouble() {
		assertEquals(Byte.valueOf((byte) 4), toByte(left));
	}

	@Test
	public void toShort_AtomicDouble() {
		assertEquals(Short.valueOf((short) 4), toShort(left));
	}

	@Test
	public void toInteger_AtomicDouble() {
		assertEquals(Integer.valueOf(4), toInteger(left));
	}

	@Test
	public void toAtomicInteger_AtomicDouble() {
		assertEquals(4, toAtomicInteger(left).intValue());
	}

	@Test
	public void toLong_AtomicDouble() {
		assertEquals(Long.valueOf(4), toLong(left));
	}

	@Test
	public void toAtomicLong_AtomicDouble() {
		assertEquals(4, toAtomicLong(left).longValue());
	}

	@Test
	public void toFloat_AtomicDouble() {
		assertEpsilonEquals(Float.valueOf(4), toFloat(left));
	}

	@Test
	public void toDouble_AtomicDouble() {
		assertEpsilonEquals(Double.valueOf(4), toDouble(left));
	}

	@Test
	public void toBigInteger_AtomicDouble() {
		BigInteger value = toBigInteger(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

	@Test
	public void toBigDecimal_AtomicDouble() {
		BigDecimal value = toBigDecimal(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

}
