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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast.byteprimitive;

import static io.sarl.lang.scoping.extensions.numbers.cast.PrimitiveByteCastExtensions.toAtomicDouble;
import static io.sarl.lang.scoping.extensions.numbers.cast.PrimitiveByteCastExtensions.toAtomicInteger;
import static io.sarl.lang.scoping.extensions.numbers.cast.PrimitiveByteCastExtensions.toAtomicLong;
import static io.sarl.lang.scoping.extensions.numbers.cast.PrimitiveByteCastExtensions.toBigDecimal;
import static io.sarl.lang.scoping.extensions.numbers.cast.PrimitiveByteCastExtensions.toBigInteger;
import static io.sarl.tests.api.tools.TestAssertions.assertEpsilonEquals;
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
@DisplayName("Calling byte cast operator")
@Tag("core")
@Tag("unit")
public class CodeTest extends AbstractSarlTest {

	private static byte left = 4;

	@Test
	public void toAtomicLong_byte() {
		AtomicLong value = toAtomicLong(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

	@Test
	public void toAtomicInteger_byte() {
		AtomicInteger value = toAtomicInteger(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

	@Test
	public void toAtomicDouble_byte() {
		AtomicDouble value = toAtomicDouble(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

	@Test
	public void toBigInteger_byte() {
		BigInteger value = toBigInteger(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

	@Test
	public void toBigDecimal_byte() {
		BigDecimal value = toBigDecimal(left);
		assertNotNull(value);
		assertEpsilonEquals(4., value.doubleValue());
	}

}
