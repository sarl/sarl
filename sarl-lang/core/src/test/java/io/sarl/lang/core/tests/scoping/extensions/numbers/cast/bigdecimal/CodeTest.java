/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast.bigdecimal;

import static io.sarl.lang.core.scoping.extensions.numbers.cast.BigDecimalCastExtensions.toAtomicDouble;
import static io.sarl.lang.core.scoping.extensions.numbers.cast.BigDecimalCastExtensions.toAtomicInteger;
import static io.sarl.tests.api.tools.TestAssertions.assertEpsilonEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.math.BigDecimal;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Calling big decimal cast operator")
@Tag("core")
@Tag("unit")
public class CodeTest {

	private static BigDecimal left = BigDecimal.valueOf(4);

	@Test
	public void toAtomicInteger_BigDecimal() {
		assertEquals(4, toAtomicInteger(left).intValue());
	}

	@Test
	public void toAtomicLong_BigDecimal() {
		assertEquals(4, toAtomicInteger(left).intValue());
	}

	@Test
	public void toAtomicDouble_BigDecimal() {
		assertEpsilonEquals(4., toAtomicDouble(left).doubleValue());
	}

}
