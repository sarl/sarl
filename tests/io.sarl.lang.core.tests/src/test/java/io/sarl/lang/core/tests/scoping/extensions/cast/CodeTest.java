/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.lang.core.tests.scoping.extensions.cast;

import org.junit.Test;

import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class CodeTest extends AbstractSarlTest {

	@Test
	public void booleanValue_CharSequence() {
		assertFalse(PrimitiveCastExtensions.booleanValue("4"));
		assertFalse(PrimitiveCastExtensions.booleanValue("0xa"));
		assertFalse(PrimitiveCastExtensions.booleanValue("-4"));
		assertFalse(PrimitiveCastExtensions.booleanValue("-0xa"));
		assertFalse(PrimitiveCastExtensions.booleanValue(""));
		assertFalse(PrimitiveCastExtensions.booleanValue("z"));
		assertFalse(PrimitiveCastExtensions.booleanValue(null));
		assertTrue(PrimitiveCastExtensions.booleanValue("true"));
		assertTrue(PrimitiveCastExtensions.booleanValue("tRue"));
		assertFalse(PrimitiveCastExtensions.booleanValue("false"));
		assertFalse(PrimitiveCastExtensions.booleanValue("fAlse"));
		assertFalse(PrimitiveCastExtensions.booleanValue("yes"));
		assertFalse(PrimitiveCastExtensions.booleanValue("no"));
		assertFalse(PrimitiveCastExtensions.booleanValue("oui"));
		assertFalse(PrimitiveCastExtensions.booleanValue("non"));
	}

	@Test
	public void toString_boolean() {
		assertEquals(Boolean.TRUE.toString(), PrimitiveCastExtensions.toString(true));
		assertEquals(Boolean.FALSE.toString(), PrimitiveCastExtensions.toString(false));
	}

	@Test
	public void toString_char() {
		assertEquals("a", PrimitiveCastExtensions.toString('a'));
	}

	@Test
	public void byteValue_CharSequence() {
		assertEquals(4, PrimitiveCastExtensions.byteValue("4"));
		assertEquals(10, PrimitiveCastExtensions.byteValue("0xa"));
		assertEquals(-4, PrimitiveCastExtensions.byteValue("-4"));
		assertEquals(-10, PrimitiveCastExtensions.byteValue("-0xa"));
		assertEquals(0, PrimitiveCastExtensions.byteValue(""));
		assertEquals(0, PrimitiveCastExtensions.byteValue("z"));
		assertEquals(0, PrimitiveCastExtensions.byteValue(null));
	}

	@Test
	public void shortValue_CharSequence() {
		assertEquals(4, PrimitiveCastExtensions.shortValue("4"));
		assertEquals(10, PrimitiveCastExtensions.shortValue("0xa"));
		assertEquals(-4, PrimitiveCastExtensions.shortValue("-4"));
		assertEquals(-10, PrimitiveCastExtensions.shortValue("-0xa"));
		assertEquals(0, PrimitiveCastExtensions.shortValue(""));
		assertEquals(0, PrimitiveCastExtensions.shortValue("z"));
		assertEquals(0, PrimitiveCastExtensions.shortValue(null));
	}

	@Test
	public void intValue_CharSequence() {
		assertEquals(4, PrimitiveCastExtensions.intValue("4"));
		assertEquals(10, PrimitiveCastExtensions.intValue("0xa"));
		assertEquals(-4, PrimitiveCastExtensions.intValue("-4"));
		assertEquals(-10, PrimitiveCastExtensions.intValue("-0xa"));
		assertEquals(0, PrimitiveCastExtensions.intValue(""));
		assertEquals(0, PrimitiveCastExtensions.intValue("z"));
		assertEquals(0, PrimitiveCastExtensions.intValue(null));
	}

	@Test
	public void longValue_CharSequence() {
		assertEquals(4l, PrimitiveCastExtensions.longValue("4"));
		assertEquals(10l, PrimitiveCastExtensions.longValue("0xa"));
		assertEquals(-4l, PrimitiveCastExtensions.longValue("-4"));
		assertEquals(-10l, PrimitiveCastExtensions.longValue("-0xa"));
		assertEquals(0l, PrimitiveCastExtensions.longValue(""));
		assertEquals(0l, PrimitiveCastExtensions.longValue("z"));
		assertEquals(0l, PrimitiveCastExtensions.longValue(null));
	}

	@Test
	public void floatValue_CharSequence() {
		assertEpsilonEquals(4f, PrimitiveCastExtensions.floatValue("4"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.floatValue("0xa"));
		assertEpsilonEquals(-4f, PrimitiveCastExtensions.floatValue("-4"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.floatValue("-0xa"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.floatValue(""));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.floatValue("z"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.floatValue(null));
	}

	@Test
	public void doubleValue_CharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.doubleValue("4"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.doubleValue("0xa"));
		assertEpsilonEquals(-4., PrimitiveCastExtensions.doubleValue("-4"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.doubleValue("-0xa"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.doubleValue(""));
		assertEpsilonEquals(0., PrimitiveCastExtensions.doubleValue("z"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.doubleValue(null));
	}

	@Test
	public void toAtomicInteger_CharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.toAtomicInteger("4").doubleValue());
		assertEpsilonEquals(10., PrimitiveCastExtensions.toAtomicInteger("0xa").doubleValue());
		assertEpsilonEquals(-4., PrimitiveCastExtensions.toAtomicInteger("-4").doubleValue());
		assertEpsilonEquals(-10., PrimitiveCastExtensions.toAtomicInteger("-0xa").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicInteger("").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicInteger("z").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicInteger(null).doubleValue());
	}

	@Test
	public void toAtomicLong_CharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.toAtomicLong("4").doubleValue());
		assertEpsilonEquals(10., PrimitiveCastExtensions.toAtomicLong("0xa").doubleValue());
		assertEpsilonEquals(-4., PrimitiveCastExtensions.toAtomicLong("-4").doubleValue());
		assertEpsilonEquals(-10., PrimitiveCastExtensions.toAtomicLong("-0xa").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicLong("").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicLong("z").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicLong(null).doubleValue());
	}

	@Test
	public void toAtomicDouble_CharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.toAtomicDouble("4").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicDouble("0xa").doubleValue());
		assertEpsilonEquals(-4., PrimitiveCastExtensions.toAtomicDouble("-4").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicDouble("-0xa").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicDouble("").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicDouble("z").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicDouble(null).doubleValue());
	}

	@Test
	public void toBigInteger_CharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.toBigInteger("4").doubleValue());
		assertEpsilonEquals(10., PrimitiveCastExtensions.toBigInteger("0xa").doubleValue());
		assertEpsilonEquals(-4., PrimitiveCastExtensions.toBigInteger("-4").doubleValue());
		assertEpsilonEquals(-10., PrimitiveCastExtensions.toBigInteger("-0xa").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigInteger("").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigInteger("z").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigInteger(null).doubleValue());
	}

	@Test
	public void toBigDecimal_CharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.toBigDecimal("4").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigDecimal("0xa").doubleValue());
		assertEpsilonEquals(-4., PrimitiveCastExtensions.toBigDecimal("-4").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigDecimal("-0xa").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigDecimal("").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigDecimal("z").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigDecimal(null).doubleValue());
	}

}
