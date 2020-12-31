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

package io.sarl.lang.core.tests.scoping.extensions.cast;

import static io.sarl.tests.api.tools.TestAssertions.assertEpsilonEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.*;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Cast operators - behavior")
@Tag("unit")
@Tag("core")
public class CodeTest extends AbstractSarlTest {

	@Test
	public void booleanValue_CharSequence() {
		assertFalse(PrimitiveCastExtensions.booleanValue("4"));
		assertFalse(PrimitiveCastExtensions.booleanValue("0xa"));
		assertFalse(PrimitiveCastExtensions.booleanValue("-4"));
		assertFalse(PrimitiveCastExtensions.booleanValue("-0xa"));
		assertFalse(PrimitiveCastExtensions.booleanValue(""));
		assertFalse(PrimitiveCastExtensions.booleanValue("z"));
		assertFalse(PrimitiveCastExtensions.booleanValue((String) null));
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
	public void booleanValue_AtomicBoolean() {
		assertFalse(PrimitiveCastExtensions.booleanValue((AtomicBoolean) null));
		assertFalse(PrimitiveCastExtensions.booleanValue(new AtomicBoolean(false)));
		assertTrue(PrimitiveCastExtensions.booleanValue(new AtomicBoolean(true)));
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
	public void charValue_CharSequence() {
		assertEquals('4', PrimitiveCastExtensions.charValue("4"));
		assertEquals('0', PrimitiveCastExtensions.charValue("0xa"));
		assertEquals('-', PrimitiveCastExtensions.charValue("-4"));
		assertEquals('-', PrimitiveCastExtensions.charValue("-0xa"));
		assertEquals('\0', PrimitiveCastExtensions.charValue(""));
		assertEquals('z', PrimitiveCastExtensions.charValue("z"));
		assertEquals('\0', PrimitiveCastExtensions.charValue(null));
	}

	@Test
	public void toCharacter_CharSequence() {
		assertEquals('4', PrimitiveCastExtensions.toCharacter("4"));
		assertEquals('0', PrimitiveCastExtensions.toCharacter("0xa"));
		assertEquals('-', PrimitiveCastExtensions.toCharacter("-4"));
		assertEquals('-', PrimitiveCastExtensions.toCharacter("-0xa"));
		assertEquals('\0', PrimitiveCastExtensions.toCharacter(""));
		assertEquals('z', PrimitiveCastExtensions.toCharacter("z"));
		assertEquals('\0', PrimitiveCastExtensions.toCharacter(null));
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
	public void toByte_CharSequence() {
		assertEquals(4, PrimitiveCastExtensions.toByte("4"));
		assertEquals(10, PrimitiveCastExtensions.toByte("0xa"));
		assertEquals(-4, PrimitiveCastExtensions.toByte("-4"));
		assertEquals(-10, PrimitiveCastExtensions.toByte("-0xa"));
		assertEquals(0, PrimitiveCastExtensions.toByte(""));
		assertEquals(0, PrimitiveCastExtensions.toByte("z"));
		assertEquals(0, PrimitiveCastExtensions.toByte(null));
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
	public void toShort_CharSequence() {
		assertEquals((short) 4, PrimitiveCastExtensions.toShort("4"));
		assertEquals((short) 10, PrimitiveCastExtensions.toShort("0xa"));
		assertEquals((short) -4, PrimitiveCastExtensions.toShort("-4"));
		assertEquals((short) -10, PrimitiveCastExtensions.toShort("-0xa"));
		assertEquals((short) 0, PrimitiveCastExtensions.toShort(""));
		assertEquals((short) 0, PrimitiveCastExtensions.toShort("z"));
		assertEquals((short) 0, PrimitiveCastExtensions.toShort(null));
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
	public void toInteger_CharSequence() {
		assertEquals(4, PrimitiveCastExtensions.toInteger("4"));
		assertEquals(10, PrimitiveCastExtensions.toInteger("0xa"));
		assertEquals(-4, PrimitiveCastExtensions.toInteger("-4"));
		assertEquals(-10, PrimitiveCastExtensions.toInteger("-0xa"));
		assertEquals(0, PrimitiveCastExtensions.toInteger(""));
		assertEquals(0, PrimitiveCastExtensions.toInteger("z"));
		assertEquals(0, PrimitiveCastExtensions.toInteger(null));
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
	public void toLong_CharSequence() {
		assertEquals(4l, PrimitiveCastExtensions.toLong("4"));
		assertEquals(10l, PrimitiveCastExtensions.toLong("0xa"));
		assertEquals(-4l, PrimitiveCastExtensions.toLong("-4"));
		assertEquals(-10l, PrimitiveCastExtensions.toLong("-0xa"));
		assertEquals(0l, PrimitiveCastExtensions.toLong(""));
		assertEquals(0l, PrimitiveCastExtensions.toLong("z"));
		assertEquals(0l, PrimitiveCastExtensions.toLong(null));
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
	public void toFloat_CharSequence() {
		assertEpsilonEquals(4f, PrimitiveCastExtensions.toFloat("4"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.toFloat("0xa"));
		assertEpsilonEquals(-4f, PrimitiveCastExtensions.toFloat("-4"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.toFloat("-0xa"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.toFloat(""));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.toFloat("z"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.toFloat(null));
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
	public void toDoubleCharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.toDouble("4"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.toDouble("0xa"));
		assertEpsilonEquals(-4., PrimitiveCastExtensions.toDouble("-4"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.toDouble("-0xa"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.toDouble(""));
		assertEpsilonEquals(0., PrimitiveCastExtensions.toDouble("z"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.toDouble(null));
	}

	@Test
	public void toAtomicBoolean_CharSequence() {
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("4").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("0xa").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("-4").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("-0xa").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("z").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean(null).get());
		assertTrue(PrimitiveCastExtensions.toAtomicBoolean("true").get());
		assertTrue(PrimitiveCastExtensions.toAtomicBoolean("tRue").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("false").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("fAlse").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("yes").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("no").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("oui").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("non").get());
	}

	@Test
	public void toAtomicBoolean_boolean() {
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean(false).get());
		assertTrue(PrimitiveCastExtensions.toAtomicBoolean(true).get());
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

	@Test
	public void toUUID_CharSequence() throws Exception {
		final String strValue0 = "433907b7-f201-4727-907a-9a34eb9a136c";
		final String strValue1 = "d51c801f-9253-4777-82ce-1bfbb5f10ea1";
		final String strValue2 = "zzzzzzzz-xxxx-zzzz-xxxx-zzzzzzzzzzzz";
		assertNull(PrimitiveCastExtensions.toUUID(null));
		assertNull(PrimitiveCastExtensions.toUUID(""));
		assertNull(PrimitiveCastExtensions.toUUID("a"));
		assertEquals(UUID.fromString(strValue0), PrimitiveCastExtensions.toUUID(strValue0));
		assertEquals(UUID.fromString(strValue1), PrimitiveCastExtensions.toUUID(strValue1));
		assertNull(PrimitiveCastExtensions.toUUID(strValue2));
	}

}
