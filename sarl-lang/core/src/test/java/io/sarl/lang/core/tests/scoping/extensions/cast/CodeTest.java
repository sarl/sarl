/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import com.google.common.util.concurrent.AtomicDouble;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.scoping.extensions.cast.PrimitiveCastExtensions;

/**
 * @author $Author: sgalland$
 * @version core 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 */
@SuppressWarnings("all")
@DisplayName("Cast operators - behavior")
@Tag("unit")
@Tag("core")
public class CodeTest {

	@Test
	public void booleanValue_Object() {
		assertFalse(PrimitiveCastExtensions.booleanValue((Object) null));
		assertFalse(PrimitiveCastExtensions.booleanValue(Boolean.FALSE));
		assertTrue(PrimitiveCastExtensions.booleanValue(Boolean.TRUE));
		assertTrue(PrimitiveCastExtensions.booleanValue((Object) "true"));
		assertFalse(PrimitiveCastExtensions.booleanValue((Object) "false"));
	}

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
	public void charValue_Object() {
		assertEquals('\4', PrimitiveCastExtensions.charValue(Character.valueOf((char) 4)));
		assertEquals('4', PrimitiveCastExtensions.charValue("4"));
		assertEquals('\0', PrimitiveCastExtensions.charValue((Object) null));
	}

	@Test
	public void charValue_CharSequence() {
		assertEquals('4', PrimitiveCastExtensions.charValue("4"));
		assertEquals('0', PrimitiveCastExtensions.charValue("0xa"));
		assertEquals('-', PrimitiveCastExtensions.charValue("-4"));
		assertEquals('-', PrimitiveCastExtensions.charValue("-0xa"));
		assertEquals('\0', PrimitiveCastExtensions.charValue(""));
		assertEquals('z', PrimitiveCastExtensions.charValue("z"));
		assertEquals('\0', PrimitiveCastExtensions.charValue((CharSequence) null));
	}

	@Test
	public void toCharacter_CharSequence() {
		assertEquals('4', PrimitiveCastExtensions.toCharacter("4"));
		assertEquals('0', PrimitiveCastExtensions.toCharacter("0xa"));
		assertEquals('-', PrimitiveCastExtensions.toCharacter("-4"));
		assertEquals('-', PrimitiveCastExtensions.toCharacter("-0xa"));
		assertEquals('\0', PrimitiveCastExtensions.toCharacter(""));
		assertEquals('z', PrimitiveCastExtensions.toCharacter("z"));
		assertEquals('\0', PrimitiveCastExtensions.toCharacter((CharSequence) null));
	}

	@Test
	public void toCharacter_Object() {
		assertEquals('\0', PrimitiveCastExtensions.toCharacter((Object) null));
		assertEquals('\4', PrimitiveCastExtensions.toCharacter(4));
		assertEquals('a', PrimitiveCastExtensions.toCharacter("abcd"));
	}

	@Test
	public void byteValue_Object() {
		assertEquals(0, PrimitiveCastExtensions.byteValue((Object) null));
		assertEquals(4, PrimitiveCastExtensions.byteValue(Byte.valueOf((byte) 4)));
		assertEquals(4, PrimitiveCastExtensions.byteValue((Object) "4"));
	}

	@Test
	public void byteValue_CharSequence() {
		assertEquals(4, PrimitiveCastExtensions.byteValue("4"));
		assertEquals(10, PrimitiveCastExtensions.byteValue("0xa"));
		assertEquals(-4, PrimitiveCastExtensions.byteValue("-4"));
		assertEquals(-10, PrimitiveCastExtensions.byteValue("-0xa"));
		assertEquals(0, PrimitiveCastExtensions.byteValue(""));
		assertEquals(0, PrimitiveCastExtensions.byteValue("z"));
		assertEquals(0, PrimitiveCastExtensions.byteValue((CharSequence) null));
	}

	@Test
	public void toByte_CharSequence() {
		assertEquals((byte) 4, PrimitiveCastExtensions.toByte("4"));
		assertEquals((byte) 10, PrimitiveCastExtensions.toByte("0xa"));
		assertEquals((byte) -4, PrimitiveCastExtensions.toByte("-4"));
		assertEquals((byte) -10, PrimitiveCastExtensions.toByte("-0xa"));
		assertEquals((byte) 0, PrimitiveCastExtensions.toByte(""));
		assertEquals((byte) 0, PrimitiveCastExtensions.toByte("z"));
		assertEquals((byte) 0, PrimitiveCastExtensions.toByte((CharSequence) null));
	}

	@Test
	public void toByte_Object() {
		assertEquals((byte) 0, PrimitiveCastExtensions.toByte((Object) null));
		assertEquals((byte) 4, PrimitiveCastExtensions.toByte("4"));
		assertEquals((byte) 4, PrimitiveCastExtensions.toByte((byte) 4));
	}

	@Test
	public void shortValue_Object() {
		assertEquals(4, PrimitiveCastExtensions.shortValue(4));
		assertEquals(4, PrimitiveCastExtensions.shortValue("4"));
		assertEquals(0, PrimitiveCastExtensions.shortValue((Object) null));
	}

	@Test
	public void shortValue_CharSequence() {
		assertEquals(4, PrimitiveCastExtensions.shortValue("4"));
		assertEquals(10, PrimitiveCastExtensions.shortValue("0xa"));
		assertEquals(-4, PrimitiveCastExtensions.shortValue("-4"));
		assertEquals(-10, PrimitiveCastExtensions.shortValue("-0xa"));
		assertEquals(0, PrimitiveCastExtensions.shortValue(""));
		assertEquals(0, PrimitiveCastExtensions.shortValue("z"));
		assertEquals(0, PrimitiveCastExtensions.shortValue((CharSequence) null));
	}

	@Test
	public void toShort_CharSequence() {
		assertEquals((short) 4, PrimitiveCastExtensions.toShort("4"));
		assertEquals((short) 10, PrimitiveCastExtensions.toShort("0xa"));
		assertEquals((short) -4, PrimitiveCastExtensions.toShort("-4"));
		assertEquals((short) -10, PrimitiveCastExtensions.toShort("-0xa"));
		assertEquals((short) 0, PrimitiveCastExtensions.toShort(""));
		assertEquals((short) 0, PrimitiveCastExtensions.toShort("z"));
		assertEquals((short) 0, PrimitiveCastExtensions.toShort((CharSequence) null));
	}

	@Test
	public void toShort_Object() {
		assertEquals((short) 0, PrimitiveCastExtensions.toShort((Object) null));
		assertEquals((short) 4, PrimitiveCastExtensions.toShort((short) 4));
		assertEquals((short) 4, PrimitiveCastExtensions.toShort(4.2));
	}

	@Test
	public void intValue_Object() {
		assertEquals(0, PrimitiveCastExtensions.intValue((Object) null));
		assertEquals(4, PrimitiveCastExtensions.intValue(4));
		assertEquals(4, PrimitiveCastExtensions.intValue("4"));
	}

	@Test
	public void intValue_CharSequence() {
		assertEquals(4, PrimitiveCastExtensions.intValue("4"));
		assertEquals(10, PrimitiveCastExtensions.intValue("0xa"));
		assertEquals(-4, PrimitiveCastExtensions.intValue("-4"));
		assertEquals(-10, PrimitiveCastExtensions.intValue("-0xa"));
		assertEquals(0, PrimitiveCastExtensions.intValue(""));
		assertEquals(0, PrimitiveCastExtensions.intValue("z"));
		assertEquals(0, PrimitiveCastExtensions.intValue((CharSequence) null));
	}

	@Test
	public void toInteger_CharSequence() {
		assertEquals(4, PrimitiveCastExtensions.toInteger("4"));
		assertEquals(10, PrimitiveCastExtensions.toInteger("0xa"));
		assertEquals(-4, PrimitiveCastExtensions.toInteger("-4"));
		assertEquals(-10, PrimitiveCastExtensions.toInteger("-0xa"));
		assertEquals(0, PrimitiveCastExtensions.toInteger(""));
		assertEquals(0, PrimitiveCastExtensions.toInteger("z"));
		assertEquals(0, PrimitiveCastExtensions.toInteger((CharSequence) null));
	}

	@Test
	public void toInteger_Object() {
		assertEquals(0, PrimitiveCastExtensions.toInteger((Object) null));
		assertEquals(4, PrimitiveCastExtensions.toInteger(4));
		assertEquals(4, PrimitiveCastExtensions.toInteger(4.0));
	}

	@Test
	public void longValue_Object() {
		assertEquals(0l, PrimitiveCastExtensions.longValue((Object) null));
		assertEquals(4l, PrimitiveCastExtensions.longValue(4));
		assertEquals(4l, PrimitiveCastExtensions.longValue("4"));
	}

	@Test
	public void longValue_CharSequence() {
		assertEquals(4l, PrimitiveCastExtensions.longValue("4"));
		assertEquals(10l, PrimitiveCastExtensions.longValue("0xa"));
		assertEquals(-4l, PrimitiveCastExtensions.longValue("-4"));
		assertEquals(-10l, PrimitiveCastExtensions.longValue("-0xa"));
		assertEquals(0l, PrimitiveCastExtensions.longValue(""));
		assertEquals(0l, PrimitiveCastExtensions.longValue("z"));
		assertEquals(0l, PrimitiveCastExtensions.longValue((CharSequence) null));
	}

	@Test
	public void toLong_CharSequence() {
		assertEquals(4l, PrimitiveCastExtensions.toLong("4"));
		assertEquals(10l, PrimitiveCastExtensions.toLong("0xa"));
		assertEquals(-4l, PrimitiveCastExtensions.toLong("-4"));
		assertEquals(-10l, PrimitiveCastExtensions.toLong("-0xa"));
		assertEquals(0l, PrimitiveCastExtensions.toLong(""));
		assertEquals(0l, PrimitiveCastExtensions.toLong("z"));
		assertEquals(0l, PrimitiveCastExtensions.toLong((CharSequence) null));
	}

	@Test
	public void toLong_Object() {
		assertEquals(0l, PrimitiveCastExtensions.toLong((Object) null));
		assertEquals(4l, PrimitiveCastExtensions.toLong(4l));
		assertEquals(4l, PrimitiveCastExtensions.toLong(4.2));
	}

	@Test
	public void floatValue_Object() {
		assertEpsilonEquals(0f, PrimitiveCastExtensions.floatValue((Object) null));
		assertEpsilonEquals(4f, PrimitiveCastExtensions.floatValue(4f));
		assertEpsilonEquals(4f, PrimitiveCastExtensions.floatValue("4"));
	}

	@Test
	public void floatValue_CharSequence() {
		assertEpsilonEquals(4f, PrimitiveCastExtensions.floatValue("4"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.floatValue("0xa"));
		assertEpsilonEquals(-4f, PrimitiveCastExtensions.floatValue("-4"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.floatValue("-0xa"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.floatValue(""));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.floatValue("z"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.floatValue((Object) null));
	}

	@Test
	public void toFloat_CharSequence() {
		assertEpsilonEquals(4f, PrimitiveCastExtensions.toFloat("4"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.toFloat("0xa"));
		assertEpsilonEquals(-4f, PrimitiveCastExtensions.toFloat("-4"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.toFloat("-0xa"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.toFloat(""));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.toFloat("z"));
		assertEpsilonEquals(0f, PrimitiveCastExtensions.toFloat((CharSequence) null));
	}

	@Test
	public void toFloat_Object() {
		assertEpsilonEquals(0f, PrimitiveCastExtensions.toFloat((Object) null));
		assertEpsilonEquals(4f, PrimitiveCastExtensions.toFloat(4.0));
		assertEpsilonEquals(4f, PrimitiveCastExtensions.toFloat(Float.valueOf(4.f)));
	}

	@Test
	public void doubleValue_Object() {
		assertEpsilonEquals(0., PrimitiveCastExtensions.doubleValue((Object) null));
		assertEpsilonEquals(4.2, PrimitiveCastExtensions.doubleValue(4.2));
		assertEpsilonEquals(4.2, PrimitiveCastExtensions.doubleValue("4.2"));
	}

	@Test
	public void doubleValue_CharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.doubleValue("4"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.doubleValue("0xa"));
		assertEpsilonEquals(-4., PrimitiveCastExtensions.doubleValue("-4"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.doubleValue("-0xa"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.doubleValue(""));
		assertEpsilonEquals(0., PrimitiveCastExtensions.doubleValue("z"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.doubleValue((CharSequence) null));
	}

	@Test
	public void toDouble_CharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.toDouble("4"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.toDouble("0xa"));
		assertEpsilonEquals(-4., PrimitiveCastExtensions.toDouble("-4"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.toDouble("-0xa"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.toDouble(""));
		assertEpsilonEquals(0., PrimitiveCastExtensions.toDouble("z"));
		assertEpsilonEquals(0., PrimitiveCastExtensions.toDouble((CharSequence) null));
	}

	@Test
	public void toDouble_Object() {
		assertEpsilonEquals(0., PrimitiveCastExtensions.toDouble((Object) null));
		assertEpsilonEquals(4.2, PrimitiveCastExtensions.toDouble(4.2));
		assertEpsilonEquals(4., PrimitiveCastExtensions.toDouble("4"));
	}

	@Test
	public void toAtomicBoolean_CharSequence() {
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("4").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("0xa").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("-4").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("-0xa").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean("z").get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean((CharSequence) null).get());
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
	public void toAtomicBoolean_Object() {
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean((Object) null).get());
		assertTrue(PrimitiveCastExtensions.toAtomicBoolean(Boolean.TRUE).get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean(Boolean.FALSE).get());
		assertTrue(PrimitiveCastExtensions.toAtomicBoolean(new AtomicBoolean(true)).get());
		assertFalse(PrimitiveCastExtensions.toAtomicBoolean(new AtomicBoolean(false)).get());
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
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicInteger((CharSequence) null).doubleValue());
	}

	@Test
	public void toAtomicInteger_Object() {
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicInteger((Object) null).doubleValue());
		assertEpsilonEquals(4., PrimitiveCastExtensions.toAtomicInteger(4).doubleValue());
		assertEpsilonEquals(4., PrimitiveCastExtensions.toAtomicInteger("4").doubleValue());
	}

	@Test
	public void toAtomicLong_CharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.toAtomicLong("4").doubleValue());
		assertEpsilonEquals(10., PrimitiveCastExtensions.toAtomicLong("0xa").doubleValue());
		assertEpsilonEquals(-4., PrimitiveCastExtensions.toAtomicLong("-4").doubleValue());
		assertEpsilonEquals(-10., PrimitiveCastExtensions.toAtomicLong("-0xa").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicLong("").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicLong("z").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicLong((CharSequence) null).doubleValue());
	}

	@Test
	public void toAtomicLong_Object() {
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicLong((Object) null).doubleValue());
		assertEpsilonEquals(4., PrimitiveCastExtensions.toAtomicLong(4).doubleValue());
		assertEpsilonEquals(5., PrimitiveCastExtensions.toAtomicLong(new AtomicLong(5)).doubleValue());
	}

	@Test
	public void toAtomicDouble_CharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.toAtomicDouble("4").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicDouble("0xa").doubleValue());
		assertEpsilonEquals(-4., PrimitiveCastExtensions.toAtomicDouble("-4").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicDouble("-0xa").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicDouble("").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicDouble("z").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicDouble((CharSequence) null).doubleValue());
	}

	@Test
	public void toAtomicDouble_Object() {
		assertEpsilonEquals(0., PrimitiveCastExtensions.toAtomicDouble((Object) null).doubleValue());
		assertEpsilonEquals(4.2, PrimitiveCastExtensions.toAtomicDouble(4.2).doubleValue());
		assertEpsilonEquals(4., PrimitiveCastExtensions.toAtomicDouble(4).doubleValue());
		assertEpsilonEquals(4.2, PrimitiveCastExtensions.toAtomicDouble(new AtomicDouble(4.2)).doubleValue());
		assertEpsilonEquals(4., PrimitiveCastExtensions.toAtomicDouble(new AtomicDouble(4)).doubleValue());
	}

	@Test
	public void toBigInteger_CharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.toBigInteger("4").doubleValue());
		assertEpsilonEquals(10., PrimitiveCastExtensions.toBigInteger("0xa").doubleValue());
		assertEpsilonEquals(-4., PrimitiveCastExtensions.toBigInteger("-4").doubleValue());
		assertEpsilonEquals(-10., PrimitiveCastExtensions.toBigInteger("-0xa").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigInteger("").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigInteger("z").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigInteger((CharSequence) null).doubleValue());
	}

	@Test
	public void toBigInteger_Object() {
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigInteger((Object) null).doubleValue());
		assertEpsilonEquals(4., PrimitiveCastExtensions.toBigInteger(4).doubleValue());
		assertEpsilonEquals(4., PrimitiveCastExtensions.toBigInteger(4.2).doubleValue());
		assertEpsilonEquals(4., PrimitiveCastExtensions.toBigInteger(new BigDecimal(4.2)).doubleValue());
		assertEpsilonEquals(4., PrimitiveCastExtensions.toBigInteger(new BigInteger("4")).doubleValue());
	}

	@Test
	public void toBigDecimal_CharSequence() {
		assertEpsilonEquals(4., PrimitiveCastExtensions.toBigDecimal("4").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigDecimal("0xa").doubleValue());
		assertEpsilonEquals(-4., PrimitiveCastExtensions.toBigDecimal("-4").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigDecimal("-0xa").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigDecimal("").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigDecimal("z").doubleValue());
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigDecimal((CharSequence) null).doubleValue());
	}

	@Test
	public void toBigDecimal_Object() {
		assertEpsilonEquals(0., PrimitiveCastExtensions.toBigDecimal((Object) null).doubleValue());
		assertEpsilonEquals(4., PrimitiveCastExtensions.toBigDecimal(4.).doubleValue());
		assertEpsilonEquals(4., PrimitiveCastExtensions.toBigDecimal("4.0").doubleValue());
		assertEpsilonEquals(4., PrimitiveCastExtensions.toBigDecimal(new BigDecimal(4.)).doubleValue());
	}

	@Test
	public void toUUID_CharSequence() throws Exception {
		final String strValue0 = "433907b7-f201-4727-907a-9a34eb9a136c";
		final String strValue1 = "d51c801f-9253-4777-82ce-1bfbb5f10ea1";
		final String strValue2 = "zzzzzzzz-xxxx-zzzz-xxxx-zzzzzzzzzzzz";
		assertNull(PrimitiveCastExtensions.toUUID((CharSequence) null));
		assertNull(PrimitiveCastExtensions.toUUID(""));
		assertNull(PrimitiveCastExtensions.toUUID("a"));
		assertEquals(UUID.fromString(strValue0), PrimitiveCastExtensions.toUUID(strValue0));
		assertEquals(UUID.fromString(strValue1), PrimitiveCastExtensions.toUUID(strValue1));
		assertNull(PrimitiveCastExtensions.toUUID(strValue2));
	}

	@Test
	public void toUUID_Object() throws Exception {
		final String strValue0 = "433907b7-f201-4727-907a-9a34eb9a136c";
		final String strValue1 = "d51c801f-9253-4777-82ce-1bfbb5f10ea1";
		final String strValue2 = "zzzzzzzz-xxxx-zzzz-xxxx-zzzzzzzzzzzz";
		assertNull(PrimitiveCastExtensions.toUUID((Object) null));
		assertNull(PrimitiveCastExtensions.toUUID((Object) ""));
		assertNull(PrimitiveCastExtensions.toUUID((Object) "a"));
		assertEquals(UUID.fromString(strValue0), PrimitiveCastExtensions.toUUID((Object) strValue0));
		assertEquals(UUID.fromString(strValue1), PrimitiveCastExtensions.toUUID((Object) strValue1));
		assertNull(PrimitiveCastExtensions.toUUID((Object) strValue2));
		final UUID rnd = UUID.randomUUID();
		assertSame(rnd, PrimitiveCastExtensions.toUUID(rnd));
	}

}
