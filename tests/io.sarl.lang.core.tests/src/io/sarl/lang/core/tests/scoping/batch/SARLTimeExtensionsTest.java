/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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
package io.sarl.lang.core.tests.scoping.batch;

import static org.junit.Assert.assertEquals;

import java.util.concurrent.TimeUnit;

import org.junit.Test;

import io.sarl.lang.scoping.batch.SARLTimeExtensions;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLTimeExtensionsTest {

	@Test
	public void millisecondsNumber() {
		assertEquals(12l, SARLTimeExtensions.milliseconds(new Byte((byte) 12)));
		assertEquals(-23l, SARLTimeExtensions.milliseconds(new Byte((byte) -23)));
		assertEquals(12l, SARLTimeExtensions.milliseconds(new Short((short) 12)));
		assertEquals(-23l, SARLTimeExtensions.milliseconds(new Short((short) -23)));
		assertEquals(12l, SARLTimeExtensions.milliseconds(new Integer(12)));
		assertEquals(-23l, SARLTimeExtensions.milliseconds(new Integer(-23)));
		assertEquals(12l, SARLTimeExtensions.milliseconds(new Long(12)));
		assertEquals(-23l, SARLTimeExtensions.milliseconds(new Long(-23)));
		assertEquals(12l, SARLTimeExtensions.milliseconds(new Float(12.56)));
		assertEquals(-23l, SARLTimeExtensions.milliseconds(new Float(-23.567)));
		assertEquals(12l, SARLTimeExtensions.milliseconds(new Double(12.56)));
		assertEquals(-23l, SARLTimeExtensions.milliseconds(new Double(-23.567)));
	}

	@Test
	public void millisecondsByte() {
		assertEquals(12l, SARLTimeExtensions.milliseconds((byte) 12));
		assertEquals(-23l, SARLTimeExtensions.milliseconds((byte) -23));
	}

	@Test
	public void millisecondsShort() {
		assertEquals(12l, SARLTimeExtensions.milliseconds((short) 12));
		assertEquals(-23l, SARLTimeExtensions.milliseconds((short) -23));
	}

	@Test
	public void millisecondsInt() {
		assertEquals(1234l, SARLTimeExtensions.milliseconds(1234));
		assertEquals(-234l, SARLTimeExtensions.milliseconds(-234));
	}

	@Test
	public void millisecondsLong() {
		assertEquals(1234l, SARLTimeExtensions.milliseconds(1234l));
		assertEquals(-234l, SARLTimeExtensions.milliseconds(-234l));
	}

	@Test
	public void millisecondsFloat() {
		assertEquals(1234l, SARLTimeExtensions.milliseconds(1234.456f));
		assertEquals(-234l, SARLTimeExtensions.milliseconds(-234.456f));
	}

	@Test
	public void millisecondsDouble() {
		assertEquals(1234l, SARLTimeExtensions.milliseconds(1234.456));
		assertEquals(-234l, SARLTimeExtensions.milliseconds(-234.456));
	}

	@Test
	public void secondsNumber() {
		assertEquals(12000l, SARLTimeExtensions.seconds(new Byte((byte) 12)));
		assertEquals(-23000l, SARLTimeExtensions.seconds(new Byte((byte) -23)));
		assertEquals(12000l, SARLTimeExtensions.seconds(new Short((short) 12)));
		assertEquals(-23000l, SARLTimeExtensions.seconds(new Short((short) -23)));
		assertEquals(12000l, SARLTimeExtensions.seconds(new Integer(12)));
		assertEquals(-23000l, SARLTimeExtensions.seconds(new Integer(-23)));
		assertEquals(12000l, SARLTimeExtensions.seconds(new Long(12)));
		assertEquals(-23000l, SARLTimeExtensions.seconds(new Long(-23)));
		assertEquals(12560l, SARLTimeExtensions.seconds(new Float(12.56)));
		assertEquals(-23566l, SARLTimeExtensions.seconds(new Float(-23.567)));
		assertEquals(12560l, SARLTimeExtensions.seconds(new Double(12.56)));
		assertEquals(-23567l, SARLTimeExtensions.seconds(new Double(-23.567)));
	}

	@Test
	public void secondsByte() {
		assertEquals(12000l, SARLTimeExtensions.seconds((byte) 12));
		assertEquals(-23000l, SARLTimeExtensions.seconds((byte) -23));
	}

	@Test
	public void secondsShort() {
		assertEquals(12000l, SARLTimeExtensions.seconds((short) 12));
		assertEquals(-23000l, SARLTimeExtensions.seconds((short) -23));
	}

	@Test
	public void secondsInt() {
		assertEquals(1234000l, SARLTimeExtensions.seconds(1234));
		assertEquals(-234000l, SARLTimeExtensions.seconds(-234));
	}

	@Test
	public void secondsLong() {
		assertEquals(1234000l, SARLTimeExtensions.seconds(1234l));
		assertEquals(-234000l, SARLTimeExtensions.seconds(-234l));
	}

	@Test
	public void secondsFloat() {
		assertEquals(1234345l, SARLTimeExtensions.seconds(1234.3456f));
		assertEquals(-234567l, SARLTimeExtensions.seconds(-234.56778f));
	}

	@Test
	public void secondsDouble() {
		assertEquals(1234345l, SARLTimeExtensions.seconds(1234.3456));
		assertEquals(-234567l, SARLTimeExtensions.seconds(-234.56778));
	}

	@Test
	public void minutesByte() {
		assertEquals(720000l, SARLTimeExtensions.minutes((byte) 12));
		assertEquals(-1380000l, SARLTimeExtensions.minutes((byte) -23));
	}

	@Test
	public void minutesShort() {
		assertEquals(720000l, SARLTimeExtensions.minutes((short) 12));
		assertEquals(-1380000l, SARLTimeExtensions.minutes((short) -23));
	}

	@Test
	public void minutesInt() {
		assertEquals(74040000l, SARLTimeExtensions.minutes(1234));
		assertEquals(-14040000l, SARLTimeExtensions.minutes(-234));
	}

	@Test
	public void minutesLong() {
		assertEquals(74040000l, SARLTimeExtensions.minutes(1234l));
		assertEquals(-14040000l, SARLTimeExtensions.minutes(-234l));
	}

	@Test
	public void minutesFloat() {
		assertEquals(74067360l, SARLTimeExtensions.minutes(1234.456f));
		assertEquals(-14067360l, SARLTimeExtensions.minutes(-234.456f));
	}

	@Test
	public void minutesDouble() {
		assertEquals(74067360l, SARLTimeExtensions.minutes(1234.456));
		assertEquals(-14067360l, SARLTimeExtensions.minutes(-234.456));
	}

	@Test
	public void hoursByte() {
		assertEquals(43200000l, SARLTimeExtensions.hours((byte) 12));
		assertEquals(-82800000l, SARLTimeExtensions.hours((byte) -23));
	}

	@Test
	public void hoursShort() {
		assertEquals(43200000l, SARLTimeExtensions.hours((short) 12));
		assertEquals(-82800000l, SARLTimeExtensions.hours((short) -23));
	}

	@Test
	public void hoursInt() {
		assertEquals(4442400000l, SARLTimeExtensions.hours(1234));
		assertEquals(-842400000l, SARLTimeExtensions.hours(-234));
	}

	@Test
	public void hoursLong() {
		assertEquals(4442400000l, SARLTimeExtensions.hours(1234l));
		assertEquals(-842400000l, SARLTimeExtensions.hours(-234l));
	}

	@Test
	public void daysByte() {
		assertEquals(10627200000l, SARLTimeExtensions.days((byte) 123));
		assertEquals(-1987200000l, SARLTimeExtensions.days((byte) -23));
	}

	@Test
	public void daysShort() {
		assertEquals(10627200000l, SARLTimeExtensions.days((short) 123));
		assertEquals(-1987200000l, SARLTimeExtensions.days((short) -23));
	}

	@Test
	public void daysInt() {
		assertEquals(106617600000l, SARLTimeExtensions.days(1234));
		assertEquals(-20217600000l, SARLTimeExtensions.days(-234));
	}

	@Test
	public void daysLong() {
		assertEquals(106617600000l, SARLTimeExtensions.days(1234l));
		assertEquals(-20217600000l, SARLTimeExtensions.days(-234l));
	}

	@Test
	public void weeksByte() {
		assertEquals(74390400000l, SARLTimeExtensions.weeks((byte) 123));
		assertEquals(-13910400000l, SARLTimeExtensions.weeks((byte) -23));
	}

	@Test
	public void weeksShort() {
		assertEquals(74390400000l, SARLTimeExtensions.weeks((short) 123));
		assertEquals(-13910400000l, SARLTimeExtensions.weeks((short) -23));
	}

	@Test
	public void weeksInt() {
		assertEquals(746323200000l, SARLTimeExtensions.weeks(1234));
		assertEquals(-141523200000l, SARLTimeExtensions.weeks(-234));
	}

	@Test
	public void weeksLong() {
		assertEquals(746323200000l, SARLTimeExtensions.weeks(1234l));
		assertEquals(-141523200000l, SARLTimeExtensions.weeks(-234l));
	}

	@Test
	public void convertFromTo() {
		assertEquals(106617600000l, (long) SARLTimeExtensions.convertFromTo(1234, TimeUnit.DAYS, TimeUnit.MILLISECONDS));
		assertEquals(-20217600000l, (long) SARLTimeExtensions.convertFromTo(-234, TimeUnit.DAYS, TimeUnit.MILLISECONDS));
	}

}
