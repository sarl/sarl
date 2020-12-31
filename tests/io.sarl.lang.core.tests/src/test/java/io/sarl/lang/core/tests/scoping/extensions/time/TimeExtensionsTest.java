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
package io.sarl.lang.core.tests.scoping.extensions.time;

import static org.junit.Assert.assertEquals;

import java.util.concurrent.TimeUnit;

import org.junit.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;

import io.sarl.lang.scoping.extensions.time.TimeExtensions;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("TimeExtensions")
@Tag("core")
@Tag("unit")
public class TimeExtensionsTest {

	@Test
	public void millisecondsNumber() {
		assertEquals(12l, TimeExtensions.milliseconds(new Byte((byte) 12)));
		assertEquals(-23l, TimeExtensions.milliseconds(new Byte((byte) -23)));
		assertEquals(12l, TimeExtensions.milliseconds(new Short((short) 12)));
		assertEquals(-23l, TimeExtensions.milliseconds(new Short((short) -23)));
		assertEquals(12l, TimeExtensions.milliseconds(new Integer(12)));
		assertEquals(-23l, TimeExtensions.milliseconds(new Integer(-23)));
		assertEquals(12l, TimeExtensions.milliseconds(new Long(12)));
		assertEquals(-23l, TimeExtensions.milliseconds(new Long(-23)));
		assertEquals(12l, TimeExtensions.milliseconds(new Float(12.56)));
		assertEquals(-23l, TimeExtensions.milliseconds(new Float(-23.567)));
		assertEquals(12l, TimeExtensions.milliseconds(new Double(12.56)));
		assertEquals(-23l, TimeExtensions.milliseconds(new Double(-23.567)));
	}

	@Test
	public void millisecondsByte() {
		assertEquals(12l, TimeExtensions.milliseconds((byte) 12));
		assertEquals(-23l, TimeExtensions.milliseconds((byte) -23));
	}

	@Test
	public void millisecondsShort() {
		assertEquals(12l, TimeExtensions.milliseconds((short) 12));
		assertEquals(-23l, TimeExtensions.milliseconds((short) -23));
	}

	@Test
	public void millisecondsInt() {
		assertEquals(1234l, TimeExtensions.milliseconds(1234));
		assertEquals(-234l, TimeExtensions.milliseconds(-234));
	}

	@Test
	public void millisecondsLong() {
		assertEquals(1234l, TimeExtensions.milliseconds(1234l));
		assertEquals(-234l, TimeExtensions.milliseconds(-234l));
	}

	@Test
	public void millisecondsFloat() {
		assertEquals(1234l, TimeExtensions.milliseconds(1234.456f));
		assertEquals(-234l, TimeExtensions.milliseconds(-234.456f));
	}

	@Test
	public void millisecondsDouble() {
		assertEquals(1234l, TimeExtensions.milliseconds(1234.456));
		assertEquals(-234l, TimeExtensions.milliseconds(-234.456));
	}

	@Test
	public void secondsNumber() {
		assertEquals(12000l, TimeExtensions.seconds(new Byte((byte) 12)));
		assertEquals(-23000l, TimeExtensions.seconds(new Byte((byte) -23)));
		assertEquals(12000l, TimeExtensions.seconds(new Short((short) 12)));
		assertEquals(-23000l, TimeExtensions.seconds(new Short((short) -23)));
		assertEquals(12000l, TimeExtensions.seconds(new Integer(12)));
		assertEquals(-23000l, TimeExtensions.seconds(new Integer(-23)));
		assertEquals(12000l, TimeExtensions.seconds(new Long(12)));
		assertEquals(-23000l, TimeExtensions.seconds(new Long(-23)));
		assertEquals(12560l, TimeExtensions.seconds(new Float(12.56)));
		assertEquals(-23566l, TimeExtensions.seconds(new Float(-23.567)));
		assertEquals(12560l, TimeExtensions.seconds(new Double(12.56)));
		assertEquals(-23567l, TimeExtensions.seconds(new Double(-23.567)));
	}

	@Test
	public void secondsByte() {
		assertEquals(12000l, TimeExtensions.seconds((byte) 12));
		assertEquals(-23000l, TimeExtensions.seconds((byte) -23));
	}

	@Test
	public void secondsShort() {
		assertEquals(12000l, TimeExtensions.seconds((short) 12));
		assertEquals(-23000l, TimeExtensions.seconds((short) -23));
	}

	@Test
	public void secondsInt() {
		assertEquals(1234000l, TimeExtensions.seconds(1234));
		assertEquals(-234000l, TimeExtensions.seconds(-234));
	}

	@Test
	public void secondsLong() {
		assertEquals(1234000l, TimeExtensions.seconds(1234l));
		assertEquals(-234000l, TimeExtensions.seconds(-234l));
	}

	@Test
	public void secondsFloat() {
		assertEquals(1234345l, TimeExtensions.seconds(1234.3456f));
		assertEquals(-234567l, TimeExtensions.seconds(-234.56778f));
	}

	@Test
	public void secondsDouble() {
		assertEquals(1234345l, TimeExtensions.seconds(1234.3456));
		assertEquals(-234567l, TimeExtensions.seconds(-234.56778));
	}

	@Test
	public void minutesByte() {
		assertEquals(720000l, TimeExtensions.minutes((byte) 12));
		assertEquals(-1380000l, TimeExtensions.minutes((byte) -23));
	}

	@Test
	public void minutesShort() {
		assertEquals(720000l, TimeExtensions.minutes((short) 12));
		assertEquals(-1380000l, TimeExtensions.minutes((short) -23));
	}

	@Test
	public void minutesInt() {
		assertEquals(74040000l, TimeExtensions.minutes(1234));
		assertEquals(-14040000l, TimeExtensions.minutes(-234));
	}

	@Test
	public void minutesLong() {
		assertEquals(74040000l, TimeExtensions.minutes(1234l));
		assertEquals(-14040000l, TimeExtensions.minutes(-234l));
	}

	@Test
	public void minutesFloat() {
		assertEquals(74067360l, TimeExtensions.minutes(1234.456f));
		assertEquals(-14067360l, TimeExtensions.minutes(-234.456f));
	}

	@Test
	public void minutesDouble() {
		assertEquals(74067360l, TimeExtensions.minutes(1234.456));
		assertEquals(-14067360l, TimeExtensions.minutes(-234.456));
	}

	@Test
	public void hoursByte() {
		assertEquals(43200000l, TimeExtensions.hours((byte) 12));
		assertEquals(-82800000l, TimeExtensions.hours((byte) -23));
	}

	@Test
	public void hoursShort() {
		assertEquals(43200000l, TimeExtensions.hours((short) 12));
		assertEquals(-82800000l, TimeExtensions.hours((short) -23));
	}

	@Test
	public void hoursInt() {
		assertEquals(4442400000l, TimeExtensions.hours(1234));
		assertEquals(-842400000l, TimeExtensions.hours(-234));
	}

	@Test
	public void hoursLong() {
		assertEquals(4442400000l, TimeExtensions.hours(1234l));
		assertEquals(-842400000l, TimeExtensions.hours(-234l));
	}

	@Test
	public void daysByte() {
		assertEquals(10627200000l, TimeExtensions.days((byte) 123));
		assertEquals(-1987200000l, TimeExtensions.days((byte) -23));
	}

	@Test
	public void daysShort() {
		assertEquals(10627200000l, TimeExtensions.days((short) 123));
		assertEquals(-1987200000l, TimeExtensions.days((short) -23));
	}

	@Test
	public void daysInt() {
		assertEquals(106617600000l, TimeExtensions.days(1234));
		assertEquals(-20217600000l, TimeExtensions.days(-234));
	}

	@Test
	public void daysLong() {
		assertEquals(106617600000l, TimeExtensions.days(1234l));
		assertEquals(-20217600000l, TimeExtensions.days(-234l));
	}

	@Test
	public void weeksByte() {
		assertEquals(74390400000l, TimeExtensions.weeks((byte) 123));
		assertEquals(-13910400000l, TimeExtensions.weeks((byte) -23));
	}

	@Test
	public void weeksShort() {
		assertEquals(74390400000l, TimeExtensions.weeks((short) 123));
		assertEquals(-13910400000l, TimeExtensions.weeks((short) -23));
	}

	@Test
	public void weeksInt() {
		assertEquals(746323200000l, TimeExtensions.weeks(1234));
		assertEquals(-141523200000l, TimeExtensions.weeks(-234));
	}

	@Test
	public void weeksLong() {
		assertEquals(746323200000l, TimeExtensions.weeks(1234l));
		assertEquals(-141523200000l, TimeExtensions.weeks(-234l));
	}

	@Test
	public void convertFromTo() {
		assertEquals(106617600000l, (long) TimeExtensions.convertFromTo(1234, TimeUnit.DAYS, TimeUnit.MILLISECONDS));
		assertEquals(-20217600000l, (long) TimeExtensions.convertFromTo(-234, TimeUnit.DAYS, TimeUnit.MILLISECONDS));
	}

}
