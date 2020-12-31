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

package io.sarl.lang.scoping.extensions.time;

import java.util.concurrent.TimeUnit;

import com.google.common.annotations.GwtCompatible;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Static methods to convert different time units to milliseconds.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@GwtCompatible
public final class TimeExtensions {

	/** Number of millis in a second.
	 */
	public static final long MILLIS_IN_SECOND = 1000;

	/** Number of millis in a minute.
	 */
	public static final long MILLIS_IN_MINUTE = 60000;

	/** Number of millis in an hour.
	 */
	public static final long MILLIS_IN_HOUR = 3600000;

	/** Number of millis in a day.
	 */
	public static final long MILLIS_IN_DAY = 86400000;

	/** Number of millis in a week.
	 */
	public static final long MILLIS_IN_WEEK = 604800000;

	/** Number of millis in a microsecond.
	 */
	public static final double MILLIS_IN_MICROSECOND = 10e-6;

	/** Number of millis in a nanosecond.
	 */
	public static final double MILLIS_IN_NANOSECOND = 10e-9;

	private TimeExtensions() {
		//
	}

	/** Convert milliseconds to milliseconds.
	 *
	 * @param milis number of milliseconds to convert.
	 * @return the number of milliseconds in <code>milis</code>.
	 */
	@Pure
	@Inline(value = "($1).longValue()")
	public static long milliseconds(Number milis) {
		return milis.longValue();
	}

	/** Convert milliseconds to milliseconds.
	 *
	 * @param milis number of milliseconds to convert.
	 * @return the number of milliseconds in <code>milis</code>.
	 */
	@Pure
	@Inline(value = "(long) ($1)")
	public static long milliseconds(byte milis) {
		return milis;
	}

	/** Convert milliseconds to milliseconds.
	 *
	 * @param milis number of milliseconds to convert.
	 * @return the number of milliseconds in <code>milis</code>.
	 */
	@Pure
	@Inline(value = "(long) ($1)")
	public static long milliseconds(short milis) {
		return milis;
	}

	/** Convert milliseconds to milliseconds.
	 *
	 * @param milis number of milliseconds to convert.
	 * @return the number of milliseconds in <code>milis</code>.
	 */
	@Pure
	@Inline(value = "(long) ($1)")
	public static long milliseconds(int milis) {
		return milis;
	}

	/** Convert milliseconds to milliseconds.
	 *
	 * @param milis number of milliseconds to convert.
	 * @return the number of milliseconds in <code>milis</code>.
	 */
	@Pure
	@Inline(value = "(long) ($1)")
	public static long milliseconds(long milis) {
		return milis;
	}

	/** Convert milliseconds to milliseconds.
	 *
	 * @param milis number of milliseconds to convert.
	 * @return the number of milliseconds in <code>milis</code>.
	 */
	@Pure
	@Inline(value = "(long) ($1)")
	public static long milliseconds(float milis) {
		return (long) milis;
	}

	/** Convert milliseconds to milliseconds.
	 *
	 * @param milis number of milliseconds to convert.
	 * @return the number of milliseconds in <code>milis</code>.
	 */
	@Pure
	@Inline(value = "(long) ($1)")
	public static long milliseconds(double milis) {
		return (long) milis;
	}

	/** Convert seconds to milliseconds.
	 *
	 * @param secs number of seconds to convert.
	 * @return the number of milliseconds in seconds.
	 */
	@Pure
	@Inline(value = "(long) (($1).doubleValue() * $2.MILLIS_IN_SECOND)", imported = {TimeExtensions.class})
	public static long seconds(Number secs) {
		return (long) (secs.doubleValue() * MILLIS_IN_SECOND);
	}

	/** Convert seconds to milliseconds.
	 *
	 * @param secs number of seconds to convert.
	 * @return the number of milliseconds in seconds.
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_SECOND", imported = {TimeExtensions.class})
	public static long seconds(byte secs) {
		return secs * MILLIS_IN_SECOND;
	}

	/** Convert seconds to milliseconds.
	 *
	 * @param secs number of seconds to convert.
	 * @return the number of milliseconds in seconds.
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_SECOND", imported = {TimeExtensions.class})
	public static long seconds(short secs) {
		return secs * MILLIS_IN_SECOND;
	}

	/** Convert seconds to milliseconds.
	 *
	 * @param secs number of seconds to convert.
	 * @return the number of milliseconds in seconds.
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_SECOND", imported = {TimeExtensions.class})
	public static long seconds(int secs) {
		return secs * MILLIS_IN_SECOND;
	}

	/** Convert seconds to milliseconds.
	 *
	 * @param secs number of seconds to convert.
	 * @return the number of milliseconds in seconds.
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_SECOND", imported = {TimeExtensions.class})
	public static long seconds(long secs) {
		return secs * MILLIS_IN_SECOND;
	}

	/** Convert seconds to milliseconds.
	 *
	 * @param secs number of seconds to convert.
	 * @return the number of milliseconds in seconds.
	 */
	@Pure
	@Inline(value = "(long) (($1) * $2.MILLIS_IN_SECOND)", imported = {TimeExtensions.class})
	public static long seconds(float secs) {
		return (long) (secs * MILLIS_IN_SECOND);
	}

	/** Convert seconds to milliseconds.
	 *
	 * @param secs number of seconds to convert.
	 * @return the number of milliseconds in seconds.
	 */
	@Pure
	@Inline(value = "(long) (($1) * $2.MILLIS_IN_SECOND)", imported = {TimeExtensions.class})
	public static long seconds(double secs) {
		return (long) (secs * MILLIS_IN_SECOND);
	}

	/** Convert minutes to milliseconds.
	 *
	 * @param mins number of minutes to convert.
	 * @return the number of milliseconds in <code>mins</code>
	 */
	@Pure
	@Inline(value = "(long) (($1).doubleValue() * $2.MILLIS_IN_MINUTE)", imported = {TimeExtensions.class})
	public static long minutes(Number mins) {
		return (long) (mins.doubleValue() * MILLIS_IN_MINUTE);
	}

	/** Convert minutes to milliseconds.
	 *
	 * @param mins number of minutes to convert.
	 * @return the number of milliseconds in <code>mins</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_MINUTE", imported = {TimeExtensions.class})
	public static long minutes(byte mins) {
		return mins * MILLIS_IN_MINUTE;
	}

	/** Convert minutes to milliseconds.
	 *
	 * @param mins number of minutes to convert.
	 * @return the number of milliseconds in <code>mins</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_MINUTE", imported = {TimeExtensions.class})
	public static long minutes(short mins) {
		return mins * MILLIS_IN_MINUTE;
	}

	/** Convert minutes to milliseconds.
	 *
	 * @param mins number of minutes to convert.
	 * @return the number of milliseconds in <code>mins</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_MINUTE", imported = {TimeExtensions.class})
	public static long minutes(int mins) {
		return mins * MILLIS_IN_MINUTE;
	}

	/** Convert minutes to milliseconds.
	 *
	 * @param mins number of minutes to convert.
	 * @return the number of milliseconds in <code>mins</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_MINUTE", imported = {TimeExtensions.class})
	public static long minutes(long mins) {
		return mins * MILLIS_IN_MINUTE;
	}

	/** Convert minutes to milliseconds.
	 *
	 * @param mins number of minutes to convert.
	 * @return the number of milliseconds in <code>mins</code>
	 */
	@Pure
	@Inline(value = "(long) (($1) * $2.MILLIS_IN_MINUTE)", imported = {TimeExtensions.class})
	public static long minutes(float mins) {
		return (long) (mins * MILLIS_IN_MINUTE);
	}

	/** Convert minutes to milliseconds.
	 *
	 * @param mins number of minutes to convert.
	 * @return the number of milliseconds in <code>mins</code>
	 */
	@Pure
	@Inline(value = "(long) (($1) * $2.MILLIS_IN_MINUTE)", imported = {TimeExtensions.class})
	public static long minutes(double mins) {
		return (long) (mins * MILLIS_IN_MINUTE);
	}

	/** Convert hours to milliseconds.
	 *
	 * @param hours number of hours to convert.
	 * @return the number of milliseconds in <code>hours</code>
	 */
	@Pure
	@Inline(value = "(long) (($1).doubleValue() * $2.MILLIS_IN_HOUR)", imported = {TimeExtensions.class})
	public static long hours(Number hours) {
		return (long) (hours.doubleValue() * MILLIS_IN_HOUR);
	}

	/** Convert hours to milliseconds.
	 *
	 * @param hours number of hours to convert.
	 * @return the number of milliseconds in <code>hours</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_HOUR", imported = {TimeExtensions.class})
	public static long hours(byte hours) {
		return hours * MILLIS_IN_HOUR;
	}

	/** Convert hours to milliseconds.
	 *
	 * @param hours number of hours to convert.
	 * @return the number of milliseconds in <code>hours</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_HOUR", imported = {TimeExtensions.class})
	public static long hours(short hours) {
		return hours * MILLIS_IN_HOUR;
	}

	/** Convert hours to milliseconds.
	 *
	 * @param hours number of hours to convert.
	 * @return the number of milliseconds in <code>hours</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_HOUR", imported = {TimeExtensions.class})
	public static long hours(int hours) {
		return hours * MILLIS_IN_HOUR;
	}

	/** Convert hours to milliseconds.
	 *
	 * @param hours number of hours to convert.
	 * @return the number of milliseconds in <code>hours</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_HOUR", imported = {TimeExtensions.class})
	public static long hours(long hours) {
		return hours * MILLIS_IN_HOUR;
	}

	/** Convert hours to milliseconds.
	 *
	 * @param hours number of hours to convert.
	 * @return the number of milliseconds in <code>hours</code>
	 */
	@Pure
	@Inline(value = "(long) (($1) * $2.MILLIS_IN_HOUR)", imported = {TimeExtensions.class})
	public static long hours(float hours) {
		return (long) (hours * MILLIS_IN_HOUR);
	}

	/** Convert hours to milliseconds.
	 *
	 * @param hours number of hours to convert.
	 * @return the number of milliseconds in <code>hours</code>
	 */
	@Pure
	@Inline(value = "(long) (($1) * $2.MILLIS_IN_HOUR)", imported = {TimeExtensions.class})
	public static long hours(double hours) {
		return (long) (hours * MILLIS_IN_HOUR);
	}

	/** Convert days to milliseconds.
	 *
	 * @param days number of days to convert.
	 * @return the number of days in <code>days</code>
	 */
	@Pure
	@Inline(value = "(long) (($1).doubleValue() * $2.MILLIS_IN_DAY)", imported = {TimeExtensions.class})
	public static long days(Number days) {
		return (long) (days.doubleValue() * MILLIS_IN_DAY);
	}

	/** Convert days to milliseconds.
	 *
	 * @param days number of days to convert.
	 * @return the number of days in <code>days</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_DAY", imported = {TimeExtensions.class})
	public static long days(byte days) {
		return days * MILLIS_IN_DAY;
	}

	/** Convert days to milliseconds.
	 *
	 * @param days number of days to convert.
	 * @return the number of days in <code>days</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_DAY", imported = {TimeExtensions.class})
	public static long days(short days) {
		return days * MILLIS_IN_DAY;
	}

	/** Convert days to milliseconds.
	 *
	 * @param days number of days to convert.
	 * @return the number of days in <code>days</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_DAY", imported = {TimeExtensions.class})
	public static long days(int days) {
		return days * MILLIS_IN_DAY;
	}

	/** Convert days to milliseconds.
	 *
	 * @param days number of days to convert.
	 * @return the number of days in <code>days</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_DAY", imported = {TimeExtensions.class})
	public static long days(long days) {
		return days * MILLIS_IN_DAY;
	}

	/** Convert days to milliseconds.
	 *
	 * @param days number of days to convert.
	 * @return the number of days in <code>days</code>
	 */
	@Pure
	@Inline(value = "(long) (($1) * $2.MILLIS_IN_DAY)", imported = {TimeExtensions.class})
	public static long days(float days) {
		return (long) (days * MILLIS_IN_DAY);
	}

	/** Convert days to milliseconds.
	 *
	 * @param days number of days to convert.
	 * @return the number of days in <code>days</code>
	 */
	@Pure
	@Inline(value = "(long) (($1) * $2.MILLIS_IN_DAY)", imported = {TimeExtensions.class})
	public static long days(double days) {
		return (long) (days * MILLIS_IN_DAY);
	}

	/** Convert weeks to milliseconds.
	 *
	 * @param weeks number of weeks to convert.
	 * @return the number of milliseconds in <code>weeks</code>
	 */
	@Pure
	@Inline(value = "(long) (($1).doubleValue() * $2.MILLIS_IN_WEEK)", imported = {TimeExtensions.class})
	public static long weeks(Number weeks) {
		return (long) (weeks.doubleValue() * MILLIS_IN_WEEK);
	}

	/** Convert weeks to milliseconds.
	 *
	 * @param weeks number of weeks to convert.
	 * @return the number of milliseconds in <code>weeks</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_WEEK", imported = {TimeExtensions.class})
	public static long weeks(byte weeks) {
		return weeks * MILLIS_IN_WEEK;
	}

	/** Convert weeks to milliseconds.
	 *
	 * @param weeks number of weeks to convert.
	 * @return the number of milliseconds in <code>weeks</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_WEEK", imported = {TimeExtensions.class})
	public static long weeks(short weeks) {
		return weeks * MILLIS_IN_WEEK;
	}

	/** Convert weeks to milliseconds.
	 *
	 * @param weeks number of weeks to convert.
	 * @return the number of milliseconds in <code>weeks</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_WEEK", imported = {TimeExtensions.class})
	public static long weeks(int weeks) {
		return weeks * MILLIS_IN_WEEK;
	}

	/** Convert weeks to milliseconds.
	 *
	 * @param weeks number of weeks to convert.
	 * @return the number of milliseconds in <code>weeks</code>
	 */
	@Pure
	@Inline(value = "($1) * $2.MILLIS_IN_WEEK", imported = {TimeExtensions.class})
	public static long weeks(long weeks) {
		return weeks * MILLIS_IN_WEEK;
	}

	/** Convert weeks to milliseconds.
	 *
	 * @param weeks number of weeks to convert.
	 * @return the number of milliseconds in <code>weeks</code>
	 */
	@Pure
	@Inline(value = "(long) (($1) * $2.MILLIS_IN_WEEK)", imported = {TimeExtensions.class})
	public static long weeks(float weeks) {
		return (long) (weeks * MILLIS_IN_WEEK);
	}

	/** Convert weeks to milliseconds.
	 *
	 * @param weeks number of weeks to convert.
	 * @return the number of milliseconds in <code>weeks</code>
	 */
	@Pure
	@Inline(value = "(long) (($1) * $2.MILLIS_IN_WEEK)", imported = {TimeExtensions.class})
	public static long weeks(double weeks) {
		return (long) (weeks * MILLIS_IN_WEEK);
	}

	/** Convert the gien amount of time in the given source unit, to the given target unit.
	 * In opposite to {@link TimeUnit#convert(long, TimeUnit)}, this function works on
	 * double floating-point values.
	 *
	 * @param time the amount of time.
	 * @param source the source unit. Never {@code null}.
	 * @param target the target unit. Never {@code null}.
	 * @return the amount of time in the target unit.
	 */
	@Pure
	@SuppressWarnings("checkstyle:cyclomaticcomplexity")
	public static double convertFromTo(double time, TimeUnit source, TimeUnit target) {
		assert source != null;
		assert target != null;
		if (source == target) {
			return time;
		}
		final double millis;
		switch (source) {
		case DAYS:
			millis = time * MILLIS_IN_DAY;
			break;
		case HOURS:
			millis = time * MILLIS_IN_HOUR;
			break;
		case MINUTES:
			millis = time * MILLIS_IN_MINUTE;
			break;
		case SECONDS:
			millis = time * MILLIS_IN_SECOND;
			break;
		case MILLISECONDS:
			millis = time;
			break;
		case NANOSECONDS:
			millis = time * MILLIS_IN_NANOSECOND;
			break;
		case MICROSECONDS:
			millis = time * MILLIS_IN_MICROSECOND;
			break;
		default:
			throw new IllegalArgumentException();
		}
		switch (target) {
		case DAYS:
			return millis / MILLIS_IN_DAY;
		case HOURS:
			return millis / MILLIS_IN_HOUR;
		case MINUTES:
			return millis / MILLIS_IN_MINUTE;
		case SECONDS:
			return millis / MILLIS_IN_SECOND;
		case MILLISECONDS:
			return millis;
		case NANOSECONDS:
			return millis / MILLIS_IN_NANOSECOND;
		case MICROSECONDS:
			return millis / MILLIS_IN_MICROSECOND;
		default:
			throw new IllegalArgumentException();
		}
	}

}
