/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.util;

/**
 * Static methods to convert different time units to milliseconds.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @deprecated see SARLTimeExtensions
 */
@Deprecated
public final class TimeExtensions {

	private static final long MILLIS_IN_SECOND = 1000;

	private static final long MILLIS_IN_MINUTE = 60 * MILLIS_IN_SECOND;

	private static final long MILLIS_IN_HOUR = 60 * MILLIS_IN_MINUTE;

	private static final long MILLIS_IN_DAY = 24 * MILLIS_IN_HOUR;

	private static final long MILLIS_IN_WEEK = 7 * MILLIS_IN_DAY;

	private TimeExtensions() {
		//
	}

	/** Convert milliseconds to milliseconds.
	 *
	 * @param milis - number of milliseconds to convert.
	 * @return the number of milliseconds in <code>milis</code>.
	 */
	public static long milliseconds(Integer milis) {
		return milis;
	}

	/** Convert seconds to milliseconds.
	 *
	 * @param secs - number of seconds to convert.
	 * @return the number of milliseconds in seconds.
	 */
	public static long seconds(Integer secs) {
		return secs * MILLIS_IN_SECOND;
	}

	/** Convert minutes to milliseconds.
	 *
	 * @param mins - number of minutes to convert.
	 * @return the number of milliseconds in <code>mins</code>
	 */
	public static long minutes(Integer mins) {
		return mins * MILLIS_IN_MINUTE;
	}

	/** Convert hours to milliseconds.
	 *
	 * @param hours - number of hours to convert.
	 * @return the number of milliseconds in <code>hours</code>
	 */
	public static long hours(Integer hours) {
		return hours * MILLIS_IN_HOUR;
	}

	/** Convert days to milliseconds.
	 *
	 * @param days - number of days to convert.
	 * @return the number of days in <code>days</code>
	 */
	public static long days(Integer days) {
		return days * MILLIS_IN_DAY;
	}

	/** Convert weeks to milliseconds.
	 *
	 * @param weeks - number of weeks to convert.
	 * @return the number of milliseconds in <code>weeks</code>
	 */
	public static long weeks(Integer weeks) {
		return weeks * MILLIS_IN_WEEK;
	}

}
