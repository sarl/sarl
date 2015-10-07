/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.scoping.batch;

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
public final class SARLTimeExtensions {

	private SARLTimeExtensions() {
		//
	}

	/** Convert milliseconds to milliseconds.
	 *
	 * @param milis - number of milliseconds to convert.
	 * @return the number of milliseconds in <code>milis</code>.
	 */
	@Pure
	@Inline(value = "$1")
	public static long milliseconds(Integer milis) {
		return milis;
	}

	/** Convert seconds to milliseconds.
	 *
	 * @param secs - number of seconds to convert.
	 * @return the number of milliseconds in seconds.
	 */
	@Pure
	@Inline(value = "$1 * TimeExtensionsConstants.MILLIS_IN_SECOND",
			imported = { TimeExtensionsConstants.class })
	public static long seconds(Integer secs) {
		return secs * TimeExtensionsConstants.MILLIS_IN_SECOND;
	}

	/** Convert minutes to milliseconds.
	 *
	 * @param mins - number of minutes to convert.
	 * @return the number of milliseconds in <code>mins</code>
	 */
	@Pure
	@Inline(value = "$1 * TimeExtensionsConstants.MILLIS_IN_MINUTE",
			imported = { TimeExtensionsConstants.class })
	public static long minutes(Integer mins) {
		return mins * TimeExtensionsConstants.MILLIS_IN_MINUTE;
	}

	/** Convert hours to milliseconds.
	 *
	 * @param hours - number of hours to convert.
	 * @return the number of milliseconds in <code>hours</code>
	 */
	@Pure
	@Inline(value = "$1 * TimeExtensionsConstants.MILLIS_IN_HOUR",
			imported = { TimeExtensionsConstants.class })
	public static long hours(Integer hours) {
		return hours * TimeExtensionsConstants.MILLIS_IN_HOUR;
	}

	/** Convert days to milliseconds.
	 *
	 * @param days - number of days to convert.
	 * @return the number of days in <code>days</code>
	 */
	@Pure
	@Inline(value = "$1 * TimeExtensionsConstants.MILLIS_IN_DAY",
			imported = { TimeExtensionsConstants.class })
	public static long days(Integer days) {
		return days * TimeExtensionsConstants.MILLIS_IN_DAY;
	}

	/** Convert weeks to milliseconds.
	 *
	 * @param weeks - number of weeks to convert.
	 * @return the number of milliseconds in <code>weeks</code>
	 */
	@Pure
	@Inline(value = "$1 * TimeExtensionsConstants.MILLIS_IN_WEEK",
			imported = { TimeExtensionsConstants.class })
	public static long weeks(Integer weeks) {
		return weeks * TimeExtensionsConstants.MILLIS_IN_WEEK;
	}

	/**
	 * Constants for the SARL time extensions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@GwtCompatible
	public final class TimeExtensionsConstants {

		/** Number of millis in a second.
		 */
		public static final long MILLIS_IN_SECOND = 1000;

		/** Number of millis in a minute.
		 */
		public static final long MILLIS_IN_MINUTE = 60 * MILLIS_IN_SECOND;

		/** Number of millis in an hour.
		 */
		public static final long MILLIS_IN_HOUR = 60 * MILLIS_IN_MINUTE;

		/** Number of millis in a day.
		 */
		public static final long MILLIS_IN_DAY = 24 * MILLIS_IN_HOUR;

		/** Number of millis in a week.
		 */
		public static final long MILLIS_IN_WEEK = 7 * MILLIS_IN_DAY;

		private TimeExtensionsConstants() {
			//
		}

	}

}
