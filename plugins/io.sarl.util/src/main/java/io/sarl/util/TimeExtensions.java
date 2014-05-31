/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.util;
/**
 * Static methods to convert different time units to milliseconds
 * 
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class TimeExtensions {
	private static final long MILLIS_IN_SECOND = 1000;
	private static final long MILLIS_IN_MINUTE = 60 * MILLIS_IN_SECOND;
	private static final long MILLIS_IN_HOUR = 60 * MILLIS_IN_MINUTE;
	private static final long MILLIS_IN_DAY = 24 * MILLIS_IN_HOUR;
	private static final long MILLIS_IN_WEEK = 7 * MILLIS_IN_DAY;

	/** Convert milliseconds to milliseconds.
	 * 
	 * @param milis
	 * @return the number of milliseconds in <var>milis</var>
	 */
	public static long milliseconds(Integer milis) {
		return milis;
	}	
	
	/** Convert seconds to milliseconds.
	 * 
	 * @param secs
	 * @return the number of milliseconds in <var>secs</var>
	 */
	public static long seconds(Integer secs) {
		return secs * MILLIS_IN_SECOND;
	}
	
	/** Convert minutes to milliseconds.
	 * 
	 * @param mins
	 * @return the number of milliseconds in <var>mins</var>
	 */
	public static long minutes(Integer mins) {
		return mins * MILLIS_IN_MINUTE;
	}
	
	/** Convert hours to milliseconds.
	 * 
	 * @param hours
	 * @return the number of milliseconds in <var>hours</var>
	 */
	public static long hours(Integer hours) {
		return hours * MILLIS_IN_HOUR;
	}
	
	/** Convert days to milliseconds.
	 * 
	 * @param days
	 * @return the number of days in <var>days</var>
	 */
	public static long days(Integer days) {
		return days * MILLIS_IN_DAY;
	}
	
	/** Convert weeks to milliseconds.
	 * 
	 * @param weeks
	 * @return the number of milliseconds in <var>weeks</var>
	 */
	public static long weeks(Integer weeks) {
		return weeks * MILLIS_IN_WEEK;
	}
}
