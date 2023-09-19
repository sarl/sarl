/*
 * $Id$
 * This file is a part of the Arakhne Foundation Classes, http://www.arakhne.org/afc
 *
 * Copyright (c) 2000-2012 Stephane GALLAND.
 * Copyright (c) 2005-10, Multiagent Team, Laboratoire Systemes et Transports,
 *                        Universite de Technologie de Belfort-Montbeliard.
 * Copyright (c) 2013-2022 The original authors, and other authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.apputils.bootiqueapp.config;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.base.Strings;

/**
 * Enumeration of the Log4j levels.
 *
 * @author $Author: sgalland$
 * @version bootiqueapp 0.13.0 20230919-093058
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid bootiqueapp
 * @since 15.0
 */
public enum Level {

	/** No logging.
	 */
	OFF(org.apache.log4j.Level.OFF, java.util.logging.Level.OFF),

	/** Error.
	 */
	ERROR(org.apache.log4j.Level.ERROR, java.util.logging.Level.SEVERE),

	/** Warning.
	 */
	WARNING(org.apache.log4j.Level.WARN, java.util.logging.Level.WARNING),

	/** Information.
	 */
	INFO(org.apache.log4j.Level.INFO, java.util.logging.Level.INFO),

	/** Debug.
	 */
	DEBUG(org.apache.log4j.Level.DEBUG, java.util.logging.Level.FINE),

	/** Trace.
	 */
	TRACE(org.apache.log4j.Level.TRACE, java.util.logging.Level.FINEST),

	/** All.
	 */
	ALL(org.apache.log4j.Level.ALL, java.util.logging.Level.ALL);

	private final org.apache.log4j.Level log4j;

	private final java.util.logging.Level jul;

	/** Constructor.
	 *
	 * @param log4j the Log4J equivalent level.
	 * @param jul the JUL  equivalent level.
	 */
	Level(org.apache.log4j.Level log4j, java.util.logging.Level jul) {
		this.log4j = log4j;
		this.jul = jul;
	}

	/** Parse a case insensitive string for obtaining the level.
	 *
	 * @param name the name to parse.
	 * @return the level.
	 * @throws NullPointerException if case the given name is {@code null} or empty.
	 * @since 16.0
	 */
	@JsonCreator
	public static Level valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			throw new NullPointerException("Name is null"); //$NON-NLS-1$
		}
		return valueOf(name.toUpperCase());
	}

	/** Replies the preferred string representation of the level within a Json stream.
	 *
	 * @return the string representation of the level.
	 * @since 16.0
	 */
	@JsonValue
	public String toJsonString() {
		return name().toLowerCase();
	}

	/** Replies the log4j level.
	 *
	 * @return the log4j level.
	 */
	public org.apache.log4j.Level toLog4j() {
		return this.log4j;
	}

	/** Replies the JUL level.
	 *
	 * @return the JUL level.
	 * @since 16.0
	 */
	public java.util.logging.Level toJul() {
		return this.jul;
	}

	/** Replies the level that is equivalent to the given Log4J level.
	 *
	 * @param level the Log4J level.
	 * @return the equivalent level.
	 * @since 16.0
	 */
	public static Level valueOf(org.apache.log4j.Level level) {
		if (level != null) {
			final int idx = level.toInt();
			for (final Level lvl : values()) {
				if (lvl.toLog4j().toInt() <= idx) {
					return lvl;
				}
			}
		}
		throw new IllegalArgumentException();
	}

	/** Replies the level that is equivalent to the given JUL level.
	 *
	 * @param level the JUL level.
	 * @return the equivalent level.
	 * @since 16.0
	 */
	public static Level valueOf(java.util.logging.Level level) {
		if (level != null) {
			final int idx = level.intValue();
			for (final Level lvl : values()) {
				if (lvl.toJul().intValue() <= idx) {
					return lvl;
				}
			}
		}
		throw new IllegalArgumentException();
	}

	/** Replies the string representation of all the labels.
	 * The labels are those returned by {@link #toJsonString()}.
	 *
	 * @return all the labels.
	 */
	public static String getLabels() {
		final StringBuilder labels = new StringBuilder();
		for (final Level level : values()) {
			if (labels.length() > 0) {
				labels.append(", "); //$NON-NLS-1$
			}
			labels.append(level.toJsonString());
		}
		return labels.toString();
	}

}
