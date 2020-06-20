/* 
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2020 the original authors or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.maven.bootiqueapp.config;

import java.util.logging.Level;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;

/** 
 * Logging level that is dedicated to be compatible with the configuration files, e.g. Json.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class LogLevel {

	private final Level level;

	/** Constructor.
	 *
	 * @param lvl the JUL level.
	 */
	LogLevel(Level lvl) {
		assert lvl != null;
		this.level = lvl;
	}

	/** Parse the given string to obtain a log level.
	 *
	 * @param name the string to parse.
	 * @return the log level.
	 */
	@JsonCreator
	@Pure
	public static LogLevel fromJson(String name) {
		if (Strings.isEmpty(name)) {
			throw new NullPointerException("Name is null");
		}
		Level lvl = null;
		try {
			lvl = Level.parse(name.toUpperCase());
		} catch (Throwable exception) {
			lvl = null;
		}
		if (lvl == null) {
			lvl = LogConfig.DEFAULT_LEVEL;
		}
		return new LogLevel(lvl);
	}

	/** Replies the preferred string representation of the level within a Json stream.
	 * 
	 * @return the string representation of the level.
	 */
	@JsonValue
	@Pure
	public String toJsonString() {
		return this.level.getName().toLowerCase();
	}

	/** Replies the JUL level associated to this logging level.
	 * 
	 * @return the JUL level
	 */
	@Pure
	public Level getJul() {
		return this.level;
	}

	/** Replies int representation of this level.
	 * 
	 * @return the int representation of this level.
	 */
	@Pure
	public int intValue() {
		return this.level.intValue();
	}

}
