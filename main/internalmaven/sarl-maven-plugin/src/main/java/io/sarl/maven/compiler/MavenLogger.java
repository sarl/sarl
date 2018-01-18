/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

package io.sarl.maven.compiler;

import java.util.Objects;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.Priority;
import org.apache.log4j.spi.LoggerFactory;
import org.apache.maven.plugin.logging.Log;

/** Apache logger that is wrapping a Maven logger.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
final class MavenLogger extends Logger {

	private final Log mavenLogger;

	/** Constructor.
	 *
	 * @param name the name of the logger.
	 * @param mavenLogger the logger.
	 */
	MavenLogger(String name, Log mavenLogger) {
		super(name);
		this.mavenLogger = mavenLogger;
		final Level level;
		if (this.mavenLogger.isDebugEnabled()) {
			level = Level.DEBUG;
		} else if (this.mavenLogger.isInfoEnabled()) {
			level = Level.INFO;
		} else if (this.mavenLogger.isWarnEnabled()) {
			level = Level.WARN;
		} else if (this.mavenLogger.isErrorEnabled()) {
			level = Level.ERROR;
		} else {
			level = Level.OFF;
		}
		setLevel(level);
	}

	@Override
	protected void forcedLog(String fqcn, Priority level, Object message, Throwable exception) {
		switch (level.toInt()) {
		case Priority.DEBUG_INT:
			this.mavenLogger.debug(Objects.toString(message), exception);
			break;
		case Priority.FATAL_INT:
		case Priority.ERROR_INT:
			this.mavenLogger.error(Objects.toString(message), exception);
			break;
		case Priority.WARN_INT:
			this.mavenLogger.warn(Objects.toString(message), exception);
			break;
		case Priority.INFO_INT:
			this.mavenLogger.info(Objects.toString(message), exception);
			break;
		default:
		}
	}

	/** Factory of logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static final class MavenLoggerFactory implements LoggerFactory {

		private final Log wrappedLogger;

		/** Constructor.
		 *
		 * @param mavenLogger the maven logger.
		 */
		MavenLoggerFactory(Log mavenLogger) {
			this.wrappedLogger = mavenLogger;
		}

		@Override
		public Logger makeNewLoggerInstance(String name) {
			return new MavenLogger(name, this.wrappedLogger);
		}

	}

}
