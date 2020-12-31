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

package io.sarl.lang.compiler.batch;

import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.log4j.Priority;
import org.apache.log4j.spi.LoggerFactory;

/** Apache logger that is converting info messages to debug messages.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
final class InternalXtextLogger extends org.apache.log4j.Logger {

	private final Logger logger;

	/** Constructor.
	 *
	 * @param name the logger's name.
	 * @param logger the original logger.
	 */
	InternalXtextLogger(String name, Logger logger) {
		super(name);
		this.logger = logger;
	}

	@Override
	protected void forcedLog(String fqcn, Priority level, Object message, Throwable exception) {
		switch (level.toInt()) {
		case Priority.OFF_INT:
			break;
		case Priority.FATAL_INT:
		case Priority.ERROR_INT:
			if (exception != null) {
				this.logger.log(Level.SEVERE, Objects.toString(message), exception);
			} else {
				this.logger.severe(Objects.toString(message));
			}
			break;
		case Priority.WARN_INT:
			if (exception != null) {
				this.logger.log(Level.WARNING, Objects.toString(message), exception);
			} else {
				this.logger.warning(Objects.toString(message));
			}
			break;
		case Priority.ALL_INT:
		case Priority.INFO_INT:
		case Priority.DEBUG_INT:
		default:
			if (exception != null) {
				this.logger.log(Level.FINEST, Objects.toString(message), exception);
			} else {
				this.logger.finest(Objects.toString(message));
			}
			break;
		}
	}

	/** Factory of internal logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	public static class InternalXtextLoggerFactory implements LoggerFactory {

		private final Logger logger;

		/** Constructor.
		 *
		 * @param logger the original logger.
		 */
		InternalXtextLoggerFactory(Logger logger) {
			this.logger = logger;
		}

		@Override
		public org.apache.log4j.Logger makeNewLoggerInstance(String name) {
			return new InternalXtextLogger(name, this.logger);
		}

	}

}
