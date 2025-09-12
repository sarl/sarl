/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.maven.compiler.utils;

import java.util.logging.Handler;
import java.util.logging.LogRecord;

import org.slf4j.Logger;

/** Adapter of the Maven logger to the JUL handler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class MavenJulHandler extends Handler {

	private final Logger mavenLogger;

	/** Constructor.
	 *
	 * @param mavenLogger the maven logger to adapt.
	 */
	public MavenJulHandler(Logger mavenLogger) {
		this.mavenLogger = mavenLogger;
	}

	@Override
	public void flush() {
		//
	}

	@Override
	public void close() throws SecurityException {
		//
	}

	@Override
	public void publish(LogRecord record) {
		final var originalLevel = record.getLevel();
		final var exception = record.getThrown();
		if (originalLevel == java.util.logging.Level.SEVERE) {
			if (exception != null) {
				this.mavenLogger.error(record.getMessage(), exception);
			} else {
				this.mavenLogger.error(record.getMessage());
			}
		} else if (originalLevel == java.util.logging.Level.WARNING) {
			if (exception != null) {
				this.mavenLogger.warn(record.getMessage(), exception);
			} else {
				this.mavenLogger.warn(record.getMessage());
			}
		} else if (originalLevel == java.util.logging.Level.INFO) {
			if (exception != null) {
				this.mavenLogger.info(record.getMessage(), exception);
			} else {
				this.mavenLogger.info(record.getMessage());
			}
		} else {
			if (exception != null) {
				this.mavenLogger.debug(record.getMessage(), exception);
			} else {
				this.mavenLogger.debug(record.getMessage());
			}
		}
	}

}
