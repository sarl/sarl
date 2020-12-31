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

package io.sarl.maven.docs;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import org.apache.maven.plugin.logging.Log;

/** Adapter of the Maven logger to the JUL handler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
final class MavenJulHandler extends Handler {

	private final Log mavenLogger;

	/** Constructor.
	 *
	 * @param mavenLogger the maven logger to adapt.
	 */
	MavenJulHandler(Log mavenLogger) {
		this.mavenLogger = mavenLogger;
	}

	@Override
	public void publish(LogRecord record) {
		final int level = record.getLevel().intValue();
		final Throwable exception = record.getThrown();
		if (level >= Level.SEVERE.intValue()) {
			if (exception != null) {
				this.mavenLogger.error(record.getMessage(), exception);
			} else {
				this.mavenLogger.error(record.getMessage());
			}
		} else if (level >= Level.WARNING.intValue()) {
			if (exception != null) {
				this.mavenLogger.warn(record.getMessage(), exception);
			} else {
				this.mavenLogger.warn(record.getMessage());
			}
		} else if (level >= Level.INFO.intValue()) {
			if (exception != null) {
				this.mavenLogger.info(record.getMessage(), exception);
			} else {
				this.mavenLogger.info(record.getMessage());
			}
		} else if (level >= Level.FINEST.intValue()) {
			if (exception != null) {
				this.mavenLogger.debug(record.getMessage(), exception);
			} else {
				this.mavenLogger.debug(record.getMessage());
			}
		}
	}

    @Override
	public void flush() {
		//
	}

	@Override
	public void close() throws SecurityException {
		//
	}

}
