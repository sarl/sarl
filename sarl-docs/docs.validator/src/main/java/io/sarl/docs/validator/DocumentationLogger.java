/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.docs.validator;

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

/** Extended Functions for obtaining information on SARL issues.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class DocumentationLogger {

	/** Name of a global property that may refer to a logger for the documentation tools. The logger must be of type {@link Logger}.
	 *
	 * @since 0.14
	 */
	public static final String GLOBAL_PROPERTY_NAME = DocumentationLogger.class.getName() + ".EXTERNAL_LOGGER"; //$NON-NLS-1$
	
	private static Logger LOGGER;
	
	/** Replies the logger associated to the documentation tools.
	 *
	 * @return the logger.
	 */
	public static Logger getLogger() {
		synchronized (DocumentationLogger.class) {
			if (LOGGER == null) {
				final var externalLogger = System.getProperties().get(GLOBAL_PROPERTY_NAME);
				if (externalLogger instanceof Logger elogger) {
					LOGGER = elogger;
				} else {
					final var junitLogger = Logger.getLogger("org.junit"); //$NON-NLS-1$
					if (junitLogger == null) {
						LOGGER = Logger.getLogger(DocumentationLogger.class.getName());
					} else {
						LOGGER = junitLogger;
					}
				}
			}
			return LOGGER;
		}
	}
	
	/** Initialize the documentation logger associated to the documentation tools.
	 *
	 * @param handler the logging handler that must be associated to the logger for generating the logging messages.
	 * @return the logger.
	 * @since 0.14
	 */
	public static Logger initializeLogger(Handler handler) {
		if (handler == null) {
			return getLogger();
		}
		synchronized (DocumentationLogger.class) {
			LOGGER = Logger.getLogger(DocumentationLogger.class.getName());
			final var handlers = LOGGER.getHandlers();
			for (final var handler0 : handlers) {
				LOGGER.removeHandler(handler0);
			}
			LOGGER.setUseParentHandlers(false);
			LOGGER.setLevel(Level.ALL);
			LOGGER.addHandler(handler);
			return LOGGER;
		}
	}
	
	/** Initialize the documentation logger associated to the documentation tools.
	 *
	 * @param logger the plugin logger.
	 * @since 0.14
	 */
	public static void initializeLogger(Logger logger) {
		if (logger == null) {
			throw new IllegalArgumentException();
		}
		synchronized (DocumentationLogger.class) {
			LOGGER = logger;
		}
	}

	/** Change the logger associated to the documentation tools.
	 *
	 * @param logger the logger.
	 * @deprecated no replacement.
	 */
	@Deprecated(forRemoval = true, since = "0.14")
	public static void setLogger(Logger logger) {
		synchronized (DocumentationLogger.class) {
			LOGGER = logger;
		}
	}

}