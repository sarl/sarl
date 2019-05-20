/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.lang.sarlc.tools;

import java.text.MessageFormat;

import org.slf4j.Logger;

import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.maven.bootiqueapp.utils.SystemPath;

/**
 * General utilities related to the class path.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public final class ClassPathUtils {

	private ClassPathUtils() {
		//
	}

	/** Build the standard class path based on the given configuration and the default class path provider.
	 *
	 * @param classpathProvider the default class path provider.
	 * @param cfg the current configuration.
	 * @param logger the logger to use.
	 * @return the concrete class path.
	 */
	public static SystemPath buildClassPath(SARLClasspathProvider classpathProvider, SarlcConfig cfg,
			Logger logger) {
		final SystemPath fullClassPath = new SystemPath();
		fullClassPath.addEntries(cfg.getBootClasspath());
		if (fullClassPath.isEmpty()) {
			try {
				classpathProvider.getBootClasspath(fullClassPath, logger);
			} catch (Throwable exception) {
				logger.error(exception.getLocalizedMessage(), exception);
			}
		}
		logger.debug(MessageFormat.format(Messages.ClassPathUtils_0, fullClassPath.toString()));
		final SystemPath userClassPath = new SystemPath();
		userClassPath.addEntries(cfg.getClasspath());
		if (userClassPath.isEmpty()) {
			try {
				classpathProvider.getClasspath(userClassPath, logger);
			} catch (Throwable exception) {
				logger.error(exception.getLocalizedMessage(), exception);
			}
		}
		logger.debug(MessageFormat.format(Messages.ClassPathUtils_1, userClassPath.toString()));
		fullClassPath.addEntries(userClassPath);
		return fullClassPath;
	}

}
