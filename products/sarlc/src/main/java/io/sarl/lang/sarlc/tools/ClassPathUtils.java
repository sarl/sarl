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

package io.sarl.lang.sarlc.tools;

import java.text.MessageFormat;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.xtext.util.JavaVersion;

import io.sarl.lang.compiler.batch.SarlBatchCompilerUtils;
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
	 * @param jversion the version of Java that is the target.
	 * @param logger the logger to use.
	 * @return the concrete class path.
	 */
	public static SystemPath buildClassPath(SARLClasspathProvider classpathProvider, SarlcConfig cfg,
			JavaVersion jversion, Logger logger) {
		final SystemPath fullClassPath = new SystemPath();
		// Boot class path
		if (!SarlBatchCompilerUtils.isModuleSupported(jversion)) {
			fullClassPath.addEntries(cfg.getBootClasspath());
		}
		logger.fine(MessageFormat.format(Messages.ClassPathUtils_0, fullClassPath.toString()));
		// User class path
		final SystemPath userClassPath = new SystemPath();
		userClassPath.addEntries(cfg.getClasspath());
		if (userClassPath.isEmpty()) {
			try {
				classpathProvider.getClassPath(userClassPath, logger);
			} catch (Throwable exception) {
				logger.log(Level.SEVERE, exception.getLocalizedMessage(), exception);
			}
		}
		logger.fine(MessageFormat.format(Messages.ClassPathUtils_1, userClassPath.toString()));
		fullClassPath.addEntries(userClassPath);
		return fullClassPath;
	}

	/** Build the standard module-path based on the given configuration and the default class path provider.
	 *
	 * @param classpathProvider the default class path provider.
	 * @param cfg the current configuration.
	 * @param jversion the version of Java that is the target.
	 * @param logger the logger to use.
	 * @return the concrete module-path.
	 */
	public static SystemPath buildModulePath(SARLClasspathProvider classpathProvider, SarlcConfig cfg,
			JavaVersion jversion, Logger logger) {
		final SystemPath fullModulePath = new SystemPath();
		if (SarlBatchCompilerUtils.isModuleSupported(jversion)) {
			// User module path
			final SystemPath userModulePath = new SystemPath();
			userModulePath.addEntries(cfg.getModulePath());
			if (userModulePath.isEmpty()) {
				try {
					classpathProvider.getModulePath(userModulePath, logger);
				} catch (Throwable exception) {
					logger.log(Level.SEVERE, exception.getLocalizedMessage(), exception);
				}
			}
			logger.fine(MessageFormat.format(Messages.ClassPathUtils_2, userModulePath.toString()));
			fullModulePath.addEntries(userModulePath);
		}
		return fullModulePath;
	}

}
