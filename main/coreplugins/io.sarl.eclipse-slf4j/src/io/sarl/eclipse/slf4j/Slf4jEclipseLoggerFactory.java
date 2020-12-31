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

package io.sarl.eclipse.slf4j;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleReference;
import org.slf4j.ILoggerFactory;
import org.slf4j.Logger;

/**
 * Factory of a SLF4J logger that is linked to the M2E plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 * @deprecated Remove when SLF4J is properly configured for Eclipse.
 */
@Deprecated
public class Slf4jEclipseLoggerFactory implements ILoggerFactory {

	private ILog eclipseLogger;

	@Override
	public Logger getLogger(String name) {
		return new Slf4jEclipseLogger(name, getEclipseLogger());
	}

	private Bundle getBundle() {
		final ClassLoader cl = getClass().getClassLoader();
		if (cl instanceof BundleReference) {
			return ((BundleReference) cl).getBundle();
		}
		return null;
	}

	/** Replies the Eclipse logger to be used.
	 *
	 * @return the logger.
	 */
	protected synchronized ILog getEclipseLogger() {
		if (this.eclipseLogger == null) {
			this.eclipseLogger = Platform.getLog(getBundle());
		}
		return this.eclipseLogger;
	}

}
