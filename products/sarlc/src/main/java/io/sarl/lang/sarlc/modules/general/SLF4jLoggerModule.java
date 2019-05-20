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

package io.sarl.lang.sarlc.modules.general;

import com.google.inject.AbstractModule;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import org.arakhne.afc.bootique.applicationdata2.annotations.DefaultApplicationName;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Module for creating the SARL SLF4J loggers.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class SLF4jLoggerModule extends AbstractModule {

	@Override
	protected void configure() {
		//
	}

	/** Provide the SLF4J logger.
	 *
	 * @param applicationName the name of the application.
	 * @return the logger.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public Logger provideSlf4jLogger(@DefaultApplicationName String applicationName) {
		return LoggerFactory.getLogger(applicationName);
	}

}
