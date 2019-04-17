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

package io.sarl.sarldoc.modules.internal;

import static io.bootique.BQCoreModule.extend;

import com.google.inject.AbstractModule;
import com.google.inject.Provider;
import com.google.inject.Singleton;
import org.arakhne.afc.bootique.applicationdata2.annotations.DefaultApplicationName;
import org.arakhne.afc.bootique.synopsishelp.annotations.ApplicationArgumentSynopsis;
import org.arakhne.afc.bootique.synopsishelp.annotations.ApplicationDetailedDescription;

import io.sarl.sarldoc.Constants;

/** Module for configuring the sarldoc application information.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class SarldocApplicationModule extends AbstractModule {

	@Override
	protected void configure() {
		// Name of the application.
		bind(String.class).annotatedWith(DefaultApplicationName.class).toInstance(Constants.PROGRAM_NAME);
		// Short description of the application.
		extend(binder()).setApplicationDescription(Messages.SarldocApplicationModule_0);
		// Long description of the application.
		bind(String.class).annotatedWith(ApplicationDetailedDescription.class).toProvider(LongDescriptionProvider.class).in(Singleton.class);
		// Synopsis of the application's arguments.
		bind(String.class).annotatedWith(ApplicationArgumentSynopsis.class).toInstance(Messages.SarldocApplicationModule_1);
	}

	/** Provider of the long description of the application.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class LongDescriptionProvider implements Provider<String> {

		@Override
		public String get() {
			return Messages.SarldocApplicationModule_2;
		}

	}

}
