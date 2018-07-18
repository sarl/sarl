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

package io.sarl.maven.bqextension.modules;

import com.google.inject.AbstractModule;
import com.google.inject.Provides;
import com.google.inject.Singleton;

/** Module for the compiler application metadata.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@BQModule("The tools for updating the application metadata.")
public class ApplicationMetadataModule extends AbstractModule {

	@Override
	protected void configure() {
		//
	}

	/** Replies an updater of application metadata.
	 *
	 * @return the updater.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public ApplicationMetadataUpdater getApplicationMetadataSetter() {
		return new ApplicationMetadataUpdater();
	}

}
