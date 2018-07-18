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

import java.lang.reflect.Field;

import io.bootique.meta.application.ApplicationMetadata;

/** Setter of application metadata.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class ApplicationMetadataUpdater {

	/** Change the application name.
	 *
	 * @param metadata the metadata to change.
	 * @param name the new name.
	 */
	@SuppressWarnings("static-method")
	public void setName(ApplicationMetadata metadata, String name) {
		try {
			final Field field = ApplicationMetadata.class.getDeclaredField("name"); //$NON-NLS-1$
			field.setAccessible(true);
			field.set(metadata, name);
		} catch (Exception exception) {
			//
		}
	}

}
