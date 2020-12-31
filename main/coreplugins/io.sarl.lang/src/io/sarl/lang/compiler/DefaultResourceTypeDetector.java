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

package io.sarl.lang.compiler;

import com.google.inject.Singleton;
import org.eclipse.emf.ecore.resource.Resource;

import io.sarl.lang.SARLConfig;


/** Detect the type of a folder.
 *
 * <p>A resource may be inside a standard source folder or a folder that is dedicated
 * to test code. Detecting in which case a resource is the purpose of this interface.
 *
 * <p>This detector does not make any assumption on the proejct structure. Consequently,
 * it cannot detect the type of the resource.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 * @see SARLConfig#FOLDER_TEST_SOURCE_SARL
 * @see SARLConfig#FOLDER_TEST_SOURCE_GENERATED
 */
@Singleton
public class DefaultResourceTypeDetector extends AbstractResourceTypeDetector {

	@Override
	public Boolean isTestResource(Resource resource) {
		return null;
	}

}
