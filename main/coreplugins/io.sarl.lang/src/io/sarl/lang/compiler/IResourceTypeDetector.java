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

import com.google.inject.ImplementedBy;
import org.eclipse.emf.ecore.resource.Resource;


/** Detect the type of a folder.
 *
 * <p>A resource may be inside a standard source folder or a folder that is dedicated
 * to test code. Detecting in which case a resource is is the purpose of this interface.
 *
 * <p>The way how a folder is detected as a test folder depends on the implementation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@ImplementedBy(DefaultResourceTypeDetector.class)
public interface IResourceTypeDetector {

	/** Replies the given resource is a test resource.
	 *
	 * @param resource the resource to test.
	 * @return {@link Boolean#TRUE} if the resource is a test resource. {@link Boolean#FALSE}
	 *     if the resource is not a test resource. {@code null} if the detector cannot determine
	 *     the type of the resource.
	 */
	Boolean isTestResource(Resource resource);

}
