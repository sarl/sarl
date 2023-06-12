/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.doclet2.html.types;

import javax.lang.model.element.TypeElement;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;

/** Tool that replies the best generator for type element.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public interface TypeDocumentationGeneratorSelector {

	/** Replies the generator for the type element.
	 *
	 * @param element the element.
	 * @param environment the generation environment.
	 * @return the generator or {@code null}.
	 */
	TypeDocumentationGenerator getTypeGeneratorFor(TypeElement element, SarlDocletEnvironment environment);

}
