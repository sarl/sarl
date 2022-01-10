/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2022 the original authors or authors.
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

package io.sarl.docs.doclet2.framework;

import java.util.Set;

import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;

import com.google.common.collect.Iterables;

/** Tool that filter elements for the selected elements.
 * This function supports the tag {@code @excludefromapidoc} to determine if the
 * document must be generated for a type.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class StandardElementFilter implements ElementFilter {

	@Override
	public Iterable<? extends TypeElement> extractTypeElements(SarlDocletEnvironment environment) {
		final Set<? extends Element> elements = environment.getIncludedElements();
		Iterable<? extends TypeElement> filtered = Iterables.filter(elements, TypeElement.class);
		//filtered = Iterables.filter(filtered, it -> it.getSimpleName().toString().equals("SARLJvmModelInferrer"));
		return filtered;
	}

}
