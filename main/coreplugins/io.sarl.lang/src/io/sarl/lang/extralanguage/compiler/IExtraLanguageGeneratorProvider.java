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

package io.sarl.lang.extralanguage.compiler;

import com.google.inject.ImplementedBy;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.generator.IGeneratorContext;

/** Provider of the extra language generators.
 *
 * <p>The generators will be used to generate extra-language files form the SARL resources.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@ImplementedBy(NullExtraLanguageGeneratorProvider.class)
public interface IExtraLanguageGeneratorProvider {

	/** Replies the generators that should be used for generating the extra language output files.
	 *
	 * @param context the generator context.
	 * @param resource the resource.
	 * @return the list of the generators.
	 */
	Iterable<IRootGenerator> getGenerators(IGeneratorContext context, Resource resource);

}
