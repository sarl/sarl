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

package io.sarl.lang.extralanguage.validator;

import java.util.List;

import com.google.inject.ImplementedBy;
import org.eclipse.emf.ecore.resource.Resource;

/** Provider of the extra language validators.
 *
 * <p>The validators will be used to validate the SARL program for extra-language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@ImplementedBy(NullExtraLanguageValidatorProvider.class)
public interface IExtraLanguageValidatorProvider {

	/** Replies the validators that should be used for validating the SARL program for the extra language.
	 *
	 * @param resource the resource for which the validators should be retreived.
	 * @return the list of the generators.
	 */
	List<AbstractExtraLanguageValidator> getValidators(Resource resource);

}
