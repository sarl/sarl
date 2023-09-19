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

package io.sarl.lang.extralanguage.validator;

import java.util.Collections;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;

/** Implementation of the provider of the extra language validators that replies no generator.
 *
 * @author $Author: sgalland$
 * @version compiler 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.6
 */
public class NullExtraLanguageValidatorProvider implements IExtraLanguageValidatorProvider {

	@Override
	public List<AbstractExtraLanguageValidator> getValidators(Resource resource) {
		return Collections.emptyList();
	}

}
