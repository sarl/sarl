/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.validation;

import javax.inject.Inject;

import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.xbase.validation.LogicalContainerAwareFeatureNameValidator;

import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.util.Utils;

/** Validator of the feature names.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLFeatureNameValidator extends LogicalContainerAwareFeatureNameValidator {

	@Inject
	private SARLGrammarKeywordAccess grammarAccess;

	/** Construct a validator for the feature's names.
	 */
	public SARLFeatureNameValidator() {
		//
	}

	@Override
	public boolean isDisallowedName(QualifiedName name) {
		final String id = name.getLastSegment();
		if (Utils.isHiddenMember(id)
				|| super.isDisallowedName(name)) {
			return true;
		}
		return this.grammarAccess.isPureKeyword(id);
	}

	@Override
	public boolean isDiscouragedName(QualifiedName name) {
		return super.isDiscouragedName(name);
	}

}
