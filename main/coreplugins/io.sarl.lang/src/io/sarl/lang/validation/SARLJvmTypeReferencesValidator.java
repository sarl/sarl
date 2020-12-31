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

package io.sarl.lang.validation;

import java.util.Map;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.validation.IssueSeverities;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.eclipse.xtext.xbase.validation.JvmTypeReferencesValidator;

/**
 * This class validates the references to JVM types.
 *
 * <p>It is overridden in order to enable warning suppression with {@link SuppressWarnings} annotation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLJvmTypeReferencesValidator extends JvmTypeReferencesValidator {

	@Inject
	private IProgrammaticWarningSuppressor warningSuppressor;

	@Override
	protected IssueSeverities getIssueSeverities(Map<Object, Object> context, EObject eObject) {
		final IssueSeverities severities = super.getIssueSeverities(context, eObject);
		return this.warningSuppressor.getIssueSeverities(context, eObject, severities);
	}

	@Override
	protected void warnRawType(JvmType type, JvmParameterizedTypeReference typeRef) {
		if (isIgnored(IssueCodes.RAW_TYPE)) {
			return;
		}
		super.warnRawType(type, typeRef);
	}

}
