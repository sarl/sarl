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

import com.google.inject.ImplementedBy;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.validation.IssueSeverities;

/** Suppress warnings programmatically.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@ImplementedBy(DefaultProgrammaticWarningSuppressor.class)
public interface IProgrammaticWarningSuppressor {

	/** Replies the issue severities for the given object.
	 *
	 * @param context the context for retrieving the severities.
	 * @param currentObject the current object.
	 * @param predefinedSeverities the severities that were pre-computed, prior to the warning suppression.
	 * @return the severties.
	 */
	IssueSeverities getIssueSeverities(Map<Object, Object> context, EObject currentObject,
			IssueSeverities predefinedSeverities);

}
