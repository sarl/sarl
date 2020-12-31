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

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.validation.IssueSeverities;
import org.eclipse.xtext.validation.IssueSeveritiesProvider;

/** A configurable issue severity provider.
 *
 * <p>This issue severity provider provides a public API for dynamically
 * and programmatically changing the severity of the issues.
 * For the Eclipse implementation of the provider, the standard severity provider
 * gets the severity levels from the preferences.
 * But, this configurable issue severity provider should not change the preferences.
 * The contract is to have internal overriding of the preferences in this provider.
 *
 * <p>Usually, the implementation of this provider is a specific {@link IssueSeveritiesProvider}
 * implementation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 * @see IssueSeveritiesProvider
 */
public interface IConfigurableIssueSeveritiesProvider {

	/** Replies the issue severities for the given resource.
	 *
	 * @param context the context for determining the severities.
	 * @return the issue severities.
	 */
	IssueSeverities getIssueSeverities(Resource context);

	/** Set the severity of the given issue code.
	 *
	 * @param code the issue code.
	 * @param severity the severity level. If {@code null}, the overriding is deleted.
	 */
	void setSeverity(String code, Severity severity);

	/** Set the severities of all the issue codes to the given value.
	 *
	 * @param severity the severity level. If {@code null}, all the overridings are deleted.
	 */
	void setAllSeverities(Severity severity);

}
