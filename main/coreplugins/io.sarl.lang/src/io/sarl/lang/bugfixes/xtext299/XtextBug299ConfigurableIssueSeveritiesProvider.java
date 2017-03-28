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

package io.sarl.lang.bugfixes.xtext299;

import javax.inject.Inject;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.preferences.IPreferenceValues;
import org.eclipse.xtext.validation.ConfigurableIssueCodesProvider;
import org.eclipse.xtext.validation.IssueSeverities;
import org.eclipse.xtext.validation.SeverityConverter;

import io.sarl.lang.validation.ConfigurableIssueSeveritiesProvider;

/** This class is fixing the
 * <a href="https://github.com/eclipse/xtext-core/pull/299">Xtext issue #299</a>.
 *
 * <p>FIXME: Remove this class when the Xtend is fixed.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class XtextBug299ConfigurableIssueSeveritiesProvider extends ConfigurableIssueSeveritiesProvider {

	@Inject
	private SeverityConverter severityConverter;

	@Inject
	private ConfigurableIssueCodesProvider issueCodesProvider;

	@Override
	public IssueSeverities getIssueSeverities(Resource context) {
		final IPreferenceValues preferenceValues = getValuesProvider().getPreferenceValues(context);
		return new IssueSeverities(preferenceValues, this.issueCodesProvider.getConfigurableIssueCodes(),
				this.severityConverter);
	}

}
