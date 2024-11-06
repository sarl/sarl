/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.ui.validation;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.google.common.base.Strings;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;
import org.eclipse.xtext.validation.ConfigurableIssueCodesProvider;
import org.eclipse.xtext.xbase.ui.validation.XbaseIssueSeveritiesProvider;

import io.sarl.lang.validation.ConfigurableIssueSeveritiesProvider.ConfigurableValueProvider;
import io.sarl.lang.validation.IConfigurableIssueSeveritiesProvider;

/** A configurable issue severity provider.
 *
 * <p>This issue severity provider provides a public API for dynamically
 * and programmatically changing the severity of the issues.
 * For the Eclipse implementation of the provider, the standard severity provider
 * gets the severity levels from the preferences.
 * But, this configurable issue severity provider should not change the preferences.
 * The contract is to have internal overriding of the preferences in this provider.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 * @since 0.5
 */
@Singleton
@SuppressWarnings("restriction")
public class UIConfigurableIssueSeveritiesProvider extends XbaseIssueSeveritiesProvider implements IConfigurableIssueSeveritiesProvider {

	private final Map<String, Severity> overridingSeverities = Collections.synchronizedMap(new HashMap<>());

	@Inject
	private ConfigurableIssueCodesProvider issueCodesProvider;

	@Override
	protected IPreferenceValuesProvider getValuesProvider() {
		final var original = super.getValuesProvider();
		if (this.overridingSeverities.isEmpty()) {
			return original;
		}
		return new ConfigurableValueProvider(original, this.overridingSeverities);
	}

	@Override
	public void setSeverity(String code, Severity severity) {
		if (!Strings.isNullOrEmpty(code)) {
			final var key = this.issueCodesProvider.getConfigurableIssueCodes().get(code);
			if (key != null) {
				if (severity == null) {
					this.overridingSeverities.remove(key.getId());
				} else {
					this.overridingSeverities.put(key.getId(), severity);
				}
			}
		}
	}

	@Override
	public void setAllSeverities(Severity severity) {
		this.overridingSeverities.clear();
		if (severity != null) {
			for (final var key : this.issueCodesProvider.getConfigurableIssueCodes().values()) {
				if (key != null) {
					this.overridingSeverities.put(key.getId(), severity);
				}
			}
		}
	}

}
