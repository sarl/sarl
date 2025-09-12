/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.ui.extralanguage.preferences;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.lib.Pair;

import io.sarl.lang.extralanguage.compiler.ExtraLanguageFeatureNameConverter;
import io.sarl.lang.extralanguage.compiler.FeatureNameConverterRuleReader;
import io.sarl.lang.extralanguage.compiler.FeaturePattern;
import io.sarl.lang.extralanguage.compiler.FeatureReplacement;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageGeneratorContext;
import io.sarl.lang.ui.compiler.ProjectAdapter;

/** Reader of the feature name conversion rules from the preferences.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class PreferenceBasedFeatureNameConverterRuleReader extends FeatureNameConverterRuleReader {

	@Inject
	private ExtraLanguagePreferenceAccess preferences;

	@Override
	public boolean initializeConversions(Map<Character, List<Pair<FeaturePattern, FeatureReplacement>>> result,
			IExtraLanguageGeneratorContext context) {
		if (context != null) {
			final var project = ProjectAdapter.getProject(context.getResource());
			final var store = this.preferences.getPreferenceStore(project);
			final var rawValue = ExtraLanguagePreferenceAccess.getString(store, context.getPreferenceID(),
							ExtraLanguagePreferenceAccess.FEATURE_NAME_CONVERSION_PROPERTY);
			return ExtraLanguagePreferenceAccess.parseConverterPreferenceValue(rawValue, (source, target) -> {
				final var shortName = FeaturePattern.simpleName(source);
				final var key = ExtraLanguageFeatureNameConverter.getKey(shortName);
				var internalStruct = result.get(Character.valueOf(key));
				if (internalStruct == null) {
					internalStruct = new ArrayList<>();
					result.put(Character.valueOf(key), internalStruct);
				}
				internalStruct.add(new Pair<>(new FeaturePattern(source), new FeatureReplacement(target)));
			});
		}
		return false;
	}

}
