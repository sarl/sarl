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

package io.sarl.lang.extralanguage.compiler;

import java.util.List;
import java.util.Map;

import org.eclipse.xtext.xbase.lib.Pair;

/** Reader of the conversion rules.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class FeatureNameConverterRuleReader {

	/** initialize the conversions mapping.
	 *
	 * @param result the result.
	 * @param context the generation context.
	 * @return {@code true} if rules are read.
	 */
	@SuppressWarnings("static-method")
	public boolean initializeConversions(Map<Character, List<Pair<FeaturePattern, FeatureReplacement>>> result,
			IExtraLanguageGeneratorContext context) {
		return false;
	}

}
