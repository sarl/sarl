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

package io.sarl.lang.bugfixes.pending.bug621;

import com.google.inject.Inject;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.xbase.typesystem.override.OverrideHelper;
import org.eclipse.xtext.xbase.typesystem.override.OverrideTester;
import org.eclipse.xtext.xbase.typesystem.override.ResolvedFeatures;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/**
 * Fixing the SARL issue 621: Error on multiple function inheritance.
 *
 * <p>Issue is due to Xtend issue 191 (https://github.com/eclipse/xtext-xtend/pull/191),
 * and the associated PR 192 (https://github.com/eclipse/xtext-xtend/pull/192)
 *
 * <p>Search for "START CHANGE" comment for finding the specific fixes of this class.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/621"
 * @see "https://github.com/eclipse/xtext-xtend/pull/191"
 * @see "https://github.com/eclipse/xtext-xtend/pull/192"
 */
@SuppressWarnings("all")
public class Bug621OverrideHelper extends OverrideHelper {

	@Inject
	private OverrideTester overrideTester;

	@Override
	public ResolvedFeatures getResolvedFeatures(LightweightTypeReference contextType) {
		return new Bug621ResolvedFeatures(contextType, this.overrideTester);
	}

	@Override
	public ResolvedFeatures getResolvedFeatures(LightweightTypeReference contextType, JavaVersion targetVersion) {
		return new Bug621ResolvedFeatures(contextType, this.overrideTester, targetVersion);
	}
	
}
