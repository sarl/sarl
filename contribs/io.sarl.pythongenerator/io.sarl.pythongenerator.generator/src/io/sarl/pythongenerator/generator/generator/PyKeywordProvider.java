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

package io.sarl.pythongenerator.generator.generator;

import javax.inject.Singleton;

import org.eclipse.xtext.xbase.lib.Functions.Function0;

import io.sarl.lang.extralanguage.compiler.IExtraLanguageKeywordProvider;

/** Provider of the Python keywords.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@Singleton
public class PyKeywordProvider implements IExtraLanguageKeywordProvider {

	private static final String SELF_KEYWORD = "self"; //$NON-NLS-1$

	private static final String SUPER_KEYWORD = "super()"; //$NON-NLS-1$

	private static final String NONE_KEYWORD = "None"; //$NON-NLS-1$

	@Override
	public Function0<? extends String> getThisKeywordLambda() {
		return () -> SELF_KEYWORD;
	}

	@Override
	public Function0<? extends String> getSuperKeywordLambda() {
		return () -> SUPER_KEYWORD;
	}

	@Override
	public Function0<? extends String> getNullKeywordLambda() {
		return () -> NONE_KEYWORD;
	}

}
