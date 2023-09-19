/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.pythongenerator;

import java.util.Arrays;
import java.util.Collection;

import com.google.inject.Inject;
import org.eclipse.xtext.generator.IOutputConfigurationProvider;

import io.sarl.lang.extralanguage.IExtraLanguageContribution;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageGeneratorProvider;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageKeywordProvider;
import io.sarl.lang.extralanguage.validator.IExtraLanguageValidatorProvider;
import io.sarl.lang.pythongenerator.configuration.PyOutputConfigurationProvider;
import io.sarl.lang.pythongenerator.generator.PyGeneratorProvider;
import io.sarl.lang.pythongenerator.generator.PyKeywordProvider;
import io.sarl.lang.pythongenerator.validator.PyValidatorProvider;

/** Provider of Python contributions.
 *
 * @author $Author: sgalland$
 * @version pythongenerator 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid pythongenerator
 * @since 0.8
 */
public class PyContribution implements IExtraLanguageContribution {

	private static final String PYTHON_IDENTIFIER = "python"; //$NON-NLS-1$

	private static final String PYTHON3_IDENTIFIER = "python3"; //$NON-NLS-1$

	@Inject
	private PyGeneratorProvider generator;

	@Inject
	private PyValidatorProvider validator;

	@Inject
	private PyOutputConfigurationProvider configuration;

	@Inject
	private PyKeywordProvider keywords;

	@Override
	public Collection<String> getIdentifiers() {
		return Arrays.asList(PYTHON3_IDENTIFIER, PYTHON_IDENTIFIER);
	}

	@Override
	public IExtraLanguageGeneratorProvider getGeneratorProvider() {
		return this.generator;
	}

	@Override
	public IExtraLanguageValidatorProvider getValidatorProvider()  {
		return this.validator;
	}

	@Override
	public IOutputConfigurationProvider getOutputConfigurationProvider() {
		return this.configuration;
	}

	@Override
	public IExtraLanguageKeywordProvider getKeywordProvider() {
		return this.keywords;
	}

}
