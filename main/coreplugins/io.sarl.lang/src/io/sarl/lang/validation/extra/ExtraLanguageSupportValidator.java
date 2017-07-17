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

package io.sarl.lang.validation.extra;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EValidator;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;

import io.sarl.lang.validation.SARLValidator;

/** The generator from SARL to the default target language and an extra target language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ExtraLanguageSupportValidator extends SARLValidator {

	private IExtraLanguageValidatorProvider extraValidatorProvider;

	private Logger logger;

	/** Change the logger.
	 *
	 * @param logger the logger.
	 */
	@Inject
	public void setLogger(Logger logger) {
		this.logger = logger;
	}

	/** Replies the logger.
	 *
	 * @return the logger.
	 */
	public Logger getLogger() {
		return this.logger;
	}

	/** Change the provider of the extra validator.
	 *
	 * @param provider the provider.
	 */
	@Inject
	public void setExtraGeneratorProvider(IExtraLanguageValidatorProvider provider) {
		this.extraValidatorProvider = provider;
	}

	/** Replies the provider of the extra validators.
	 *
	 * @return the provider.
	 */
	public IExtraLanguageValidatorProvider getExtraValidatorProvider() {
		return this.extraValidatorProvider;
	}

	/** Launch the validation for the extra languages.
	 *
	 * @param object the object to check
	 */
	@Check(CheckType.EXPENSIVE)
	public void extraLanguageValidation(EObject object) {
		final Iterable<EValidator> validators = getExtraValidatorProvider().getValidators(object.eResource());
		if (validators != null) {
			for (final EValidator validator : validators) {
				try {
					validator.validate(object, getChain(), getContext());
				} catch (Throwable exception) {
					getLogger().log(Level.SEVERE, exception.getLocalizedMessage(), exception);
				}
			}
		}
	}

}
