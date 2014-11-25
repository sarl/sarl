/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang;

import org.eclipse.xtext.validation.ConfigurableIssueCodesProvider;
import org.eclipse.xtext.xbase.controlflow.IEarlyExitComputer;
import org.eclipse.xtext.xbase.typesystem.util.ExtendedEarlyExitComputer;
import org.eclipse.xtext.xbase.validation.EarlyExitValidator;
import org.eclipse.xtext.xbase.validation.FeatureNameValidator;

import io.sarl.lang.controlflow.SARLEarlyExitComputer;
import io.sarl.lang.controlflow.SARLExtendedEarlyExitComputer;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.signature.DefaultActionSignatureProvider;
import io.sarl.lang.validation.SARLConfigurableIssueCodesProvider;
import io.sarl.lang.validation.SARLEarlyExitValidator;
import io.sarl.lang.validation.SARLFeatureNameValidator;

/**
 * Use this class to register components to be used at runtime / without the
 * Equinox extension registry.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLRuntimeModule extends io.sarl.lang.AbstractSARLRuntimeModule {

	/** Replies the type of the provider of SARL action signatures.
	 *
	 * @return the type of provider for inferred action signatures.
	 */
	@SuppressWarnings("static-method")
	public Class<? extends ActionSignatureProvider> bindActionSignatureProvider() {
		return DefaultActionSignatureProvider.class;
	}

	/** Replies the type of the extended component that computes the early-exit flags.
	 * @return the type of the early-exit computer.
	 */
	@SuppressWarnings("static-method")
	public Class<? extends ExtendedEarlyExitComputer> bindExtendedEarlyExitComputer() {
		return SARLExtendedEarlyExitComputer.class;
	}

	/** Replies the type of the extended component that computes the early-exit flags.
	 * @return the type of the early-exit computer.
	 */
	@SuppressWarnings("static-method")
	public Class<? extends IEarlyExitComputer> bindEarlyExitComputer() {
		return SARLEarlyExitComputer.class;
	}

	/** Replies the type of the extended component that validates the early-exit flags.
	 * @return the type of the early-exit validator.
	 */
	@SuppressWarnings("static-method")
	public Class<? extends EarlyExitValidator> bindEarlyExitValidator() {
		return SARLEarlyExitValidator.class;
	}

	@Override
	public Class<? extends ConfigurableIssueCodesProvider> bindConfigurableIssueCodesProvider() {
		assert (
				super.bindConfigurableIssueCodesProvider().isAssignableFrom(
						SARLConfigurableIssueCodesProvider.class))
						: "The class SARLConfigurableIssueCodesProvider does not extend " //$NON-NLS-1$
							+ "the class provided by default by Xbase"; //$NON-NLS-1$
				//
				return SARLConfigurableIssueCodesProvider.class;
	}

	@Override
	public Class<? extends FeatureNameValidator> bindFeatureNameValidator() {
		assert (
				super.bindFeatureNameValidator().isAssignableFrom(
						SARLFeatureNameValidator.class))
						: "The class SARLFeatureNameValidator does not extend " //$NON-NLS-1$
							+ "the class provided by default by Xbase"; //$NON-NLS-1$
				//
				return SARLFeatureNameValidator.class;
	}

}
