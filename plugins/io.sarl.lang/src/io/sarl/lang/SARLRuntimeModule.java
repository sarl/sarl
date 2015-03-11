/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import io.sarl.lang.bugfixes.SARLContextPDAProvider;
import io.sarl.lang.controlflow.SARLEarlyExitComputer;
import io.sarl.lang.controlflow.SARLExtendedEarlyExitComputer;
import io.sarl.lang.genmodel.SARLHiddenTokenSequencer;
import io.sarl.lang.scoping.batch.SARLImplicitlyImportedFeatures;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.signature.DefaultActionSignatureProvider;
import io.sarl.lang.validation.SARLConfigurableIssueCodesProvider;
import io.sarl.lang.validation.SARLEarlyExitValidator;
import io.sarl.lang.validation.SARLFeatureNameValidator;

import org.eclipse.xtext.serializer.acceptor.ISyntacticSequenceAcceptor;
import org.eclipse.xtext.serializer.analysis.IContextPDAProvider;
import org.eclipse.xtext.serializer.sequencer.IHiddenTokenSequencer;
import org.eclipse.xtext.validation.ConfigurableIssueCodesProvider;
import org.eclipse.xtext.xbase.controlflow.IEarlyExitComputer;
import org.eclipse.xtext.xbase.scoping.batch.ImplicitlyImportedFeatures;
import org.eclipse.xtext.xbase.typesystem.util.ExtendedEarlyExitComputer;
import org.eclipse.xtext.xbase.validation.EarlyExitValidator;
import org.eclipse.xtext.xbase.validation.FeatureNameValidator;

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

	/** Replies the type of the implicitly imported code extensions.
	 * @return the type of the implicitly imported code extensions.
	 */
	@SuppressWarnings("static-method")
	public Class<? extends ImplicitlyImportedFeatures> bindImplicitlyImportedFeatures() {
		return SARLImplicitlyImportedFeatures.class;
	}

	/** Replies the type of the provider of the issues that could be configured by the end user.
	 * @return the type of the provider of configurable issue codes.
	 */
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

	/** Replies the type of the validator of the feature names.
	 * @return the type of the validator of the feature names.
	 */
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

	/** Replies the provider of hidden token sequencer.
	 * @return the provider of hidden token sequencer.
	 */
	@SuppressWarnings("static-method")
	public Class<? extends IHiddenTokenSequencer> bindHiddenTokenSequencer() {
		return SARLHiddenTokenSequencer.class;
	}

	/** Replies the provider of hidden token sequencer.
	 * @return the provider of hidden token sequencer.
	 */
	@SuppressWarnings("static-method")
	public Class<? extends ISyntacticSequenceAcceptor> bindSyntacticSequenceAcceptor() {
		return SARLHiddenTokenSequencer.class;
	}

	/** Replies the provider of context PDA that is used by the serializer.
	 * This specific SARL implementation is provided for fixing the
	 * <a href="https://github.com/sarl/sarl/issues/277">issue #277</a>.
	 * @return the context PDA provider
	 */
	@SuppressWarnings("static-method")
	public Class<? extends IContextPDAProvider> bindContextPDAProvider() {
		return SARLContextPDAProvider.class;
	}

}
