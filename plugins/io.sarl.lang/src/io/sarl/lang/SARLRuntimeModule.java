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

import io.sarl.lang.actionprototype.ActionPrototypeProvider;
import io.sarl.lang.actionprototype.DefaultActionPrototypeProvider;
import io.sarl.lang.bugfixes.SARLContextPDAProvider;
import io.sarl.lang.controlflow.SARLEarlyExitComputer;
import io.sarl.lang.controlflow.SARLExtendedEarlyExitComputer;
import io.sarl.lang.genmodel.SARLHiddenTokenSequencer;
import io.sarl.lang.jvmmodel.JvmModelInferrerProber;
import io.sarl.lang.jvmmodel.SARLJvmModelInferrer;
import io.sarl.lang.sarl.SarlFactory;
import io.sarl.lang.scoping.batch.SARLImplicitlyImportedFeatures;
import io.sarl.lang.validation.SARLConfigurableIssueCodesProvider;
import io.sarl.lang.validation.SARLEarlyExitValidator;
import io.sarl.lang.validation.SARLFeatureNameValidator;

import org.eclipse.xtend.core.macro.declaration.IResourceChangeRegistry;
import org.eclipse.xtend.core.macro.declaration.NopResourceChangeRegistry;
import org.eclipse.xtend.core.xtend.XtendFactory;
import org.eclipse.xtext.serializer.acceptor.ISyntacticSequenceAcceptor;
import org.eclipse.xtext.serializer.analysis.IContextPDAProvider;
import org.eclipse.xtext.serializer.sequencer.IHiddenTokenSequencer;
import org.eclipse.xtext.validation.ConfigurableIssueCodesProvider;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.eclipse.xtext.xbase.controlflow.IEarlyExitComputer;
import org.eclipse.xtext.xbase.file.AbstractFileSystemSupport;
import org.eclipse.xtext.xbase.file.JavaIOFileSystemSupport;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelInferrer;
import org.eclipse.xtext.xbase.scoping.batch.ImplicitlyImportedFeatures;
import org.eclipse.xtext.xbase.typesystem.util.ExtendedEarlyExitComputer;
import org.eclipse.xtext.xbase.validation.EarlyExitValidator;
import org.eclipse.xtext.xbase.validation.FeatureNameValidator;

import com.google.common.base.Optional;
import com.google.inject.Provides;

/**
 * Use this class to register components to be used at runtime / without the
 * Equinox extension registry.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("static-method")
public class SARLRuntimeModule extends io.sarl.lang.AbstractSARLRuntimeModule {

	/** Replies the Xbase factory.
	 * @return the Xbase factory.
	 */
	public XbaseFactory bindXbaseFactory() {
		return XbaseFactory.eINSTANCE;
	}

	/** Replies the Xtend factory.
	 * @return the Xtend factory.
	 */
	public XtendFactory bindXtendFactory() {
		return XtendFactory.eINSTANCE;
	}

	/** Replies the SARL factory.
	 * @return the SARL factory.
	 */
	public SarlFactory bindSarlFactory() {
		return SarlFactory.eINSTANCE;
	}

	/** Replies the type of the provider of SARL action signatures.
	 * @return the type of provider for inferred action signatures.
	 */
	public Class<? extends ActionPrototypeProvider> bindActionSignatureProvider() {
		return DefaultActionPrototypeProvider.class;
	}

	/** Replies the type of the extended component that computes the early-exit flags.
	 * @return the type of the early-exit computer.
	 */
	public Class<? extends ExtendedEarlyExitComputer> bindExtendedEarlyExitComputer() {
		return SARLExtendedEarlyExitComputer.class;
	}

	/** Replies the type of the extended component that computes the early-exit flags.
	 * @return the type of the early-exit computer.
	 */
	public Class<? extends IEarlyExitComputer> bindEarlyExitComputer() {
		return SARLEarlyExitComputer.class;
	}

	/** Replies the type of the extended component that validates the early-exit flags.
	 * @return the type of the early-exit validator.
	 */
	public Class<? extends EarlyExitValidator> bindEarlyExitValidator() {
		return SARLEarlyExitValidator.class;
	}

	/** Replies the type of the implicitly imported code extensions.
	 * @return the type of the implicitly imported code extensions.
	 */
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
	public Class<? extends IHiddenTokenSequencer> bindHiddenTokenSequencer() {
		return SARLHiddenTokenSequencer.class;
	}

	/** Replies the provider of hidden token sequencer.
	 * @return the provider of hidden token sequencer.
	 */
	public Class<? extends ISyntacticSequenceAcceptor> bindSyntacticSequenceAcceptor() {
		return SARLHiddenTokenSequencer.class;
	}

	/** Replies the provider of context PDA that is used by the serializer.
	 * This specific SARL implementation is provided for fixing the
	 * <a href="https://github.com/sarl/sarl/issues/277">issue #277</a>.
	 * @return the context PDA provider
	 */
	public Class<? extends IContextPDAProvider> bindContextPDAProvider() {
		return SARLContextPDAProvider.class;
	}

	/** Provides an optional {@link JvmModelInferrerProber}.
	 *
	 * By default, no prober is provided (it is absent).
	 * The prober is provided during unit tests.
	 *
	 * @return an optional {@link JvmModelInferrerProber}.
	 */
	@Provides
	public Optional<JvmModelInferrerProber> getOptionalJvmModelInferrerProber() {
		return Optional.absent();
	}

	/** Bind to the SARL JVM model inferred.
	 *
	 * This should be contributed by org.eclipse.xtext.generator.xbase.XbaseGeneratorFragment
	 * in the supertype, but is not since SARL extends the Xtend model inferrer.
	 *
	 * @return the type of the SARL model inferrer.
	 */
	public Class<? extends IJvmModelInferrer> bindIJvmModelInferrer() {
		return SARLJvmModelInferrer.class;
	}

	/** Replies the type of the component that is managing the file system.
	 * @return the type of the file system support.
	 */
	public Class<? extends AbstractFileSystemSupport> bindAbstractFileSystemSupport() {
		return JavaIOFileSystemSupport.class;
	}
	
	/** Replies the type of the component that dispatchs the changes in resources
	 * @return the type of the resource change registry.
	 */
	public Class<? extends IResourceChangeRegistry> bindResourceChangeRegistry() {
		return NopResourceChangeRegistry.class;
	}
	
}
