/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang;

import org.eclipse.xtext.validation.ConfigurableIssueCodesProvider;

import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.signature.DefaultActionSignatureProvider;
import io.sarl.lang.validation.SARLConfigurableIssueCodesProvider;
import io.sarl.lang.validation.SARLFeatureNameValidator;

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 * 
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLRuntimeModule extends io.sarl.lang.AbstractSARLRuntimeModule {
	
	/**
	 * @return the type of provider for inferred action signatures.
	 */
	@SuppressWarnings("static-method")
	public Class<? extends ActionSignatureProvider> bindActionSignatureProvider() {
		return DefaultActionSignatureProvider.class;
	}
	
	/** {@inheritDoc}
	 */
	@Override
	public Class<? extends ConfigurableIssueCodesProvider> bindConfigurableIssueCodesProvider() {
		return SARLConfigurableIssueCodesProvider.class;
	}
	
	/** {@inheritDoc}
	 */
	@Override
	public Class<? extends org.eclipse.xtext.xbase.validation.FeatureNameValidator> bindFeatureNameValidator() {
		return SARLFeatureNameValidator.class;
	}
	
}
