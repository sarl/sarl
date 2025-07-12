/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.bspl.lang;

import org.eclipse.xtext.generator.IContextualOutputConfigurationProvider;
import org.eclipse.xtext.generator.IGenerator;
import org.eclipse.xtext.generator.IOutputConfigurationProvider;
import org.eclipse.xtext.validation.ConfigurableIssueCodesProvider;

import io.sarl.bspl.lang.compiler.BSPLGenerator;
import io.sarl.bspl.lang.compiler.BSPLOutputConfigurationProvider;
import io.sarl.bspl.lang.validation.StandardBsplConfigurableIssueCodesProvider;

/**
 * Use this class to register components to be used at runtime / without the
 * Equinox extension registry.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@SuppressWarnings("static-method")
public class BSPLRuntimeModule extends AbstractBSPLRuntimeModule {

	@Override
	public Class<? extends ConfigurableIssueCodesProvider> bindConfigurableIssueCodesProvider() {
		return StandardBsplConfigurableIssueCodesProvider.class;
	}

	@Override
	public Class<? extends IGenerator> bindIGenerator() {
		return BSPLGenerator.class;
	}

	/** Provide the SARL-BSPL compiler configuration.
	 *
	 * @return the configuration type provider.
	 */
	public Class<? extends IOutputConfigurationProvider> bindIOutputConfigurationProvider() {
		return BSPLOutputConfigurationProvider.class;
	}
	
	/** Provide the SARL-BSPL compiler configuration.
	 *
	 * @return the configuration type provider.
	 */
	public Class<? extends IContextualOutputConfigurationProvider> bindIContextualOutputConfigurationProvider() {
		return BSPLOutputConfigurationProvider.class;
	}

}
