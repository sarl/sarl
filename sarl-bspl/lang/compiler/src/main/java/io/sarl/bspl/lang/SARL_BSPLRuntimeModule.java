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

import org.eclipse.xtext.generator.IGenerator;
import org.eclipse.xtext.validation.ConfigurableIssueCodesProvider;

import io.sarl.bspl.lang.compiler.SarlBsplGenerator;
import io.sarl.bspl.lang.validation.StandardSarlConfigurableIssueCodesProvider;

/**
 * Use this class to register components to be used at runtime / without the
 * Equinox extension registry.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARL_BSPLRuntimeModule extends AbstractSARL_BSPLRuntimeModule {

	/** Replies the type of the provider of configurable issue codes.
	 * 
	 * @return the type of provider.
	 */
	@Override
	public Class<? extends ConfigurableIssueCodesProvider> bindConfigurableIssueCodesProvider() {
		return StandardSarlConfigurableIssueCodesProvider.class;
	}

	/** Specify the generator of code.
	 *
	 * @return the type of the generator of code.
	 */
	@Override
	public Class<? extends IGenerator> bindIGenerator() {
		super.bindIGenerator();
		return SarlBsplGenerator.class;
	}

}
