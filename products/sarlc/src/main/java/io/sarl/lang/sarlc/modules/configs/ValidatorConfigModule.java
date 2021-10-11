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

package io.sarl.lang.sarlc.modules.configs;

import static io.bootique.BQCoreModule.extend;
import static io.sarl.lang.sarlc.configs.subconfigs.ValidatorConfig.ALL_ERRORS_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.ValidatorConfig.ALL_WARNINGS_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.ValidatorConfig.IGNORE_WARNINGS_NAME;

import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.meta.application.OptionMetadata;
import org.arakhne.afc.bootique.variables.VariableDecls;

/** Module for creating and configuring the sarlc validator's configuration.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class ValidatorConfigModule implements BQModule {

	private static final String WNONE_OPTION = "wnone"; //$NON-NLS-1$

	private static final String WALL_OPTION = "wall"; //$NON-NLS-1$

	private static final String WERROR_OPTION = "werror"; //$NON-NLS-1$

	@Override
	public void configure(Binder binder) {
		VariableDecls.extend(binder).declareVar(IGNORE_WARNINGS_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				WNONE_OPTION, Messages.ValidatorConfigModule_0)
				.build())
			.mapConfigPath(WNONE_OPTION, IGNORE_WARNINGS_NAME);

		VariableDecls.extend(binder).declareVar(ALL_WARNINGS_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				WALL_OPTION, Messages.ValidatorConfigModule_2)
				.build())
			.mapConfigPath(WALL_OPTION, ALL_WARNINGS_NAME);

		VariableDecls.extend(binder).declareVar(ALL_ERRORS_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				WERROR_OPTION, Messages.ValidatorConfigModule_1)
				.build())
			.mapConfigPath(WERROR_OPTION, ALL_ERRORS_NAME);
	}

}
