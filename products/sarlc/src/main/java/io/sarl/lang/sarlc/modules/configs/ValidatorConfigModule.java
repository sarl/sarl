/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import com.google.inject.AbstractModule;
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
public class ValidatorConfigModule extends AbstractModule {

	@Override
	protected void configure() {
		VariableDecls.extend(binder()).declareVar(IGNORE_WARNINGS_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"nowarn", Messages.ValidatorConfigModule_0) //$NON-NLS-1$
				.configPath(IGNORE_WARNINGS_NAME)
				.build());

		VariableDecls.extend(binder()).declareVar(ALL_WARNINGS_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"wall", Messages.ValidatorConfigModule_2) //$NON-NLS-1$
				.configPath(ALL_WARNINGS_NAME)
				.build());

		VariableDecls.extend(binder()).declareVar(ALL_ERRORS_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"werror", Messages.ValidatorConfigModule_1) //$NON-NLS-1$
				.configPath(ALL_ERRORS_NAME)
				.build());
	}

}
