/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

package io.sarl.lang.sarlc.modules;

import java.util.Set;

import com.google.inject.Provider;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import io.bootique.log.BootLogger;

import io.sarl.lang.sarlc.commands.PrintConfigCommand;
import io.sarl.maven.bqextension.configs.Config;
import io.sarl.maven.bqextension.modules.AbstractConfigModule;
import io.sarl.maven.bqextension.modules.BQModule;

/** Module for the sarlc tool.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@BQModule("The runtime for printed out the current Yaml configuration.")
public class PrintConfigModule extends AbstractConfigModule {

	@Override
	protected void configure() {
		associateCommand(PrintConfigCommand.class);
	}

	/** Provide the command for running the compiler.
	 *
	 * @param bootLogger the boot logger.
	 * @param configs the configurations.
	 * @return the command.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public PrintConfigCommand providePrintConfigCommand(BootLogger bootLogger, Provider<Set<Config>> configs) {
		return new PrintConfigCommand(bootLogger, configs);
	}

}
