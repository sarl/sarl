/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.sarldoc.modules.commands;

import static io.bootique.BQCoreModule.extend;

import java.util.logging.Logger;

import javax.inject.Provider;
import javax.inject.Singleton;

import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Provides;
import org.arakhne.afc.vmutil.DynamicURLClassLoader;

import io.sarl.docs.sarldoc.commands.SarldocFakeCommand;
import io.sarl.docs.sarldoc.configs.SarldocConfig;
import io.sarl.docs.sarldoc.modules.internal.SarldocDynamicClassLoader;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.tools.PathDetector;
import io.sarl.lang.sarlc.tools.SARLClasspathProvider;

/** Module for the sarldoc fake command.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$compiler
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class SarldocFakeCommandModule implements BQModule {

	@Override
	public void configure(Binder binder) {
		extend(binder).addCommand(SarldocFakeCommand.class);
	}

	/** Provide the command for running sarldoc.
	 *
	 * @param sarldocClassLoader the class loader of sarldoc.
	 * @param dconfig the provider of sarldoc configuration.
	 * @param cconfig the provider of sarlc configuration.
	 * @param defaultBootClasspath the provider of the default boot class path.
	 * @param pathDetector the provider of path detector.
	 * @param loggerProvider the provider of JUL logger.
	 * @return the command.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public SarldocFakeCommand provideSarldocCommand(
			@SarldocDynamicClassLoader DynamicURLClassLoader sarldocClassLoader, Provider<SarldocConfig> dconfig,
			Provider<SarlcConfig> cconfig, Provider<SARLClasspathProvider> defaultBootClasspath,
			Provider<PathDetector> pathDetector, Provider<Logger> loggerProvider) {
		return new SarldocFakeCommand(sarldocClassLoader, loggerProvider, dconfig, cconfig, defaultBootClasspath, pathDetector);
	}

}