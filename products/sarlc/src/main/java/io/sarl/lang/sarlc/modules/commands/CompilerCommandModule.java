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

package io.sarl.lang.sarlc.modules.commands;

import static io.bootique.BQCoreModule.extend;

import com.google.inject.AbstractModule;
import com.google.inject.Provider;
import com.google.inject.Provides;
import com.google.inject.Singleton;

import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.sarlc.commands.CompilerCommand;
import io.sarl.lang.sarlc.commands.PathDetector;
import io.sarl.lang.sarlc.configs.SarlConfig;

/** Module for the compiler command.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class CompilerCommandModule extends AbstractModule {

	@Override
	protected void configure() {
		extend(binder()).addCommand(CompilerCommand.class);
	}

	/** Provide the command for running the compiler.
	 *
	 * @param compiler the compiler.
	 * @param configuration the SARLC configuration.
	 * @param pathDetector the detector of paths.
	 * @return the command.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public CompilerCommand provideSarlcCompilerCommand(Provider<SarlBatchCompiler> compiler,
			Provider<SarlConfig> configuration, Provider<PathDetector> pathDetector) {
		return new CompilerCommand(compiler, configuration, pathDetector);
	}

}
