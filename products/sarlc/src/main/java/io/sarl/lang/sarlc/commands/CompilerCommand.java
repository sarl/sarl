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

package io.sarl.lang.sarlc.commands;

import com.google.common.base.Strings;
import com.google.inject.Provider;
import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.meta.application.CommandMetadata;

import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.sarlc.Constants;
import io.sarl.lang.sarlc.configs.SarlConfig;
import io.sarl.lang.util.OutParameter;

/**
 * Command for compiling with SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class CompilerCommand extends CommandWithMetadata {

	private final Provider<SarlBatchCompiler> compiler;

	private final Provider<SarlConfig> configuration;

	private final Provider<PathDetector> pathDetector;

	/** Constructor.
	 *
	 * @param compiler the SARL batch compiler.
	 * @param configuration the configuration of the tool.
	 * @param pathDetector the detector of path.
	 */
	public CompilerCommand(Provider<SarlBatchCompiler> compiler, Provider<SarlConfig> configuration,
			Provider<PathDetector> pathDetector) {
		super(CommandMetadata
				.builder(CompilerCommand.class)
				.description(Messages.CompilerCommand_0));
		this.compiler = compiler;
		this.configuration = configuration;
		this.pathDetector = pathDetector;
	}

	@Override
	@SuppressWarnings("checkstyle:npathcomplexity")
	public CommandOutcome run(Cli cli) {
		if (cli.standaloneArguments().isEmpty()) {
			return CommandOutcome.failed(Constants.ERROR_CODE, Messages.CompilerCommand_1);
		}

		final SarlConfig config = this.configuration.get();
		final PathDetector paths = this.pathDetector.get();
		paths.setSarlOutputPath(config.getOutputPath());
		paths.setClassOutputPath(config.getClassOutputPath());
		paths.setWorkingPath(config.getWorkingPath());
		paths.resolve(cli.standaloneArguments());

		final SarlBatchCompiler comp = this.compiler.get();

		comp.setOutputPath(paths.getSarlOutputPath());
		comp.setClassOutputPath(paths.getClassOutputPath());
		comp.setTempDirectory(paths.getWorkingPath());

		for (final String cliArg : cli.standaloneArguments()) {
			comp.addSourcePath(cliArg);
		}

		final OutParameter<String> firstErrorMessage = new OutParameter<>();
		comp.addIssueMessageListener((issue, uri, message) -> {
			if (firstErrorMessage.get() == null) {
				firstErrorMessage.set(message);
			}
		});

		if (!comp.compile()) {
			return CommandOutcome.failed(Constants.ERROR_CODE, Strings.nullToEmpty(firstErrorMessage.get()));
		}
		return CommandOutcome.succeeded();
	}

}
