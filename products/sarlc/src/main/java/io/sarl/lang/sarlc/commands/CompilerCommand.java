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
import me.tongfei.progressbar.ProgressBar;
import me.tongfei.progressbar.ProgressBarBuilder;
import me.tongfei.progressbar.ProgressBarStyle;
import org.eclipse.core.runtime.IProgressMonitor;

import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.sarlc.Constants;
import io.sarl.lang.sarlc.configs.ProgressBarConfig;
import io.sarl.lang.sarlc.configs.SarlConfig;
import io.sarl.lang.sarlc.tools.PathDetector;
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

	/** Name of the option for enabling the progress bar.
	 */
	public static final String PROGRESS_OPTION_NAME = "progress"; //$NON-NLS-1$

	private final Provider<SarlBatchCompiler> compiler;

	private final Provider<SarlConfig> configuration;

	private final Provider<PathDetector> pathDetector;

	private final Provider<ProgressBarConfig> commandConfig;

	/** Constructor.
	 *
	 * @param compiler the SARL batch compiler.
	 * @param configuration the configuration of the tool.
	 * @param pathDetector the detector of path.
	 * @param commandConfig the configuration of the command.
	 */
	public CompilerCommand(Provider<SarlBatchCompiler> compiler, Provider<SarlConfig> configuration,
			Provider<PathDetector> pathDetector, Provider<ProgressBarConfig> commandConfig) {
		super(CommandMetadata
				.builder(CompilerCommand.class)
				.description(Messages.CompilerCommand_0));
		this.compiler = compiler;
		this.configuration = configuration;
		this.pathDetector = pathDetector;
		this.commandConfig = commandConfig;
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

		final ProgressBarConfig commandConfig = this.commandConfig.get();
		final boolean compilationResult;
		if (commandConfig.getEnable()) {
			compilationResult = comp.compile(new ConsoleProgressMonitor(commandConfig.getStyle()));
		} else {
			compilationResult = comp.compile();
		}
		if (!compilationResult) {
			return CommandOutcome.failed(Constants.ERROR_CODE, Strings.nullToEmpty(firstErrorMessage.get()));
		}
		return CommandOutcome.succeeded();
	}

	/** Progress monitor that outputs on the console.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	private static class ConsoleProgressMonitor implements IProgressMonitor {

		private final ProgressBarBuilder builder;

		private ProgressBar bar;

		private boolean isCanceled;

		/** Constructor.
		 *
		 * @param style the style of the progress bar.
		 */
		ConsoleProgressMonitor(io.sarl.lang.sarlc.configs.ProgressBarStyle style) {
			this.builder = new ProgressBarBuilder().setStyle(style.toBackgroundStyle());
		}

		private void ensureBar() {
			if (this.bar == null) {
				this.bar = this.builder.build();
			}
		}

		@Override
		public void beginTask(String name, int totalWorkUnits) {
			if (!Strings.isNullOrEmpty(name)) {
				this.builder.setTaskName(name);
			}
			this.builder.setStyle(ProgressBarStyle.COLORFUL_UNICODE_BLOCK).setInitialMax(totalWorkUnits);
		}

		@Override
		public void done() {
			ensureBar();
			this.bar.stepTo(this.bar.getMax());
			this.bar.close();
		}

		@Override
		public void internalWorked(double workUnits) {
			ensureBar();
			this.bar.setExtraMessage("").stepTo((long) workUnits); //$NON-NLS-1$
		}

		@Override
		public boolean isCanceled() {
			return this.isCanceled;
		}

		@Override
		public void setCanceled(boolean cancel) {
			this.isCanceled = cancel;
		}

		@Override
		public void setTaskName(String name) {
			if (!Strings.isNullOrEmpty(name)) {
				this.builder.setTaskName(name);
			}
		}

		@Override
		public void subTask(String name) {
			//
		}

		@Override
		public void worked(int workUnits) {
			ensureBar();
			this.bar.setExtraMessage("").stepTo(workUnits); //$NON-NLS-1$
		}

	}

}
