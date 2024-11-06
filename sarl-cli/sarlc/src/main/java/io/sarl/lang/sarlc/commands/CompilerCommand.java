/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import java.io.IOException;
import java.text.MessageFormat;
import java.util.concurrent.atomic.AtomicInteger;

import javax.inject.Provider;

import com.google.common.base.Strings;
import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.di.BQInject;
import io.bootique.meta.application.CommandMetadata;
import me.tongfei.progressbar.ProgressBar;
import me.tongfei.progressbar.ProgressBarBuilder;
import me.tongfei.progressbar.ProgressBarStyle;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.xtext.diagnostics.Severity;

import io.sarl.apputils.bootiqueapp.BootiqueMain;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.core.util.OutParameter;
import io.sarl.lang.sarlc.configs.ProgressBarConfig;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.tools.PathDetector;

/**
 * Command for compiling with SARL.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version sarlc 0.14.0 20241106-161410
 * @mavengroupid io.sarl.cli
 * @mavenartifactid sarlc
 * @since 0.8
 */
public class CompilerCommand extends CommandWithMetadata {

	/** Name of the option for enabling the progress bar.
	 */
	public static final String PROGRESS_OPTION_NAME = "progress"; //$NON-NLS-1$

	private final Provider<SarlBatchCompiler> compiler;

	private final Provider<SarlcConfig> configuration;

	private final Provider<PathDetector> pathDetector;

	private final Provider<ProgressBarConfig> progressConfig;

	/** Constructor.
	 *
	 * @param compiler the SARL batch compiler.
	 * @param configuration the configuration of the tool.
	 * @param pathDetector the detector of path.
	 * @param progressConfig the configuration of the progress bar.
	 */
	@BQInject
	public CompilerCommand(Provider<SarlBatchCompiler> compiler, Provider<SarlcConfig> configuration,
			Provider<PathDetector> pathDetector, Provider<ProgressBarConfig> progressConfig) {
		super(CommandMetadata
				.builder(CompilerCommand.class)
				.description(Messages.CompilerCommand_0));
		this.compiler = compiler;
		this.configuration = configuration;
		this.pathDetector = pathDetector;
		this.progressConfig = progressConfig;
	}

	@Override
	public CommandOutcome run(Cli cli) {
		if (cli.standaloneArguments().isEmpty()) {
			return CommandOutcome.failed(BootiqueMain.ERROR_CODE, Messages.CompilerCommand_1);
		}

		final var config = this.configuration.get();
		final var paths = this.pathDetector.get();
		paths.setSarlOutputPath(config.getOutputPath());
		paths.setClassOutputPath(config.getClassOutputPath());
		paths.setTempDirectory(config.getTempDirectory());
		try {
			paths.resolve(cli.standaloneArguments());
		} catch (IOException exception) {
			return CommandOutcome.failed(BootiqueMain.ERROR_CODE, exception);
		}

		final var comp = this.compiler.get();

		comp.setOutputPath(paths.getSarlOutputPath());
		comp.setClassOutputPath(paths.getClassOutputPath());
		comp.setTempDirectory(paths.getTempDirectory());

		for (final var cliArg : cli.standaloneArguments()) {
			comp.addSourcePath(cliArg);
		}

		final var firstErrorMessage = new OutParameter<String>();
		final var nbErrors = new AtomicInteger(0);
		final var nbWarnings = new AtomicInteger(0);
		comp.addIssueMessageListener((severity, issue, uri, message) -> {
			if (firstErrorMessage.get() == null) {
				firstErrorMessage.set(message);
			}
			if (issue.isSyntaxError() || severity == Severity.ERROR) {
				nbErrors.incrementAndGet();
			} else if (issue.getSeverity() == Severity.WARNING) {
				nbWarnings.incrementAndGet();
			}
		});

		final var nbFiles = new AtomicInteger(0);
		comp.addCompiledResourceReceiver(it -> nbFiles.incrementAndGet());

		// Configuration of the extra-language generators
		final var extraGenerators = config.getExtraGenerators();
		if (!Strings.isNullOrEmpty(extraGenerators)) {
			comp.setExtraLanguageGenerators(extraGenerators);
		}

		return runCompiler(comp, firstErrorMessage, nbErrors, nbWarnings, nbFiles);
	}

	private CommandOutcome runCompiler(SarlBatchCompiler comp, OutParameter<String> firstErrorMessage,
			AtomicInteger nbErrors, AtomicInteger nbWarnings, AtomicInteger nbFiles) {
		final var progressConfig = this.progressConfig.get();
		final boolean compilationResult;
		if (progressConfig.getEnable()) {
			compilationResult = comp.compile(new ConsoleProgressMonitor(progressConfig.getStyle()));
		} else {
			compilationResult = comp.compile();
		}
		if (!compilationResult) {
			showErrorAndWarningCount(comp, nbErrors, nbWarnings, nbFiles);
			return CommandOutcome.failed(BootiqueMain.ERROR_CODE, Strings.nullToEmpty(firstErrorMessage.get()));
		}
		showWarningCount(comp, nbWarnings, nbFiles);
		return CommandOutcome.succeeded();
	}

	private static void showErrorAndWarningCount(SarlBatchCompiler comp, Number errs, Number warns, Number files) {
		final var errValue = errs.longValue();
		if (errValue > 0) {
			final var warnValue = warns.longValue();
			final String msg;
			if (errValue > 1) {
				if (warnValue > 1) {
					msg = Messages.CompilerCommand_2;
				} else if (warnValue == 1) {
					msg = Messages.CompilerCommand_3;
				} else {
					msg = Messages.CompilerCommand_4;
				}
			} else {
				if (warnValue > 1) {
					msg = Messages.CompilerCommand_5;
				} else if (warnValue == 1) {
					msg = Messages.CompilerCommand_6;
				} else {
					msg = Messages.CompilerCommand_7;
				}
			}
			comp.getLogger().info(MessageFormat.format(msg, Long.valueOf(errValue), Long.valueOf(warnValue)));
		} else {
			showWarningCount(comp, warns, files);
		}
	}

	private static void showWarningCount(SarlBatchCompiler comp, Number warns, Number files) {
		final var value = warns.longValue();
		final String msg;
		if (value > 0) {
			if (value > 1) {
				msg = Messages.CompilerCommand_8;
			} else {
				msg = Messages.CompilerCommand_9;
			}
		} else {
			msg = Messages.CompilerCommand_10;
		}
		comp.getLogger().info(MessageFormat.format(msg, Long.valueOf(value), files));
	}

	/** Progress monitor that outputs on the console.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version sarlc 0.14.0 20241106-161410
	 * @mavengroupid io.sarl.cli
	 * @mavenartifactid sarlc
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

		@SuppressWarnings("resource")
		@Override
		public void done() {
			ensureBar();
			this.bar.stepTo(this.bar.getMax());
			this.bar.close();
		}

		@SuppressWarnings("resource")
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

		@SuppressWarnings("resource")
		@Override
		public void worked(int workUnits) {
			ensureBar();
			this.bar.setExtraMessage("").stepTo(workUnits); //$NON-NLS-1$
		}

	}

}
