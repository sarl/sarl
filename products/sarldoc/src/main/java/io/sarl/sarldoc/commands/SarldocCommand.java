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

package io.sarl.sarldoc.commands;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.lang.reflect.Method;
import java.text.MessageFormat;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;
import javax.inject.Provider;

import com.google.common.base.Strings;
import io.bootique.cli.Cli;
import io.bootique.command.CommandManager;
import io.bootique.command.CommandOutcome;
import io.bootique.command.ManagedCommand;
import org.arakhne.afc.vmutil.DynamicURLClassLoader;
import org.arakhne.afc.vmutil.FileSystem;

import io.sarl.lang.sarlc.commands.CompilerCommand;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.tools.PathDetector;
import io.sarl.lang.sarlc.tools.SARLClasspathProvider;
import io.sarl.maven.bootiqueapp.BootiqueMain;
import io.sarl.sarldoc.configs.SarldocConfig;

/**
 * Command for launching sarldoc.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class SarldocCommand extends AbstractSarldocCommand {

	private static final String JAVADOC_NOTE_PREFIX = "Note:";  //$NON-NLS-1$

	private static final String MAIN_CLASS = "com.sun.tools.javadoc.Main";  //$NON-NLS-1$

	private static final String STANDARD_DOCLET = "com.sun.tools.doclets.standard.Standard"; //$NON-NLS-1$

	private final Provider<CommandManager> commandManagerProvider;

	/** Constructor with all the fields set to {@code null}.
	 * A command created with this constructor cannot be run. But is could be used for obtaining the
	 * command options.
	 *
	 * @since 0.12
	 */
	public SarldocCommand() {
		this(null, null, null, null, null, null, null);
	}

	/** Constructor.
	 *
	 * @param sarldocClassLoader the dynamic class loader that could be used by sarldoc.
	 * @param commandManager the provider of the manager of the commands.
	 * @param logger the logger to be used by the command.
	 * @param config the sarldoc configuration provider.
	 * @param sarlcConfig the sarlc configuration provider.
	 * @param defaultClasspath the provider of default classpaths.
	 * @param pathDetector the detector of paths.
	 */
	public SarldocCommand(DynamicURLClassLoader sarldocClassLoader, Provider<CommandManager> commandManager,
			Provider<Logger> logger, Provider<SarldocConfig> config, Provider<SarlcConfig> sarlcConfig,
			Provider<SARLClasspathProvider> defaultClasspath, Provider<PathDetector> pathDetector) {
		super(sarldocClassLoader, logger, config, sarlcConfig, defaultClasspath, pathDetector, null);
		this.commandManagerProvider = commandManager;
	}

	@Override
	protected CommandOutcome runSarlc(Cli cli, Logger logger) {
		CommandOutcome outcome;
		try {
			logger.info(Messages.SarldocCommand_1);
			final CommandManager cmdManager = this.commandManagerProvider.get();
			final ManagedCommand compilerCommand = cmdManager.lookupByType(CompilerCommand.class);
			outcome = compilerCommand.getCommand().run(cli);
		} catch (Exception e) {
			outcome = CommandOutcome.failed(1, e);
		}
		return outcome;
	}

	@Override
	@SuppressWarnings("checkstyle:magicnumber")
	protected CommandOutcome runJavadoc(DynamicURLClassLoader classLoader, String javadocExecutable,
			List<String> cmd, SarldocConfig docconfig, SarlcConfig cconfig, Logger logger,
			AtomicInteger errorCount, AtomicInteger warningCount) {
		// Execute the Javadoc
		try {
			// Find main class
			final Class<?> mainClass = classLoader.loadClass(MAIN_CLASS);
			final Method mainMethod = mainClass.getMethod("execute", //$NON-NLS-1$
					String.class,
					PrintWriter.class,
					PrintWriter.class,
					PrintWriter.class,
					String.class,
					ClassLoader.class,
					String[].class);

			// Delete target directory
			logger.info(MessageFormat.format(Messages.SarldocCommand_5, docconfig.getDocumentationOutputDirectory().getAbsolutePath()));
			FileSystem.delete(docconfig.getDocumentationOutputDirectory().getAbsoluteFile());

			final String[] cliArgs = new String[cmd.size()];
			cmd.toArray(cliArgs);

			// Run Javadoc
			try (PrintWriter errWriter = new PrintWriter(new ErrorWriter(logger, errorCount))) {
				try (PrintWriter warnWriter = new PrintWriter(new WarningWriter(logger, warningCount))) {
					try (PrintWriter infoWriter = new PrintWriter(new InformationWriter(logger, warningCount))) {
						final Object[] args = new Object[7];
						args[0] = javadocExecutable;
						args[1] = errWriter;
						args[2] = warnWriter;
						args[3] = infoWriter;
						args[4] = STANDARD_DOCLET;
						args[5] = classLoader;
						args[6] = cliArgs;
						final Integer code = (Integer) mainMethod.invoke(null, args);
						if (code.intValue() == 0) {
							return CommandOutcome.succeeded();
						}
						return CommandOutcome.failed(BootiqueMain.ERROR_CODE, EMPTY_STRING);
					}
				}
			}
		} catch (Throwable exception) {
			return CommandOutcome.failed(BootiqueMain.ERROR_CODE, exception);
		}
	}

	/** Print writer that is able to output message with the logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private abstract static class LogWriter extends Writer {

		/** Logger.
		 */
		protected final Logger logger;

		/** Number of errors.
		 */
		protected final AtomicInteger errorCount;

		/** Number of errors.
		 */
		protected final AtomicInteger warningCount;

		/** Constructor.
		 *
		 * @param logger the logger.
		 * @param errorCount the counter of errors.
		 * @param warningCount the counter of warnings.
		 */
		LogWriter(Logger logger, AtomicInteger errorCount, AtomicInteger warningCount) {
			this.logger = logger;
			this.errorCount = errorCount;
			this.warningCount = warningCount;
		}

		@Override
		public final void flush() throws IOException {
			//
		}

		@Override
		public final void close() throws IOException {
			//
		}

		@Override
		public final void write(char[] cbuf, int off, int len) throws IOException {
			String message = new String(cbuf, off, len);
			message = message.trim();
			if (!Strings.isNullOrEmpty(message)) {
				log(message);
			}
		}

		protected abstract void log(String message);

	}

	/** Print writer that is able to output information with the logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class InformationWriter extends LogWriter {

		/** Constructor.
		 *
		 * @param logger the logger.
		 * @param warningCount the counter of warnings.
		 */
		InformationWriter(Logger logger, AtomicInteger warningCount) {
			super(logger, null, warningCount);
		}

		@Override
		protected void log(String message) {
			if (message.startsWith(JAVADOC_NOTE_PREFIX)) {
				this.logger.warning(message.substring(JAVADOC_NOTE_PREFIX.length()).trim());
				this.warningCount.incrementAndGet();
			} else {
				this.logger.info(message);
			}
		}

	}

	/** Print writer that is able to output warning message with the logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class WarningWriter extends LogWriter {

		/** Constructor.
		 *
		 * @param logger the logger.
		 * @param warningCount the counter of warnings.
		 */
		WarningWriter(Logger logger, AtomicInteger warningCount) {
			super(logger, null, warningCount);
		}

		@Override
		protected void log(String message) {
			this.logger.warning(message);
			this.warningCount.incrementAndGet();
		}

	}

	/** Print writer that is able to output error message with the logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class ErrorWriter extends LogWriter {

		/** Constructor.
		 *
		 * @param logger the logger.
		 * @param errorCount the counter of errors.
		 */
		ErrorWriter(Logger logger, AtomicInteger errorCount) {
			super(logger, errorCount, null);
		}

		@Override
		protected void log(String message) {
			this.logger.severe(message);
			this.errorCount.incrementAndGet();
		}

	}

}
