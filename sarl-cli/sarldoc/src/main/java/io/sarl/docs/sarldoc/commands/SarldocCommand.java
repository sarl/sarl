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

package io.sarl.docs.sarldoc.commands;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import javax.inject.Provider;
import javax.tools.Diagnostic;
import javax.tools.DiagnosticListener;
import javax.tools.DocumentationTool;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

import com.google.common.base.Strings;
import io.bootique.cli.Cli;
import io.bootique.command.CommandManager;
import io.bootique.command.CommandOutcome;
import io.bootique.command.ManagedCommand;
import io.bootique.di.BQInject;
import org.arakhne.afc.vmutil.FileSystem;

import io.sarl.apputils.bootiqueapp.BootiqueMain;
import io.sarl.docs.sarldoc.configs.SarldocConfig;
import io.sarl.docs.sarldoc.tools.DocumentationPathDetector;
import io.sarl.lang.sarlc.commands.CompilerCommand;
import io.sarl.lang.sarlc.configs.SarlcConfig;

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

	private final Provider<CommandManager> commandManagerProvider;

	/** Constructor.
	 *
	 * @param commandManager the provider of the manager of the commands.
	 * @param logger the logger to be used by the command.
	 * @param config the sarldoc configuration provider.
	 * @param sarlcConfig the sarlc configuration provider.
	 * @param pathDetector the detector of paths.
	 */
	@BQInject
	public SarldocCommand(
			Provider<CommandManager> commandManager, Provider<Logger> logger,
			Provider<SarldocConfig> config, Provider<SarlcConfig> sarlcConfig,
			Provider<DocumentationPathDetector> pathDetector) {
		super(logger, config, sarlcConfig, pathDetector, null);
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

	/** Format the diagnostic message.
	 *
	 * @param diagnostic the diagnostic object.
	 * @return the message.
	 * @since 0.13
	 */
	protected static String formatDiagnosticMessage(Diagnostic<? extends JavaFileObject> diagnostic) {
		final String code = diagnostic.getCode();
		final JavaFileObject source = diagnostic.getSource();
		final long line = diagnostic.getLineNumber();
		final long column = diagnostic.getColumnNumber();

		String diagMessage = diagnostic.getMessage(null);
		if (Strings.isNullOrEmpty(diagMessage)) {
			diagMessage = MessageFormat.format(Messages.SarldocCommand_10, code);
		}

		final String sourceName;
		if (source != null) {
			sourceName = source.getName();
		} else {
			sourceName = null;
		}

		//		if (Strings.isNullOrEmpty(code)) {
		//			if (Strings.isNullOrEmpty(sourceName)) {
		//				return MessageFormat.format(Messages.SarldocCommand_11, diagMessage);
		//			}
		//			return MessageFormat.format(Messages.SarldocCommand_12, diagMessage, sourceName, line, column);
		//		}
		//		if (Strings.isNullOrEmpty(sourceName)) {
		//			return MessageFormat.format(Messages.SarldocCommand_13, diagMessage, code);
		//		}
		//		return MessageFormat.format(Messages.SarldocCommand_14, diagMessage, code, sourceName, line, column);
		if (Strings.isNullOrEmpty(sourceName)) {
			return MessageFormat.format(Messages.SarldocCommand_11, diagMessage);
		}
		return MessageFormat.format(Messages.SarldocCommand_12, diagMessage, sourceName,
				Long.valueOf(line), Long.valueOf(column));
	}

	@Override
	@SuppressWarnings("resource")
	protected CommandOutcome runJavadoc(
			Collection<File> sourceFiles,
			DocumentationPathDetector paths,
			Class<?> docletClass,
			List<String> javadocOptions,
			SarldocConfig docconfig,
			Logger logger,
			AtomicInteger errorCount,
			AtomicInteger warningCount) {
		// Execute the Javadoc
		try {
			final File outFolder = paths.getDocumentationOutputPath().getAbsoluteFile();
			logger.info(MessageFormat.format(Messages.SarldocCommand_5, outFolder.getAbsolutePath()));
			FileSystem.delete(outFolder);
			outFolder.mkdirs();

			final DiagnosticListener<? super JavaFileObject> diagnosticListener = (diagnostic) -> {
				switch (diagnostic.getKind()) {
				case ERROR:
					errorCount.incrementAndGet();
					logger.severe(formatDiagnosticMessage(diagnostic));
					break;
				case MANDATORY_WARNING:
				case WARNING:
					warningCount.incrementAndGet();
					logger.warning(formatDiagnosticMessage(diagnostic));
					break;
				case NOTE:
					logger.info(formatDiagnosticMessage(diagnostic));
					break;
				case OTHER:
					logger.config(formatDiagnosticMessage(diagnostic));
					break;
				default:
					// Nothing to be logged
					break;
				}
			};

			final DocumentationTool docTool = ToolProvider.getSystemDocumentationTool();
			final StandardJavaFileManager fileManager = docTool.getStandardFileManager(diagnosticListener, null, null);

			final Iterable<? extends JavaFileObject> javaFiles = fileManager.getJavaFileObjectsFromFiles(sourceFiles);

			fileManager.setLocation(DocumentationTool.Location.DOCUMENTATION_OUTPUT, Arrays.asList(outFolder));

			final DocumentationTool.DocumentationTask docTask = docTool.getTask(
					new ErrorWriter(logger, errorCount),
					fileManager, diagnosticListener, docletClass, javadocOptions, javaFiles);
			final boolean result = docTask.call().booleanValue();
			if (result) {
				return CommandOutcome.succeeded();
			}
			return CommandOutcome.failed(BootiqueMain.ERROR_CODE, EMPTY_STRING);
		} catch (Throwable exception) {
			return CommandOutcome.failed(BootiqueMain.ERROR_CODE, exception);
		}
	}

	/** Print writer that is able to output message with the logger as severe messages.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class ErrorWriter extends Writer {

		/** Logger.
		 */
		protected final Logger logger;

		/** Number of errors.
		 */
		protected final AtomicInteger errorCount;

		/** Constructor.
		 *
		 * @param logger the logger.
		 * @param errorCount the counter of errors.
		 */
		ErrorWriter(Logger logger, AtomicInteger errorCount) {
			this.logger = logger;
			this.errorCount = errorCount;
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

		/** Log the message.
		 *
		 * @param message the error message.
		 */
		protected void log(String message) {
			this.logger.severe(message);
			this.errorCount.incrementAndGet();
		}

	}

}
