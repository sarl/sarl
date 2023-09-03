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

package io.sarl.docs.sarldoc.commands;

import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import javax.inject.Provider;

import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.di.BQInject;
import org.arakhne.afc.vmutil.json.JsonBuffer;

import io.sarl.docs.sarldoc.configs.SarldocConfig;
import io.sarl.docs.sarldoc.tools.DocumentationPathDetector;
import io.sarl.lang.sarlc.configs.SarlcConfig;

/**
 * Fake command for launching sarldoc.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class SarldocFakeCommand extends AbstractSarldocCommand {

	private static final String CLI_NAME = "fake"; //$NON-NLS-1$

	/** Constructor.
	 *
	 * @param logger the logger to be used by the command.
	 * @param config the sarldoc configuration provider.
	 * @param sarlcConfig the sarlc configuration provider.
	 * @param pathDetector the detector of paths.
	 */
	@BQInject
	public SarldocFakeCommand(
			Provider<Logger> logger, Provider<SarldocConfig> config, Provider<SarlcConfig> sarlcConfig,
			Provider<DocumentationPathDetector> pathDetector) {
		super(logger, config, sarlcConfig, pathDetector, CLI_NAME);
	}

	@Override
	protected CommandOutcome runSarlc(Cli cli, Logger logger) {
		return CommandOutcome.succeeded();
	}

	@Override
	protected CommandOutcome runJavadoc(
			Collection<File> sourceFiles,
			DocumentationPathDetector paths,
			Class<?> docletClass,
			List<String> javadocOptions,
			SarldocConfig docconfig,
			Logger logger,
			AtomicInteger errorCount,
			AtomicInteger warningCount) {
		final JsonBuffer buffer = new JsonBuffer();
		buffer.add("documentation.output", docconfig.getDocumentationOutputDirectory()); //$NON-NLS-1$
		buffer.add("source.files", sourceFiles); //$NON-NLS-1$
		buffer.add("javadoc.options", javadocOptions); //$NON-NLS-1$
		buffer.add("doclet.type", docletClass.getName()); //$NON-NLS-1$
		buffer.add("paths.class.output", paths.getClassOutputPath()); //$NON-NLS-1$
		buffer.add("paths.sarl.output", paths.getSarlOutputPath()); //$NON-NLS-1$
		buffer.add("paths.temp", paths.getTempDirectory()); //$NON-NLS-1$
		logger.info(buffer.toString());
		return CommandOutcome.succeeded();
	}

}
