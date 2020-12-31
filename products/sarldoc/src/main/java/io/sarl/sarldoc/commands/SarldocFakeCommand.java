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

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;
import javax.inject.Provider;

import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import org.arakhne.afc.vmutil.DynamicURLClassLoader;
import org.arakhne.afc.vmutil.json.JsonBuffer;

import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.tools.PathDetector;
import io.sarl.lang.sarlc.tools.SARLClasspathProvider;
import io.sarl.sarldoc.configs.SarldocConfig;

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

	/** Constructor with all the fields set to {@code null}.
	 * A command created with this constructor cannot be run. But is could be used for obtaining the
	 * command options.
	 *
	 * @since 0.12
	 */
	public SarldocFakeCommand() {
		this(null, null, null, null, null, null);
	}

	/** Constructor.
	 *
	 * @param sarldocClassLoader the dynamic class loader that could be used by sarldoc.
	 * @param logger the logger to be used by the command.
	 * @param config the sarldoc configuration provider.
	 * @param sarlcConfig the sarlc configuration provider.
	 * @param defaultClasspath the provider of default classpaths.
	 * @param pathDetector the detector of paths.
	 */
	public SarldocFakeCommand(DynamicURLClassLoader sarldocClassLoader, Provider<Logger> logger,
			Provider<SarldocConfig> config, Provider<SarlcConfig> sarlcConfig,
			Provider<SARLClasspathProvider> defaultClasspath, Provider<PathDetector> pathDetector) {
		super(sarldocClassLoader, logger, config, sarlcConfig, defaultClasspath, pathDetector, CLI_NAME);
	}

	@Override
	protected CommandOutcome runSarlc(Cli cli, Logger logger) {
		return CommandOutcome.succeeded();
	}

	@Override
	protected CommandOutcome runJavadoc(DynamicURLClassLoader classLoader, String javadocExecutable,
			List<String> cmd, SarldocConfig docconfig, SarlcConfig cconfig, Logger logger,
			AtomicInteger notUsed1, AtomicInteger notUsed2) {
		final JsonBuffer buffer = new JsonBuffer();
		buffer.add("javadoc.executable", javadocExecutable); //$NON-NLS-1$
		buffer.add("javadoc.command", cmd); //$NON-NLS-1$
		buffer.add("class.path", Arrays.asList(classLoader.getURLs())); //$NON-NLS-1$
		buffer.add("documentation.output", docconfig.getDocumentationOutputDirectory()); //$NON-NLS-1$
		buffer.add("java.output", cconfig.getOutputPath()); //$NON-NLS-1$
		buffer.add("class.output", cconfig.getClassOutputPath()); //$NON-NLS-1$
		buffer.add("temp.directory", cconfig.getTempDirectory()); //$NON-NLS-1$
		logger.info(buffer.toString());
		return CommandOutcome.succeeded();
	}

}
