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

import java.io.StringWriter;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.fasterxml.jackson.dataformat.yaml.snakeyaml.DumperOptions;
import com.fasterxml.jackson.dataformat.yaml.snakeyaml.Yaml;
import com.google.inject.Provider;
import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.log.BootLogger;
import io.bootique.meta.application.CommandMetadata;

import io.sarl.maven.bqextension.configs.Config;
import io.sarl.maven.bqextension.configs.Configs;

/**
 * Command for showing configuration.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class PrintConfigCommand extends CommandWithMetadata {

	private static final String CLI_NAME = "printconfig"; //$NON-NLS-1$

	private static final char CLI_SHORTNAME = 'C';

	private final BootLogger bootLogger;

	private final Provider<Set<Config>> configs;

	/** Constructor.
	 *
	 * @param bootLogger the boot logger.
	 * @param configs the configurations.
	 */
	public PrintConfigCommand(BootLogger bootLogger, Provider<Set<Config>> configs) {
		super(CommandMetadata
	            .builder(PrintConfigCommand.class)
	            .description(Messages.PrintConfigCommand_0)
	            .name(CLI_NAME).shortName(CLI_SHORTNAME));
		this.bootLogger = bootLogger;
		this.configs = configs;
	}

	@Override
	public CommandOutcome run(Cli cli) {
		final Map<String, Object> yaml = new TreeMap<>();
		extractYaml(yaml, this.configs.get());
		this.bootLogger.stdout(generateYaml(yaml));
		return CommandOutcome.succeeded();
	}

	/** Extract the Yaml definition from the given configurations.
	 *
	 * @param yaml the Yaml to fill out.
	 * @param configs the configurations.
	 */
	@SuppressWarnings("static-method")
	protected void extractYaml(Map<String, Object> yaml, Set<Config> configs) {
		for (final Config config : configs) {
			Configs.defineConfig(yaml, config);
		}
	}

	/** Generate the Yaml representation of the given map.
	 *
	 * @param yaml the Yaml to print out.
	 * @return the Yaml representation.
	 */
	@SuppressWarnings("static-method")
	protected String generateYaml(Map<String, Object> yaml) {
		final DumperOptions options = new DumperOptions();
		options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
		options.setDefaultScalarStyle(DumperOptions.ScalarStyle.PLAIN);
		options.setPrettyFlow(true);
		final Yaml yamlObj = new Yaml(options);
		final StringWriter writer = new StringWriter();
		yamlObj.dump(yaml, writer);
		return writer.toString();
	}

}
