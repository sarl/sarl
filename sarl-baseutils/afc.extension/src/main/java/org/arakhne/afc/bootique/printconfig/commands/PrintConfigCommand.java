/*
 * $Id$
 * This file is a part of the Arakhne Foundation Classes, http://www.arakhne.org/afc
 *
 * Copyright (c) 2000-2012 Stephane GALLAND.
 * Copyright (c) 2005-10, Multiagent Team, Laboratoire Systemes et Transports,
 *                        Universite de Technologie de Belfort-Montbeliard.
 * Copyright (c) 2013-2022 The original authors, and other authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.arakhne.afc.bootique.printconfig.commands;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import javax.inject.Provider;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator.Feature;
import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.di.Injector;
import io.bootique.log.BootLogger;
import io.bootique.meta.application.CommandMetadata;
import io.bootique.meta.application.OptionMetadata;
import io.bootique.meta.config.ConfigMetadataNode;
import io.bootique.meta.module.ModulesMetadata;
import joptsimple.OptionSpec;

import org.arakhne.afc.bootique.printconfig.configs.Configs;
import org.arakhne.afc.vmutil.locale.Locale;

/**
 * Command for showing configuration.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 15.0
 */
public class PrintConfigCommand extends CommandWithMetadata {

	private static final String CLI_NAME = "printconfig"; //$NON-NLS-1$

	private static final char CLI_SHORTNAME = 'C';

	private static final String JSON_OPTION = "json"; //$NON-NLS-1$

	private static final String XML_OPTION = "xml"; //$NON-NLS-1$

	private static final String XML_ROOT_NAME = "configuration"; //$NON-NLS-1$

	private static final int ERROR_CODE = 255;

	private final Provider<BootLogger> bootLogger;

	private final Provider<ModulesMetadata> modulesMetadata;

	private final Injector injector;

	/** Constructor.
	 *
	 * @param bootLogger the boot logger.
	 * @param modulesMetadata the metadata of the bootique modules.
	 * @param injector the injector.
	 */
	public PrintConfigCommand(Provider<BootLogger> bootLogger, Provider<ModulesMetadata> modulesMetadata, Injector injector) {
		super(CommandMetadata
				.builder(PrintConfigCommand.class)
				.description(Locale.getString("COMMAND_DESCRIPTION", JSON_OPTION, XML_OPTION)) //$NON-NLS-1$
				.name(CLI_NAME).shortName(CLI_SHORTNAME)
				.addOption(
						OptionMetadata.builder(JSON_OPTION)
						.description(Locale.getString("JSON_DESCRIPTION")) //$NON-NLS-1$
						.build())
				.addOption(
						OptionMetadata.builder(XML_OPTION)
						.description(Locale.getString("XML_DESCRIPTION")) //$NON-NLS-1$
						.build()));
		this.bootLogger = bootLogger;
		this.modulesMetadata = modulesMetadata;
		this.injector = injector;
	}

	@Override
	public CommandOutcome run(Cli cli) {
		final Map<String, Object> values = new TreeMap<>();
		extractConfigValues(values, Configs.extractConfigs(this.modulesMetadata.get()));
		// Search for the last format option
		final List<OptionSpec<?>> options = cli.detectedOptions();
		String lastOpt = null;
		for (int i = options.size() - 1; lastOpt == null && i >= 0; --i) {
			final OptionSpec<?> opt = options.get(i);
			if (opt.options().contains(JSON_OPTION)) {
				lastOpt = JSON_OPTION;
			} else if (opt.options().contains(XML_OPTION)) {
				lastOpt = XML_OPTION;
			}
		}
		// Generate the output
		final String content;
		try {
			if (lastOpt == JSON_OPTION) {
				content = generateJson(values);
			} else if (lastOpt == XML_OPTION) {
				content = generateXml(values);
			} else {
				content = generateYaml(values);
			}
		} catch (JsonProcessingException exception) {
			return CommandOutcome.failed(ERROR_CODE, exception.getLocalizedMessage(), exception);
		}
		this.bootLogger.get().stdout(content);
		return CommandOutcome.succeeded();
	}

	/** Extract the definition from the given configurations.
	 *
	 * @param yaml the to fill out.
	 * @param configs the configurations.
	 */
	protected void extractConfigValues(Map<String, Object> yaml, List<ConfigMetadataNode> configs) {
		for (final ConfigMetadataNode config : configs) {
			Configs.defineConfig(yaml, config, this.injector);
		}
	}

	/** Generate the Yaml representation of the given map.
	 *
	 * @param map the map to print out.
	 * @return the Yaml representation.
	 * @throws JsonProcessingException when the Json cannot be processed.
	 */
	@SuppressWarnings("static-method")
	protected String generateYaml(Map<String, Object> map) throws JsonProcessingException {
		final YAMLFactory yamlFactory = new YAMLFactory();
		yamlFactory.configure(Feature.WRITE_DOC_START_MARKER, false);
		final ObjectMapper mapper = new ObjectMapper(yamlFactory);
		return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(map);
	}

	/** Generate the Json representation of the given map.
	 *
	 * @param map the map to print out.
	 * @return the Json representation.
	 * @throws JsonProcessingException when the Json cannot be processed.
	 */
	@SuppressWarnings("static-method")
	protected String generateJson(Map<String, Object> map) throws JsonProcessingException {
		final ObjectMapper mapper = new ObjectMapper();
		return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(map);
	}

	/** Generate the Xml representation of the given map.
	 *
	 * @param map the map to print out.
	 * @return the Xml representation.
	 * @throws JsonProcessingException when XML cannot be processed.
	 */
	@SuppressWarnings({"static-method"})
	protected String generateXml(Map<String, Object> map) throws JsonProcessingException {
		final XmlMapper mapper = new XmlMapper();
		return mapper.writerWithDefaultPrettyPrinter().withRootName(XML_ROOT_NAME).writeValueAsString(map);
	}

}
