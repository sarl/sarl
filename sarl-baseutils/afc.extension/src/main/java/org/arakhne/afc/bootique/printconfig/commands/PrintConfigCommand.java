/*
 * $Id$
 * This file is a part of the Arakhne Foundation Classes, http://www.arakhne.org/afc
 *
 * Copyright (c) 2000-2012 Stephane GALLAND.
 * Copyright (c) 2005-10, Multiagent Team, Laboratoire Systemes et Transports,
 *                        Universite de Technologie de Belfort-Montbeliard.
 * Copyright (c) 2013-2025 The original authors, and other authors.
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

import jakarta.inject.Provider;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator.Feature;
import com.google.common.base.Strings;
import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.di.Injector;
import io.bootique.log.BootLogger;
import io.bootique.meta.application.CommandMetadata;
import io.bootique.meta.application.OptionMetadata;
import io.bootique.meta.config.ConfigMetadataNode;
import io.bootique.meta.module.ModulesMetadata;
import org.arakhne.afc.bootique.printconfig.configs.Configs;
import org.arakhne.afc.vmutil.locale.Locale;

/**
 * Command for showing configuration.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version afc.extension 0.15.0 20250909-115746
 * @mavengroupid io.sarl.baseutils
 * @mavenartifactid afc.extension
 * @since 15.0
 */
public class PrintConfigCommand extends CommandWithMetadata {

	private static final String CLI_NAME = "printconfig"; //$NON-NLS-1$

	private static final char CLI_SHORTNAME = 'C';

	private static final String JSON_OPTION = "json"; //$NON-NLS-1$

	private static final String XML_OPTION = "xml"; //$NON-NLS-1$

	private static final String XML_ROOT_NAME = "configuration"; //$NON-NLS-1$

	private static final String JVM_KEY = "jvm"; //$NON-NLS-1$

	private static final String JAVA_CLASSPATH_KEY = "java.class.path"; //$NON-NLS-1$

	private static final String JAVA_MODULEPATH_KEY = "jdk.module.path"; //$NON-NLS-1$

	private static final String JAVA_UPGRADEPATH_KEY = "jdk.module.upgrade.path"; //$NON-NLS-1$

	private static final String JAVA_MODULEMAIN_KEY = "jdk.module.main"; //$NON-NLS-1$

	private static final String JAVA_MODULEMAINCLASS_KEY = "jdk.module.main.class"; //$NON-NLS-1$

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
						.build())
				.build());
		this.bootLogger = bootLogger;
		this.modulesMetadata = modulesMetadata;
		this.injector = injector;
	}

	@Override
	public CommandOutcome run(Cli cli) {
		final var values = new TreeMap<String, Object>();
		extractJvmValues(values);
		extractConfigValues(values, Configs.extractConfigs(this.modulesMetadata.get()));
		// Search for the last format option
		final var options = cli.detectedOptions();
		String lastOpt = null;
		for (var i = options.size() - 1; lastOpt == null && i >= 0; --i) {
			final var opt = options.get(i);
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

	/** Extract the definitions from JVM properties.
	 * 
	 * @param yaml the structure to fill out.
	 */
	@SuppressWarnings("static-method")
	protected void extractJvmValues(Map<String, Object> yaml) {
		final var jvmValues = new TreeMap<String, Object>();
		try {
			Configs.defineScalar(jvmValues, JAVA_CLASSPATH_KEY, Strings.nullToEmpty(System.getProperty(JAVA_CLASSPATH_KEY)));
		} catch (Exception ex) {
			// Be silent
		}
		try {
			Configs.defineScalar(jvmValues, JAVA_MODULEPATH_KEY, Strings.nullToEmpty(System.getProperty(JAVA_MODULEPATH_KEY)));
		} catch (Exception ex) {
			// Be silent
		}
		try {
			Configs.defineScalar(jvmValues, JAVA_UPGRADEPATH_KEY, Strings.nullToEmpty(System.getProperty(JAVA_UPGRADEPATH_KEY)));
		} catch (Exception ex) {
			// Be silent
		}
		try {
			Configs.defineScalar(jvmValues, JAVA_MODULEMAIN_KEY, Strings.nullToEmpty(System.getProperty(JAVA_MODULEMAIN_KEY)));
		} catch (Exception ex) {
			// Be silent
		}
		try {
			Configs.defineScalar(jvmValues, JAVA_MODULEMAINCLASS_KEY, Strings.nullToEmpty(System.getProperty(JAVA_MODULEMAINCLASS_KEY)));
		} catch (Exception ex) {
			// Be silent
		}
		if (!jvmValues.isEmpty()) {
			yaml.put(JVM_KEY, jvmValues);
		}
	}

	/** Extract the definitions from the given configurations.
	 *
	 * @param yaml the structure to fill out.
	 * @param configs the configurations.
	 */
	protected void extractConfigValues(Map<String, Object> yaml, List<ConfigMetadataNode> configs) {
		for (final var config : configs) {
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
		final var yamlFactory = new YAMLFactory();
		yamlFactory.configure(Feature.WRITE_DOC_START_MARKER, false);
		final var mapper = new ObjectMapper(yamlFactory);
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
		final var mapper = new ObjectMapper();
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
		final var mapper = new XmlMapper();
		return mapper.writerWithDefaultPrettyPrinter().withRootName(XML_ROOT_NAME).writeValueAsString(map);
	}

}
