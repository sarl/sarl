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

package io.sarl.maven.bootiqueapp.mdconfig;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.google.common.primitives.Primitives;
import io.bootique.annotation.BQConfigProperty;
import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.meta.MetadataNode;
import io.bootique.meta.application.CommandMetadata;
import io.bootique.meta.config.ConfigListMetadata;
import io.bootique.meta.config.ConfigMapMetadata;
import io.bootique.meta.config.ConfigMetadataNode;
import io.bootique.meta.config.ConfigMetadataVisitor;
import io.bootique.meta.config.ConfigObjectMetadata;
import io.bootique.meta.config.ConfigValueMetadata;
import io.bootique.meta.module.ModuleMetadata;
import io.bootique.meta.module.ModulesMetadata;
import org.eclipse.xtext.util.Strings;

/**
 * Command for displaying the help for configuration parameters on the standard output using a Markdown format.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class GenerateMarkdownConfigCommand extends CommandWithMetadata {

	private static final String CLI_NAME = "generatemarkdownconfighelp"; //$NON-NLS-1$

	private static final String CLI_ROOT_PROPERTY = CLI_NAME + ".root"; //$NON-NLS-1$

	private static final String PIPE = "&#124;";

	private final ModulesMetadata modulesMetadata;

	/** Constructor.
	 *
	 * @param modulesMetadata the description of the modules.
	 */
	public GenerateMarkdownConfigCommand(ModulesMetadata modulesMetadata) {
		this(modulesMetadata, GenerateMarkdownConfigCommand.class, Messages.GenerateMarkdownConfigCommand_0);
	}

	/** Constructor.
	 *
	 * @param modulesMetadata the description of the modules.
	 * @param commandType the type of the command.
	 * @param description the description of the command.
	 */
	protected GenerateMarkdownConfigCommand(ModulesMetadata modulesMetadata, Class<? extends GenerateMarkdownConfigCommand> commandType, String description) {
		super(CommandMetadata
				.builder(commandType)
				.description(description)
				.name(CLI_NAME));
		this.modulesMetadata = modulesMetadata;
	}

	/** Replies the list of configuration parameters.
	 *
	 * @param modulesMetadata the description of the modules.
	 * @param selectedRoot the name of the configuration root that must be selected and for which the configuration parameters
	 *     must be replied. If the name is empty or {@code null}, all the roots are selected by default.
	 * @return the configuration parameters.
	 */
	public static List<ConfigMetadataNode> getConfigurationParameters(ModulesMetadata modulesMetadata, String selectedRoot) {
		final Predicate<ConfigMetadataNode> filter;
		if (Strings.isEmpty(selectedRoot)) {
			filter = (it) -> true;
		} else {
			final String prefix = selectedRoot + "."; //$NON-NLS-1$
			filter = (it) -> it.getName().equals(selectedRoot) || it.getName().startsWith(prefix);
		}
		final List<ModuleMetadata> sortedModules = modulesMetadata
				.getModules()
				.stream()
				.sorted(Comparator.comparing(ModuleMetadata::getName))
				.collect(Collectors.toList());
		final List<ConfigMetadataNode> sortedConfigs = sortedModules.stream()
				.map(ModuleMetadata::getConfigs)
				.flatMap(Collection::stream)
				.filter(filter)
				.sorted(Comparator.comparing(MetadataNode::getName))
				.collect(Collectors.toList());
		return sortedConfigs;
	}

	/** Replies the list of configuration parameters.
	 *
	 * @param modulesMetadata the description of the modules.
	 * @param selectedRoot the name of the configuration root that must be selected and for which the configuration parameters
	 *     must be replied. If the name is empty or {@code null}, all the roots are selected by default.
	 * @param replacePipes indicates if the pipe character must be replaced by its HTML equivalent character.
	 * @param addLineIfNoData indicates if a line should be added if there is no data from the modules.
	 * @return the configuration parameters.
	 */
	public static List<List<String>> getConfigurationParametersAsStrings(ModulesMetadata modulesMetadata, String selectedRoot, boolean replacePipes, boolean addLineIfNoData) {
		final List<ConfigMetadataNode> parameters = getConfigurationParameters(modulesMetadata, selectedRoot);
		final List<List<String>> matrix = new ArrayList<>(parameters.size());
		final Visitor visitor = new Visitor(matrix, replacePipes);
		for (final ConfigMetadataNode parameter : parameters) {
			parameter.accept(visitor);
		}
		if (addLineIfNoData && matrix.isEmpty()) {
			addToMatrix(matrix);
		}
		return matrix;
	}

	private static void addToMatrix(List<List<String>> matrix, String property, String type, String description) {
		matrix.add(Arrays.asList("", property, type, description)); //$NON-NLS-1$
	}

	private static void addToMatrix(List<List<String>> matrix, String module, String description) {
		matrix.add(Arrays.asList(module, "", "", description)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static void addToMatrix(List<List<String>> matrix) {
		matrix.add(Arrays.asList("", "", "", "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	}

	@Override
	public CommandOutcome run(Cli cli) {
		final String rootName = System.getProperty(CLI_ROOT_PROPERTY, null);
		final List<List<String>> parameters = getConfigurationParametersAsStrings(this.modulesMetadata, rootName, true, true);
		final StringBuilder content = new StringBuilder();	
		for (final List<String> row : parameters) {
			boolean first = true;
			for (final String cell : row) {
				if (first) {
					first = false;
					content.append("| "); // $NON-NLS-1$
				} else {
					content.append(" | "); // $NON-NLS-1$
				}
				content.append(cell);
			}
			content.append(" |\n"); // $NON-NLS-1$
		}
		System.out.println(content.toString());
		return CommandOutcome.succeeded();
	}

	/**
	 * Command for displaying the help for configuration parameters on the standard output using a Markdown format.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.12
	 */
	private static class Visitor implements ConfigMetadataVisitor<Void> {

		private final List<List<String>> matrix;

		private final Set<String> added = new TreeSet<>();

		private final boolean replacePipes;

		private final Pattern setPattern = Pattern.compile("^set([A-Z])([a-zA-Z0-9]+)$"); //$NON-NLS-1$

		/** Constructor.
		 *
		 * @param matrix the matrix to build up.
		 * @param replacePipes indicates if the pipe character must be replaced by its HTML equivalent character.
		 */
		Visitor(List<List<String>> matrix, boolean replacePipes) {
			this.matrix = matrix;
			this.replacePipes = replacePipes;
		}

		private String prepareStr(String value) {
			final String fixedValue = Strings.emptyIfNull(value);
			if (this.replacePipes) {
				return fixedValue.replace("|", PIPE);
			}
			return fixedValue;
		}

		private void printMetadata(ConfigMetadataNode metadata) {
			final String name = prepareStr(metadata.getName());
			if (this.added.add(name)) {
				addToMatrix(this.matrix, name,
						prepareStr(metadata.getDescription()));
				final Class<?> type = (Class<?>) metadata.getType();
				if (type == null) {
					return;
				}
				for (final Method setterMethod : type.getMethods()) {
					if (Modifier.isPublic(setterMethod.getModifiers())
							&& !Modifier.isAbstract(setterMethod.getModifiers())
							&& !Modifier.isStatic(setterMethod.getModifiers())
							&& setterMethod.getParameterCount() == 1) {
						final BQConfigProperty annotation = setterMethod.getAnnotation(BQConfigProperty.class);
						if (annotation != null) {
							final Matcher matcher = this.setPattern.matcher(setterMethod.getName());
							if (matcher.matches()) {
								final String firstLetter = matcher.group(1);
								final String rest = matcher.group(2);
								final String propertyName = firstLetter.toLowerCase() + rest;
								addToMatrix(
										this.matrix,
										prepareStr(propertyName),
										prepareStr(getTypeLabel(setterMethod.getParameterTypes()[0])),
										prepareStr(annotation.value()));
							}
						}
					}
				}
			}
		}

		@SuppressWarnings("unchecked")
		private String getTypeLabel(Class<?> type) {
			final Class<?> uwt = Primitives.unwrap(type);
			if (uwt.isPrimitive()) {
				return uwt.getSimpleName();
			}
			if (CharSequence.class.isAssignableFrom(uwt)) {
				return "string";
			}
			if (uwt.isEnum()) {
				final Class<? extends Enum<?>> etype = (Class<? extends Enum<?>>) uwt;
				final StringBuilder buf = new StringBuilder();
				for (final Enum<?> cst : etype.getEnumConstants()) {
					if (buf.length() > 0) {
						buf.append(prepareStr(" | "));
					}
					buf.append("*\""); //$NON-NLS-1$
					buf.append(cst.name().toLowerCase());
					buf.append("\"*"); //$NON-NLS-1$
				}
				return buf.toString();
			}
			if (uwt instanceof Class) {
				Class<?> classType = (Class<?>) type;
				if (Map.class.isAssignableFrom(classType)) {
					return Map.class.getSimpleName();
				}
				else if (Collection.class.isAssignableFrom(classType)) {
					return Collection.class.getSimpleName();
				}
			}
			return uwt.getSimpleName();
		}

		@Override
		public Void visitObjectMetadata(ConfigObjectMetadata metadata) {
			printMetadata(metadata);
			//
			final List<ConfigObjectMetadata> selfAndSubconfigs = metadata
					.getAllSubConfigs()
					.map(md -> md.accept(new ConfigMetadataVisitor<ConfigObjectMetadata>() {
						@Override
						public ConfigObjectMetadata visitObjectMetadata(ConfigObjectMetadata visited) {
							return visited.isAbstractType() || visited.getProperties().isEmpty() ? null : visited;
						}
					}))
					.filter(md -> md != null)
					.collect(Collectors.toList());
			//
			if (!selfAndSubconfigs.isEmpty()) {
				selfAndSubconfigs.forEach(md -> {
					printMetadata(md);
				});
			}
			return null;
		}

		@Override
		public Void visitValueMetadata(ConfigValueMetadata metadata) {
			printMetadata(metadata);
			return null;
		}

		@Override
		public Void visitListMetadata(ConfigListMetadata metadata) {
			printMetadata(metadata);
			return null;
		}

		@Override
		public Void visitMapMetadata(ConfigMapMetadata metadata) {
			printMetadata(metadata);
			return null;
		}

	}

}
