/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.apputils.bootiqueapp.mdhelp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.di.Injector;
import io.bootique.help.HelpOption;
import io.bootique.help.HelpOptions;
import io.bootique.meta.application.ApplicationMetadata;
import io.bootique.meta.application.CommandMetadata;

import io.sarl.lang.core.util.CliUtilities;

/**
 * Command for displaying the help on the standard output using a Markdown format.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@SuppressWarnings({ "removal", "deprecation" })
public class GenerateMarkdownHelpCommand extends CommandWithMetadata {

	private static final String CLI_NAME = "generatemarkdownhelp"; //$NON-NLS-1$

	private static final String PIPE = "&#124;"; //$NON-NLS-1$

	private final Injector injector;

	/** Constructor.
	 *
	 * @param injector the injector to be used.
	 */
	public GenerateMarkdownHelpCommand(Injector injector) {
		this(injector, GenerateMarkdownHelpCommand.class, Messages.GenerateMarkdownHelpCommand_0);
	}

	/** Constructor.
	 *
	 * @param injector the injector to be used.
	 * @param commandType the type of the command.
	 * @param description the description of the command.
	 */
	protected GenerateMarkdownHelpCommand(Injector injector, Class<? extends GenerateMarkdownHelpCommand> commandType, String description) {
		super(CommandMetadata
	            .builder(commandType)
	            .description(description)
	            .name(CLI_NAME));
		this.injector = injector;
	}

	/** Replies the list of command-line options that are defined inside the runtime injector.
	 *
	 * @param runtime the runtime injector.
	 * @return the options.
	 */
	public static List<HelpOption> getOptions(Injector runtime) {
		final var application = runtime.getInstance(ApplicationMetadata.class);
		final var helpOptions = new HelpOptions();

		application.getCommands().forEach(c -> {
			helpOptions.add(c.asOption());
			c.getOptions().forEach(o -> helpOptions.add(o));
		});

		application.getOptions().forEach(o -> helpOptions.add(o));

		return helpOptions.getOptions();
	}
	
	/** Replies the options of the program in the form of a matrix of strings of characters.
	 * The replied value is a table with 2 columns: the options' names, and the options' descriptions.
	 * Each line of the table is a different option.
	 *
	 * @param runtime the runtime injector.
	 * @param replacePipes indicates if the pipe character must be replaced by its HTML equivalent character.
	 * @return the options of the program as a matrix of strings.
	 */
	public static List<List<String>> getOptionsAsStrings(Injector runtime, boolean replacePipes) {
		final var options = getOptions(runtime);
		final var matrix = new ArrayList<List<String>>(options.size());
		for (final var option : options) {
			var valueName = option.getOption().getValueName();
			if (valueName == null || valueName.length() == 0) {
				valueName = "val"; //$NON-NLS-1$
			} else if (replacePipes) {
				valueName = valueName.replace("|", PIPE); //$NON-NLS-1$
			}

			final var buffer0 = new StringBuilder();
			if (option.isShortNameAllowed()) {
				buffer0.append("-"); //$NON-NLS-1$
				buffer0.append(String.valueOf(option.getOption().getShortName()));

				switch (option.getOption().getValueCardinality()) {
				case REQUIRED:
					buffer0.append(" "); //$NON-NLS-1$
					buffer0.append(valueName);
					break;
				case OPTIONAL:
					buffer0.append(" ["); //$NON-NLS-1$
					buffer0.append(valueName);
					buffer0.append("]"); //$NON-NLS-1$
					break;
				case NONE:
				default:
				}
			}

			if (option.isLongNameAllowed()) {

				if (option.isShortNameAllowed()) {
					buffer0.append("<br>"); //$NON-NLS-1$
				}
				buffer0.append(CliUtilities.getCommandLineLongOptionPrefix());
				buffer0.append(option.getOption().getName());
				switch (option.getOption().getValueCardinality()) {
				case REQUIRED:
					buffer0.append("="); //$NON-NLS-1$
					buffer0.append(valueName);
					break;
				case OPTIONAL:
					buffer0.append("[="); //$NON-NLS-1$
					buffer0.append(valueName);
					buffer0.append("]"); //$NON-NLS-1$
					break;
				case NONE:
				default:
				}
			}

			final var buffer1 = new StringBuilder();

			final var description = option.getOption().getDescription();
			if (description != null) {
				var text = description.replaceAll("[ \t\n\r\f]+", " "); //$NON-NLS-1$ //$NON-NLS-2$
				text = text.replace("<", "&lt;");  //$NON-NLS-1$//$NON-NLS-2$
				text = text.replace(">", "&gt;");  //$NON-NLS-1$//$NON-NLS-2$
				if (replacePipes) {
					text = text.replace("|", PIPE);  //$NON-NLS-1$
				}
				buffer1.append(description);
			}

			matrix.add(Arrays.asList(buffer0.toString(), buffer1.toString()));
		}

		return matrix;
	}

	@Override
	public CommandOutcome run(Cli cli) {
		final var options = getOptionsAsStrings(this.injector, true);
		final var content = new StringBuilder();	
		for (final var row : options) {
			boolean first = true;
			for (final var cell : row) {
				if (first) {
					first = false;
					content.append("| "); // $NON-NLS-1$ //$NON-NLS-1$
				} else {
					content.append(" | "); // $NON-NLS-1$ //$NON-NLS-1$
				}
				content.append(cell);
			}
			content.append(" |\n"); // $NON-NLS-1$ //$NON-NLS-1$
		}
		System.out.println(content.toString());
		return CommandOutcome.succeeded();
	}

}
