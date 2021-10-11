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

package io.sarl.lang.sarlc.commands;

import java.util.Set;
import java.util.TreeSet;

import javax.inject.Provider;

import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.log.BootLogger;
import io.bootique.meta.application.CommandMetadata;

import io.sarl.lang.extralanguage.IExtraLanguageContribution;
import io.sarl.lang.extralanguage.IExtraLanguageContributions;

/**
 * Command for showing up the list of extra languages that are available on the classpath.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class ExtraLanguageListCommand extends CommandWithMetadata {

	/** Name of the option for enabling the progress bar.
	 */
	public static final String EXTRA_LANGUAGE_LIST_OPTION_NAME = "print-generators"; //$NON-NLS-1$

	/** Name of the option for enabling the progress bar.
	 */
	public static final char EXTRA_LANGUAGE_LIST_OPTION_SHORT_NAME = 'G';

	private final BootLogger bootLogger;

	private final Provider<IExtraLanguageContributions> contributions;

	/** Constructor.
	 *
	 * @param bootLogger the logger.
	 * @param contributions the provider of the extra-language contributions.
	 */
	public ExtraLanguageListCommand(BootLogger bootLogger, Provider<IExtraLanguageContributions> contributions) {
		super(CommandMetadata
				.builder(ExtraLanguageListCommand.class)
				.description(Messages.ExtraLanguageListCommand_0)
				.name(EXTRA_LANGUAGE_LIST_OPTION_NAME)
				.shortName(EXTRA_LANGUAGE_LIST_OPTION_SHORT_NAME));
		this.bootLogger = bootLogger;
		this.contributions = contributions;
	}

	@Override
	public CommandOutcome run(Cli cli) {
		final Set<String> identifiers = new TreeSet<>();
		for (final IExtraLanguageContribution contribution : this.contributions.get().getContributions()) {
			identifiers.addAll(contribution.getIdentifiers());
		}
		final StringBuilder text = new StringBuilder();
		for (final String id : identifiers) {
			text.append(id).append("\n"); //$NON-NLS-1$
		}
		this.bootLogger.stdout(text.toString());
		return CommandOutcome.succeeded();
	}

}
