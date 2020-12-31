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

package io.sarl.maven.bootiqueapp.version;

import java.text.MessageFormat;

import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.log.BootLogger;
import io.bootique.meta.application.CommandMetadata;

import io.sarl.lang.SARLVersion;

/**
 * Command for showing the sarlc version.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class VersionCommand extends CommandWithMetadata {

	private static final String CLI_NAME = "version"; //$NON-NLS-1$

	private final BootLogger bootLogger;

	/** Constructor.
	 *
	 * @param bootLogger the logger.
	 */
	public VersionCommand(BootLogger bootLogger) {
		this(bootLogger, VersionCommand.class, Messages.VersionCommand_0);
	}

	/** Constructor.
	 *
	 * @param bootLogger the logger.
	 * @param commandType the type of the command.
	 * @param description the description of the command.
	 * @since 0.11
	 */
	protected VersionCommand(BootLogger bootLogger, Class<? extends VersionCommand> commandType,
			String description) {
		super(CommandMetadata
	            .builder(commandType)
	            .description(description)
	            .name(CLI_NAME));
		this.bootLogger = bootLogger;
	}

	/** Replies the message that should be output by the command.
	 *
	 * @return the message.
	 * @since 0.11
	 */
	protected String getVersionMessage() {
		return MessageFormat.format(Messages.VersionCommand_1,
				SARLVersion.SARL_RELEASE_VERSION, SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING,
				System.getProperty("java.vm.name"), //$NON-NLS-1$
				System.getProperty("java.version"), //$NON-NLS-1$
				System.getProperty("java.home")); //$NON-NLS-1$
	}
	
	@Override
	public CommandOutcome run(Cli cli) {
		final StringBuilder text = new StringBuilder();
		text.append(getVersionMessage());
		this.bootLogger.stdout(text.toString());
		return CommandOutcome.succeeded();
	}

}
