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

package io.sarl.lang.sarlc.modules.general;

import static io.bootique.BQCoreModule.extend;

import java.text.MessageFormat;

import javax.inject.Provider;

import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import org.arakhne.afc.bootique.applicationdata2.annotations.DefaultApplicationName;
import org.arakhne.afc.bootique.synopsishelp.annotations.ApplicationArgumentSynopsis;
import org.arakhne.afc.bootique.synopsishelp.annotations.ApplicationDetailedDescription;

import io.sarl.lang.SARLConfig;
import io.sarl.lang.sarlc.Constants;
import io.sarl.lang.sarlc.commands.CompilerCommand;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.util.CliUtilities;
import io.sarl.maven.bootiqueapp.utils.SystemProperties;

/** Module for configuring the sarlc application information.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class SarlcApplicationModule implements BQModule {

	@Override
	public void configure(Binder binder) {
		// Name of the application.
		binder.bind(String.class, DefaultApplicationName.class).toInstance(
				SystemProperties.getValue(SarlcConfig.PREFIX + ".programName", Constants.PROGRAM_NAME)); //$NON-NLS-1$
		// Short description of the application.
		extend(binder).setApplicationDescription(Messages.SarlcApplicationModule_0);
		// Long description of the application.
		binder.bind(String.class, ApplicationDetailedDescription.class).toProvider(LongDescriptionProvider.class).inSingletonScope();
		// Synopsis of the application's arguments.
		binder.bind(String.class, ApplicationArgumentSynopsis.class).toInstance(Messages.SarlcApplicationModule_1);
		// Default command
		extend(binder).setDefaultCommand(CompilerCommand.class);
	}

	/** Provider of the long description of the application.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	private static class LongDescriptionProvider implements Provider<String> {

		@Override
		public String get() {
			final String sarlOutputDirectory = SARLConfig.FOLDER_SOURCE_GENERATED;
			final String sarlOutputDirectoryOption = CliUtilities.getCommandLineOption(Constants.SARL_OUTPUT_DIRECTORY_OPTION);
			final String javaOutputDirectory = SARLConfig.FOLDER_BIN;
			final String javaOutputDirectoryOption = CliUtilities.getCommandLineOption(Constants.JAVA_OUTPUT_DIRECTORY_OPTION);
			return MessageFormat.format(Messages.SarlcApplicationModule_2,
					sarlOutputDirectory, sarlOutputDirectoryOption,
					javaOutputDirectory, javaOutputDirectoryOption);
		}

	}

}
