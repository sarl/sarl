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

package io.sarl.docs.sarldoc.modules.internal;

import static io.bootique.BQCoreModule.extend;

import java.text.MessageFormat;
import java.util.logging.Logger;

import javax.inject.Provider;
import javax.inject.Singleton;

import io.bootique.config.ConfigurationFactory;
import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Key;
import io.bootique.di.Provides;
import org.arakhne.afc.bootique.applicationdata2.annotations.DefaultApplicationName;
import org.arakhne.afc.bootique.synopsishelp.annotations.ApplicationArgumentSynopsis;
import org.arakhne.afc.bootique.synopsishelp.annotations.ApplicationDetailedDescription;

import io.sarl.apputils.bootiqueapp.utils.SystemProperties;
import io.sarl.docs.sarldoc.Constants;
import io.sarl.docs.sarldoc.commands.SarldocCommand;
import io.sarl.docs.sarldoc.configs.SarldocConfig;
import io.sarl.lang.SARLConfig;
import io.sarl.lang.core.util.CliUtilities;
import io.sarl.lang.sarlc.configs.ProgressBarConfig;
import io.sarl.lang.sarlc.tools.DefaultPathDetector;
import io.sarl.lang.sarlc.tools.PathDetector;
import io.sarl.lang.sarlc.tools.SARLClasspathProvider;
import io.sarl.lang.sarlc.tools.SarlEmbededSdkClasspathProvider;

/** Module for configuring the sarldoc application information.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class SarldocApplicationModule implements BQModule {

	@Override
	public void configure(Binder binder) {
		binder.bind(PathDetector.class).to(DefaultPathDetector.class).inSingletonScope();
		binder.bind(SARLClasspathProvider.class).to(SarlEmbededSdkClasspathProvider.class).inSingletonScope();

		// Name of the application.
		binder.bind(Key.get(String.class, DefaultApplicationName.class)).toInstance(
				SystemProperties.getValue(SarldocConfig.PREFIX + ".programName", Constants.PROGRAM_NAME)); //$NON-NLS-1$
		// Short description of the application.
		extend(binder).setApplicationDescription(Messages.SarldocApplicationModule_0);
		// Long description of the application.
		binder.bind(Key.get(String.class, ApplicationDetailedDescription.class)).toProvider(LongDescriptionProvider.class).inSingletonScope();
		// Synopsis of the application's arguments.
		binder.bind(Key.get(String.class, ApplicationArgumentSynopsis.class)).toInstance(Messages.SarldocApplicationModule_1);
		// Default command
		extend(binder).setDefaultCommand(SarldocCommand.class);
	}

	/** Provider of the long description of the application.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class LongDescriptionProvider implements Provider<String> {

		@Override
		public String get() {
			final String sarlOutputDirectory = SARLConfig.FOLDER_SOURCE_GENERATED;
			final String sarlOutputDirectoryOption = CliUtilities.getCommandLineOption(io.sarl.lang.sarlc.Constants.SARL_OUTPUT_DIRECTORY_OPTION);
			final String javaOutputDirectory = SARLConfig.FOLDER_BIN;
			final String javaOutputDirectoryOption = CliUtilities.getCommandLineOption(io.sarl.lang.sarlc.Constants.JAVA_OUTPUT_DIRECTORY_OPTION);
			final String docOutputDirectory = SarldocConfig.DOC_OUTPUT_DIRECTORY_VALUE;
			final String docOutputDirectoryOption = CliUtilities.getCommandLineOption(Constants.DOCUMENTATION_OUTPUT_DIRECTORY_OPTION);
			return MessageFormat.format(Messages.SarldocApplicationModule_2,
					sarlOutputDirectory, sarlOutputDirectoryOption,
					javaOutputDirectory, javaOutputDirectoryOption,
					docOutputDirectory, docOutputDirectoryOption);
		}

	}

	@Singleton
	@Provides
	public Logger provideRootLogger(ConfigurationFactory configFactory, Provider<ProgressBarConfig> config) {
		final Logger root = Logger.getAnonymousLogger();
		if (root != null) {
			ProgressBarConfig cfg = config.get();
			if (cfg.getEnable()) {
				root.setLevel(cfg.getLevel().toJul());
			}
		}
		return root;
	}

}