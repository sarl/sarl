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

package io.sarl.docs.sarldoc.modules.configs;

import static io.bootique.BQCoreModule.extend;
import static io.sarl.docs.sarldoc.configs.SarldocConfig.DOC_OUTPUT_DIRECTORY_FILE;
import static io.sarl.docs.sarldoc.configs.SarldocConfig.DOC_OUTPUT_DIRECTORY_NAME;
import static io.sarl.docs.sarldoc.configs.SarldocConfig.ENABLE_AUTHOR_TAG_NAME;
import static io.sarl.docs.sarldoc.configs.SarldocConfig.ENABLE_DEPRECATED_TAG_NAME;
import static io.sarl.docs.sarldoc.configs.SarldocConfig.ENABLE_SINCE_TAG_NAME;
import static io.sarl.docs.sarldoc.configs.SarldocConfig.ENABLE_VERSION_TAG_NAME;
import static io.sarl.docs.sarldoc.configs.SarldocConfig.JAVADOC_EXECUTABLE_NAME;
import static io.sarl.docs.sarldoc.configs.SarldocConfig.LOCALE_DEFAULT;
import static io.sarl.docs.sarldoc.configs.SarldocConfig.LOCALE_NAME;
import static io.sarl.docs.sarldoc.configs.SarldocConfig.TAGS_NAME;
import static io.sarl.docs.sarldoc.configs.SarldocConfig.TITLE_NAME;
import static io.sarl.docs.sarldoc.configs.SarldocConfig.VISIBILITY_NAME;

import java.text.MessageFormat;

import org.arakhne.afc.bootique.variables.VariableDecls;

import io.bootique.BQModule;
import io.bootique.config.ConfigurationFactory;
import io.bootique.di.Binder;
import io.bootique.di.Provides;
import io.bootique.meta.application.OptionMetadata;
import io.sarl.docs.sarldoc.Constants;
import io.sarl.docs.sarldoc.configs.Placement;
import io.sarl.docs.sarldoc.configs.SarldocConfig;
import io.sarl.docs.sarldoc.configs.Visibility;
import jakarta.inject.Singleton;

/**
 * Module for creating and configuring the configuration that is specific to sarldoc.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class SarldocConfigModule implements BQModule {

	private static final String JAVADOC_OPTION = "javadoc";  //$NON-NLS-1$

	private static final String DOCTITLE_OPTION = "doctitle";  //$NON-NLS-1$

	private static final String TAGS_OPTION = "tags";  //$NON-NLS-1$

	private static final String LOCALE_OPTION = "locale";  //$NON-NLS-1$

	private static final String VERSIONTAG_OPTION = "versiontag";  //$NON-NLS-1$

	private static final String AUTHORTAG_OPTION = "authortag";  //$NON-NLS-1$

	private static final String DEPRECATEDTAG_OPTION = "deprecatedtag";  //$NON-NLS-1$

	private static final String SINCETAG_OPTION = "sincetag";  //$NON-NLS-1$

	private static final String VISIBILITY_OPTION = "visibility";  //$NON-NLS-1$

	@Override
	public void configure(Binder binder) {
		VariableDecls.extend(binder).declareVar(JAVADOC_EXECUTABLE_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				JAVADOC_OPTION,
				Messages.SarldocConfigModule_0)
				.valueRequired(Messages.SarldocConfigModule_3)
				.build())
			.mapConfigPath(JAVADOC_OPTION, JAVADOC_EXECUTABLE_NAME);

		VariableDecls.extend(binder).declareVar(DOC_OUTPUT_DIRECTORY_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				Constants.DOCUMENTATION_OUTPUT_DIRECTORY_OPTION,
				MessageFormat.format(Messages.SarldocConfigModule_1, DOC_OUTPUT_DIRECTORY_FILE))
				.valueRequired(Messages.SarldocConfigModule_4)
				.build())
			.mapConfigPath(Constants.DOCUMENTATION_OUTPUT_DIRECTORY_OPTION, DOC_OUTPUT_DIRECTORY_NAME);

		VariableDecls.extend(binder).declareVar(TITLE_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				DOCTITLE_OPTION,
				Messages.SarldocConfigModule_2)
				.valueRequired(Messages.SarldocConfigModule_5)
				.build())
			.mapConfigPath(DOCTITLE_OPTION, TITLE_NAME);

		final var customTagHelp = new StringBuilder();
		for (final var placement : Placement.values()) {
			if (customTagHelp.length() > 0) {
				customTagHelp.append(Messages.SarldocConfigModule_6);
			}
			customTagHelp.append(MessageFormat.format(Messages.SarldocConfigModule_7,
					Character.valueOf(placement.toChar()), placement.toJsonString(),
					placement.getDocumentation()));
		}

		extend(binder).addOption(OptionMetadata.builder(
				TAGS_OPTION,
				MessageFormat.format(Messages.SarldocConfigModule_8,
						customTagHelp.toString()))
				.valueRequired(Messages.SarldocConfigModule_9)
				.build())
			.mapConfigPath(TAGS_OPTION, TAGS_NAME);

		extend(binder).addOption(OptionMetadata.builder(
				LOCALE_OPTION,
				MessageFormat.format(Messages.SarldocConfigModule_10,
						LOCALE_DEFAULT.toString()))
				.valueRequired(Messages.SarldocConfigModule_11)
				.build())
			.mapConfigPath(LOCALE_OPTION, LOCALE_NAME);

		final var truefalseString = Boolean.TRUE.toString() + "|" + Boolean.FALSE.toString(); //$NON-NLS-1$

		extend(binder).addOption(OptionMetadata.builder(
				VERSIONTAG_OPTION,
				MessageFormat.format(Messages.SarldocConfigModule_15, "@version", Boolean.TRUE)) //$NON-NLS-1$
				.valueOptionalWithDefault(truefalseString, Boolean.TRUE.toString())
				.build())
			.mapConfigPath(VERSIONTAG_OPTION, ENABLE_VERSION_TAG_NAME);

		extend(binder).addOption(OptionMetadata.builder(
				AUTHORTAG_OPTION,
				MessageFormat.format(Messages.SarldocConfigModule_15, "@author", Boolean.TRUE)) //$NON-NLS-1$
				.valueOptionalWithDefault(truefalseString, Boolean.TRUE.toString())
				.build())
			.mapConfigPath(AUTHORTAG_OPTION, ENABLE_AUTHOR_TAG_NAME);

		extend(binder).addOption(OptionMetadata.builder(
				DEPRECATEDTAG_OPTION,
				MessageFormat.format(Messages.SarldocConfigModule_15, "@deprecated", Boolean.TRUE)) //$NON-NLS-1$
				.valueOptionalWithDefault(truefalseString, Boolean.TRUE.toString())
				.build())
			.mapConfigPath(DEPRECATEDTAG_OPTION, ENABLE_DEPRECATED_TAG_NAME);

		extend(binder).addOption(OptionMetadata.builder(
				SINCETAG_OPTION,
				MessageFormat.format(Messages.SarldocConfigModule_15, "@since", Boolean.TRUE)) //$NON-NLS-1$
				.valueOptionalWithDefault(truefalseString, Boolean.TRUE.toString())
				.build())
			.mapConfigPath(SINCETAG_OPTION, ENABLE_SINCE_TAG_NAME);

		final var visibilityDoc = new StringBuilder();
		final var visibilityValues = new StringBuilder();
		for (final var visibility : Visibility.values()) {
			if (visibilityDoc.length() > 0) {
				visibilityDoc.append(Messages.SarldocConfigModule_6);
			}
			visibilityDoc.append(visibility.toJsonString());
			if (visibilityValues.length() > 0) {
				visibilityValues.append(Messages.SarldocConfigModule_16);
			}
			visibilityValues.append(visibility.toJsonString());
		}
		extend(binder).addOption(OptionMetadata.builder(
				VISIBILITY_OPTION,
				MessageFormat.format(Messages.SarldocConfigModule_17,
						visibilityDoc.toString(), Visibility.getDefault().toJsonString()))
				.valueOptionalWithDefault(visibilityValues.toString(), Visibility.getDefault().toJsonString())
				.build())
			.mapConfigPath(VISIBILITY_OPTION, VISIBILITY_NAME);
	}

	/** Replies the instance of the sarldoc configuration.
	 *
	 * @param configFactory accessor to the bootique factory.
	 * @return the path configuration accessor.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public SarldocConfig getSarldocConfig(ConfigurationFactory configFactory) {
		// The following code is for debugging the creation of the config object.
		//		try {
		//			configFactory.config(SarldocConfig.class, "sarldoc");
		//		} catch (Throwable ex) {
		//			var swriter = new StringWriter();
		//			var writer = new PrintWriter(swriter);
		//			ex.printStackTrace(writer);
		//			logger.get().log(Level.SEVERE, "YYYYYYYYYYYYYYYYYYYYYYYYYY" + swriter.toString());
		//		}
		//		return new SarldocConfig();
		return SarldocConfig.getConfiguration(configFactory);
	}

}
