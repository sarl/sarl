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

package io.sarl.lang.sarlc.modules.configs;

import static io.bootique.BQCoreModule.extend;
import static io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig.COMPRESS_INLINE_EXPRESSIONS_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig.FILE_ENCODING_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig.GENERATE_CLONE_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig.GENERATE_EQUALITY_TESTS_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig.GENERATE_INLINES_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig.GENERATE_PURES_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig.GENERATE_SERIAL_IDS_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig.GENERATE_TOSTRING_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig.JAVA_COMPILER_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig.JAVA_VERSION_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig.OUTPUT_STORAGES_NAME;
import static io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig.OUTPUT_TRACES_NAME;

import java.nio.charset.Charset;
import java.text.MessageFormat;

import com.google.inject.AbstractModule;
import io.bootique.meta.application.OptionMetadata;
import org.arakhne.afc.bootique.variables.VariableDecls;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.compiler.GeneratorConfig2;

/** Module for creating and configuring the sarlc compiler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class CompilerConfigModule extends AbstractModule {

	@Override
	protected void configure() {
		VariableDecls.extend(binder()).declareVar(FILE_ENCODING_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"encoding", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerConfigModule_0, Charset.defaultCharset().displayName()))
				.configPath(FILE_ENCODING_NAME)
				.valueOptional(Messages.CompilerConfigModule_1)
				.defaultValue(Charset.defaultCharset().displayName())
				.build());

		VariableDecls.extend(binder()).declareVar(JAVA_VERSION_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"javaversion", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerConfigModule_2, SARLVersion.MINIMAL_JDK_VERSION))
				.configPath(JAVA_VERSION_NAME)
				.valueOptional(Messages.CompilerConfigModule_3)
				.defaultValue(SARLVersion.MINIMAL_JDK_VERSION)
				.build());

		VariableDecls.extend(binder()).declareVar(JAVA_COMPILER_NAME);
		final String trueFalseValues = MessageFormat.format(Messages.CompilerConfigModule_5,
				Boolean.TRUE, Boolean.FALSE);
		extend(binder()).addOption(OptionMetadata.builder(
				"javacompiler", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerConfigModule_4, Boolean.TRUE))
				.configPath(JAVA_COMPILER_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.TRUE.toString())
				.build());

		VariableDecls.extend(binder()).declareVar(OUTPUT_TRACES_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"writetraces", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerConfigModule_6, Boolean.TRUE))
				.configPath(OUTPUT_TRACES_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.TRUE.toString())
				.build());

		VariableDecls.extend(binder()).declareVar(OUTPUT_STORAGES_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"writestorages", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerConfigModule_7, Boolean.TRUE))
				.configPath(OUTPUT_STORAGES_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.TRUE.toString())
				.build());

		VariableDecls.extend(binder()).declareVar(GENERATE_INLINES_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"generateinlines", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerConfigModule_8,
						GeneratorConfig2.DEFAULT_GENERATE_INLINE_ANNOTATION))
				.configPath(GENERATE_INLINES_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_INLINE_ANNOTATION))
				.build());

		VariableDecls.extend(binder()).declareVar(COMPRESS_INLINE_EXPRESSIONS_NAME);

		VariableDecls.extend(binder()).declareVar(GENERATE_PURES_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"generatepures", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerConfigModule_9, GeneratorConfig2.DEFAULT_GENERATE_PURE_ANNOTATION))
				.configPath(GENERATE_PURES_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_PURE_ANNOTATION))
				.build());

		VariableDecls.extend(binder()).declareVar(GENERATE_EQUALITY_TESTS_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"generateequalitytests", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerConfigModule_10, GeneratorConfig2.DEFAULT_GENERATE_EQUALITY_TEST_FUNCTIONS))
				.configPath(GENERATE_EQUALITY_TESTS_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_EQUALITY_TEST_FUNCTIONS))
				.build());

		VariableDecls.extend(binder()).declareVar(GENERATE_TOSTRING_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"generatetostring", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerConfigModule_11, GeneratorConfig2.DEFAULT_GENERATE_TOSTRING_FUNCTION))
				.configPath(GENERATE_TOSTRING_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_TOSTRING_FUNCTION))
				.build());

		VariableDecls.extend(binder()).declareVar(GENERATE_CLONE_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"generateclones", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerConfigModule_12, GeneratorConfig2.DEFAULT_GENERATE_CLONE_FUNCTION))
				.configPath(GENERATE_CLONE_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_CLONE_FUNCTION))
				.build());

		VariableDecls.extend(binder()).declareVar(GENERATE_SERIAL_IDS_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"generateserials", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerConfigModule_13, GeneratorConfig2.DEFAULT_GENERATE_SERIAL_NUMBER_FIELD))
				.configPath(GENERATE_SERIAL_IDS_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_SERIAL_NUMBER_FIELD))
				.build());
	}

}
