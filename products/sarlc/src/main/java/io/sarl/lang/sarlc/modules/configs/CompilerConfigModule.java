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

import javax.inject.Provider;
import javax.inject.Singleton;

import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Injector;
import io.bootique.di.Provides;
import io.bootique.meta.application.OptionMetadata;
import org.arakhne.afc.bootique.variables.VariableDecls;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.compiler.GeneratorConfig2;
import io.sarl.lang.compiler.batch.IJavaBatchCompiler;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.configs.subconfigs.JavaCompiler;

/** Module for creating and configuring the sarlc compiler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class CompilerConfigModule implements BQModule {

	private static final String ENCODING_OPTION = "encoding"; //$NON-NLS-1$

	private static final String JAVASOURCE_OPTION = "java-source"; //$NON-NLS-1$

	private static final String JAVACOMPILER_OPTION = "java-compiler"; //$NON-NLS-1$

	private static final String WRITETRACES_OPTION = "write-traces"; //$NON-NLS-1$

	private static final String WRITESTORAGES_OPTION = "write-storages"; //$NON-NLS-1$

	private static final String GENERATEINLINES_OPTION = "generate-inlines"; //$NON-NLS-1$

	private static final String GENERATEPURES_OPTION = 	"generate-pures"; //$NON-NLS-1$

	private static final String GENERATEEQUALITYTESTS_OPTION = "generate-equality-tests"; //$NON-NLS-1$

	private static final String GENERATETOSTRING_OPTION = "generate-tostring"; //$NON-NLS-1$

	private static final String GENERATECLONES_OPTION = "generate-clones"; //$NON-NLS-1$

	private static final String GENERATESERIALS_OPTION = "generate-serials"; //$NON-NLS-1$

	@Override
	public void configure(Binder binder) {
		VariableDecls.extend(binder).declareVar(FILE_ENCODING_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				ENCODING_OPTION,
				MessageFormat.format(Messages.CompilerConfigModule_0, ENCODING_OPTION))
				.valueOptionalWithDefault(Messages.CompilerConfigModule_1, Charset.defaultCharset().displayName())
				.build())
			.mapConfigPath(ENCODING_OPTION, FILE_ENCODING_NAME);

		VariableDecls.extend(binder).declareVar(JAVA_VERSION_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				JAVASOURCE_OPTION,
				MessageFormat.format(Messages.CompilerConfigModule_2,
						SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH,
						SARLVersion.INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT))
				.valueOptionalWithDefault(Messages.CompilerConfigModule_3, SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH)
				.build())
			.mapConfigPath(JAVASOURCE_OPTION, JAVA_VERSION_NAME);

		VariableDecls.extend(binder).declareVar(JAVA_COMPILER_NAME);
		String jcompilerValues = null;
		for (final JavaCompiler jc : JavaCompiler.values()) {
			if (jcompilerValues == null) {
				jcompilerValues = jc.toJsonString();
			} else {
				jcompilerValues = MessageFormat.format(Messages.CompilerConfigModule_5,
						jcompilerValues, jc.toJsonString());
			}
		}
		extend(binder).addOption(OptionMetadata.builder(
				JAVACOMPILER_OPTION,
				MessageFormat.format(Messages.CompilerConfigModule_4, JavaCompiler.getDefault().toJsonString()))
				.valueOptionalWithDefault(jcompilerValues, JavaCompiler.getDefault().toJsonString())
				.build())
			.mapConfigPath(JAVACOMPILER_OPTION, JAVA_COMPILER_NAME);

		final String trueFalseValues = MessageFormat.format(Messages.CompilerConfigModule_5,
				Boolean.TRUE.toString(), Boolean.FALSE.toString());
		VariableDecls.extend(binder).declareVar(OUTPUT_TRACES_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				WRITETRACES_OPTION,
				MessageFormat.format(Messages.CompilerConfigModule_6, Boolean.TRUE))
				.valueOptionalWithDefault(trueFalseValues, Boolean.TRUE.toString())
				.build())
			.mapConfigPath(WRITETRACES_OPTION, OUTPUT_TRACES_NAME);

		VariableDecls.extend(binder).declareVar(OUTPUT_STORAGES_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				WRITESTORAGES_OPTION,
				MessageFormat.format(Messages.CompilerConfigModule_7, Boolean.TRUE))
				.valueOptionalWithDefault(trueFalseValues, Boolean.TRUE.toString())
				.build())
			.mapConfigPath(WRITESTORAGES_OPTION, OUTPUT_STORAGES_NAME);

		VariableDecls.extend(binder).declareVar(GENERATE_INLINES_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				GENERATEINLINES_OPTION,
				MessageFormat.format(Messages.CompilerConfigModule_8,
						GeneratorConfig2.DEFAULT_GENERATE_INLINE_ANNOTATION))
				.valueOptionalWithDefault(trueFalseValues, Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_INLINE_ANNOTATION))
				.build())
			.mapConfigPath(GENERATEINLINES_OPTION, GENERATE_INLINES_NAME);

		VariableDecls.extend(binder).declareVar(COMPRESS_INLINE_EXPRESSIONS_NAME);

		VariableDecls.extend(binder).declareVar(GENERATE_PURES_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				GENERATEPURES_OPTION,
				MessageFormat.format(Messages.CompilerConfigModule_9, GeneratorConfig2.DEFAULT_GENERATE_PURE_ANNOTATION))
				.valueOptionalWithDefault(trueFalseValues, Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_PURE_ANNOTATION))
				.build())
			.mapConfigPath(GENERATEPURES_OPTION, GENERATE_PURES_NAME);

		VariableDecls.extend(binder).declareVar(GENERATE_EQUALITY_TESTS_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				GENERATEEQUALITYTESTS_OPTION,
				MessageFormat.format(Messages.CompilerConfigModule_10, GeneratorConfig2.DEFAULT_GENERATE_EQUALITY_TEST_FUNCTIONS))
				.valueOptionalWithDefault(trueFalseValues, Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_EQUALITY_TEST_FUNCTIONS))
				.build())
			.mapConfigPath(GENERATEEQUALITYTESTS_OPTION, GENERATE_EQUALITY_TESTS_NAME);

		VariableDecls.extend(binder).declareVar(GENERATE_TOSTRING_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				GENERATETOSTRING_OPTION,
				MessageFormat.format(Messages.CompilerConfigModule_11, GeneratorConfig2.DEFAULT_GENERATE_TOSTRING_FUNCTION))
				.valueOptionalWithDefault(trueFalseValues, Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_TOSTRING_FUNCTION))
				.build())
			.mapConfigPath(GENERATETOSTRING_OPTION, GENERATE_TOSTRING_NAME);

		VariableDecls.extend(binder).declareVar(GENERATE_CLONE_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				GENERATECLONES_OPTION,
				MessageFormat.format(Messages.CompilerConfigModule_12, GeneratorConfig2.DEFAULT_GENERATE_CLONE_FUNCTION))
				.valueOptionalWithDefault(trueFalseValues, Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_CLONE_FUNCTION))
				.build())
			.mapConfigPath(GENERATECLONES_OPTION, GENERATE_CLONE_NAME);

		VariableDecls.extend(binder).declareVar(GENERATE_SERIAL_IDS_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				GENERATESERIALS_OPTION,
				MessageFormat.format(Messages.CompilerConfigModule_13, GeneratorConfig2.DEFAULT_GENERATE_SERIAL_NUMBER_FIELD))
				.valueOptionalWithDefault(trueFalseValues, Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_SERIAL_NUMBER_FIELD))
				.build())
			.mapConfigPath(GENERATESERIALS_OPTION, GENERATE_SERIAL_IDS_NAME);
	}

	/** Provide a Java batch compiler based on the Bootique configuration.
	 *
	 * @param injector the injector.
	 * @param config the bootique configuration.
	 * @return the batch compiler.
	 * @since 0.8
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public IJavaBatchCompiler providesJavaBatchCompiler(Injector injector, Provider<SarlcConfig> config) {
		final SarlcConfig cfg = config.get();
		final IJavaBatchCompiler compiler = cfg.getCompiler().getJavaCompiler().newCompilerInstance();
		injector.injectMembers(compiler);
		return compiler;
	}

}
