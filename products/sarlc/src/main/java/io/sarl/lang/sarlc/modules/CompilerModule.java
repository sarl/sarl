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

package io.sarl.lang.sarlc.modules;

import static io.bootique.BQCoreModule.extend;
import static io.sarl.lang.sarlc.configs.CompilerConfig.COMPRESS_INLINE_EXPRESSIONS_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.FILE_ENCODING_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.GENERATE_CLONE_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.GENERATE_EQUALITY_TESTS_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.GENERATE_INLINES_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.GENERATE_PURES_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.GENERATE_SERIAL_IDS_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.GENERATE_TOSTRING_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.JAVA_COMPILER_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.JAVA_VERSION_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.OUTPUT_STORAGES_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.OUTPUT_TRACES_NAME;

import java.nio.charset.Charset;
import java.text.MessageFormat;
import java.util.Map.Entry;

import com.google.inject.Injector;
import com.google.inject.Provider;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import io.bootique.config.ConfigurationFactory;
import io.bootique.meta.application.OptionMetadata;
import org.apache.log4j.Level;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.compiler.GeneratorConfig2;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.sarlc.commands.CompilerCommand;
import io.sarl.lang.sarlc.configs.CompilerConfig;
import io.sarl.lang.sarlc.configs.LoggingConfig;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.configs.ValidatorConfig;
import io.sarl.maven.bqextension.modules.AbstractConfigModule;
import io.sarl.maven.bqextension.modules.BQConfigTypes;
import io.sarl.maven.bqextension.modules.BQModule;

/** Module for the sarlc specific commands.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@BQModule("The SARL batch compiler.")
@BQConfigTypes(CompilerConfig.class)
public class CompilerModule extends AbstractConfigModule {

	@Override
	protected void configure() {
		associateEnvironmentVariable(FILE_ENCODING_NAME);
		associateOption(OptionMetadata.builder(
				"encoding", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerModule_0, Charset.defaultCharset().displayName()))
				.configPath(FILE_ENCODING_NAME)
				.valueOptional(Messages.CompilerModule_1)
				.defaultValue(Charset.defaultCharset().displayName()));

		associateEnvironmentVariable(JAVA_VERSION_NAME);
		associateOption(OptionMetadata.builder(
				"javaversion", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerModule_2, SARLVersion.MINIMAL_JDK_VERSION))
				.configPath(JAVA_VERSION_NAME)
				.valueOptional(Messages.CompilerModule_3)
				.defaultValue(SARLVersion.MINIMAL_JDK_VERSION));

		associateEnvironmentVariable(JAVA_COMPILER_NAME);
		final String trueFalseValues = MessageFormat.format(Messages.CompilerModule_5,
				Boolean.TRUE, Boolean.FALSE);
		associateOption(OptionMetadata.builder(
				"javacompiler", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerModule_4, Boolean.TRUE))
				.configPath(JAVA_COMPILER_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.TRUE.toString()));

		associateEnvironmentVariable(OUTPUT_TRACES_NAME);
		associateOption(OptionMetadata.builder(
				"writetraces", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerModule_6, Boolean.TRUE))
				.configPath(OUTPUT_TRACES_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.TRUE.toString()));

		associateEnvironmentVariable(OUTPUT_STORAGES_NAME);
		associateOption(OptionMetadata.builder(
				"writestorages", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerModule_7, Boolean.TRUE))
				.configPath(OUTPUT_STORAGES_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.TRUE.toString()));

		associateEnvironmentVariable(GENERATE_INLINES_NAME);
		associateOption(OptionMetadata.builder(
				"generateinlines", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerModule_8,
						GeneratorConfig2.DEFAULT_GENERATE_INLINE_ANNOTATION))
				.configPath(GENERATE_INLINES_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_INLINE_ANNOTATION)));

		associateEnvironmentVariable(COMPRESS_INLINE_EXPRESSIONS_NAME);

		associateEnvironmentVariable(GENERATE_PURES_NAME);
		associateOption(OptionMetadata.builder(
				"generatepures", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerModule_9, GeneratorConfig2.DEFAULT_GENERATE_PURE_ANNOTATION))
				.configPath(GENERATE_PURES_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_PURE_ANNOTATION)));

		associateEnvironmentVariable(GENERATE_EQUALITY_TESTS_NAME);
		associateOption(OptionMetadata.builder(
				"generateequalitytests", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerModule_10, GeneratorConfig2.DEFAULT_GENERATE_EQUALITY_TEST_FUNCTIONS))
				.configPath(GENERATE_EQUALITY_TESTS_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_EQUALITY_TEST_FUNCTIONS)));

		associateEnvironmentVariable(GENERATE_TOSTRING_NAME);
		associateOption(OptionMetadata.builder(
				"generatetostring", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerModule_11, GeneratorConfig2.DEFAULT_GENERATE_TOSTRING_FUNCTION))
				.configPath(GENERATE_TOSTRING_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_TOSTRING_FUNCTION)));

		associateEnvironmentVariable(GENERATE_CLONE_NAME);
		associateOption(OptionMetadata.builder(
				"generateclones", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerModule_12, GeneratorConfig2.DEFAULT_GENERATE_CLONE_FUNCTION))
				.configPath(GENERATE_CLONE_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_CLONE_FUNCTION)));

		associateEnvironmentVariable(GENERATE_SERIAL_IDS_NAME);
		associateOption(OptionMetadata.builder(
				"generateserials", //$NON-NLS-1$
				MessageFormat.format(Messages.CompilerModule_13, GeneratorConfig2.DEFAULT_GENERATE_SERIAL_NUMBER_FIELD))
				.configPath(GENERATE_SERIAL_IDS_NAME)
				.valueOptional(trueFalseValues)
				.defaultValue(Boolean.toString(GeneratorConfig2.DEFAULT_GENERATE_SERIAL_NUMBER_FIELD)));

		extend(binder()).setDefaultCommand(CompilerCommand.class);
	}

	/** Replies the instance of the compiler configuration.
	 *
	 * @param configFactory accessor to the bootique factory.
	 * @param injector the current injector.
	 * @return the compiler configuration accessor.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public CompilerConfig provideCompilerConfig(ConfigurationFactory configFactory, Injector injector) {
		final CompilerConfig config = CompilerConfig.getConfiguration(configFactory);
		injector.injectMembers(config);
		return config;
	}

	/** Provide the command for running the compiler.
	 *
	 * @param compiler the compiler.
	 * @param configuration the SARLC configuration.
	 * @return the command.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public CompilerCommand provideSarlcCompilerCommand(Provider<SarlBatchCompiler> compiler,
			Provider<SarlcConfig> configuration) {
		return new CompilerCommand(compiler, configuration);
	}

	/** Replies the SARL batch compiler.
	 *
	 * @param injector the current injector.
	 * @param sarlcConfig the configuration for the paths.
	 * @param compilerConfig the configuration for the compiler.
	 * @param validatorConfig the configuration for the validator.
	 * @param loggingConfig the configuration for the loggers.
	 * @return the SARL batch compiler
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public SarlBatchCompiler provideSarlBatchCompiler(Injector injector, SarlcConfig sarlcConfig,
			CompilerConfig compilerConfig,
			LoggingConfig loggingConfig,
			ValidatorConfig validatorConfig) {
		final SarlBatchCompiler compiler = new SarlBatchCompiler();
		injector.injectMembers(compiler);

		//final Logger logger = injector.getInstance(Logger.class);
		//compiler.setLogger(logger);

		if (!Strings.isEmpty(sarlcConfig.getClasspath())) {
			compiler.setClassPath(sarlcConfig.getClasspath());
		}

		if (!Strings.isEmpty(sarlcConfig.getBootClasspath())) {
			compiler.setBootClassPath(sarlcConfig.getBootClasspath());
		}

		if (!Strings.isEmpty(compilerConfig.getFileEncoding())) {
			compiler.setFileEncoding(compilerConfig.getFileEncoding());
		}

		if (!Strings.isEmpty(compilerConfig.getJavaVersion())) {
			compiler.setJavaSourceVersion(compilerConfig.getJavaVersion());
		}

		compiler.setJavaPostCompilationEnable(compilerConfig.getJavaCompiler());
		compiler.setWriteTraceFiles(compilerConfig.getOutputTraceFiles());
		compiler.setWriteStorageFiles(compilerConfig.getOutputTraceFiles());

		compiler.setGenerateInlineAnnotation(compilerConfig.getGenerateInlines());
		compiler.setUseExpressionInterpreterForInlineAnnotation(compilerConfig.getCompressInlineExpressions());
		compiler.setGeneratePureAnnotation(compilerConfig.getGeneratePures());
		compiler.setGenerateEqualityTestFunctions(compilerConfig.getGenerateEqualityTests());
		compiler.setGenerateToStringFunctions(compilerConfig.getGenerateToString());
		compiler.setGenerateCloneFunctions(compilerConfig.getGenerateClone());
		compiler.setGenerateSerialNumberFields(compilerConfig.getGenerateSerialIds());

		final Level level = loggingConfig.getLog4JLevelObject();
		if (Level.DEBUG.isGreaterOrEqual(level)) {
			compiler.setJavaCompilerVerbose(true);
		}
		compiler.getLogger().setLevel(level);

		if (validatorConfig.getAllErrors()) {
			compiler.setAllWarningSeverities(Severity.ERROR);
		} else if (validatorConfig.getIgnoreWarnings()) {
			compiler.setAllWarningSeverities(Severity.IGNORE);
		}

		for (final Entry<String, Severity> entry : validatorConfig.getWarningLevels().entrySet()) {
			compiler.setWarningSeverity(entry.getKey(), entry.getValue());
		}

		return compiler;
	}

}
