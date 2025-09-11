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

package io.sarl.lang.sarlc.modules.general;

import static io.sarl.apputils.bootiqueapp.batchcompiler.lang.SARLRuntimeModule.SARL_INJECTOR_NAME;

import java.util.logging.Logger;

import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.Strings;

import com.google.inject.Injector;

import io.bootique.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Provides;
import io.sarl.lang.compiler.batch.IJavaBatchCompiler;
import io.sarl.lang.compiler.batch.IssueMessageFormatter;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.compiler.batch.SarlBatchCompilerUtils;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.configs.subconfigs.JavaCompiler;
import io.sarl.lang.sarlc.tools.ClassPathUtils;
import io.sarl.lang.sarlc.tools.SARLClasspathProvider;
import jakarta.inject.Named;
import jakarta.inject.Provider;
import jakarta.inject.Singleton;

/** Module for creating the SARL batch compiler with the configuration provided by bootique modules.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class SarlBatchCompilerModule implements BQModule {

	@Override
	public void configure(Binder binder) {
		//
	}

	/** Replies the formatter of the issue messages.
	 *
	 * @return the formatter
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public IssueMessageFormatter provideIssueMessageFormatter() {
		return (severity, issue, uriToProblem) -> {
			// Use the default formatter
			return null;
		};
	}

	/** Provide a Java batch compiler based on the Bootique configuration.
	 *
	 * @param config the bootique configuration.
	 * @param guiceInjector the injector used for the SARL compiler.
	 * @return the batch compiler.
	 * @since 0.8
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public IJavaBatchCompiler providesJavaBatchCompiler(
			Provider<SarlcConfig> config,
			@Named(SARL_INJECTOR_NAME) Injector guiceInjector) {
		final var cfg = config.get();
		final var compiler = cfg.getCompiler().getJavaCompiler().newCompilerInstance();
		guiceInjector.injectMembers(compiler);
		return compiler;
	}

	/** Replies the SARL batch compiler.
	 *
	 * @param config the configuration for the paths.
	 * @param defaultClasspath the SARL boot class path that must be used by default.
	 * @param guiceInjector the current injector for the SARL compiler.
	 * @param logger the logger.
	 * @param formatterProvider the provider of message formatter.
	 * @param javaCompilerProvider the provider of the Java compiler.
	 * @return the SARL batch compiler
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public SarlBatchCompiler provideSarlBatchCompiler(
			Provider<SarlcConfig> config,
			Provider<SARLClasspathProvider> defaultClasspath,
			@Named(SARL_INJECTOR_NAME) Injector guiceInjector, 
			Provider<Logger> logger,
			Provider<IssueMessageFormatter> formatterProvider,
			Provider<IJavaBatchCompiler> javaCompilerProvider) {
		final var cfg = config.get();
		final var compilerConfig = cfg.getCompiler();
		final var validatorConfig = cfg.getValidator();

		final var compiler = new SarlBatchCompiler();
		guiceInjector.injectMembers(compiler);

		if (!Strings.isEmpty(compilerConfig.getFileEncoding())) {
			compiler.setFileEncoding(compilerConfig.getFileEncoding());
		}

		final var jversion = SarlBatchCompilerUtils.parseJavaVersion(compilerConfig.getJavaVersion());
		compiler.setJavaSourceVersion(jversion.getQualifier());

		final var classpathProvider = defaultClasspath.get();
		final var fullClassPath = ClassPathUtils.buildClassPath(classpathProvider, cfg, jversion, logger.get());
		compiler.setClassPath(fullClassPath.toFileList());
		final var fullModulePath = ClassPathUtils.buildModulePath(classpathProvider, cfg, jversion, logger.get());
		compiler.setModulePath(fullModulePath.toFileList());

		compiler.setOptimizationLevel(cfg.getCompiler().getOptimizationLevelObject());
		compiler.setWriteTraceFiles(compilerConfig.getOutputTraceFiles());
		compiler.setWriteStorageFiles(compilerConfig.getOutputTraceFiles());

		compiler.setGenerateInlineAnnotation(compilerConfig.getGenerateInlines());
		compiler.setUseExpressionInterpreterForInlineAnnotation(compilerConfig.getCompressInlineExpressions());
		compiler.setGeneratePureAnnotation(compilerConfig.getGeneratePures());
		compiler.setGenerateEqualityTestFunctions(compilerConfig.getGenerateEqualityTests());
		compiler.setGenerateToStringFunctions(compilerConfig.getGenerateToString());
		compiler.setGenerateCloneFunctions(compilerConfig.getGenerateClone());
		compiler.setGenerateSerialNumberFields(compilerConfig.getGenerateSerialIds());

		if (validatorConfig.getAllErrors()) {
			compiler.setAllWarningSeverities(Severity.ERROR);
		} else if (validatorConfig.getIgnoreWarnings()) {
			compiler.setAllWarningSeverities(Severity.IGNORE);
		}

		for (final var entry : validatorConfig.getWarningLevels().entrySet()) {
			compiler.setWarningSeverity(entry.getKey(), entry.getValue());
		}

		final var issueMessageFormatter = formatterProvider.get();
		compiler.setIssueMessageFormatter(issueMessageFormatter);

		final var jcompiler = compilerConfig.getJavaCompiler();
		compiler.setJavaPostCompilationEnable(jcompiler != JavaCompiler.NONE);
		compiler.setJavaCompiler(javaCompilerProvider.get());

		return compiler;
	}

}
