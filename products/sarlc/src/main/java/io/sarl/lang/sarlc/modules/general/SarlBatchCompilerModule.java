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

import java.util.Map.Entry;
import java.util.logging.Logger;

import javax.inject.Provider;
import javax.inject.Singleton;

import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Injector;
import io.bootique.di.Provides;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.compiler.batch.IJavaBatchCompiler;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.compiler.batch.SarlBatchCompiler.IssueMessageFormatter;
import io.sarl.lang.compiler.batch.SarlBatchCompilerUtils;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig;
import io.sarl.lang.sarlc.configs.subconfigs.JavaCompiler;
import io.sarl.lang.sarlc.configs.subconfigs.ValidatorConfig;
import io.sarl.lang.sarlc.tools.ClassPathUtils;
import io.sarl.lang.sarlc.tools.SARLClasspathProvider;
import io.sarl.maven.bootiqueapp.utils.SystemPath;

/** Module for creating the SARL batch compiler with the configuration provided by bootique modules.
 *
 * @author $Author: sgalland$
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
		return (issue, uriToProblem) -> {
			// Use the default formatter.
			return null;
		};
	}

	/** Replies the SARL batch compiler.
	 *
	 * @param injector the current injector.
	 * @param config the configuration for the paths.
	 * @param defaultClasspath the SARL boot class path that must be used by default.
	 * @param issueMessageFormater the formatter of the issue messages.
	 * @param javaCompilerProvider a provider of Java batch compiler.
	 * @param logger the logger.
	 * @return the SARL batch compiler
	 */
	@SuppressWarnings({"static-method", "checkstyle:npathcomplexity"})
	@Provides
	@Singleton
	public SarlBatchCompiler provideSarlBatchCompiler(
			Injector injector, Provider<SarlcConfig> config, Provider<SARLClasspathProvider> defaultClasspath,
			Provider<IssueMessageFormatter> issueMessageFormater,
			Provider<IJavaBatchCompiler> javaCompilerProvider,
			Provider<Logger> logger) {
		final SarlcConfig cfg = config.get();
		final CompilerConfig compilerConfig = cfg.getCompiler();
		final ValidatorConfig validatorConfig = cfg.getValidator();

		final SarlBatchCompiler compiler = new SarlBatchCompiler();
		injector.injectMembers(compiler);

		if (!Strings.isEmpty(compilerConfig.getFileEncoding())) {
			compiler.setFileEncoding(compilerConfig.getFileEncoding());
		}

		final JavaVersion jversion = SarlBatchCompilerUtils.parserJavaVersion(compilerConfig.getJavaVersion());
		compiler.setJavaSourceVersion(jversion.getQualifier());

		final SARLClasspathProvider classpathProvider = defaultClasspath.get();
		final SystemPath fullClassPath = ClassPathUtils.buildClassPath(classpathProvider, cfg, jversion, logger.get());
		compiler.setClassPath(fullClassPath.toFileList());
		final SystemPath fullModulePath = ClassPathUtils.buildModulePath(classpathProvider, cfg, jversion, logger.get());
		compiler.setModulePath(fullModulePath.toFileList());

		final JavaCompiler jcompiler = compilerConfig.getJavaCompiler();
		compiler.setJavaPostCompilationEnable(jcompiler != JavaCompiler.NONE);
		compiler.setJavaCompiler(javaCompilerProvider.get());
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

		for (final Entry<String, Severity> entry : validatorConfig.getWarningLevels().entrySet()) {
			compiler.setWarningSeverity(entry.getKey(), entry.getValue());
		}

		compiler.setIssueMessageFormatter(issueMessageFormater.get());

		return compiler;
	}

}
