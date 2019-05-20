/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Provider;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.Strings;
import org.slf4j.Logger;

import io.sarl.lang.compiler.batch.IJavaBatchCompiler;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.compiler.batch.SarlBatchCompiler.IssueMessageFormatter;
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
public class SarlBatchCompilerModule extends AbstractModule {

	@Override
	protected void configure() {
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

		final SARLClasspathProvider classpathProvider = defaultClasspath.get();
		final SystemPath fullClassPath;
		fullClassPath = ClassPathUtils.buildClassPath(classpathProvider, cfg, logger.get());
		compiler.setClassPath(fullClassPath.toFileList());

		if (!Strings.isEmpty(cfg.getJavaBootClasspath())) {
			compiler.setBootClassPath(cfg.getJavaBootClasspath());
		}

		if (!Strings.isEmpty(compilerConfig.getFileEncoding())) {
			compiler.setFileEncoding(compilerConfig.getFileEncoding());
		}

		if (!Strings.isEmpty(compilerConfig.getJavaVersion())) {
			compiler.setJavaSourceVersion(compilerConfig.getJavaVersion());
		}

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
