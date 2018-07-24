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

package io.sarl.lang.sarlc.modules.general;

import java.util.Map.Entry;

import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.sarlc.configs.SarlConfig;
import io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig;
import io.sarl.lang.sarlc.configs.subconfigs.ValidatorConfig;

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

	/** Replies the SARL batch compiler.
	 *
	 * @param injector the current injector.
	 * @param sarlcConfig the configuration for the paths.
	 * @return the SARL batch compiler
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public SarlBatchCompiler provideSarlBatchCompiler(
			Injector injector, SarlConfig sarlcConfig) {
		final CompilerConfig compilerConfig = sarlcConfig.getCompiler();
		final ValidatorConfig validatorConfig = sarlcConfig.getValidator();

		final SarlBatchCompiler compiler = new SarlBatchCompiler();
		injector.injectMembers(compiler);

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
