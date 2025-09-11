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
package io.sarl.lang.tests.modules.compiler.batch;

import java.io.File;
import java.util.List;

import com.google.inject.Inject;
import com.google.inject.Provider;
import org.eclipse.xtext.validation.Issue;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;

import io.sarl.lang.compiler.batch.EcjBatchCompiler;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;


/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version batchcompiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid batchcompiler
 */
@SuppressWarnings("all")
@DisplayName("SarlBatchCompiler with ecj")
@Tag("core")
@Tag("compiler-run")
public class EcjSarlBatchCompilerTest extends AbstractBatchCompilerTest {

	@Inject
	private Provider<SarlBatchCompiler> compilerProvider;
	
	@Override
	public boolean runBatchCompiler(File basePath, File sourcePath, File sarlcOutputFolder, File javacOutputFolder, File tempFolder, List<Issue> issues) {
		var compiler = this.compilerProvider.get();
		compiler.setBasePath(basePath.getAbsolutePath());
		compiler.setSourcePath(sourcePath.getAbsolutePath());
		compiler.setOutputPath(sarlcOutputFolder);
		compiler.setClassOutputPath(javacOutputFolder);
		compiler.setTempDirectory(tempFolder);
		compiler.setJavaCompilerVerbose(false);
		compiler.setGenerateInlineAnnotation(false);
		compiler.setReportInternalProblemsAsIssues(true);
		compiler.addIssueMessageListener((severity, issue, uri, message) -> {
			issues.add(issue);
		});
		//
		compiler.setJavaCompiler(new EcjBatchCompiler());
		//
		return compiler.compile();
	}

}
