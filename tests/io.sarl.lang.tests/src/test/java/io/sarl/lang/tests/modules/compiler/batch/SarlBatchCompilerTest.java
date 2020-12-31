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
package io.sarl.lang.tests.modules.compiler.batch;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;
import javax.inject.Provider;

import org.eclipse.xtext.validation.Issue;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;

import io.sarl.lang.compiler.batch.SarlBatchCompiler;


/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("SarlBatchCompiler")
@Tag("core")
@Tag("compiler-run")
public class SarlBatchCompilerTest extends AbstractBatchCompilerTest {

	@Inject
	private Provider<SarlBatchCompiler> compilerProvider;
	
	@Override
	public void runBatchCompiler(File basePath, File sourcePath, File sarlcOutputFolder, File javacOutputFolder, File tempFolder) throws Exception {
		SarlBatchCompiler compiler = this.compilerProvider.get();
		compiler.setBasePath(basePath.getAbsolutePath());
		compiler.setSourcePath(sourcePath.getAbsolutePath());
		compiler.setOutputPath(sarlcOutputFolder);
		compiler.setClassOutputPath(javacOutputFolder);
		compiler.setTempDirectory(tempFolder);
		compiler.setJavaCompilerVerbose(false);
		compiler.setGenerateInlineAnnotation(false);
		compiler.setReportInternalProblemsAsIssues(true);
		final List<Issue> issues = new ArrayList<>();
		compiler.addIssueMessageListener((issue, uri, message) -> {
			issues.add(issue);
		});
		if (!compiler.compile()) {
			throw new RuntimeException("Compilation error: " + issues.toString());
		}
	}

}
