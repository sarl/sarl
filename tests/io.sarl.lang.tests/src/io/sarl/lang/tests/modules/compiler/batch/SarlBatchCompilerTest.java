/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import javax.inject.Inject;
import javax.inject.Provider;

import org.apache.log4j.Level;

import io.sarl.lang.compiler.batch.SarlBatchCompiler;


/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SarlBatchCompilerTest extends AbstractBatchCompilerTest {

	@Inject
	private Provider<SarlBatchCompiler> compilerProvider;
	
	@Override
	public void runBatchCompiler(File basePath, File sourcePath, File sarlcOutputFolder, File javacOutputFolder, File tempFolder) throws Exception {
		SarlBatchCompiler compiler = this.compilerProvider.get();
		compiler.setBasePath(basePath.getAbsolutePath());
		compiler.setSourcePath(sourcePath.getAbsolutePath());
		compiler.setOutputPath(sarlcOutputFolder);
		compiler.setTempDirectory(tempFolder);
		compiler.setJavaCompilerVerbose(false);
		compiler.setGenerateInlineAnnotation(false);
		compiler.getLogger().setLevel(Level.ERROR);
		if (!compiler.compile()) {
			throw new RuntimeException("Compilation error");
		}
	}

}
