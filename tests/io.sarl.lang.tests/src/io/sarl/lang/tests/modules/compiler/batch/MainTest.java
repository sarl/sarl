/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.junit.Before;
import org.junit.Test;

import io.sarl.lang.actionprototype.ActionParameterTypes;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.compiler.batch.Main;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class MainTest extends AbstractBatchCompilerTest {

	@Override
	public void runBatchCompiler(File basePath, File sourcePath, File sarlcOutputFolder, File javacOutputFolder, File tempFolder) throws Exception {
		Main.runCompiler(new String[] {
				"--quiet",
				"--dir", sarlcOutputFolder.getAbsolutePath(),
				"--tempdir", tempFolder.getAbsolutePath(),
				"--javac", Boolean.FALSE.toString(),
				"-inlines", Boolean.FALSE.toString(),
				sourcePath.getAbsolutePath(),
		});
	}

}
