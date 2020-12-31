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
package io.sarl.maven.compiler;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.nio.file.FileSystems;
import java.nio.file.Path;

import org.apache.maven.it.Verifier;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Bug #317")
@Tag("maven")
@Tag("mvn-run")
public class Bug317Test extends AbstractMojoTest {

	@Test
	public void compile() throws Exception {
		Verifier verifier = executeMojo("bug317", "compile");
		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "compiler", "tests", "MyAgent.java");
		assertNotNull(path);
		verifier.assertFilePresent(path.toString());
	}
	
}
