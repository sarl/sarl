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

import org.apache.maven.it.VerificationException;
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
@DisplayName("mojo: compile")
@Tag("maven")
@Tag("mvn-run")
public class CompileMojoTest extends AbstractMojoTest {

	@Test
	public void invalidXtext() throws Exception {
		assertException(VerificationException.class, () -> {
			executeMojo("prj1", "compile");
		});
	}

	@Test
	public void invalidSdk() throws Exception {
		assertException(VerificationException.class, () -> {
			executeMojo("prj2", "compile");
		});
	}

	@Test
	public void validCode() throws Exception {
		System.setProperty("verifier.forkMode", "embedded");
		executeMojo("prj3", "compile");
	}

}
