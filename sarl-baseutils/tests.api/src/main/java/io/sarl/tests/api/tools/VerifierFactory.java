/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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
package io.sarl.tests.api.tools;

import java.io.File;

/** Factory for verifiers.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public final class VerifierFactory {

	private VerifierFactory() {
		//
	}
	
	/** Build a verifier based on the Maven running in the given process.
	 *
	 * @param basedir the base directory of the project.
	 * @param outputOnConsole indicates if the standard output of the process must be on the console, or not.
	 * @param process the maven process.
	 * @return the verifier tool.
	 */
	public static Verifier build(File basedir, boolean outputOnConsole, Process process) {
		return new AbstractForkMavenMojoTest.ProcessVerifier(basedir, outputOnConsole, process);
	}

	/** Build a verifier based on the Maven running in the given process.
	 *
	 * @param basedir the base directory of the project.
	 * @param outputOnConsole indicates if the standard output of the process must be on the console, or not.
	 * @param verifier the Maven verifier.
	 * @return the verifier tool.
	 */
	public static Verifier build(File basedir, boolean outputOnConsole, org.apache.maven.shared.verifier.Verifier verifier) {
		return new AbstractEmbeddedMavenMojoTest.EmbeddedVerifier(basedir, outputOnConsole, verifier);
	}

}
