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
package io.sarl.tests.api.tools;

import java.io.File;
import java.util.Collections;
import java.util.List;

import org.apache.maven.shared.verifier.VerificationException;

/** Abstract test of Maven Mojo in the current process.
 * 
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version tests.api 0.15.1 20250911-224823
 * @mavengroupid io.sarl.baseutils
 * @mavenartifactid tests.api
 */
public abstract class AbstractEmbeddedMavenMojoTest extends AbstractMojoTest  {

	/** Replies the group id of the plugin that is under test.
	 *
	 * @return the groupid or {@code null} if no plugin in the current project is tested.
	 */
	protected abstract String getPluginGroupId();
	
	/** Replies the artefact id of the plugin that is under test.
	 *
	 * @return the artefact id or {@code null} if no plugin in the current project is tested.
	 */
	protected abstract String getPluginArtefactId(); 

	/** Replies the version of the plugin that is under test.
	 *
	 * @return the version or {@code null} if no plugin in the current project is tested.
	 */
	protected abstract String getPluginVersion(); 

	@Override
	protected Verifier runMaven(File baseDir, boolean outputOnConsole, List<String> command) throws Exception {
		// Remove the command because Maven is embedded
		command.remove(0);

		org.apache.maven.shared.verifier.Verifier verifier = new org.apache.maven.shared.verifier.Verifier(baseDir.getAbsolutePath(), true);
		verifier.setAutoclean(false);
		verifier.setForkJvm(false);
		try {
			for (final String arg : command) {
				verifier.addCliArgument(arg);
			}
			verifier.execute();
		} catch (VerificationException ex) {
			if (isDebug()) {
				throw new Error(ex);
			}
		}
		return VerifierFactory.build(baseDir, outputOnConsole, verifier);
	}

	/** Wrapper for accessing the results of a Maven run.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version tests.api 0.15.1 20250911-224823
	 * @mavengroupid io.sarl.baseutils
	 * @mavenartifactid tests.api
	 * @since 0.13
	 */
	static class EmbeddedVerifier extends Verifier {

		private final org.apache.maven.shared.verifier.Verifier mavenVerifier;

		/** Constructor.
		 *
		 * @param basedir the base directory of the project.
		 * @param outputOnConsole indicates if the standard output of the process must be on the console, or not.
		 * @param verifier the Maven verifier to be packed.
		 */
		EmbeddedVerifier(File basedir, boolean outputOnConsole, org.apache.maven.shared.verifier.Verifier verifier) {
			super(basedir, outputOnConsole);
			this.mavenVerifier = verifier;
		}
		
		/** Wait for the termination of the process.
		 *
		 * @throws Exception the command was interrupted.
		 */
		@Override
		public void waitFor() throws Exception {
			if (this.outputOnConsole) {
				//
			}
		}

		/** Assert that there is no error in the Maven run.
		 *
		 * @throws Exception if the error log cannot be read.
		 */
		@Override
		public void assertErrorFreeLog() throws Exception {
			final List<String> lines;
			if (this.outputOnConsole) {
				lines = this.mavenVerifier.loadFile(
						this.mavenVerifier.getBasedir(),
						this.mavenVerifier.getLogFileName(),
						false);
			} else {
				lines = Collections.emptyList();
			}
			try {
				this.mavenVerifier.verifyErrorFreeLog();
			} catch (VerificationException ex) {
				if (this.outputOnConsole) {
					for (final String line : lines) {
						System.err.println(line);
					}
				}
				throw ex;
			}
		}

		/** Assert that there is error in the Maven run.
		 *
		 * @param expectedErrorMessage the expected error message that must appear on the error console of Maven.
		 * @throws Exception if the error log cannot be read.
		 */
		@Override
		public void assertErrorLog(String expectedErrorMessage) throws Exception {
			this.mavenVerifier.verifyTextInLog(expectedErrorMessage);
		}

	}

}
