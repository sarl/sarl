/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.PrintStream;
import java.util.List;
import java.util.concurrent.TimeUnit;

/** Abstract test of Maven Mojo in a fork process for Maven.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractForkMavenMojoTest extends AbstractMojoTest  {

	@Override
	protected Verifier runMaven(File baseDir, boolean outputOnConsole, List<String> command) throws Exception {
		final String[] args = new String[command.size()];
		command.toArray(args);
		final Process process = Runtime.getRuntime().exec(args, null, baseDir);
		Verifier verifier = VerifierFactory.build(baseDir, outputOnConsole, process);
		verifier.waitFor();
		return verifier;
	}

	/** Wrapper for accessing the results of a Maven run.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	static class ProcessVerifier extends Verifier {

		private final Process process;

		private final StringBuilder output = new StringBuilder();

		private final StringBuilder errorOutput = new StringBuilder();

		/** Constructor.
		 *
		 * @param basedir the base directory of the project.
		 * @param outputOnConsole indicates if the standard output of the process must be on the console, or not.
		 * @param process the maven process.
		 */
		ProcessVerifier(File basedir, boolean outputOnConsole, Process process) {
			super(basedir, outputOnConsole);
			this.process = process;
		}

		private void readStream(BufferedReader reader) throws Exception {
			String line = reader.readLine();
			while (line != null) {
				StringBuilder out;
				PrintStream console;
				if (line.startsWith("[ERROR]")) { //$NON-NLS-1$
					out = this.errorOutput;
					console = System.err;
				} else {
					out = this.output;
					console = System.out;
				}
				if (out.length() > 0) {
					out.append("\n"); //$NON-NLS-1$
				}
				out.append(line);
				if (this.outputOnConsole) {
					console.println(line);
				}
				line = reader.readLine();
			}
		}
		
		/** Wait for the termination of the process.
		 *
		 * @throws Exception the command was interrupted.
		 */
		@SuppressWarnings("resource")
		@Override
		public void waitFor() throws Exception {
			readStream(this.process.inputReader());
			readStream(this.process.errorReader());
			this.process.waitFor(5, TimeUnit.MINUTES);
		}

		/** Assert that there is no error in the Maven run.
		 *
		 * @throws Exception if the error log cannot be read.
		 */
		@Override
		public void assertErrorFreeLog() throws Exception {
			assertEquals(0, this.process.exitValue(), "Maven exit code is " + this.process.exitValue()); //$NON-NLS-1$
			
			assertEquals("", this.errorOutput.toString().trim()); //$NON-NLS-1$
		}

		/** Assert that there is error in the Maven run.
		 *
		 * @param expectedErrorMessage the expected error message that must appear on the error console of Maven.
		 * @throws Exception if the error log cannot be read.
		 */
		@Override
		public void assertErrorLog(String expectedErrorMessage) throws Exception {
			assertNotEquals(0, this.process.exitValue(), "Maven exit code is " + this.process.exitValue()); //$NON-NLS-1$
			final String log = this.errorOutput.toString().trim();
			assertTrue(log.contains(expectedErrorMessage), "Error log does not contains: " + expectedErrorMessage); //$NON-NLS-1$
		}

	}

}
