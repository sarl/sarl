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

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.nio.file.Path;

import org.arakhne.afc.vmutil.FileSystem;

/** Wrapper for accessing the results of a Maven run.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public abstract class Verifier {

	/** Base directory of the test project.
	 */
	protected final File basedir;
	
	/** Indicates if the output is print out on the console.
	 */
	protected final boolean outputOnConsole;

	/** Constructor.
	 *
	 * @param basedir the base directory of the project.
	 * @param outputOnConsole indicates if the standard output of the process must be on the console, or not.
	 */
	protected Verifier(File basedir, boolean outputOnConsole) {
		this.basedir = basedir;
		this.outputOnConsole = outputOnConsole;
	}

	/** Replies the base directory.
	 *
	 * @return the base directory.
	 */
	public File getBaseDir() {
		return this.basedir;
	}
	
	/** Wait for the termination of the process.
	 *
	 * @throws Exception the command was interrupted.
	 */
	public abstract void waitFor() throws Exception;

	/** Assert that there is no error in the Maven run.
	 *
	 * @throws Exception if the error log cannot be read.
	 */
	public abstract void assertErrorFreeLog() throws Exception;

	/** Assert that there is error in the Maven run.
	 *
	 * @param expectedErrorMessage the expected error message that must appear on the error console of Maven.
	 * @throws Exception if the error log cannot be read.
	 */
	public abstract void assertErrorLog(String expectedErrorMessage) throws Exception;

	/** Test if the file with the given relative path exists.
	 *
	 * @param path the path that is relative to the base directory.
	 */
	public void assertFilePresent(Path path) {
		final var filename = FileSystem.join(this.basedir, path.toFile());
		assertTrue(filename.exists(), "File not found: " + filename.getAbsolutePath()); //$NON-NLS-1$
	}

	/** Read the file content.
	 *
	 * @param path the relative path of the file relatively to the base directory.
	 * @return the file content.
	 * @throws Exception if the file cannot be read.
	 */
	public String readFile(Path path) throws Exception {
		final var content = new StringBuilder();
		final var filename = FileSystem.join(this.basedir, path.toFile());
		try (final var reader = new BufferedReader(new FileReader(filename))) {
			var line = reader.readLine();
			while (line != null) {
				content.append(line).append("\n"); //$NON-NLS-1$
				line = reader.readLine();
			}
		}
		return content.toString();
	}

}
