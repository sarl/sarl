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

package io.sarl.maven.docs.testing;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;

import org.arakhne.afc.vmutil.OperatingSystem;

import io.sarl.lang.util.CliUtilities;

/** Functions for shell.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class ShellExtensions {

	/** Reply the name of an executable for the system.
	 * Test if the given command corresponds to an existing file.
	 *
	 * @param command the command.
	 * @return the name.
	 * @throws FileNotFoundException if the given command does not correspond to an existing file.
	 * @throws IOException if the absolute path of the file cannot be computed.
	 */
	public static String makeExecName(String... command) throws FileNotFoundException, IOException {
		final StringBuilder buffer = new StringBuilder();
		for (int i = 0; i < command.length - 1; ++i) {
			if (i > 0) {
				buffer.append(File.separator);
			}
			buffer.append(command[i]);
		}
		if (command.length > 1) {
			buffer.append(File.separator);
		}
		final OperatingSystem os = OperatingSystem.getCurrentOS();
		switch (os) {
		case WIN:
			buffer.append(command[command.length - 1]);
			buffer.append(".exe"); //$NON-NLS-1$
			break;
		case MACOSX:
			buffer.append(command[command.length - 1]);
			//buffer.append(".cmd"); //$NON-NLS-1$
			break;
		default:
			buffer.append(command[command.length - 1]);
		}
		final String cmd = buffer.toString();
		File file = new File(cmd);
		if (!file.isAbsolute()) {
			file = new File(System.getProperty("SARL_DOC_CURRENT_FOLDER") + File.separator + file.getPath()).getCanonicalFile();
		}
		if (!file.exists()) {
			throw new FileNotFoundException(file.toString());
		}
		return file.getCanonicalPath();
	}

	/** Reply a well formatted option for the system.
	 *
	 * @param opt the option.
	 * @return the formatted option.
	 */
	public static String makeCliOption(String opt) {
		return CliUtilities.getCommandLineOption(opt);
	}

	/** Reply a well formatted option for the system that is defining a property and its value.
	 *
	 * @param name the name of the property.
	 * @param value the value to give to the property.
	 * @return the formatted option.
	 */
	public static String makeCliDefinition(String name, String value) {
		return CliUtilities.getCommandLineDefinition(name, value);
	}

	/** Run a shell command and capture its standard output and without exception output.
	 *
	 * @param command the command.
	 * @return the standard output.
	 */
	public static String runShellSilently(String... command) {
		try {
			return runShell(command);
		} catch (Throwable exceptionToIgnore) {
			return "ERROR: " + exceptionToIgnore; //$NON-NLS-1$
		}
	}

	/** Run a shell command and capture its standard output.
	 *
	 * @param command the command.
	 * @return the standard output.
	 * @throws IOException if the process output cannot be catch.
	 * @throws InterruptedException if the process is interrupted.
	 */
	public static String runShell(String... command) throws IOException, InterruptedException {
		final ProcessBuilder processBuilder = new ProcessBuilder();
		processBuilder.command(command);
		final Process process = processBuilder.start();
		final StringBuilder output = new StringBuilder();
		try (final BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
			String line = reader.readLine();
			while (line  != null) {
				output.append(line + "\n");
				line = reader.readLine();
			}
			process.waitFor();
		}
		return output.toString();
	}

}