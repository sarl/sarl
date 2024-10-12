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

package io.sarl.docs.validator;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;

import org.apache.commons.io.output.ByteArrayOutputStream;
import org.arakhne.afc.vmutil.OperatingSystem;

import io.sarl.lang.core.util.CliUtilities;

/** Functions for shell.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class ShellExtensions {
	
	/** Public singleton that is the shell command provider to be used.
	 *
	 * @since 0.13
	 */
	public static ShellCommandProvider shellCommandProvider;

	/** Reply the name of an executable for the system.
	 * Test if the given command corresponds to an existing file.
	 *
	 * @param command the command.
	 * @return the name.
	 * @throws FileNotFoundException if the given command does not correspond to an existing file.
	 * @throws IOException if the absolute path of the file cannot be computed.
	 */
	public static String makeExecName(String... command) throws FileNotFoundException, IOException {
		final var buffer = new StringBuilder();
		for (var i = 0; i < command.length - 1; ++i) {
			if (i > 0) {
				buffer.append(File.separator);
			}
			buffer.append(command[i]);
		}
		if (command.length > 1) {
			buffer.append(File.separator);
		}
		final var os = OperatingSystem.getCurrentOS();
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
		final var cmd = buffer.toString();
		File file = new File(cmd);
		if (!file.isAbsolute()) {
			file = new File(System.getProperty("SARL_DOC_CURRENT_FOLDER") + File.separator + file.getPath()).getCanonicalFile(); //$NON-NLS-1$
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
		final var processBuilder = new ProcessBuilder();
		processBuilder.command(command);
		final var process = processBuilder.start();
		final var output = new StringBuilder();
		try (final var reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
			var line = reader.readLine();
			while (line  != null) {
				output.append(line + "\n"); //$NON-NLS-1$
				line = reader.readLine();
			}
			process.waitFor();
		}
		return output.toString();
	}

	/** Run a command and capture its standard output.
	 *
	 * @param command the command.
	 * @return the standard output.
	 * @throws Exception if any problem occurs when running the command.
	 * @since 0.13
	 */
	public static String runJavaCommand(Runnable command) throws Exception {
		final var oldStdOut = System.out;
		final var oldStdErr = System.err;
		try {
			try (final var buffer = new ByteArrayOutputStream()) {
				try (final var newOut = new PrintStream(buffer)) {
					System.setOut(newOut);
					System.setErr(newOut);
					command.run();
				}
				buffer.flush();
				return new String(buffer.toByteArray());
			}
		} finally {
			System.setOut(oldStdOut);
			System.setErr(oldStdErr);
		}
	}

	/** Find an executable command provided in the Maven configuration.
	 *
	 * @param commandName the name of the command.
	 * @return the path to the command.
	 * @throws FileNotFoundException if the command cannot be found.
	 * @since 0.13
	 */
	public static String whichShellCommand(String commandName) throws FileNotFoundException {
		final var provider = shellCommandProvider;
		if (provider != null) {
			final var command = provider.getShellCommand(commandName);
			if (command != null) {
				final var executable = command.executable();
				if (executable != null) {
					return executable.getAbsolutePath();
				}
			}
		}
		throw new FileNotFoundException(commandName);
	}

}