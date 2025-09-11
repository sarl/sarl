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
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import org.arakhne.afc.vmutil.FileSystem;

/** Utilities that are related to operating system shell.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version tests.api 0.15.1 20250911-224823
 * @mavengroupid io.sarl.baseutils
 * @mavenartifactid tests.api
 * @since 0.13
 */
public class TestShell {

	private TestShell() {
		//
	}

	/** Run a command-line program with a operating system call.
	 *
	 * @param script the name of the script to launch.
	 * @param arguments the command-line arguments to pass to program.
	 * @return the standard output stream.
	 * @throws Exception if there is some issue when running the program.
	 */
	@SuppressWarnings("resource")
	public static String run(String... arguments) throws Exception {
		final var builder = new ProcessBuilder(arguments);
		builder.redirectErrorStream(true);
		final var proc = builder.start();
		proc.waitFor(30, TimeUnit.SECONDS);
		final var out = new StringBuilder();
		final var is = proc.inputReader();
		String line;
		while ((line = is.readLine()) != null) {
			out.append(line).append("\n"); //$NON-NLS-1$
		}
		return out.toString();
	}

	/** Run a command-line program with a operating system call but without overriding the input and output streams.
	 *
	 * @param script the name of the script to launch.
	 * @param arguments the command-line arguments to pass to program.
	 * @throws Exception if there is some issue when running the program.
	 */
	public static void runWithoutStreamOverriding(String... arguments) throws Exception {
		final var builder = new ProcessBuilder(arguments);
		final var proc = builder.start();
		proc.waitFor(30, TimeUnit.SECONDS);
	}

	/** Replies the merging of the given arguments.
	 *
	 * @param javaCmd the Java command.
	 * @param jarFile the runnable Jar.
	 * @param arguments the other arguments.
	 * @return all the arguments.
	 */
	public static String[] mergeJarArguments(String javaCmd, String jarFile, String... arguments) {
		final var result = new String[arguments.length + 3];
		result[0] = javaCmd;
		result[1] = "-jar"; //$NON-NLS-1$
		result[2] = jarFile;
		System.arraycopy(arguments, 0, result, 3, arguments.length);
		return result;
	}

	/** Find a jar that could be used for running a program.
	 *
	 * @param artifact the name of the artifact.
	 * @return the jar path.typeName
	 * @throws FileNotFoundException if there is no executable jar.
	 */
	public static File findExecutableJar(String artifact) throws FileNotFoundException {
		final var pattern = Pattern.compile(Pattern.quote(artifact) + "\\-[0-9\\.]+(?:\\-SNAPSHOT)?\\.jar"); //$NON-NLS-1$
		return findExecutableJar(artifact, pattern);
	}

	/** Find a jar that could be used for running a program.
	 *
	 * @param artifact the name of the artifact.
	 * @param typeName the name of the artifact type.
	 * @return the jar path.
	 * @throws FileNotFoundException if there is no executable jar.
	 */
	public static File findExecutableJar(String artifact, String typeName) throws FileNotFoundException {
		final var pattern = Pattern.compile(Pattern.quote(artifact) + "\\-[0-9\\.]+(?:\\-SNAPSHOT)?\\-" + Pattern.quote(typeName) + "\\.jar"); //$NON-NLS-1$ //$NON-NLS-2$
		return findExecutableJar(artifact, pattern);
	}

	private static File findExecutableJar(String artifact, Pattern filename) throws FileNotFoundException {
		final var f0 = findExecutableJar(new File(FileSystem.CURRENT_DIRECTORY), filename);
		if (f0 != null) {
			return f0;
		}
		final var brotherRoot = FileSystem.join(new File(FileSystem.PARENT_DIRECTORY), artifact);
		final var f1 = findExecutableJar(brotherRoot, filename);
		if (f1 != null) {
			return f1;
		}
		throw new FileNotFoundException("No executable jar found for artifact '" + artifact + "'"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static File findExecutableJar(File root, Pattern filename) {
		final var folder = FileSystem.join(root, "target").getAbsoluteFile(); //$NON-NLS-1$
		final FileFilter filter = it -> {
			final Matcher matcher = filename.matcher(it.getName());
			return matcher.matches();
		};
		final var files = folder.listFiles(filter);
		if (files != null && files.length == 1) {
			return files[0];
		}
		return null;
	}

	/** Replies the path to the executable java program.
	 *
	 * @return the java program
	 * @throws FileNotFoundException if there is no executable file.
	 */
	public static File findExecutableJava() throws FileNotFoundException {
		final var pathvar = System.getenv("PATH"); //$NON-NLS-1$
		if (!Strings.isNullOrEmpty(pathvar)) {
			for (final var name : pathvar.split(Pattern.quote(File.pathSeparator))) {
				final var path = FileSystem.convertStringToFile(name);
				final var exec = FileSystem.join(path, "java"); //$NON-NLS-1$
				if (exec.canExecute()) {
					return exec;
				}
			}
		}
		throw new FileNotFoundException();
	}

}
