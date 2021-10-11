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

package io.sarl.lang.compiler.batch;

import java.io.File;
import java.io.PrintWriter;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.xtext.util.JavaVersion;

/** Abstract implementation for the Java batch compilers.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public abstract class AbstractJavaBatchCompiler implements IJavaBatchCompiler {

	/** Build a OS-dependent path from the given list of files.
	 *
	 * @param path the list of files.
	 * @param progress the progress indicator.
	 * @return the OS-dependent path.
	 */
	protected static String buildPath(Iterable<File> path, IProgressMonitor progress) {
		final StringBuilder cmd = new StringBuilder();
		boolean first = true;
		for (final File classpathPath : path) {
			if (progress.isCanceled()) {
				return null;
			}
			if (classpathPath.exists()) {
				if (first) {
					first = false;
				} else {
					cmd.append(File.pathSeparator);
				}
				cmd.append(classpathPath.getAbsolutePath());
			}
		}
		return cmd.toString();
	}

	/** Fill the given list with the Java files from the given root folder, recursively.
	 *
	 * @param list the list to fill out.
	 * @param root the root folder to explore.
	 * @return {@code true} if a file was added into the {@code list}.
	 * @see #addFolderIfJavaFileDeeply(List, File)
	 */
	protected boolean addJavaFilesDeeply(List<String> list, File root) {
		final Deque<File> folders = new LinkedList<>();
		if (root.exists()) {
			if (root.isDirectory()) {
				folders.addLast(root);
			} else {
				list.add(root.getAbsolutePath());
				return true;
			}
		}
		boolean changed = false;
		while (!folders.isEmpty()) {
			final File current = folders.removeFirst();
			assert current.isDirectory();
			for (final File subfile : current.listFiles()) {
				if (subfile.isDirectory()) {
					folders.addLast(subfile);
				} else if (isJavaExtension(subfile)) {
					list.add(subfile.getAbsolutePath());
					changed = true;
				}
			}
		}
		return changed;
	}

	/** Fill the given root folder to the list if a Java file is found inside, recursively.
	 *
	 * @param list the list to fill out.
	 * @param root the root folder to explore.
	 * @return {@code true} if a file was added into the {@code list}.
	 * @since 0.12
	 * @see #addJavaFilesDeeply(List, File)
	 */
	protected boolean addFolderIfJavaFileDeeply(List<String> list, File root) {
		final Deque<File> folders = new LinkedList<>();
		if (root.exists() && root.isDirectory()) {
			folders.addLast(root);
		}
		while (!folders.isEmpty()) {
			final File current = folders.removeFirst();
			assert current.isDirectory();
			for (final File subfile : current.listFiles()) {
				if (subfile.isDirectory()) {
					folders.addLast(subfile);
				} else if (isJavaExtension(subfile)) {
					list.add(root.getAbsolutePath());
					return true;
				}
			}
		}
		return false;
	}

	/** Replies if the given file is a JAva source file, i.e. with {@code .java} file extension.
	 *
	 * @param file the file to test.
	 * @return {@code true} if the file is considered as a Java file.
	 */
	protected boolean isJavaExtension(File file) {
		if (file != null) {
			if (file.isDirectory()) {
				return true;
			}
			final String name = file.getName();
			final int index = name.lastIndexOf('.');
			if (index > 1 && ".java".equalsIgnoreCase(name.substring(index))) { //$NON-NLS-1$
				return true;
			}
		}
		return false;
	}

	@Override
	@SuppressWarnings({ "checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity",
		"checkstyle:parameternumber" })
	public final CompilerStatus compile(File classDirectory, Iterable<File> sourcePathDirectories,
			Iterable<File> classPathEntries,
			Iterable<File> modulePathEntries,
			String javaVersion,
			String encoding,
			boolean isCompilerMoreVerbose,
			OptimizationLevel optimizationLevel,
			PrintWriter outWriter,
			PrintWriter errWriter,
			Logger logger,
			IProgressMonitor progress) {
		final JavaVersion jversion = SarlBatchCompilerUtils.parserJavaVersion(javaVersion);
		return compile(classDirectory, sourcePathDirectories, classPathEntries, modulePathEntries,
				jversion, SarlBatchCompilerUtils.isModuleSupported(jversion),
				encoding, isCompilerMoreVerbose, optimizationLevel, outWriter, errWriter, logger, progress);
	}

	/** Run the Java compiler.
	 *
	 * @param classDirectory the output directory.
	 * @param sourcePathDirectories the source directories.
	 * @param classPathEntries classpath entries.
	 * @param modulePathEntries classpath entries. No more used.
	 * @param javaVersion the version of Java that is the target, e.g. {@code 11}.
	 * @param isModuleSupport indicates if modules are supported.
	 * @param encoding the encoding of the files.
	 * @param isCompilerMoreVerbose indicates if the Java compiler should be more verbose.
	 * @param optimizationLevel the level of optimization to apply to the byte code or {@code null} if the defaults must be applied.
	 * @param outWriter the standard output writer.
	 * @param errWriter the standard error writer.
	 * @param logger the logger to use for debugging messages.
	 * @param progress monitor of the progress of the compilation.
	 * @return the success status; Never {@code null}.
	 */
	@SuppressWarnings("checkstyle:parameternumber")
	protected abstract CompilerStatus compile(File classDirectory, Iterable<File> sourcePathDirectories,
			Iterable<File> classPathEntries,
			Iterable<File> modulePathEntries,
			JavaVersion javaVersion,
			boolean isModuleSupport,
			String encoding,
			boolean isCompilerMoreVerbose,
			OptimizationLevel optimizationLevel,
			PrintWriter outWriter,
			PrintWriter errWriter,
			Logger logger,
			IProgressMonitor progress);

}
