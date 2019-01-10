/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

import com.google.common.collect.Lists;
import com.google.inject.Singleton;
import org.apache.commons.io.output.WriterOutputStream;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.xtext.util.Strings;
import org.slf4j.Logger;

/** A wrapper on top of the Oracle Java Compiler (javac).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@Singleton
public class JavacBatchCompiler implements IJavaBatchCompiler {

	@Override
	@SuppressWarnings({ "checkstyle:parameternumber", "checkstyle:cyclomaticcomplexity",
		"checkstyle:npathcomplexity", "resource" })
	public boolean compile(File classDirectory, Iterable<File> sourcePathDirectories,
			Iterable<File> classPathEntries,
			List<File> bootClassPathEntries,
			String javaVersion,
			String encoding,
			boolean isCompilerMoreVerbose,
			OptimizationLevel optimizationLevel,
			PrintWriter outWriter,
			PrintWriter errWriter,
			Logger logger,
			IProgressMonitor progress) {
		assert progress != null;

		final List<String> commandLineArguments = Lists.newArrayList();
		if (optimizationLevel != null) {
			switch (optimizationLevel) {
			case G2:
				commandLineArguments.add("-g:none"); //$NON-NLS-1$
				break;
			case G1:
				commandLineArguments.add("-g:none"); //$NON-NLS-1$
				break;
			case G0:
			default:
				commandLineArguments.add("-g"); //$NON-NLS-1$
			}
		}
		commandLineArguments.add("-nowarn"); //$NON-NLS-1$
		if (isCompilerMoreVerbose) {
			commandLineArguments.add("-verbose"); //$NON-NLS-1$
		}
		if (progress.isCanceled()) {
			return false;
		}
		if (!bootClassPathEntries.isEmpty()) {
			final StringBuilder cmd = new StringBuilder();
			boolean first = true;
			for (final File entry : bootClassPathEntries) {
				if (progress.isCanceled()) {
					return false;
				}
				if (entry.exists()) {
					if (first) {
						first = false;
					} else {
						cmd.append(File.pathSeparator);
					}
					cmd.append(entry.getAbsolutePath());
				}
			}
			if (cmd.length() > 0) {
				commandLineArguments.add("-bootclasspath"); //$NON-NLS-1$
				commandLineArguments.add(cmd.toString());
			}
		}
		final Iterator<File> classPathIterator = classPathEntries.iterator();
		if (classPathIterator.hasNext()) {
			final StringBuilder cmd = new StringBuilder();
			boolean first = true;
			while (classPathIterator.hasNext()) {
				final File classpathPath = classPathIterator.next();
				if (progress.isCanceled()) {
					return false;
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
			if (cmd.length() > 0) {
				commandLineArguments.add("-cp"); //$NON-NLS-1$
				commandLineArguments.add(cmd.toString());
			}
		}
		if (progress.isCanceled()) {
			return false;
		}
		if (!classDirectory.exists()) {
			classDirectory.mkdirs();
		}
		commandLineArguments.add("-d"); //$NON-NLS-1$
		commandLineArguments.add(classDirectory.getAbsolutePath());
		commandLineArguments.add("-source"); //$NON-NLS-1$
		commandLineArguments.add(javaVersion);
		commandLineArguments.add("-target"); //$NON-NLS-1$
		commandLineArguments.add(javaVersion);
		if (!Strings.isEmpty(encoding)) {
			commandLineArguments.add("-encoding"); //$NON-NLS-1$
			commandLineArguments.add(encoding);
		}
		if (progress.isCanceled()) {
			return false;
		}

		boolean hasSourceFile = false;
		for (final File sourceFolder : sourcePathDirectories) {
			if (progress.isCanceled()) {
				return false;
			}
			if (addJavaFilesDeeply(commandLineArguments, sourceFolder.getAbsoluteFile())) {
				hasSourceFile = true;
			}
		}

		final String[] arguments = new String[commandLineArguments.size()];
		commandLineArguments.toArray(arguments);

		if (logger != null && logger.isDebugEnabled()) {
			logger.debug(Messages.JavacBatchCompiler_0, Strings.concat(" ", commandLineArguments)); //$NON-NLS-1$
		}

		if (!hasSourceFile || progress.isCanceled()) {
			return false;
		}

		final OutputStream stdout = new WriterOutputStream(outWriter);

		final OutputStream stderr = new JavacErrorStream(errWriter, logger);

		final JavaCompiler systemCompiler = ToolProvider.getSystemJavaCompiler();
		final int retcode = systemCompiler.run(null, stdout, stderr, arguments);
		return retcode == 0;
	}

	private static boolean addJavaFilesDeeply(List<String> list, File root) {
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
			for (final File subfile : current.listFiles(name -> isJavaExtension(name))) {
				if (subfile.isDirectory()) {
					folders.addLast(subfile);
				} else {
					list.add(subfile.getAbsolutePath());
					changed = true;
				}
			}
		}
		return changed;
	}

	private static boolean isJavaExtension(File file) {
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

	/** Wrap a stderr writer for supporting specific Javac error messages.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	private static class JavacErrorStream extends WriterOutputStream {

		private static final String NOTE_PREFIX = "Note:"; //$NON-NLS-1$

		private final Logger logger;

		/** Constructor.
		 *
		 * @param writer the writer to wrap.
		 * @param logger the logger to use for printing out special messages.
		 */
		JavacErrorStream(PrintWriter writer, Logger logger) {
			super(writer);
			this.logger = logger;
		}

		@Override
		public void write(byte[] buffer, int offset, int length) throws IOException {
			final String msg = new String(buffer, offset, length);
			if (msg.startsWith(NOTE_PREFIX)) {
				if (this.logger != null) {
					this.logger.info(msg.substring(NOTE_PREFIX.length()).trim());
				}
				return;
			}
			super.write(buffer, offset, length);
		}
	}

}
