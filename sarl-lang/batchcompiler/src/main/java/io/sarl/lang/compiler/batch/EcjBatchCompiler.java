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

package io.sarl.lang.compiler.batch;

import java.io.File;
import java.io.PrintWriter;
import java.text.MessageFormat;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.common.collect.Lists;
import com.google.inject.Singleton;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.compiler.CompilationProgress;
import org.eclipse.jdt.core.compiler.batch.BatchCompiler;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.Strings;

/** A wrapper on top of the Eclipse Compiler for Java (ECJ), aka. JDT.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version batchcompiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid batchcompiler
 * @since 0.8
 */
@Singleton
public class EcjBatchCompiler extends AbstractJavaBatchCompiler {

	@Override
	public String getName() {
		return "Eclipse JDT Compiler"; //$NON-NLS-1$
	}

	@Override
	public CompilerStatus compile(File classDirectory, Iterable<File> sourcePathDirectories,
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
			IProgressMonitor progress) {
		assert progress != null;

		final var commandLineArguments = Lists.<String>newArrayList();
		//
		// Optimization
		//
		if (optimizationLevel != null) {
			switch (optimizationLevel) {
			case G2:
				commandLineArguments.add("-g:none"); //$NON-NLS-1$
				commandLineArguments.add("-O"); //$NON-NLS-1$
				break;
			case G1:
				commandLineArguments.add("-g:none"); //$NON-NLS-1$
				break;
			case G0:
			default:
				commandLineArguments.add("-g"); //$NON-NLS-1$
				commandLineArguments.add("-preserveAllLocals"); //$NON-NLS-1$
			}
		}
		//
		// Verbosity
		//
		commandLineArguments.add("-nowarn"); //$NON-NLS-1$
		if (isCompilerMoreVerbose) {
			commandLineArguments.add("-verbose"); //$NON-NLS-1$
		}
		if (progress.isCanceled()) {
			return CompilerStatus.CANCELED;
		}
		//
		// Classpath
		//
		final var classPathIterator = classPathEntries.iterator();
		if (classPathIterator.hasNext()) {
			final var path = buildPath(classPathEntries, progress);
			if (path == null) {
				return CompilerStatus.CANCELED;
			}
			if (!Strings.isEmpty(path)) {
				commandLineArguments.add("-cp"); //$NON-NLS-1$
				commandLineArguments.add(path);
			}
		}
		if (progress.isCanceled()) {
			return CompilerStatus.CANCELED;
		}
		//
		// Module-path
		//
		if (isModuleSupport) {
			final var path = buildPath(modulePathEntries, progress);
			if (path == null) {
				return CompilerStatus.CANCELED;
			}
			if (!Strings.isEmpty(path)) {
				commandLineArguments.add("-p"); //$NON-NLS-1$
				commandLineArguments.add(path);
			}
		}
		//
		// Output directory
		//
		if (!classDirectory.exists()) {
			classDirectory.mkdirs();
		}
		commandLineArguments.add("-d"); //$NON-NLS-1$
		commandLineArguments.add(classDirectory.getAbsolutePath());
		//
		// Java version support
		//
		commandLineArguments.add("-" + ensureCompatibleJavaVersion(javaVersion)); //$NON-NLS-1$
		//
		// Error management
		//
		commandLineArguments.add("-proceedOnError"); //$NON-NLS-1$
		//
		// File encoding
		//
		if (!Strings.isEmpty(encoding)) {
			commandLineArguments.add("-encoding"); //$NON-NLS-1$
			commandLineArguments.add(encoding);
		}
		if (progress.isCanceled()) {
			return CompilerStatus.CANCELED;
		}

		//
		// Source folders
		//
		var hasSourceFile = false;
		for (final var sourceFolder : sourcePathDirectories) {
			if (progress.isCanceled()) {
				return CompilerStatus.CANCELED;
			}
			if (addFolderIfJavaFileDeeply(commandLineArguments, sourceFolder.getAbsoluteFile())) {
				hasSourceFile = true;
			}
		}
		if (!hasSourceFile) {
			return CompilerStatus.NOTHING_TO_COMPILE;
		}
		if (progress.isCanceled()) {
			return CompilerStatus.CANCELED;
		}

		//
		// Run the Eclipse compiler
		//
		final var arguments = new String[commandLineArguments.size()];
		commandLineArguments.toArray(arguments);

		if (logger != null && logger.isLoggable(Level.FINEST)) {
			final var buf = new StringBuilder();
			for (final var str : commandLineArguments) {
				if (!Strings.isEmpty(str)) {
					buf.append(str).append("\n"); //$NON-NLS-1$
				}
			}
			logger.finest(MessageFormat.format(Messages.EcjBatchCompiler_0, buf.toString()));
		}

		if (progress.isCanceled()) {
			return CompilerStatus.CANCELED;
		}

		final var status = BatchCompiler.compile(arguments, outWriter, errWriter,
				new ProgressMonitorCompilationProgress(progress));
		return status ? CompilerStatus.COMPILATION_SUCCESS : CompilerStatus.COMPILATION_FAILURE;
	}

	/** Replies the Java version that is compatible with the command line of ECJ.
	 *
	 * @param javaVersion the Java version to validate.
	 * @return the validated Java version.
	 */
	public static String ensureCompatibleJavaVersion(JavaVersion javaVersion) {
		switch (javaVersion) {
		case JAVA21:
			// The command line of ECJ accepts the version 19 as the maximum
			return "19"; //$NON-NLS-1$
		case JAVA8:
		case JAVA9:
		case JAVA10:
		case JAVA11:
		case JAVA17:
		default:
			break;
		}
		return javaVersion.getQualifier();
	}

	/** Wrap a Eclipse IProgressMonitor into a JDT compilation progress.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version batchcompiler 0.15.1 20250911-224823
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid batchcompiler
	 */
	private static class ProgressMonitorCompilationProgress extends CompilationProgress {

		private final IProgressMonitor monitor;

		/** Constructor.
		 * @param monitor the progress monitor.
		 */
		ProgressMonitorCompilationProgress(IProgressMonitor monitor) {
			assert monitor != null;
			this.monitor = monitor;
		}

		@Override
		public void begin(int remainingWork) {
			//
		}

		@Override
		public void done() {
			//
		}

		@Override
		public boolean isCanceled() {
			return this.monitor.isCanceled();
		}

		@Override
		public void setTaskName(String name) {
			this.monitor.subTask(name);
		}

		@Override
		public void worked(int workIncrement, int remainingWork) {
			//
		}

	}

}

