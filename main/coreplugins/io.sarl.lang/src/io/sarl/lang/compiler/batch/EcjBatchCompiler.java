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
import java.io.PrintWriter;
import java.util.Iterator;
import java.util.List;

import com.google.common.collect.Lists;
import com.google.inject.Singleton;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.compiler.CompilationProgress;
import org.eclipse.jdt.core.compiler.batch.BatchCompiler;
import org.eclipse.xtext.util.Strings;
import org.slf4j.Logger;

/** A wrapper on top of the Eclipse Compiler for Java (ECJ).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@Singleton
public class EcjBatchCompiler implements IJavaBatchCompiler {

	@Override
	@SuppressWarnings({ "checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity",
		"checkstyle:parameternumber" })
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
		commandLineArguments.add("-" + javaVersion); //$NON-NLS-1$
		commandLineArguments.add("-proceedOnError"); //$NON-NLS-1$
		if (!Strings.isEmpty(encoding)) {
			commandLineArguments.add("-encoding"); //$NON-NLS-1$
			commandLineArguments.add(encoding);
		}
		if (progress.isCanceled()) {
			return false;
		}

		for (final File sourceFolder : sourcePathDirectories) {
			if (progress.isCanceled()) {
				return false;
			}
			if (sourceFolder.exists()) {
				commandLineArguments.add(sourceFolder.getAbsolutePath());
			}
		}

		final String[] arguments = new String[commandLineArguments.size()];
		commandLineArguments.toArray(arguments);

		if (logger != null && logger.isDebugEnabled()) {
			logger.debug(Messages.EcjBatchCompiler_0, Strings.concat("\n", commandLineArguments)); //$NON-NLS-1$
		}

		if (progress.isCanceled()) {
			return false;
		}

		return BatchCompiler.compile(arguments, outWriter, errWriter,
				new ProgressMonitorCompilationProgress(progress));
	}

	/** Wrap a Eclipse IProgressMonitor into a JDT compilation progress.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
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
