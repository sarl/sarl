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
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.text.MessageFormat;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceLoader;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

import com.google.common.collect.Lists;
import com.google.inject.Singleton;
import org.apache.commons.io.output.WriterOutputStream;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.Strings;

/** A wrapper on top of the Oracle Java Compiler (javac).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@Singleton
public class JavacBatchCompiler extends AbstractJavaBatchCompiler {

	@Override
	public String getName() {
		return "Javac"; //$NON-NLS-1$
	}
	
	@Override
	@SuppressWarnings({ "checkstyle:parameternumber", "checkstyle:cyclomaticcomplexity",
		"checkstyle:npathcomplexity", "resource" })
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

		//
		// Optimization
		//
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
		String path = buildPath(classPathEntries, progress);
		if (path == null || progress.isCanceled()) {
			return CompilerStatus.CANCELED;
		}
		if (!Strings.isEmpty(path)) {
			commandLineArguments.add("-cp"); //$NON-NLS-1$
			commandLineArguments.add(path);
		}

		//
		// Modulepath
		//
		if (isModuleSupport) {
			path = buildPath(modulePathEntries, progress);
			if (path == null || progress.isCanceled()) {
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
		// Source and target version of Java
		//
		commandLineArguments.add("-source"); //$NON-NLS-1$
		commandLineArguments.add(javaVersion.getQualifier());
		commandLineArguments.add("-target"); //$NON-NLS-1$
		commandLineArguments.add(javaVersion.getQualifier());
		if (javaVersion.isAtLeast(JavaVersion.JAVA11)) {
			//TODO commandLineArguments.add("-release"); //$NON-NLS-1$
			//TODO commandLineArguments.add(javaVersion.getQualifier());
		}

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
		boolean hasSourceFile = false;
		for (final File sourceFolder : sourcePathDirectories) {
			if (progress.isCanceled()) {
				return CompilerStatus.CANCELED;
			}
			if (addJavaFilesDeeply(commandLineArguments, sourceFolder.getAbsoluteFile())) {
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
		// Invoke the javac compiler
		//
		final String[] arguments = new String[commandLineArguments.size()];
		commandLineArguments.toArray(arguments);

		if (logger != null && logger.isLoggable(Level.FINEST)) {
			logger.finest(MessageFormat.format(Messages.JavacBatchCompiler_0, Strings.concat(" ", commandLineArguments))); //$NON-NLS-1$
		}

		final WriterOutputStream stdout = new WriterOutputStream(outWriter, Charset.defaultCharset());

		final JavacErrorStream stderr = new JavacErrorStream(errWriter, logger);

		if (progress.isCanceled()) {
			return CompilerStatus.CANCELED;
		}

		final JavaCompiler systemCompiler = getSystemJavaCompiler();
		final int retcode = systemCompiler.run(null, stdout, stderr, arguments);
		return retcode == 0 ? CompilerStatus.COMPILATION_SUCCESS : CompilerStatus.COMPILATION_FAILURE;
	}

	/** Replies the JDK compiler.
	 *
	 * @return the JDK compiler.
	 * @since 0.12
	 */
	protected JavaCompiler getSystemJavaCompiler() {
		final PrivilegedAction<JavaCompiler> action = () -> ToolProvider.getSystemJavaCompiler();
		final JavaCompiler standardCompiler = AccessController.doPrivileged(action);
		if (standardCompiler == null) {
			// This branch is defined for solving the problem introduced by JMS modules.
			ServiceLoader<JavaCompiler> sl = ServiceLoader.load(JavaCompiler.class);
			final Iterator<JavaCompiler> iter = sl.iterator();
			while (iter.hasNext()) {
				JavaCompiler tool = iter.next();
				if (tool != null) {
					return tool;
				}
			}
		}
		return standardCompiler;
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
			super(writer, Charset.defaultCharset());
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
