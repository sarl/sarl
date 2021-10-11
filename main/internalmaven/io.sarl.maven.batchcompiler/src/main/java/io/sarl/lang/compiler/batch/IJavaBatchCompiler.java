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
import java.util.logging.Logger;

import com.google.inject.ImplementedBy;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.xtext.xbase.lib.Pure;

/** An object that represents a Java compiler.
 *
 * <p>If not specific injection configuration is defined, the {@link JavacBatchCompiler Javac batch compiler}
 * is used as the default implementation of this interface.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@ImplementedBy(EcjBatchCompiler.class)
public interface IJavaBatchCompiler {

	/** Run the Java compiler.
	 *
	 * @param classDirectory the output directory.
	 * @param sourcePathDirectories the source directories.
	 * @param classPathEntries classpath entries.
	 * @param modulePathEntries classpath entries. No more used.
	 * @param javaVersion the version of Java that is the target, e.g. {@code 11}.
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
	CompilerStatus compile(File classDirectory, Iterable<File> sourcePathDirectories,
			Iterable<File> classPathEntries,
			Iterable<File> modulePathEntries,
			String javaVersion,
			String encoding,
			boolean isCompilerMoreVerbose,
			OptimizationLevel optimizationLevel,
			PrintWriter outWriter,
			PrintWriter errWriter,
			Logger logger,
			IProgressMonitor progress);

	/** Replies the name of the Java compiler.
	 *
	 * @return the name, never {@code null}.
	 * @since 0.12
	 */
	@Pure
	String getName();

}
