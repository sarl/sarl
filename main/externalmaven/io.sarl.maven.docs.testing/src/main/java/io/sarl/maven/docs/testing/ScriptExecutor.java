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

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.function.UnaryOperator;

import com.google.inject.ImplementedBy;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.compiler.batch.ICompilatedResourceReceiver;

/** Represents an code executor.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@ImplementedBy(SarlScriptExecutor.class)
public interface ScriptExecutor {

	/** Name of the system property that should contains the filename of the
	 * documentation file that is currently parsed.
	 * @since 0.12
	 */
	public static final String PROP_CURRENT_FILE = "SARL_DOC_CURRENT_FILE"; //$NON-NLS-1$

	/** Name of the system property that should contains the filename of the
	 * folder in which is located the documentation file that is currently parsed.
	 * @since 0.12
	 */
	public static final String PROP_CURRENT_FOLDER = "SARL_DOC_CURRENT_FOLDER"; //$NON-NLS-1$

	/** Name of the system property that should contains class loader for the executed script.
	 * @since 0.12
	 */
	public static final String PROP_CLASS_LOADER = "SARL_DOC_SCRIPT_CLASS_LOADER"; //$NON-NLS-1$

	/** Change the folder that the executor should used for its temporary data.
	 *
	 * @param dirname the folder name.
	 */
	void setTempFolder(File dirname);

	/** Change the classpath that the compiler should used.
	 *
	 * @param classpath the classpath.
	 */
	void setClassPath(String classpath);

	/** Change the module-path that the compiler should used.
	 *
	 * @param modulePath the module-path.
	 * @since 0.12
	 */
	void setModulePath(String modulePath);

	/** Change the builder of class loader for this executor.
	 * The contract is that the builder replies a class loader that is child of the class loader
	 * given as argument to the builder's creation function.
	 *
	 * @param builder class loader.
	 * @since 0.12
	 */
	void setClassLoaderBuilder(UnaryOperator<ClassLoader> builder);

	/** Change the version of the Java sources.
	 *
	 * @param version the version.
	 */
	void setJavaSourceVersion(String version);

	/** Replies if the version of Java supports Java modules.
	 *
	 * @return {@code true} if the Java modules are supported.
	 * @since 0.12
	 */
	@Pure
	boolean isModuleSupported();

	/** Compile the given code and replies the issues.
	 *
	 * @param lineno the line number where the code is located.
	 * @param code the code to compile.
	 * @return the issues.
	 * @throws Exception if compilation failed.
	 */
	default List<String> compile(int lineno, String code) throws Exception {
		List<String> issues = new ArrayList<>();
		final CompiledFile file = compile(lineno, code, issues, null);
		if (file != null && file.getRootFolder() != null) {
			FileSystem.delete(file.getRootFolder());
		}
		return issues;
	}

	/** Compile the given code and replies the issues.
	 *
	 * @param lineno the line number where the code is located.
	 * @param code the code to compile.
	 * @param issues the issues that were found in the code.
	 * @param receiver the listener on the succesfully compiled resources.
	 * @return the file which contains the compiled code.
	 * @throws Exception if compilation failed.
	 */
	CompiledFile compile(int lineno, String code, List<String> issues, ICompilatedResourceReceiver receiver) throws Exception;

	/** Execute the given code for obtaining a string.
	 *
	 * @param lineno the line number where the code is located.
	 * @param code the code to execute.
	 * @return the result of the execution.
	 * @throws Exception any error during execution.
	 */
	Object execute(int lineno, String code) throws Exception;

	/** Represents a compiled file.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	public final class CompiledFile {

		private final File rootFolder;

		private final File file;

		/** Constructor.
		 *
		 * @param root the root folder.
		 * @param file the compiled file.
		 */
		public CompiledFile(File root, File file) {
			this.rootFolder = root;
			this.file = file;
		}

		/** Replies the root folder.
		 *
		 * @return the root folder.
		 */
		public File getRootFolder() {
			return this.rootFolder;
		}

		/** Replies the compiled file.
		 *
		 * @return the compiled file.
		 */
		public File getCompiledFile() {
			return this.file;
		}

	}
}
