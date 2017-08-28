/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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
import java.util.List;

import com.google.inject.ImplementedBy;

import io.sarl.lang.compilation.compiler.batch.ICompilatedResourceReceiver;

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

	/** Change the folder that the executor should used for its temporary data.
	 *
	 * @param dirname the folder name.
	 */
	void setTempFolder(File dirname);

	/** Change the classpath that the executor should used.
	 *
	 * @param classpath the classpath.
	 */
	void setClassPath(String classpath);

	/** Change the boot classpath that the executor should used.
	 *
	 * @param classpath the boot classpath.
	 */
	void setBootClassPath(String classpath);

	/** Change the version of the Java sources.
	 *
	 * @param version the version.
	 */
	void setJavaSourceVersion(String version);

	/** Compile the given code and replies the issues.
	 *
	 * @param lineno the line number where the code is located.
	 * @param code the code to compile.
	 * @return the issues.
	 * @throws Exception if compilation failed.
	 */
	List<String> compile(int lineno, String code) throws Exception;

	/** Compile the given code and replies the issues.
	 *
	 * @param lineno the line number where the code is located.
	 * @param code the code to compile.
	 * @param issues the issues that were found in the code.
	 * @param receiver the listener on the succesfully compiled resources.
	 * @return the file which contains the compiled code.
	 * @throws Exception if compilation failed.
	 */
	File compile(int lineno, String code, List<String> issues, ICompilatedResourceReceiver receiver) throws Exception;

	/** Execute the given code for obtaining a string.
	 *
	 * @param lineno the line number where the code is located.
	 * @param code the code to execute.
	 * @return the result of the execution.
	 * @throws Exception any error during execution.
	 */
	Object execute(int lineno, String code) throws Exception;

}
