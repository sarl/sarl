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

package io.sarl.lang.sarlc;

import io.sarl.lang.sarlc.modules.general.SarlcApplicationModuleProvider;
import io.sarl.maven.bootiqueapp.BootiqueMain;
import io.sarl.maven.bootiqueapp.batchcompiler.BootiqueBatchCompilerMain;

/** Main entry point for the SARL batch compiler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
public final class Main {

	private Main() {
		//
	}

	/** Main program of the batch compiler.
	 *
	 * <p>This function stops the VM.
	 *
	 * @param args the command line arguments.
	 * @see #run(String[])
	 */
	public static void main(String[] args) {
		System.exit(run(args));
	}

	/** Main program of the batch compiler.
	 *
	 * <p>This function does not stop the VM.
	 *
	 * @param args the command line arguments.
	 * @return the exit code.
	 * @see #main(String[])
	 */
	public static int run(String... args) {
		return createMainObject().runCommand(args);
	}

	/** Replies the default name of the program.
	 *
	 * @return the default name of the program.
	 */
	public static String getDefaultCompilerProgramName() {
		return Constants.PROGRAM_NAME;
	}

	/** Create the instance of the bootique main launcher.
	 *
	 * @return the main launcher.
	 */
	protected static BootiqueMain createMainObject() {
		return new BootiqueBatchCompilerMain(new SarlcApplicationModuleProvider());
	}

}
