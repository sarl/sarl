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

package io.sarl.lang.sarlc;

import java.util.List;

import io.bootique.help.HelpOption;

import io.sarl.lang.sarlc.modules.general.SarlcApplicationModuleProvider;
import io.sarl.maven.bootiqueapp.BootiqueMain;

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
	 * @param args the command line arguments.
	 */
	public static void main(String[] args) {
		final int retCode = createMainObject().runCommand(args);
		System.exit(retCode);
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
		return new BootiqueMain(new SarlcApplicationModuleProvider());
	}

	/** Replies the options of the program.
	 *
	 * @return the options of the program.
	 */
	public static List<HelpOption> getOptions() {
		return createMainObject().getOptions();
	}

}
