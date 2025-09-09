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

package io.sarl.docs.sarldoc;

import io.sarl.apputils.bootiqueapp.BootiqueMain;
import io.sarl.apputils.bootiqueapp.batchcompiler.BootiqueBatchCompilerMain;
import io.sarl.docs.sarldoc.modules.commands.SarldocCommandModuleProvider;
import io.sarl.docs.sarldoc.modules.commands.SarldocFakeCommandModuleProvider;
import io.sarl.docs.sarldoc.modules.internal.SarldocApplicationModuleProvider;

/** Main entry point for the SARL API documentation generator.
 *
 * @author $Author: sgalland$
 * @version sarldoc 0.15.0 20250909-115750
 * @mavengroupid io.sarl.cli
 * @mavenartifactid sarldoc
 * @since 0.10
 */
public final class Main {

	private Main() {
		//
	}

	/** Main program of the API documentation generator.
	 *
	 * <p>This function never returns.
	 *
	 * @param args the command line arguments.
	 * @see #run(String...)
	 */
	public static void main(String[] args) {
		System.exit(run(args));
	}

	/** Main program of the API documentation generator.
	 *
	 * <p>This function returns.
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
	public static String getDefaultProgramName() {
		return Constants.PROGRAM_NAME;
	}

	/** Create the instance of the bootique main launcher.
	 *
	 * @return the main launcher.
	 */
	protected static BootiqueMain createMainObject() {
		return new BootiqueBatchCompilerMain(
				new SarldocApplicationModuleProvider(),
				new SarldocCommandModuleProvider(),
				new SarldocFakeCommandModuleProvider());
	}

}
