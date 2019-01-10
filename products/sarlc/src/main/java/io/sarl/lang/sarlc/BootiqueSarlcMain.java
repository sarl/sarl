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

import com.google.common.base.Throwables;
import com.google.inject.Injector;
import com.google.inject.ProvisionException;
import io.bootique.BQRuntime;
import io.bootique.Bootique;
import io.bootique.command.CommandOutcome;
import io.bootique.help.HelpOption;
import io.bootique.help.HelpOptions;
import io.bootique.meta.application.ApplicationMetadata;
import org.apache.log4j.Logger;

import io.sarl.lang.SARLStandaloneSetup;

/** Class that implements the standard main function for running a SARL application
 * with bootique modules.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class BootiqueSarlcMain {

	/** Create the compiler runtime.
	 *
	 * @param args the command line arguments.
	 * @return the runtime.
	 */
	@SuppressWarnings("static-method")
	protected BQRuntime createRuntime(String... args) {
		SARLStandaloneSetup.doPreSetup();
		final BQRuntime runtime = Bootique.app(args).autoLoadModules().createRuntime();
		SARLStandaloneSetup.doPostSetup(runtime.getInstance(Injector.class));
		return runtime;
	}

	/** Run the batch compiler.
	 *
	 * <p>This function runs the compiler and exits with the return code.
	 *
	 * @param args the command line arguments.
	 * @return the exit code.
	 */
	public int runCompiler(String... args) {
		try {
			final BQRuntime runtime = createRuntime(args);
			final CommandOutcome outcome = runtime.run();
			if (!outcome.isSuccess() && outcome.getException() != null) {
				Logger.getRootLogger().error(outcome.getMessage(), outcome.getException());
			}
			return outcome.getExitCode();
		} catch (ProvisionException exception) {
			final Throwable ex = Throwables.getRootCause(exception);
			if (ex != null) {
				Logger.getRootLogger().error(ex.getLocalizedMessage());
			} else {
				Logger.getRootLogger().error(exception.getLocalizedMessage());
			}
		} catch (Throwable exception) {
			Logger.getRootLogger().error(exception.getLocalizedMessage());
		}
		return Constants.ERROR_CODE;
	}

	/** Replies the options of the program.
	 *
	 * @return the options of the program.
	 */
	public List<HelpOption> getOptions() {
		final BQRuntime runtime = createRuntime();
		final ApplicationMetadata application = runtime.getInstance(ApplicationMetadata.class);
		final HelpOptions helpOptions = new HelpOptions();

		application.getCommands().forEach(c -> {
			helpOptions.add(c.asOption());
			c.getOptions().forEach(o -> helpOptions.add(o));
		});

		application.getOptions().forEach(o -> helpOptions.add(o));

		return helpOptions.getOptions();
	}

}
