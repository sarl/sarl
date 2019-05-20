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

package io.sarl.maven.bootiqueapp;

import java.util.List;

import com.google.common.base.Throwables;
import com.google.inject.Injector;
import com.google.inject.ProvisionException;
import io.bootique.BQModuleProvider;
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
public class BootiqueMain {

	/** Return code when failure.
	 */
	public static final int ERROR_CODE = 255;

	private final BQModuleProvider[] providers;

	private final boolean experimental;

	/** Constructor.
	 *
	 * @param providers the providers of module that injects application-specific components.
	 */
	public BootiqueMain(BQModuleProvider... providers) {
		this(false, providers);
	}

	/** Constructor.
	 *
	 * @param experimental indicates if the application is experimental.
	 * @param providers the providers of module that injects application-specific components.
	 */
	public BootiqueMain(boolean experimental, BQModuleProvider... providers) {
		this.experimental = experimental;
		this.providers = providers;
	}

	/** Replies if this application is an experimental application.
	 *
	 * <p>Usually a warning message is displayed at launch time when the application is experimental.
	 *
	 * @return {@code true} if experimental.
	 */
	public boolean isExperimental() {
		return this.experimental;
	}

	/** Create the compiler runtime.
	 *
	 * @param args the command line arguments.
	 * @return the runtime.
	 */
	protected BQRuntime createRuntime(String... args) {
		SARLStandaloneSetup.doPreSetup();
		Bootique bootique = Bootique.app(args).autoLoadModules();
		if (this.providers != null) {
			for (final BQModuleProvider provider : this.providers) {
				bootique = bootique.module(provider);
			}
		}
		final BQRuntime runtime = bootique.createRuntime();
		SARLStandaloneSetup.doPostSetup(runtime.getInstance(Injector.class));
		return runtime;
	}

	/** Run the associated commands.
	 *
	 * <p>This function runs the command and exits with the return code.
	 *
	 * @param args the command line arguments.
	 * @return the exit code.
	 */
	public int runCommand(String... args) {
		try {
			if (isExperimental()) {
				Logger.getRootLogger().warn(Messages.BootiqueMain_0);
			}
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
		return ERROR_CODE;
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
