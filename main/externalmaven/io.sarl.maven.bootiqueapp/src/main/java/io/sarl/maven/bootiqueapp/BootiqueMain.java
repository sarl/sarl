/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2020 the original authors or authors.
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

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.common.base.Strings;
import com.google.inject.CreationException;
import com.google.inject.ProvisionException;
import com.google.inject.spi.Message;
import io.bootique.BQModuleProvider;
import io.bootique.BQRuntime;
import io.bootique.Bootique;
import io.bootique.BootiqueException;
import io.bootique.command.CommandOutcome;
import io.bootique.help.HelpOption;
import io.bootique.help.HelpOptions;
import io.bootique.meta.application.ApplicationMetadata;

import io.sarl.util.JulPatternFormatter;

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

	/** Default log format for a bootique app.
	 */
	public static final java.lang.String DEFAULT_LOG_FORMAT = "%-5p %m%n";

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
	 * @param sarlLanguageSetup indicates if the SARL language setup should be realized.
	 * @param args the command line arguments.
	 * @return the runtime.
	 */
	protected BQRuntime createRuntime(String... args) {
		Bootique bootique = Bootique.app(args).autoLoadModules();
		if (this.providers != null) {
			for (final BQModuleProvider provider : this.providers) {
				bootique = bootique.module(provider);
			}
		}
		final BQRuntime runtime = bootique.createRuntime();
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
		final Logger rootLogger = Logger.getGlobal();
		try {
			// Configure the root logger with the default bootique configuration (because it is not yet available)
			for (final Handler handler : rootLogger.getHandlers()) {
				handler.setFormatter(new JulPatternFormatter(DEFAULT_LOG_FORMAT));
			}			
			if (isExperimental()) {
				rootLogger.warning(Messages.BootiqueMain_0);
			}
			final BQRuntime runtime = createRuntime(args);

			final CommandOutcome outcome = runtime.run();
			if (!outcome.isSuccess() && outcome.getException() != null) {
				rootLogger.log(Level.SEVERE, outcome.getMessage(), outcome.getException());
			}
			return outcome.getExitCode();
		} catch (BootiqueException exception) {
			final CommandOutcome outcome = exception.getOutcome();
			if (outcome != null) {
				if (outcome.getException() != null) {
					rootLogger.log(Level.SEVERE, outcome.getMessage(), outcome.getException());
				} else {
					rootLogger.severe(outcome.getMessage());
				}
				return outcome.getExitCode();
			}
			rootLogger.severe(exception.getLocalizedMessage());
		} catch (ProvisionException exception) {
			final Set<String> msgs = new HashSet<>();
			for (final Message message : exception.getErrorMessages()) {
				if (message != null) {
					final String msg = message.getMessage();
					if (!Strings.isNullOrEmpty(msg) && msgs.add(msg)) {
						rootLogger.severe(msg);
					}
				}
			}
		} catch (CreationException exception) {
			final Set<String> msgs = new HashSet<>();
			for (final Message message : exception.getErrorMessages()) {
				if (message != null) {
					final String msg = message.getMessage();
					if (!Strings.isNullOrEmpty(msg) && msgs.add(msg)) {
						rootLogger.severe(msg);
					}
				}
			}
		} catch (Throwable exception) {
			rootLogger.log(Level.SEVERE, exception.getLocalizedMessage(), exception);
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

		/*application.getCommands().forEach(c -> {
			helpOptions.add(c.asOption());
			c.getOptions().forEach(o -> helpOptions.add(o));
		});*/

		application.getOptions().forEach(o -> helpOptions.add(o));

		return helpOptions.getOptions();
	}

}
