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

package io.sarl.apputils.bootiqueapp;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import io.bootique.BQModuleProvider;
import io.bootique.BQRuntime;
import io.bootique.Bootique;
import io.bootique.BootiqueException;
import io.bootique.di.Injector;
import io.bootique.help.HelpOption;

import io.sarl.apputils.bootiqueapp.mdhelp.GenerateMarkdownHelpCommand;
import io.sarl.apputils.bootiqueapp.utils.JulPatternFormatter;

/** Class that implements the standard main function for running a SARL application
 * with bootique modules.
 *
 * @author $Author: sgalland$
 * @version bootiqueapp 0.15.0 20250909-115749
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid bootiqueapp
 * @since 0.8
 */
@SuppressWarnings("removal")
public class BootiqueMain {

	/** Return code when failure.
	 */
	public static final int ERROR_CODE = 255;

	/** Default log format for a bootique app.
	 */
	public static final String DEFAULT_LOG_FORMAT = "%-5p %m%n"; //$NON-NLS-1$

	/** Pattern for command-line definition.
	 */
	public static final String D_PATTERN = "(?:(?:\\-\\-?)|(?:\\/))D([^=]+)=(.*)"; //$NON-NLS-1$

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
	 * This function loads any modules available on class-path that expose {@code BQModuleProvider} provider.
	 * Use with caution, you may load more modules than you expected.
	 * Make sure only needed Bootique dependencies are included on class-path.
	 * If in doubt, switch to explicit Module loading via {@link #createRuntime(Class[], String...)}.
	 *
	 * @param args the command line arguments.
	 * @return the runtime.
	 */
	protected BQRuntime createRuntime(String... args) {
		var bootique = Bootique.app(args).autoLoadModules();
		return createRuntime(bootique);
	}

	/** Create the compiler runtime.
	 *
	 * @param modules the list of module to be loaded.
	 * @param args the command line arguments.
	 * @return the runtime.
	 * @since 0.12
	 */
	protected BQRuntime createRuntime(Class<? extends BQModuleProvider>[] modules, String... args) {
		var bootique = Bootique.app(args);
		for (final var providerType : modules) {
			BQModuleProvider provider;
			try {
				provider = providerType.getConstructor().newInstance();
			} catch (Throwable exception) {
				provider = null;
			}
			if (provider != null) {
				bootique = bootique.moduleProvider(provider);
			}
		}
		return createRuntime(bootique);
	}

	private BQRuntime createRuntime(Bootique bootique) {
		var bootiqueInstance = bootique;
		if (this.providers != null) {
			for (final var provider : this.providers) {
				bootiqueInstance = bootiqueInstance.moduleProvider(provider);
			}
		}
		final BQRuntime runtime = bootiqueInstance.createRuntime();
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
		final var rootLogger = Logger.getGlobal();
		try {
			// Configure the root logger with the default bootique configuration (because it is not yet available)
			for (final var handler : rootLogger.getHandlers()) {
				handler.setFormatter(new JulPatternFormatter(DEFAULT_LOG_FORMAT));
			}			
			if (isExperimental()) {
				rootLogger.warning(Messages.BootiqueMain_0);
			}
			final var fargs = filterCommandLineArguments(args);
			final var runtime = createRuntime(fargs);

			final var outcome = runtime.run();
			if (!outcome.isSuccess() && outcome.getException() != null) {
				rootLogger.log(Level.SEVERE, outcome.getMessage(), outcome.getException());
			}
			return outcome.getExitCode();
		} catch (BootiqueException exception) {
			final var outcome = exception.getOutcome();
			if (outcome != null) {
				if (outcome.getException() != null) {
					rootLogger.log(Level.SEVERE, outcome.getMessage(), outcome.getException());
				} else {
					rootLogger.severe(outcome.getMessage());
				}
				return outcome.getExitCode();
			}
			rootLogger.severe(exception.getLocalizedMessage());
		} catch (Throwable exception) {
			rootLogger.log(Level.SEVERE, exception.getLocalizedMessage(), exception);
		}
		return ERROR_CODE;
	}

	/** Replies the options of the program.
	 * This function loads any modules available on class-path that expose {@code BQModuleProvider} provider.
	 * Use with caution, you may load more modules than you expected.
	 * Make sure only needed Bootique dependencies are included on class-path.
	 * If in doubt, switch to explicit Module loading via {@link #getOptionsForModules(Class...)}.
	 *
	 * @return the options of the program.
	 */
	public final List<HelpOption> getOptions() {
		final var runtime = createRuntime();
		return GenerateMarkdownHelpCommand.getOptions(runtime.getInstance(Injector.class));
	}

	/** Replies the options of the program that are provided by a specific set of modules.
	 *
	 * @param modules the list of modules to be loaded.
	 * @return the options of the program.
	 * @since 0.12
	 */
	public final List<HelpOption> getOptionsForModules(Class<? extends BQModuleProvider>[] modules) {
		final var runtime = createRuntime(modules);
		return GenerateMarkdownHelpCommand.getOptions(runtime.getInstance(Injector.class));
	}

	/** Replies the statically defined modules.
	 *
	 * @param names the list of fully-qualified names of the modules providers.
	 * @return the list of the providers.
	 * @since 0.12
	 */
	@SuppressWarnings("unchecked")
	public static Class<? extends BQModuleProvider>[] getStaticModuleProvidersFor(String[] names) {
		final var providers = new ArrayList<Class<? extends BQModuleProvider>>(names.length);
		for (var i = 0; i < names.length; ++i) {
			try {
				final var type = Class.forName(names[i]);
				if (type != null && BQModuleProvider.class.isAssignableFrom(type)) {
					providers.add(type.asSubclass(BQModuleProvider.class));
				}
			} catch (Throwable exception) {
				//
			}
		}
		return providers.toArray(new Class[providers.size()]);
	}

	/** Filter the command-line arguments for extracting the -D parameters.
	 *
	 * @param args the arguments.
	 * @return the filtered arguments.
	 * @since 0.12
	 */
	public static String[] filterCommandLineArguments(String[] args) {
		final var list = new ArrayList<String>();
		final var pattern = Pattern.compile(D_PATTERN);
		for (final var arg : args) {
			final var matcher = pattern.matcher(arg);
			if (matcher.matches()) {
				final var name = matcher.group(1);
				final var value = matcher.group(2);
				System.setProperty(name, value);
			} else {
				list.add(arg);
			}
		}
		final var tab = new String[list.size()];
		list.toArray(tab);
		return tab;
	}

}
