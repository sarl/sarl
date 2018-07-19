/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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
import io.bootique.help.HelpOption;
import io.bootique.help.HelpOptions;
import io.bootique.meta.application.ApplicationMetadata;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;

import io.sarl.lang.SARLRuntimeModule;
import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.modules.CompilerModule;
import io.sarl.lang.sarlc.modules.LoggingModule;
import io.sarl.lang.sarlc.modules.PrintConfigModule;
import io.sarl.lang.sarlc.modules.SarlcModule;
import io.sarl.lang.sarlc.modules.ValidatorModule;
import io.sarl.lang.sarlc.modules.VersionModule;
import io.sarl.maven.bqextension.Constants;
import io.sarl.maven.bqextension.modules.ApplicationMetadataModule;
import io.sarl.maven.bqextension.modules.ApplicationMetadataUpdater;
import io.sarl.maven.bqextension.modules.DocumentedModuleProvider;

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
	 * <p>This function never returns. It invokes {@link #runCompiler(String...)}
	 * and stop the JVM with the replied exist code.
	 *
	 * @param args the command line arguments.
	 * @see #runCompiler(String...)
	 */
	public static void main(String[] args) {
		final int retCode = runCompiler(args);
		System.exit(retCode);
	}

	/** Create the compiler runtime.
	 *
	 * @param args the command line arguments.
	 * @return the runtime.
	 */
	protected static BQRuntime createRuntime(String... args) {
		SARLStandaloneSetup.doPreSetup();
		Bootique bootique = Bootique.app(args);
		bootique = DocumentedModuleProvider.module(bootique,
				SARLRuntimeModule.class, "The core of SARL runtime."); //$NON-NLS-1$
		bootique = DocumentedModuleProvider.modules(bootique,
				LoggingModule.class, PrintConfigModule.class,
				VersionModule.class, ApplicationMetadataModule.class,
				SarlcModule.class, CompilerModule.class, ValidatorModule.class);
		bootique = bootique.autoLoadModules();
		final BQRuntime runtime = bootique.createRuntime();
		forceApplicationName(runtime);
		SARLStandaloneSetup.doPostSetup(runtime.getInstance(Injector.class));
		return runtime;
	}

	private static void forceApplicationName(BQRuntime runtime) {
		final ApplicationMetadataUpdater updater = runtime.getInstance(ApplicationMetadataUpdater.class);
		final ApplicationMetadata metadata = runtime.getInstance(ApplicationMetadata.class);
		final SarlcConfig sarlcConfig = runtime.getInstance(SarlcConfig.class);
		updater.setName(metadata, sarlcConfig.getCompilerProgramName());
	}

	/** Run the batch compiler.
	 *
	 * <p>This function runs the compiler and exits with the return code.
	 *
	 * @param args the command line arguments.
	 * @return the exit code.
	 * @see #main(String[])
	 */
	public static int runCompiler(String... args) {
		configureLogger();
		try {
			final BQRuntime runtime = createRuntime(args);
			runtime.run();
		} catch (ProvisionException exception) {
			final Throwable ex = Throwables.getRootCause(exception);
			if (ex != null) {
				Logger.getRootLogger().error(ex.getLocalizedMessage());
			} else {
				Logger.getRootLogger().error(exception.getLocalizedMessage());
			}
			return Constants.ERROR_CODE;
		} catch (Throwable exception) {
			Logger.getRootLogger().error(exception.getLocalizedMessage());
			return Constants.ERROR_CODE;
		}
		return Constants.SUCCESS_CODE;
	}

	private static void configureLogger() {
		final Logger root = Logger.getRootLogger();
		root.removeAllAppenders();
		root.addAppender(new ConsoleAppender(
				new PatternLayout(Constants.LOGGER_PATTERN)));
	}

	/** Replies the default name of the program.
	 *
	 * @return the default name of the program.
	 */
	public static String getDefaultCompilerProgramName() {
		return SarlcConfig.COMPILER_PROGRAM_VALUE;
	}

	/** Replies the options of the program.
	 *
	 * @return the options of the program.
	 */
	public static List<HelpOption> getOptions() {
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
