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
package io.sarl.bspl.lang.tests;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.testing.OnTheFlyJavaCompiler2;

import com.google.inject.Binder;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.Provider;
import com.google.inject.util.Modules;

import io.sarl.bspl.lang.BSPLStandaloneSetup;
import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.tests.SARLInjectorProvider;

/** Override the module definition for tests only.
 *
 * <p>This class is implemented for overriding the default Java version of the on-the-fly Java compiler
 * that is used by the testing framework. Indeed, the default Java version for this compiler is Java 6. But,
 * we are expecting another version, as described in {@link SARLVersion}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ExtendedBSPLInjectorProvider extends BSPLInjectorProvider {

	@Override
	protected Injector internalCreateInjector() {
		return new BSPLStandaloneSetup() {
			@Override
			public Injector createInjector() {
				return Guice.createInjector(Modules.override(createRuntimeModule()).with(createRuntimeTestModule()));
			}
		}.createInjectorAndDoEMFRegistration();
	}

	/** Create the module for tests of the runtime libraries.
	 *
	 * @return the module.
	 */
	@SuppressWarnings("static-method")
	protected Module createRuntimeTestModule() {
		return new Module() {

			@Override
			public void configure(Binder binder) {
				binder.bind(OnTheFlyJavaCompiler2.class).toProvider(JavaCompilerProvider.class).asEagerSingleton();
			}

		};
	}

	/** Provider of the on-the-fly Java compiler.
	 *
	 * <p>This class is implemented for overriding the default Java version of the on-the-fly Java compiler
	 * that is used by the testing framework. Indeed, the default Java version for this compiler is Java 6. But,
	 * we are expecting another version, as described in {@link SARLVersion}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class JavaCompilerProvider implements Provider<OnTheFlyJavaCompiler2> {

		private static JavaVersion parseJavaVersion(String javaVersion) {
			var version = javaVersion;
			if (Strings.isEmpty(version)) {
				version = SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT;
			}
			return JavaVersion.fromQualifier(version);
		}

		@Override
		public OnTheFlyJavaCompiler2 get() {
			final var jversion = parseJavaVersion(null);
			return new OnTheFlyJavaCompiler2(
					SARLInjectorProvider.class.getClassLoader(),
					jversion);
		}

	}

	/** Provider of the loggers.
	 *
	 * <p>The logger is configured to be not verbose.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class LoggerProvider implements Provider<Logger> {

		@Override
		public Logger get() {
			final var logger = Logger.getAnonymousLogger();
			logger.setLevel(Level.OFF);
			return logger;
		}

	}

}
