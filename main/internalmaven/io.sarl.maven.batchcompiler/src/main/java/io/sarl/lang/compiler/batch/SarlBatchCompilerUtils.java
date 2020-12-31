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

package io.sarl.lang.compiler.batch;

import java.io.File;
import java.util.Iterator;
import java.util.ServiceLoader;

import com.google.inject.ImplementedBy;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.SARLVersion;

/** Utility functions for the SARL batch compiler API.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class SarlBatchCompilerUtils {

	/** Create a default Java batch compiler, without injection.
	 *
	 * @return the Java batch compiler, never {@code null}.
	 */
	@Pure
	public static IJavaBatchCompiler newDefaultJavaBatchCompiler() {
		try {
			final ServiceLoader<IJavaBatchCompilerFactory> loader = ServiceLoader.load(IJavaBatchCompilerFactory.class);
			final Iterator<IJavaBatchCompilerFactory> iterator = loader.iterator();
			try {
				while (iterator.hasNext()) {
					final IJavaBatchCompilerFactory factory = iterator.next();
					try {
						if (factory != null) {
							final IJavaBatchCompiler compiler = factory.createBatchCompiler();
							if (compiler != null) {
								return compiler;
							}
						}
					} catch (Throwable exception) {
						// Do nothing with the exception
					}
				}
			} catch (Throwable exception) {
				// Do nothing with the exception
			}
			final ImplementedBy annotation = IJavaBatchCompiler.class.getAnnotation(ImplementedBy.class);
			if (annotation != null) {
				final Class<?> type = annotation.value();
				if (type != null) {
					final Class<? extends IJavaBatchCompiler> ctype = type.asSubclass(IJavaBatchCompiler.class);
					final IJavaBatchCompiler compiler = ctype.getConstructor().newInstance();
					if (compiler != null) {
						return compiler;
					}
					
				}
			}
		} catch (Exception exception) {
			throw new InvalidSarlBatchCompilerError(exception);
		}
		throw new InvalidSarlBatchCompilerError();
	}

	/** Replies the type of the default Java batch compiler.
	 *
	 * @return the type of the default Java batch compiler, never {@code null}.
	 * @since 0.12
	 */
	@Pure
	public static Class<? extends IJavaBatchCompiler> getDefaultJavaBatchCompilerImplementationType() {
		try {
			final ServiceLoader<IJavaBatchCompilerFactory> loader = ServiceLoader.load(IJavaBatchCompilerFactory.class);
			final Iterator<IJavaBatchCompilerFactory> iterator = loader.iterator();
			try {
				while (iterator.hasNext()) {
					final IJavaBatchCompilerFactory factory = iterator.next();
					try {
						if (factory != null) {
							final Class<? extends IJavaBatchCompiler> type = factory.getType();
							if (type != null) {
								return type;
							}
						}
					} catch (Throwable exception) {
						// Do nothing with the exception
					}
				}
			} catch (Throwable exception) {
				// Do nothing with the exception
			}
			final ImplementedBy annotation = IJavaBatchCompiler.class.getAnnotation(ImplementedBy.class);
			if (annotation != null) {
				final Class<?> type = annotation.value();
				if (type != null) {
					return type.asSubclass(IJavaBatchCompiler.class);
				}
			}
		} catch (Exception exception) {
			throw new InvalidSarlBatchCompilerError(exception);
		}
		throw new InvalidSarlBatchCompilerError();
	}

	/** Parse a Java version.
	 *
	 * @param javaVersion the version of JAva to test.
	 * @return the java version object.
	 * @since 0.12
	 */
	@Pure
	public static JavaVersion parserJavaVersion(String javaVersion) {
		String version = javaVersion;
		if (Strings.isEmpty(version)) {
			version = SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT;
		}
		return JavaVersion.fromQualifier(version);
	}

	/** Replies if the given version of Java supports the modules.
	 *
	 * @param javaVersion the version of JAva to test.
	 * @return {@code true} if the Java modules are supported.
	 * @since 0.12
	 */
	@Pure
	public static boolean isModuleSupported(String javaVersion) {
		return isModuleSupported(parserJavaVersion(javaVersion));
	}

	/** Replies if the given version of Java supports the modules.
	 *
	 * @param javaVersion the version of JAva to test.
	 * @return {@code true} if the Java modules are supported.
	 * @since 0.12
	 */
	@Pure
	public static boolean isModuleSupported(JavaVersion javaVersion) {
		return javaVersion.isAtLeast(JavaVersion.JAVA9);
	}

	/** Replies if the given folder contains the definition of a module.
	 * A module definition is written into a file named {@code module-info.java}.
	 *
	 * @param folder the folder to test.
	 * @return {@code true} if the folder contains the module definition.
	 * @since 0.12
	 */
	@Pure
	public static boolean isModuleFolder(File folder) {
		final File file = new File(folder, "module-info.java"); // $NON-NLS-1$
		return file.isFile();
	}

}
