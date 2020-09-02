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

package io.sarl.maven.compiler;

import com.google.common.base.Strings;
import org.apache.maven.project.MavenProject;

import io.sarl.lang.compiler.batch.EcjBatchCompiler;
import io.sarl.lang.compiler.batch.IJavaBatchCompiler;
import io.sarl.lang.compiler.batch.JavacBatchCompiler;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;

/**
 * Type of Java compielr to use by the SARL compiler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public enum JavaCompiler {
	/** The Java compiler that is embedded in the curent Maven context.
	 */
	MAVEN {
		@Override
		public IJavaBatchCompiler newCompilerInstance(MavenProject project, MavenHelper helper, boolean isTestContext) {
			return new MavenBatchCompiler(helper, isTestContext);
		}
	},

	/** No Java compiler.
	 */
	NONE {
		@Override
		public IJavaBatchCompiler newCompilerInstance(MavenProject project, MavenHelper helper, boolean isTestContext) {
			return SarlBatchCompiler.newDefaultJavaBatchCompiler();
		}
	},

	/** Eclipse Compiler for Java (ECJ).
	 */
	ECJ {
		@Override
		public IJavaBatchCompiler newCompilerInstance(MavenProject project, MavenHelper helper, boolean isTestContext) {
			return new EcjBatchCompiler();
		}
	},

	/** Oracle Java Compiler (javac).
	 */
	JAVAC {
		@Override
		public IJavaBatchCompiler newCompilerInstance(MavenProject project, MavenHelper helper, boolean isTestContext) {
			return new JavacBatchCompiler();
		}
	};

	/** Parse the given case insensitive string for obtaining the java compiler.
	 *
	 * @param name the string to parse.
	 * @return the java compiler, or {@code null} if none.
	 */
	public static JavaCompiler valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			return null;
		}
		try {
			return valueOf(name.toUpperCase());
		} catch (Throwable exception) {
			return null;
		}
	}

	/** Replies the name that is preferred within the Maven configuration.
	 *
	 * @return the name.
	 */
	public String getNameInMavenConfiguration() {
		return name().toLowerCase();
	}

	/** Create an instance of the Java batch compiler, without injection.
	 *
	 * @param project the current Maven project.
	 * @param helper the Maven helper for accessing special services.
	 * @param isTestContext indicates if the compiler is created within the context of test code compilation.
	 * @return the Java batch compiler, never {@code null}.
	 */
	public abstract IJavaBatchCompiler newCompilerInstance(MavenProject project, MavenHelper helper, boolean isTestContext);

	/** Replies the default Java compiler.
	 *
	 * @return the java compiler.
	 */
	public static JavaCompiler getDefault() {
		return JAVAC;
	}

}
