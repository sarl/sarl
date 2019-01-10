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

package io.sarl.lang.sarlc.configs.subconfigs;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.base.Strings;

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
	/** No Java compiler.
	 */
	NONE {
		@Override
		public IJavaBatchCompiler newCompilerInstance() {
			return SarlBatchCompiler.newDefaultJavaBatchCompiler();
		}
	},

	/** Eclipse Compiler for Java (ECJ).
	 */
	ECJ {
		@Override
		public IJavaBatchCompiler newCompilerInstance() {
			return new EcjBatchCompiler();
		}
	},

	/** Oracle Java Compiler (javac).
	 */
	JAVAC {
		@Override
		public IJavaBatchCompiler newCompilerInstance() {
			return new JavacBatchCompiler();
		}
	};

	/** Parse the given case insensitive string for obtaining the java compiler.
	 *
	 * @param name the string to parse.
	 * @return the java compiler.
	 */
	@JsonCreator
	public static JavaCompiler valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			throw new NullPointerException("Name is null"); //$NON-NLS-1$
		}
		return valueOf(name.toUpperCase());
	}

	/** Replies the Json string representation of this java compiler.
	 *
	 * @return the Json string representation.
	 */
	@JsonValue
	public String toJsonString() {
		return name().toLowerCase();
	}

	/** Create an instance of a Java compiler, without injection.
	 *
	 * @return the compiler instance, never {@code null}.
	 */
	public abstract IJavaBatchCompiler newCompilerInstance();

	/** Replies the default compiler to be used.
	 *
	 * @return the compiler.
	 */
	public static JavaCompiler getDefault() {
		return ECJ;
	}

}
