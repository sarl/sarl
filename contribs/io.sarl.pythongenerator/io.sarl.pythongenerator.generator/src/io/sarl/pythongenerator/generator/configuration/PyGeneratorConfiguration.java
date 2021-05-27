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

package io.sarl.pythongenerator.generator.configuration;

/** Configuration for the SARL-to-Python generator.
 *
 * @author <a href="http://www.ciad-lab.fr/author-10836/">St&eacute;phane Galland</a>
 * @version io.sarl.pythongenerator.generator 0.12.0 20210527-171007
 * @mavengroupid io.sarl.pythongenerator
 * @mavenartifactid io.sarl.pythongenerator.generator
 * @since 0.8
 */
public class PyGeneratorConfiguration {

	/** Default value for the flag that indicates if the JVM types (not in {@code io.sarl.*})
	 * are implicitly supported by the type converter.
	 */
	public static final boolean DEFAULT_IMPLICIT_JVM_TYPES = true;

	/**
	 * Whether the JVM types are implicitly recognized by the type converter..
	 */
	private boolean isImplicitJvmTypes = DEFAULT_IMPLICIT_JVM_TYPES;

	/** Replies if the JVM types (not in {@code io.sarl.*}) are implicitly supported by the type converter.
	 *
	 * @return {@code true} if the implicit types are converted. {@code false} if no implicit type is converted.
	 */
	public boolean isImplicitJvmTypes() {
		return this.isImplicitJvmTypes;
	}

	/** Set if the JVM types (not in {@code io.sarl.*}) are implicitly supported by the type converter.
	 *
	 * @param enable {@code true} if the implicit types are converted. {@code false} if no implicit type is converted.
	 */
	public void setImplicitJvmTypes(boolean enable) {
		this.isImplicitJvmTypes = enable;
	}

}
