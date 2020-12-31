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

import com.google.common.base.Strings;

/** Level of optimization of the Java code.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public enum OptimizationLevel {

	/**
	 * No optimization, debugging information included.
	 */
	G0,

	/**
	 * No optimization, no debugging information included.
	 */
	G1,

	/**
	 * Optimization, no debugging information included.
	 */
	G2;

	/** Replies the default optimization level.
	 *
	 * @return the default optimization level.
	 */
	public static OptimizationLevel getDefault() {
		return G0;
	}

	/** Parse the given case insensitive string for obtaining the optimization level.
	 *
	 * @param name the string to parse.
	 * @return the optimization level, or {@code null} if the string cannot be parsed.
	 */
	public static OptimizationLevel valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			return null;
		}
		try {
			return valueOf(name.toUpperCase());
		} catch (Exception exception) {
			return null;
		}
	}

	/** Replies the string representation of this optimization level.
	 *
	 * @return the string representation.
	 */
	public String getCaseInsensitiveName() {
		return name().toLowerCase();
	}

}
