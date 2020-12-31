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

package io.sarl.sarldoc.configs;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.base.Strings;

/**
 * Visibility of members.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public enum Visibility {
	/** Only public members are considered.
	 */
	PUBLIC,

	/** Only protected members are considered.
	 */
	PROTECTED,

	/** Only package members are considered.
	 */
	PACKAGE,

	/** Only private members are considered.
	 */
	PRIVATE;

	/** Replies the default visibility for the documented elements.
	 *
	 * @return the default visibility.
	 */
	public static Visibility getDefault() {
		return PROTECTED;
	}

	/** Parse the given case insensitive string for obtaining the visibility.
	 *
	 * @param name the string to parse.
	 * @return the visibility.
	 * @throws NullPointerException when the specified name is null
	 */
	@JsonCreator
	public static Visibility valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			throw new NullPointerException("Name is null"); //$NON-NLS-1$
		}
		return valueOf(name.toUpperCase());
	}

	/** Replies the Json string representation of this visibility.
	 *
	 * @return the Json string representation.
	 */
	@JsonValue
	public String toJsonString() {
		return name().toLowerCase();
	}

}
