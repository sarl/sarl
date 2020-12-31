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

package io.sarl.sre.boot.configs.subconfigs;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.base.Strings;

/**
 * Type of root context at boot time.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8.0
 */
public enum RootContextType {

	/** The identifier is the default one.
	 */
	DEFAULT,

	/** The identifier is computed randomly.
	 */
	RANDOM,

	/** The identifier is computed from the name of the boot agent.
	 */
	BOOT_AGENT_NAME;

	/** Parse the given case insensitive string for obtaining the type.
	 *
	 * @param name the string to parse.
	 * @return the type.
	 * @throws NullPointerException when the specified name is null
	 */
	@JsonCreator
	public static RootContextType valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			throw new NullPointerException("name is null"); //$NON-NLS-1$
		}
		try {
			final RootContextType type = valueOf(name.toUpperCase());
			if (type != null) {
				return type;
			}
		} catch (Throwable exception) {
			//
		}
		throw new IllegalArgumentException("illegal value for name: " + name); //$NON-NLS-1$
	}

	/** Replies the Json string representation of this type.
	 *
	 * @return the Json string representation.
	 */
	@JsonValue
	public String toJsonString() {
		return name().toLowerCase();
	}

	/** Replies the default type of context id computation.
	 *
	 * @return the default type.
	 */
	public static RootContextType getDefault() {
		return DEFAULT;
	}

	/** Replies the Json labels for the types of context id.
	 *
	 * @return the labels.
	 */
	public static String getJsonLabels() {
		final StringBuilder buffer = new StringBuilder();
		boolean first = true;
		for (final RootContextType type : values()) {
			if (first) {
				first = false;
			} else {
				buffer.append(", "); //$NON-NLS-1$
			}
			buffer.append(type.toJsonString());
		}
		return buffer.toString();
	}

}
