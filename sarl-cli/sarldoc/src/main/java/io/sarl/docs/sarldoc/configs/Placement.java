/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.sarldoc.configs;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;
import java.util.TreeSet;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.base.Strings;

/**
 * Placement for a custom tag.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public enum Placement {

	/** Disable the tag.
	 */
	DISABLE('X', Messages.Placement_0),

	/** All elements.
	 */
	ALL('a', Messages.Placement_1),
	/** Into the overview.
	 */
	OVERVIEW('o', Messages.Placement_2),

	/** Into the packages.
	 */
	PACKAGES('p', Messages.Placement_3),

	/** Into the types (classes and interfaces).
	 */
	TYPES('t', Messages.Placement_4),

	/** Into constructors.
	 */
	CONSTRUCTORS('c', Messages.Placement_5),

	/** Into the methods.
	 */
	METHODS('m', Messages.Placement_6),

	/** Into the fields.
	 */
	FIELDS('f', Messages.Placement_7);

	private final char placement;

	private final String documentation;

	/** Constructor.
	 *
	 * @param placement the placement character.
	 * @param documentation the placement documentation.
	 */
	Placement(char placement, String documentation) {
		this.placement = placement;
		this.documentation = documentation;
	}

	/** Replies the character representation of this placement.
	 *
	 * @return the character representation of this placement.
	 */
	public char toChar() {
		return this.placement;
	}

	/** Replies the documentation text associated to this placement.
	 *
	 * @return the documentation text for this placement.
	 */
	public String getDocumentation() {
		return this.documentation;
	}

	/** Replies the default visibility for the documented elements.
	 *
	 * @return the default visibility.
	 */
	public static Placement getDefault() {
		return ALL;
	}

	/** Parse the given case insensitive string for obtaining the placement.
	 *
	 * @param name the string to parse.
	 * @return the placement.
	 * @throws NullPointerException when the specified name is null
	 */
	@JsonCreator
	public static Placement valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			throw new NullPointerException("Name is null"); //$NON-NLS-1$
		}
		return valueOf(name.toUpperCase());
	}

	/** Parse the given string for obtaining the placements.
	 *
	 * <p>The string contains the placements' characters.
	 *
	 * @param text the string to parse.
	 * @return the placements.
	 */
	public static List<Placement> valuesOf(String text) {
		final var all = new TreeMap<Character, Placement>();
		for (final var placement : values()) {
			all.put(Character.valueOf(placement.toChar()), placement);
		}

		final var placements = new TreeSet<Placement>();
		final var len = text.length();
		for (var i = 0; i < len; ++i) {
			final var p = all.get(Character.valueOf(text.charAt(i)));
			if (p != null) {
				placements.add(p);
			}
		}

		return new ArrayList<>(placements);
	}

	/** Replies the Json string representation of this placement.
	 *
	 * @return the Json string representation.
	 */
	@JsonValue
	public String toJsonString() {
		return name().toLowerCase();
	}

}
