/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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
 *
 *------- FORKED SOURCE CODE:
 *
 * THIS CODE IS FORKED FROM JDK.JAVADOC INTERNAL PACKAGE AND ADAPTED TO THE SARL PURPOSE.
 * THE FORK WAS NECESSARY BECAUSE IT IS IMPOSSIBLE TO SUBCLASS THE TYPES FOR THE.
 * STANDARD HTML DOCLET THAT IS PROVIDED BY JDK.JAVADOC MODULE.
 *
 * Copyright (c) 2003, 2021, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package io.sarl.docs.doclet2.framework;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import com.google.common.base.Strings;

import jdk.javadoc.doclet.Taglet.Location;

/** Location of a custom tag.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.13.0 20230919-093059
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public enum CustomTagLocation {

	/** The custom tag is accepted everywhere.
	 */
	EVERYWHERE('a') {
		@Override
		public Location[] toJavadocLocation() {
			final List<Location> list = new ArrayList<>();
			for (final CustomTagLocation location : getAllActiveLocations()) {
				list.addAll(Arrays.asList(location.toJavadocLocation()));
			}
			return list.toArray(new Location[list.size()]);
		}
	},

	/** The custom tag is accepted in the overview page.
	 */
	OVERVIEW('o') {
		@Override
		public Location[] toJavadocLocation() {
			return new Location[] {
					Location.OVERVIEW
			};
		}
	},

	/** The custom tag is accepted in the package documentation.
	 */
	PACKAGE('p') {
		@Override
		public Location[] toJavadocLocation() {
			return new Location[] {
					Location.PACKAGE
			};
		}
	},

	/** The custom tag is accepted in the type documentation.
	 */
	TYPE('t') {
		@Override
		public Location[] toJavadocLocation() {
			return new Location[] {
					Location.TYPE
			};
		}
	},

	/** The custom tag is accepted in the constructor documentation.
	 */
	CONSTRUCTOR('c') {
		@Override
		public Location[] toJavadocLocation() {
			return new Location[] {
					Location.CONSTRUCTOR
			};
		}
	},

	/** The custom tag is accepted in the method documentation.
	 */
	METHOD('m') {
		@Override
		public Location[] toJavadocLocation() {
			return new Location[] {
					Location.METHOD
			};
		}
	},

	/** The custom tag is accepted in the field documentation.
	 */
	FIELD('t') {
		@Override
		public Location[] toJavadocLocation() {
			return new Location[] {
					Location.FIELD
			};
		}
	},

	/** The custom tag is accepted in the module documentation.
	 */
	MODULE('u') {
		@Override
		public Location[] toJavadocLocation() {
			return new Location[] {
					Location.MODULE
			};
		}
	},

	/** The custom tag is disabled.
	 */
	DISABLED('X') {
		@Override
		public Location[] toJavadocLocation() {
			return new Location[0];
		}
	};

	private final char symbol;

	private CustomTagLocation(char symbol) {
		this.symbol = symbol;
	}
	
	/** Replies the Javadoc location that corresponds to this custom tag location.
	 *
	 * @return the javadoc location.
	 */
	public abstract Location[] toJavadocLocation();

	/** Find the custom tag location that corresponds to the given symbol.
	 *
	 * @param symbol the symbol to search for.
	 * @return the location, or {@code null} if the given symbol is not recognized.
	 */
	public static CustomTagLocation valueOf(char symbol) {
		for (final CustomTagLocation location : values()) {
			if (location.symbol == symbol) {
				return location;
			}
		}
		return null;
	}

	/** Parse the sequence of characters for extracting the custom tag locations.
	 *
	 * @param specification the sequence of characters to analyze.
	 * @return the list of detected locations.
	 */
	public static List<CustomTagLocation> parse(String specification) {
		final List<CustomTagLocation> list = new ArrayList<>();
		if (!Strings.isNullOrEmpty(specification)) {
			for (final char symbol : specification.toCharArray()) {
				final CustomTagLocation location = valueOf(symbol);
				if (location != null) {
					list.add(location);
				}
			}
		}
		return list;
	}

	/** Replies all the locations that may receive a tag.
	 *
	 * @return the locations that are active for tags.
	 */
	public static List<CustomTagLocation> getAllActiveLocations() {
		return Arrays.asList(values()).stream().filter(it -> it != DISABLED && it != EVERYWHERE).collect(Collectors.toList());
	}

}
