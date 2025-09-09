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

import java.lang.ref.WeakReference;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import jdk.javadoc.doclet.Doclet;
import jdk.javadoc.doclet.DocletEnvironment;
import jdk.javadoc.doclet.Taglet;
import jdk.javadoc.doclet.Taglet.Location;

/** Manager of taglets.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.15.0 20250909-115750
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public class TagletManagerImpl implements TagletManager {

	/**
	 * The map of all taglets.
	 */
	private final Map<String, Taglet> allTaglets = new TreeMap<>();

	/**
	 * Block (non-line) taglets, grouped by Site
	 */
	private Map<Location, Set<String>> blockTagletsByLocation = new TreeMap<>();

	/**
	 * The taglets that can appear inline in descriptive text.
	 */
	private final Set<String> inlineTags = new TreeSet<>();

	private WeakReference<DocletEnvironment> environment;

	private WeakReference<Doclet> doclet;

	private static String unifyId(String name) {
		return name.toLowerCase();
	}

	@Override
	public synchronized void addTaglet(Taglet taglet, boolean override) {
		final var uname = unifyId(taglet.getName());
		final boolean doAddition;
		if (override) {
			doAddition = true;
		} else {
			doAddition = !this.allTaglets.containsKey(uname);
		}
		if (doAddition) {
			addTaglet(uname, taglet);
		}
	}

	/** Add the instance of taglet.
	 *
	 * @param uname the identifier of the taglet.
	 * @param taglet the taglet to add.
	 */
	protected void addTaglet(String uname, Taglet taglet) {
		try {
			this.allTaglets.put(uname, taglet);
			if (taglet.isInlineTag()) {
				this.inlineTags.add(uname);
			} else {
				for (final var loc : taglet.getAllowedLocations()) {
					final var list = this.blockTagletsByLocation.computeIfAbsent(loc, it -> {
						return new TreeSet<>();
					});
					list.add(uname);
				}
			}
			final var env = this.environment == null ? null : this.environment.get();
			if (env != null) {
				final var doclet = this.doclet == null ? null : this.doclet.get();
				taglet.init(env, doclet);
			}
		} catch (Throwable ex) {
			throw new RuntimeException(ex);
		}
	}

	@Override
	public Taglet getBlockTaglet(Location location, String name) {
		final var uname = unifyId(name);
		final var taglet = this.allTaglets.get(uname);
		if (taglet != null) {
			final var theset = this.blockTagletsByLocation.get(location);
			if (theset != null && theset.contains(uname)) {
				return taglet;
			}
		}
		return null;
	}

	@Override
	public synchronized Taglet getInlineTaglet(String name) {
		final var uname = unifyId(name);
		final var taglet = this.allTaglets.get(uname);
		if (taglet != null && this.inlineTags.contains(uname)) {
			return taglet;
		}
		return null;
	}

	@Override
	public synchronized Collection<Taglet> getAllTaglets() {
		return Collections.unmodifiableCollection(this.allTaglets.values());
	}

	@Override
	public synchronized void init(DocletEnvironment environment, Doclet doclet) {
		this.environment = new WeakReference<>(environment);
		this.doclet = new WeakReference<>(doclet);
		for (final var taglet : getAllTaglets()) {
			taglet.init(environment, doclet);
		}
	}

}
