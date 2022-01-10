/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2022 the original authors or authors.
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
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
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
	public synchronized void addTaglet(Taglet taglet) {
		try {
			final String uname = unifyId(taglet.getName());
			this.allTaglets.put(uname, taglet);
			if (taglet.isInlineTag()) {
				this.inlineTags.add(uname);
			} else {
				for (final Location loc : taglet.getAllowedLocations()) {
					final Set<String> list = this.blockTagletsByLocation.computeIfAbsent(loc, it -> {
						return new TreeSet<>();
					});
					list.add(uname);
				}
			}
			final DocletEnvironment env = this.environment == null ? null : this.environment.get();
			if (env != null) {
				final Doclet doclet = this.doclet == null ? null : this.doclet.get();
				taglet.init(env, doclet);
			}
		} catch (Throwable ex) {
			throw new RuntimeException(ex);
		}
	}

	@Override
	public Taglet getBlockTaglet(Location location, String name) {
		final String uname = unifyId(name);
		final Taglet taglet = this.allTaglets.get(uname);
		if (taglet != null) {
			final Set<String> theset = this.blockTagletsByLocation.get(location);
			if (theset != null && theset.contains(uname)) {
				return taglet;
			}
		}
		return null;
	}

	@Override
	public synchronized Taglet getInlineTaglet(String name) {
		final String uname = unifyId(name);
		final Taglet taglet = this.allTaglets.get(uname);
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
		for (final Taglet taglet : getAllTaglets()) {
			taglet.init(environment, doclet);
		}
	}

}
