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

package io.sarl.docs.doclet2.html.framework;

import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import org.eclipse.xtext.xbase.lib.Pair;

/** Options provided on the CLI to the doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class DefaultDocletOptions implements DocletOptions {

	/** User tag that is used to ignore elements.
	 */
	public static final String EXCLUDEFROMAPIDOC_TAG = "excludefromapidoc"; //$NON-NLS-1$
	
	/** User tag that is used to specify the Maven group id.
	 */
	public static final String MAVENGROUPID_TAG = "mavengroupid"; //$NON-NLS-1$

	/** User tag that is used to specify the Maven artifact id.
	 */
	public static final String MAVENARTIFACTID_TAG = "mavenartifactid"; //$NON-NLS-1$

	/** User tag that is used to specify a private API element.
	 */
	public static final String PRIVATEAPI_TAG = "privateapi"; //$NON-NLS-1$

	private Path outputDirectory;

	private boolean fake;

	private boolean offline;

	private boolean enableHtmlComments;

	private boolean enableDeprecated = true;

	private boolean enableSinceTag = true;

	private boolean enableVersionTag;

	private boolean enableAuthorTag;

	private Set<String> additionalTags = new TreeSet<>();

	private String copyrightText;

	private String title;

	private final List<Pair<String, Pattern>> groups = new ArrayList<>();

	private String charset;

	/** Constructor.
	 */
	public DefaultDocletOptions() {
		addDefaultUserTags();
	}

	/** Invoked to add the default user tags.
	 */
	protected void addDefaultUserTags() {
		addUserTag(EXCLUDEFROMAPIDOC_TAG);
		addUserTag(MAVENGROUPID_TAG);
		addUserTag(MAVENARTIFACTID_TAG);
		addUserTag(PRIVATEAPI_TAG);
	}

	@Override
	public Path getOutputDirectory() {
		if (this.outputDirectory == null) {
			return Path.of("").toAbsolutePath(); //$NON-NLS-1$
		}
		return this.outputDirectory;
	}
	
	@Override
	public void setOutputDirectory(Path output) {
		this.outputDirectory = output;
	}

	@Override
	public boolean isFakeOutput() {
		return this.fake;
	}
	
	@Override
	public void setFakeOutput(boolean fake) {
		this.fake = fake;
	}

	@Override
	public void addUserTag(String tagName) {
		this.additionalTags.add(tagName.toLowerCase().trim());
	}

	@Override
	public Set<String> getUserTags() {
		return Collections.unmodifiableSet(this.additionalTags);
	}

	@Override
	public void setOffline(boolean offline) {
		this.offline = offline;
	}

	@Override
	public boolean isOffline() {
		return this.offline;
	}

	@Override
	public void setHtmlCommentsEnabled(boolean enable) {
		this.enableHtmlComments = enable;
	}

	@Override
	public boolean isHtmlCommentsEnabled() {
		return this.enableHtmlComments;
	}

	@Override
	public void setDeprecatedFeaturesEnabled(boolean enable) {
		this.enableDeprecated = enable;
	}

	@Override
	public boolean isDeprecatedFeaturesEnabled() {
		return this.enableDeprecated;
	}

	@Override
	public void setSinceTagsEnabled(boolean enable) {
		this.enableSinceTag = enable;
	}

	@Override
	public boolean isSinceTagsEnabled() {
		return this.enableSinceTag;
	}
	
	@Override
	public void setVersionTagsEnabled(boolean enable) {
		this.enableVersionTag = enable;
	}

	@Override
	public boolean isVersionTagsEnabled() {
		return this.enableVersionTag;
	}

	@Override
	public void setAuthorTagsEnabled(boolean enable) {
		this.enableAuthorTag = enable;
	}

	@Override
	public boolean isAuthorTagsEnabled() {
		return this.enableAuthorTag;
	}

	@Override
	public void setCopyrightText(String text) {
		this.copyrightText = text;
	}

	@Override
	public String getCopyrightText() {
		return this.copyrightText;
	}

	@Override
	public void setTitle(String text) {
		this.title = text;
	}

	@Override
	public String getTitle() {
		return this.title;
	}

	@Override
	public void addGroup(String heading, String... groupPatterns) {
		final StringBuilder pat = new StringBuilder();
		for (final String groupPattern : groupPatterns) {
			if (pat.length() > 0) {
				pat.append("|"); //$NON-NLS-1$
			}
			final String p0 = "(?:" + groupPattern.replaceAll(Pattern.quote("."), "\\\\.").replaceAll(Pattern.quote("*"), ".*") + ")"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
			pat.append(p0);
		}
		pat.insert(0, "^(?:"); //$NON-NLS-1$
		pat.append(")$"); //$NON-NLS-1$
		final Pattern regex = Pattern.compile(pat.toString());
		this.groups.add(Pair.of(heading, regex));
	}

	@Override
	public List<Pair<String, Pattern>> getGroups() {
		return Collections.unmodifiableList(this.groups);
	}

	@Override
	public void setCharset(String name) {
		this.charset = name;
	}

	@Override
	public String getCharset() {
		if (Strings.isNullOrEmpty(this.charset)) {
			return Charset.defaultCharset().name();
		}
		return this.charset;
	}

}
