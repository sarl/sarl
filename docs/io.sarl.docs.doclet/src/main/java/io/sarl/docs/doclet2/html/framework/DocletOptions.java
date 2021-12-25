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

package io.sarl.docs.doclet2.html.framework;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import org.eclipse.xtext.xbase.lib.Pair;

/** Options provided on the CLI to the doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class DocletOptions {

	/** User tag that is used to ignore elements.
	 */
	public static final String EXCLUDEFROMAPIDOC_TAG = "excludefromapidoc";
	
	/** User tag that is used to specify the Maven group id.
	 */
	public static final String MAVENGROUPID_TAG = "mavengroupid";

	/** User tag that is used to specify the Maven artifact id.
	 */
	public static final String MAVENARTIFACTID_TAG = "mavenartifactid";

	/** User tag that is used to specify a private API element.
	 */
	public static final String PRIVATEAPI_TAG = "privateapi";

	private Path outputDirectory;

	private boolean fake;

	private boolean offline;

	private boolean enableHtmlComments;

	private Set<String> additionalTags = new TreeSet<>();

	private String copyrightText;

	private String title;

	private final List<Pair<String, Pattern>> groups = new ArrayList<>();

	/** Constructor.
	 */
	public DocletOptions() {
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
	
	/** Replies the output directory.
	 *
	 * @return the output directory.
	 */
	public Path getOutputDirectory() {
		if (this.outputDirectory == null) {
			return Path.of("").toAbsolutePath();
		}
		return this.outputDirectory;
	}
	
	/** Change the output directory.
	 *
	 * @param output the output directory.
	 */
	public void setOutputDirectory(Path output) {
		this.outputDirectory = output;
	}

	/** Replies if outputs are fake.
	 *
	 * @return {@code true} if outputs are fake.
	 */
	public boolean isFakeOutput() {
		return this.fake;
	}
	
	/** Set if the outputs are fake.
	 *
	 * @param fake the fake flag.
	 */
	public void setFakeOutput(boolean fake) {
		this.fake = fake;
	}

	/** Add a user tag from the CLI configuration. The user tags will be recognized by the doclet and no error will be generated when
	 * they are used.
	 *
	 * @param tagName the name of the tag (without @).
	 */
	public void addUserTag(String tagName) {
		this.additionalTags.add(tagName.toLowerCase().trim());
	}

	/** Replies the user tags from the CLI configuration. The user tags will be recognized by the doclet and no error will be generated when
	 * they are used.
	 *
	 * @return the names of the tags (without @).
	 */
	public Set<String> getUserTags() {
		return Collections.unmodifiableSet(this.additionalTags);
	}

	/** Change the offline status of the doclet.
	 *
	 * @param offline is {@code true} to force the doclet to be offline.
	 */
	public void setOffline(boolean offline) {
		this.offline = offline;
	}

	/** Replies if the doclet is offline or not.
	 *
	 * @return {@code true} to force the doclet to be offline.
	 */
	public boolean isOffline() {
		return this.offline;
	}

	/** Change the flag that indicates if the {@code @comment} tags are translated to HTML comments or to HTML blocks that are hidden.
	 *
	 * @param enable is {@code true} to convert {@code @comment} to HTML comment.
	 */
	public void setHtmlCommentsEnabled(boolean enable) {
		this.enableHtmlComments = enable;
	}

	/** Replies if the {@code @comment} tags are translated to HTML comments or to HTML blocks that are hidden.
	 *
	 * @return {@code true} to convert {@code @comment} to HTML comment.
	 */
	public boolean isHtmlCommentsEnabled() {
		return this.enableHtmlComments;
	}

	/** Change the copyright text.
	 *
	 * @param text the text.
	 */
	public void setCopyrightText(String text) {
		this.copyrightText = text;
	}

	/** Replies the copyright text.
	 *
	 * @return the text.
	 */
	public String getCopyrightText() {
		return this.copyrightText;
	}

	/** Change the title.
	 *
	 * @param text the text.
	 */
	public void setTitle(String text) {
		this.title = text;
	}

	/** Replies the title.
	 *
	 * @return the text.
	 */
	public String getTitle() {
		return this.title;
	}

	/** Add a group of package that is used on the overview page.
	 *
	 * @param heading the title of the group.
	 * @param groupPatterns the package patterns.
	 */
	public void addGroup(String heading, String... groupPatterns) {
		final StringBuilder pat = new StringBuilder();
		for (final String groupPattern : groupPatterns) {
			if (pat.length() > 0) {
				pat.append("|");
			}
			final String p0 = "(?:" + groupPattern.replaceAll(Pattern.quote("."), "\\\\.").replaceAll(Pattern.quote("*"), ".*") + ")";
			pat.append(p0);
		}
		pat.insert(0, "^(?:");
		pat.append(")$");
		final Pattern regex = Pattern.compile(pat.toString());
		this.groups.add(Pair.of(heading, regex));
	}

	/** Replies all the groups of packages.
	 *
	 * @return the groups of packages.
	 */
	public List<Pair<String, Pattern>> getGroups() {
		return Collections.unmodifiableList(this.groups);
	}

}
