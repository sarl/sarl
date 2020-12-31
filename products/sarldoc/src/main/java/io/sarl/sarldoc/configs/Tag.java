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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.base.Strings;
import io.bootique.annotation.BQConfigProperty;

/**
 * Configuration for custom API documentation tag.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class Tag {

	private static final String SEPARATOR = ":"; //$NON-NLS-1$

	private String name;

	private String header;

	private List<Placement> placement;

	/** Replies the name of the custom tag.
	 *
	 * @return the name of the tag, or {@code null} if none was specified.
	 */
	public String getName() {
		return this.name;
	}

	/** Change the name of the custom tag.
	 *
	 * @param name the name of the tag.
	 */
	@BQConfigProperty("Specify the name of the custom name. The provided "
			+ "name is the text that should appear after the '@' character.")
	public void setName(String name) {
		this.name = name;
	}

	/** Replies the heading text of the custom tag.
	 *
	 * @return the heading text of the tag, or {@code null} if none was specified.
	 */
	public String getHeader() {
		if (Strings.isNullOrEmpty(this.header)) {
			this.header = getName();
		}
		return this.header;
	}

	/** Change the heading text of the custom tag.
	 *
	 * @param text the heading text of the tag.
	 */
	@BQConfigProperty("Specify the text that is used as heading into the documentation each "
			+ "time the custom tag is used.")
	public void setHeader(String text) {
		this.header = text;
	}

	/** Replies the placements associated to this custom tag.
	 *
	 * @return the placements.
	 */
	public List<Placement> getPlacements() {
		if (this.placement == null || this.placement.isEmpty()) {
			return Collections.singletonList(Placement.getDefault());
		}
		return Collections.unmodifiableList(this.placement);
	}

	/** Change the placements associated to this custom tag.
	 *
	 * @param placements the placements.
	 */
	@BQConfigProperty("Specify the placements for this custom tag.")
	public void setPlacements(List<Placement> placements) {
		if (placements == null || placements.isEmpty()) {
			this.placement = null;
		} else {
			this.placement = new ArrayList<>(placements);
		}
	}

	/** Parse the given string for obtaining the tag.
	 *
	 * @param text the string to parse.
	 * @return the tag.
	 * @throws NullPointerException when the specified text is null
	 */
	@JsonCreator
	public static Tag valueOf(String text) {
		if (Strings.isNullOrEmpty(text)) {
			throw new NullPointerException("text is null"); //$NON-NLS-1$
		}
		final Tag tag = new Tag();
		final String[] parts = text.split(Pattern.quote(SEPARATOR));
		if (parts != null) {
			if (parts.length > 0) {
				tag.setName(Strings.emptyToNull(parts[0]));
			}
			if (parts.length > 1) {
				tag.setPlacements(Placement.valuesOf(parts[1]));
			}
			if (parts.length > 2) {
				tag.setHeader(Strings.emptyToNull(parts[2]));
			}
		}
		if (!Strings.isNullOrEmpty(tag.getName())) {
			return tag;
		}
		return null;
	}

	/** Parse the given string for obtaining the tag.
	 *
	 * @param text the string to parse.
	 * @return the tag.
	 */
	@JsonCreator
	public static List<Tag> valuesOf(String text) {
		final List<Tag> tags = new ArrayList<>();
		if (!Strings.isNullOrEmpty(text)) {
			final String[] parts = text.split(Pattern.quote(SEPARATOR));
			if (parts != null) {
				for (int i = 0; (i + 2) < parts.length;) {
					final Tag tag = new Tag();
					tag.setName(Strings.emptyToNull(parts[i]));
					++i;
					tag.setPlacements(Placement.valuesOf(parts[i]));
					++i;
					tag.setHeader(Strings.emptyToNull(parts[i]));
					++i;
					if (!Strings.isNullOrEmpty(tag.getName())) {
						tags.add(tag);
					}
				}
			}
		}
		return tags;
	}

	/** Replies the Json string representation of this tag.
	 *
	 * @return the Json string representation.
	 */
	@JsonValue
	@Override
	public String toString() {
		final StringBuilder buffer = new StringBuilder();
		final String name = getName();
		if (!Strings.isNullOrEmpty(name)) {
			buffer.append(name);
		}
		buffer.append(SEPARATOR);
		for (final Placement placement : getPlacements()) {
			buffer.append(placement.toChar());
		}
		buffer.append(SEPARATOR);
		final String header = getHeader();
		if (!Strings.isNullOrEmpty(header)) {
			buffer.append(header);
		}
		return buffer.toString();
	}

}
