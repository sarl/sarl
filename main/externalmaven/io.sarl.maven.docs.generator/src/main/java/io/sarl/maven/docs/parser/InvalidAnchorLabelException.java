/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.maven.docs.parser;

import java.text.MessageFormat;

/** An anchor is not valid.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class InvalidAnchorLabelException extends RuntimeException {

	private static final long serialVersionUID = 4282853898682563456L;

	private final String anchor;

	/** Constructor.
	 *
	 * @param anchor the failing anchor.
	 * @param existingAnchors the set of existing anchors.
	 */
	public InvalidAnchorLabelException(String anchor, String... existingAnchors) {
		super(MessageFormat.format(Messages.InvalidAnchorLabelException_0, anchor, format(existingAnchors)));
		this.anchor = anchor;
	}

	private static String format(String[] anchors) {
		final StringBuilder builder = new StringBuilder();
		for (final String anchor : anchors) {
			if (builder.length() > 0) {
				builder.append(", "); //$NON-NLS-1$
			}
			builder.append(anchor);
		}
		return builder.toString();
	}

	/** Replies the failing anchor.
	 *
	 * @return the anchor.
	 */
	public String getAnchor() {
		return this.anchor;
	}

}
