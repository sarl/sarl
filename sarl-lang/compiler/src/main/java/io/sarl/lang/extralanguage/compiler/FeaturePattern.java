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
 */

package io.sarl.lang.extralanguage.compiler;

import java.util.Deque;
import java.util.Objects;
import java.util.regex.Pattern;

import org.eclipse.xtext.util.Strings;

/** Feature pattern.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class FeaturePattern {

	private static final String SEPARATOR_PATTERN = "\\/+"; //$NON-NLS-1$

	/** Character that is representing all the patterns.
	 */
	public static final char ALL_PATTERN_CHAR = '*';

	private static final String ALL_PATTERN = Character.toString(ALL_PATTERN_CHAR);

	private static final String ANY_PATTERN = ".*"; //$NON-NLS-1$

	private static final char OPARENTHESIS = '(';

	private static final char CPARENTHESIS = ')';

	private static final char SEPARATOR = '/';

	private static final char DOT = '.';

	private final boolean isNameReplacement;

	private final String rawFeature;

	private final Pattern featurePattern;

	private final Pattern[] pathPatterns;

	/** Constructor.
	 *
	 * <p>The general format of the textual representation is:
	 * <pre>{@code 
	 * [path/]featureIdentifier
	 * }</pre>
	 * The {@code featureIdentifier} is the identifier of the feature.
	 * The {@code path} is a sequence of type identifiers or field names.
	 *
	 * <p>The special character {@code *} may be used for specifying "anything".
	 *
	 * @param specification the textual representation of the pattern.
	 */
	public FeaturePattern(String specification) {
		final var elements = specification.split(SEPARATOR_PATTERN);
		final var last = elements[elements.length - 1];
		if (last.contains(ALL_PATTERN)) {
			this.rawFeature = null;
			this.featurePattern = Pattern.compile(protect(last));
		} else {
			this.rawFeature = last;
			this.featurePattern = null;
		}
		this.pathPatterns = new Pattern[elements.length - 1];
		if (this.pathPatterns.length > 0) {
			for (var i = 0; i < this.pathPatterns.length; ++i) {
				this.pathPatterns[i] = Pattern.compile(protect(elements[i]));
			}
		}
		this.isNameReplacement = specification.charAt(specification.length() - 1) != CPARENTHESIS;
	}

	@Override
	public String toString() {
		if (this.rawFeature != null) {
			return this.rawFeature;
		}
		return Objects.toString(this.featurePattern);
	}

	private static String protect(String source) {
		if (Strings.equal(source, ALL_PATTERN)) {
			return ANY_PATTERN;
		}
		final var builder = new StringBuilder();
		var first = true;
		for (final var element : source.split(Pattern.quote(ALL_PATTERN))) {
			if (first) {
				first = false;
			} else {
				builder.append(ANY_PATTERN);
			}
			builder.append(Pattern.quote(element));
		}
		return builder.toString();
	}

	/** Replies if this pattern is only for a simple name replacement.
	 *
	 * @return {@code true} if the pattern corresponds to a simple name only.
	 */
	public boolean isNameReplacement() {
		return this.isNameReplacement;
	}

	/** Replies if the pattern matches the given identifier.
	 *
	 * @param feature the feature to test.
	 * @return {@code true} if the pattern matches.
	 */
	public boolean matches(Deque<String> feature) {
		boolean match;
		if (this.rawFeature != null) {
			match = this.rawFeature.equals(feature.getLast());
		} else {
			final var featureMatcher = this.featurePattern.matcher(feature.getLast());
			match = featureMatcher.matches();
		}
		if (match && this.pathPatterns.length > 0) {
			final var iterator = feature.descendingIterator();
			// Skip last
			iterator.next();
			for (var j = this.pathPatterns.length - 1;
					match && iterator.hasNext() && j >= 0;
					--j) {
				final var component = iterator.next();
				final var pathPattern = this.pathPatterns[j];
				final var pathMatcher = pathPattern.matcher(component);
				match = pathMatcher.matches();
			}
		}
		return match;
	}

	/** Replies the simple name of the feature from the given textual representation.
	 *
	 * @param textualRepresentation the textual representation.
	 * @return the simple name.
	 */
	public static String simpleName(String textualRepresentation) {
		var start = textualRepresentation.lastIndexOf(SEPARATOR);
		var end = textualRepresentation.length();
		if (start < 0) {
			start = 0;
		} else {
			++start;
		}
		if (textualRepresentation.charAt(textualRepresentation.length() - 1) == CPARENTHESIS) {
			end = textualRepresentation.lastIndexOf(OPARENTHESIS);
			assert end > 0;
		}
		final var dot = textualRepresentation.lastIndexOf(DOT, end - 1);
		if (dot + 1 >= start) {
			start = dot + 1;
		}
		return textualRepresentation.substring(start, end);
	}

}
