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

package io.sarl.lang.mwe2.externalspec;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import com.google.inject.Injector;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.IGuiceAwareGeneratorComponent;

import io.sarl.lang.mwe2.keywords.GrammarKeywordAccessConfig;

/**
 * The configuration for the external highlighting tools.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ExternalHighlightingConfig implements IGuiceAwareGeneratorComponent {

	/** Do not add the native types by default.
	 */
	public static final boolean DEFAULT_ADD_NATIVE_TYPES = false;

	/** Do not use the keywords and ignored keywords from the {@link GrammarKeywordAccessConfig}.
	 */
	public static final boolean DEFAULT_INHERIT_GRAMMAR_KEYWORD_ACCESS = false;

	private final Set<String> additionalLiterals = new TreeSet<>();

	private final Set<String> additionalKeywords = new TreeSet<>();

	private final Set<String> excludedKeywords = new TreeSet<>();

	private final Set<String> additionalPunctuation = new TreeSet<>();

	private final Set<String> specialKeywords = new TreeSet<>();

	private final Set<String> typeDeclarationKeywords = new TreeSet<>();

	private Boolean addNativeTypes;

	private Boolean inheritFromGrammarKeywordAccess;

	private ColorConfig colors = new ColorConfig();

	@Override
	public void initialize(Injector injector) {
		injector.injectMembers(this);
	}

	/** Indicates if the keywords and ignored keywords from the {@link GrammarKeywordAccessConfig}
	 * are used.
	 *
	 * @param inheritFromGrammarKeywordAccess <code>true</code> for using inherited configuration.
	 */
	public void setInheritFromGrammarKeywordAccesss(boolean inheritFromGrammarKeywordAccess) {
		this.inheritFromGrammarKeywordAccess = inheritFromGrammarKeywordAccess;
	}

	/** Replies if the keywords and ignored keywords from the {@link GrammarKeywordAccessConfig}
	 * are used.
	 *
	 * @return <code>true</code> for using inherited configuration.
	 */
	@Pure
	public boolean getInheritFromGrammarKeywordAccesss() {
		if (this.inheritFromGrammarKeywordAccess == null) {
			return DEFAULT_INHERIT_GRAMMAR_KEYWORD_ACCESS;
		}
		return this.inheritFromGrammarKeywordAccess.booleanValue();
	}

	/** Indicates if the native types must be added in the keyword list.
	 *
	 * @param addNativeTypes <code>true</code> for adding the native types.
	 */
	public void setAddNativeTypes(boolean addNativeTypes) {
		this.addNativeTypes = addNativeTypes;
	}

	/** Replies if the native types must be added in the keyword list.
	 *
	 * @return <code>true</code> for adding the native types.
	 */
	@Pure
	public boolean getAddNativeTypes() {
		if (this.addNativeTypes == null) {
			return DEFAULT_ADD_NATIVE_TYPES;
		}
		return this.addNativeTypes.booleanValue();
	}

	/** Add a literal that is not inside the SARL grammar.
	 *
	 * @param literal the additional literal.
	 */
	public void addLiteral(String literal) {
		if (!Strings.isEmpty(literal)) {
			this.additionalLiterals.add(literal);
		}
	}

	/** Replies the literals that are not inside the SARL grammar.
	 *
	 * @return the additional literals.
	 */
	@Pure
	public Set<String> getLiterals() {
		return this.additionalLiterals;
	}

	/** Add a keyword that is not inside the SARL grammar.
	 *
	 * @param keyword the additional keyword.
	 */
	public void addKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.additionalKeywords.add(keyword);
		}
	}

	/** Replies the keywords that are not inside the SARL grammar.
	 *
	 * @return the additional keywords.
	 */
	@Pure
	public Set<String> getKeywords() {
		return this.additionalKeywords;
	}

	/** Ignore a keyword.
	 *
	 * @param keyword the keyword to ignore.
	 */
	public void addIgnoreKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.excludedKeywords.add(keyword);
		}
	}

	/** Replies the keywords to ignore.
	 *
	 * @return the keywords to ignore.
	 */
	@Pure
	public Set<String> getIgnoredKeywords() {
		return this.excludedKeywords;
	}

	/** Mark a keyword as special. The meaning of a special keyword depends on the highlighting
	 * generator.
	 *
	 * <p>The keyword will be ignored if it is not a keyword from the grammars or added with
	 * {@link #addKeyword(String)}.
	 *
	 * <p>A special keyword cannot be a type declaration keyword.
	 *
	 * @param keyword the keyword to mark as special
	 */
	public void addSpecialKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.specialKeywords.add(keyword);
		}
	}

	/** Replies the special keywords. The meaning of a special keyword depends on the highlighting
	 * generator.
	 *
	 * @return the special keywords.
	 */
	@Pure
	public Set<String> getSpecialKeywords() {
		return this.specialKeywords;
	}

	/** Mark a keyword as a type declaration keyword. The meaning of a type declaration keyword depends on the highlighting
	 * generator.
	 *
	 * <p>The keyword will be ignored if it is not a keyword from the grammars or added with
	 * {@link #addKeyword(String)}.
	 *
	 * <p>A type declaration keyword cannot be a special keyword.
	 *
	 * @param keyword the keyword to mark as special
	 */
	public void addTypeDeclarationKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.typeDeclarationKeywords.add(keyword);
		}
	}

	/** Replies the type declaration keywords. The meaning of a type declaration keyword depends on the highlighting
	 * generator.
	 *
	 * @return the special keywords.
	 */
	@Pure
	public Set<String> getTypeDeclarationKeywords() {
		return this.typeDeclarationKeywords;
	}

	/** Add a punctuation symbol that is not inside the SARL grammar.
	 *
	 * @param symbol the additional punctuation symbol.
	 */
	public void addPunctuation(String symbol) {
		if (!Strings.isEmpty(symbol)) {
			this.additionalPunctuation.add(symbol);
		}
	}

	/** Replies the punctuation symbols that are not inside the SARL grammar.
	 *
	 * @return the additional punctuation symbols.
	 */
	@Pure
	public Set<String> getPunctuation() {
		return this.additionalPunctuation;
	}

	/** Set the color configuration.
	 *
	 * @param colors the color configuration.
	 */
	public void setColors(ColorConfig colors) {
		this.colors = colors;
	}

	/** Replies the color configuration.
	 *
	 * @return the color configuration, or {@code null}
	 */
	@Pure
	public ColorConfig getColors() {
		return this.colors;
	}

	/**
	 * Color for LaTeX.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Color {

		private static final int MIN_COLOR = 0;

		private static final int MAX_COLOR = 255;

		private String name = "SARLdefault"; //$NON-NLS-1$

		private int red;

		private int green;

		private int blue;

		/** Change the name of the color.
		 *
		 * @param name the name of the color.
		 */
		public void setName(String name) {
			this.name = name;
		}

		/** Replies the name of the color.
		 *
		 * @return the name of the color.
		 */
		public String getName() {
			return this.name;
		}

		/** Set the color components.
		 *
		 * <p>The Red-Green-Blue components are integers between 0 (inclusive) and 255 (inclusive).
		 * The components are separated by any character that is not a digit.
		 *
		 * @param rgb the color.
		 */
		public void setRgb(String rgb) {
			final String[] components = rgb.split("[^0-9]+"); //$NON-NLS-1$
			this.red = MIN_COLOR;
			if (components.length > 0) {
				this.red = Integer.valueOf(components[0]);
				if (this.red < MIN_COLOR || this.red > MAX_COLOR) {
					throw new NumberFormatException("red is not between " //$NON-NLS-1$
							+ MAX_COLOR + " and " + MAX_COLOR); //$NON-NLS-1$
				}
			}
			this.green = MIN_COLOR;
			if (components.length > 1) {
				this.green = Integer.valueOf(components[1]);
				if (this.green < MIN_COLOR || this.green > MAX_COLOR) {
					throw new NumberFormatException("green is not between " //$NON-NLS-1$
							+ MIN_COLOR + " and " + MAX_COLOR); //$NON-NLS-1$
				}
			}
			this.blue = MIN_COLOR;
			if (components.length > 1) {
				this.blue = Integer.valueOf(components[2]);
				if (this.blue < MIN_COLOR || this.blue > MAX_COLOR) {
					throw new NumberFormatException("blue is not between " //$NON-NLS-1$
							+ MIN_COLOR + " and " + MAX_COLOR); //$NON-NLS-1$
				}
			}
		}

		/** Replies the red component.
		 *
		 * @return the red.
		 */
		public int getRed() {
			return this.red;
		}

		/** Replies the green component.
		 *
		 * @return the green.
		 */
		public int getGreen() {
			return this.green;
		}

		/** Replies the blue component.
		 *
		 * @return the blue.
		 */
		public int getBlue() {
			return this.blue;
		}

	}

	/**
	 * Color configuration for LaTeX.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ColorConfig {

		private static final String DEFAULT_COLOR = "black"; //$NON-NLS-1$

		private final Map<String, Color> colors = new TreeMap<>();

		private String commentColor;

		private String stringColor;

		private String keywordColor;

		private String identifierColor;

		private String lineNumberColor;

		/** Set a color.
		 *
		 * @param color the color.
		 */
		public void setColor(Color color) {
			if (color != null) {
				this.colors.put(color.getName(), color);
			}
		}

		/** Replies the colors.
		 *
		 * @return the colors.
		 */
		public Map<String, Color> getColors() {
			return this.colors;
		}

		/** Set the color of the comments.
		 *
		 * @param name the name of the color to use.
		 */
		public void setCommentColor(String name) {
			this.commentColor = name;
		}

		/** Replies the color of the comments.
		 *
		 * @return the name of the color to use.
		 */
		public String getCommentColor() {
			if (Strings.isEmpty(this.commentColor)) {
				return DEFAULT_COLOR;
			}
			return this.commentColor;
		}

		/** Set the color of the strings.
		 *
		 * @param name the name of the color to use.
		 */
		public void setStringColor(String name) {
			this.stringColor = name;
		}

		/** Replies the color of the strings.
		 *
		 * @return the name of the color to use.
		 */
		public String getStringColor() {
			if (Strings.isEmpty(this.stringColor)) {
				return DEFAULT_COLOR;
			}
			return this.stringColor;
		}

		/** Set the color of the keywords.
		 *
		 * @param name the name of the color to use.
		 */
		public void setKeywordColor(String name) {
			this.keywordColor = name;
		}

		/** Replies the color of the keywords.
		 *
		 * @return the name of the color to use.
		 */
		public String getKeywordColor() {
			if (Strings.isEmpty(this.keywordColor)) {
				return DEFAULT_COLOR;
			}
			return this.keywordColor;
		}

		/** Set the color of the identifiers.
		 *
		 * @param name the name of the color to use.
		 */
		public void setIdentifierColor(String name) {
			this.identifierColor = name;
		}

		/** Replies the color of the identifiers.
		 *
		 * @return the name of the color to use.
		 */
		public String getIdentifierColor() {
			if (Strings.isEmpty(this.identifierColor)) {
				return DEFAULT_COLOR;
			}
			return this.identifierColor;
		}

		/** Set the color of the line numbers.
		 *
		 * @param name the name of the color to use.
		 */
		public void setLineNumberColor(String name) {
			this.lineNumberColor = name;
		}

		/** Replies the color of the line numbers.
		 *
		 * @return the name of the color to use.
		 */
		public String getLineNumberColor() {
			if (Strings.isEmpty(this.lineNumberColor)) {
				return DEFAULT_COLOR;
			}
			return this.lineNumberColor;
		}

	}

}

