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

package io.sarl.lang.mwe2.externalspec.latex;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Set;

import com.google.common.base.Joiner;
import com.google.common.io.Files;
import com.google.inject.Injector;
import com.ibm.icu.text.SimpleDateFormat;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xtext.generator.CodeConfig;

import io.sarl.lang.mwe2.externalspec.AbstractExternalHighlightingFragment2;
import io.sarl.lang.mwe2.externalspec.ExternalHighlightingConfig.Color;
import io.sarl.lang.mwe2.externalspec.ExternalHighlightingConfig.ColorConfig;
import io.sarl.lang.mwe2.externalspec.IStyleAppendable;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the LaTeX listings.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class LaTeXListingsGenerator2 extends AbstractExternalHighlightingFragment2<IStyleAppendable> {

	/** The default basename pattern for {@link MessageFormat}.
	 */
	public static final String BASENAME_PATTERN = "{0}-listing.sty"; //$NON-NLS-1$

	/** Default definition for the basic style for floating algorithms (without colors).
	 */
	public static final String DEFAULT_FLOAT_BASIC_STYLE = "\\smaller\\smaller"; //$NON-NLS-1$

	/** Default definition for the basic style for floating algorithms (with colors).
	 */
	public static final String DEFAULT_COLORIZED_FLOAT_BASIC_STYLE = "\\normalcolor\\smaller\\smaller"; //$NON-NLS-1$

	/** Default definition for the basic style for inline code (without color).
	 */
	public static final String DEFAULT_INLINE_BASIC_STYLE = "{}"; //$NON-NLS-1$

	/** Default definition for the basic style for inline code (with colors).
	 */
	public static final String DEFAULT_COLORIZED_INLINE_BASIC_STYLE = "\\normalcolor"; //$NON-NLS-1$;

	/** Default definition for the identifier style (without color).
	 */
	public static final String DEFAULT_IDENTIFIER_STYLE = "\\ttfamily"; //$NON-NLS-1$

	/** Default definition for the identifier style (with color).
	 */
	public static final String DEFAULT_COLORIZED_IDENTIFIER_STYLE = "\\color{SARLidentifier}" //$NON-NLS-1$
			+ DEFAULT_IDENTIFIER_STYLE;

	/** Default definition for the comment style (without color).
	 */
	public static final String DEFAULT_COMMENT_STYLE = "\\smaller\\bfseries"; //$NON-NLS-1$

	/** Default definition for the identifier style (with color).
	 */
	public static final String DEFAULT_COLORIZED_COMMENT_STYLE = "\\color{SARLcomment}" //$NON-NLS-1$
			+ DEFAULT_COMMENT_STYLE;

	/** Default definition for the string style (without color).
	 */
	public static final String DEFAULT_STRING_STYLE = "\\ttfamily"; //$NON-NLS-1$

	/** Default definition for the string style (with color).
	 */
	public static final String DEFAULT_COLORIZED_STRING_STYLE = "\\color{SARLstring}" //$NON-NLS-1$
			+ DEFAULT_STRING_STYLE;

	/** Default definition for the keyword style (without color).
	 */
	public static final String DEFAULT_KEYWORD_STYLE = "\\bfseries"; //$NON-NLS-1$

	/** Default definition for the keyword style (with color).
	 */
	public static final String DEFAULT_COLORIZED_KEYWORD_STYLE = "\\color{SARLkeyword}" //$NON-NLS-1$
			+ DEFAULT_KEYWORD_STYLE;

	/** Default definition for the overridable TeX requirements.
	 */
	public static final String[] DEFAULT_REQUIREMENTS = new String[] {
		"relsize", //$NON-NLS-1$
	};

	private String floatBasicStyle;

	private String identifierStyle;

	private String commentStyle;

	private String stringStyle;

	private String keywordStyle;

	private String inlineBasicStyle;

	private boolean showLines = true;

	private int lineStep = 2;

	private int tabSize = 2;

	private boolean showSpecialCharacters;

	private Collection<String> requirements;

	@Override
	protected IStyleAppendable newStyleAppendable() {
		return new TeXAppendable(getCodeConfig(), getLanguageSimpleName(), getLanguageVersion());
	}

	@Override
	public String toString() {
		return "LaTeX listings"; //$NON-NLS-1$
	}

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
		setBasenameTemplate(BASENAME_PATTERN);
	}

	/** Set the TeX style of the standard code in floats.
	 *
	 * @param style the TeX code that describes the style. If {@code null}
	 *     or empty, the default style is applied.
	 */
	public void setFloatBasicStyle(String style) {
		this.floatBasicStyle = style;
	}

	/** Set the TeX style of the inline standard code.
	 *
	 * @param style the TeX code that describes the style. If {@code null}
	 *     or empty, the default style is applied.
	 */
	public void setInlineBasicStyle(String style) {
		this.inlineBasicStyle = style;
	}

	/** Set the TeX style of the identifiers.
	 *
	 * @param style the TeX code that describes the style. If {@code null}
	 *     or empty, the default style is applied.
	 */
	public void setIdentifierStyle(String style) {
		this.identifierStyle = style;
	}

	/** Set the TeX style of the comments.
	 *
	 * @param style the TeX code that describes the style. If {@code null}
	 *     or empty, the default style is applied.
	 */
	public void setCommentStyle(String style) {
		this.commentStyle = style;
	}

	/** Set the TeX style of the strings of characters.
	 *
	 * @param style the TeX code that describes the style. If {@code null}
	 *     or empty, the default style is applied.
	 */
	public void setStringStyle(String style) {
		this.stringStyle = style;
	}

	/** Set the TeX style of the strings of keywords.
	 *
	 * @param style the TeX code that describes the style. If {@code null}
	 *     or empty, the default style is applied.
	 */
	public void setKeywordStyle(String style) {
		this.keywordStyle = style;
	}

	/** Set if the TeX style shows the line numbers.
	 *
	 * @param showLineNumbers <code>true</code> for showing the line numbers.
	 */
	public void setLineNumbers(boolean showLineNumbers) {
		this.showLines = showLineNumbers;
	}

	/** Set the step between two line numbers.
	 *
	 * @param step the step between two line numbers.
	 */
	public void setLineStep(int step) {
		this.lineStep = step;
	}

	/** Set the size of the tabs.
	 *
	 * @param size the size of one tab character.
	 */
	public void setTabSize(int size) {
		this.tabSize = size;
	}

	/** Set if the TeX style shows the special characters (including spaces).
	 *
	 * @param showSpecialChars <code>true</code> for showing the special characters.
	 */
	public void setShowSpecialChars(boolean showSpecialChars) {
		this.showSpecialCharacters = showSpecialChars;
	}

	/** Clear the list of the overridable requirements.
	 */
	public void clearRequirements() {
		if (this.requirements == null) {
			this.requirements = new ArrayList<>();
		} else {
			this.requirements.clear();
		}
	}

	/** Add a TeX requirement.
	 *
	 * @param requirement the name of the TeX package.
	 */
	public void addRequirement(String requirement) {
		if (!Strings.isEmpty(requirement)) {
			if (this.requirements == null) {
				this.requirements = new ArrayList<>();
			}
			this.requirements.add(requirement);
		}
	}

	@SuppressWarnings({"checkstyle:parameternumber", "checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity"})
	@Override
	protected void generate(IStyleAppendable it, Set<String> literals, Set<String> expressionKeywords,
			Set<String> modifiers, Set<String> primitiveTypes, Set<String> punctuation, Set<String> ignored,
			Set<String> specialKeywords, Set<String> typeDeclarationKeywords) {
		final ColorConfig colors = getHighlightingConfig().getColors();

		final Set<String> texKeywords = sortedConcat(expressionKeywords, modifiers, primitiveTypes,
				specialKeywords, typeDeclarationKeywords, literals);

		it.appendHeader();

		final String basename = getBasename(
				MessageFormat.format(getBasenameTemplate(), getLanguageSimpleName().toLowerCase()));
		final String simpleBasename = Files.getNameWithoutExtension(basename);

		it.appendNl("\\NeedsTeXFormat{LaTeX2e}[1995/12/01]"); //$NON-NLS-1$
		final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd"); //$NON-NLS-1$
		it.appendNl("\\ProvidesPackage'{'{0}'}'[{1}]", simpleBasename, dateFormat.format(new Date())); //$NON-NLS-1$

		if (generateOptions(it)) {
			it.appendNl("\\ProcessOptions*"); //$NON-NLS-1$
		}

		it.appendNl("\\RequirePackage{algpseudocode}"); //$NON-NLS-1$
		it.appendNl("\\RequirePackage{listings}"); //$NON-NLS-1$
		it.appendNl("\\RequirePackage{xspace}"); //$NON-NLS-1$
		Collection<String> requirements = this.requirements;
		if (requirements == null) {
			requirements = Arrays.asList(DEFAULT_REQUIREMENTS);
		}
		for (final String requirement : requirements) {
			it.appendNl("\\RequirePackage'{'{0}'}'", requirement); //$NON-NLS-1$
		}
		if (getEnableColors()) {
			it.appendNl("\\RequirePackage{xcolor}"); //$NON-NLS-1$
		}

		if (getEnableColors()) {
			for (final Color color : colors.getColors().values()) {
				it.appendNl("\\definecolor'{'{0}'}{'RGB'}{'{1},{2},{3}'}'", //$NON-NLS-1$
						color.getName(), color.getRed(), color.getGreen(), color.getBlue());
			}
			it.appendNl("\\colorlet'{'SARLcomment'}{'{0}'}'", colors.getCommentColor()); //$NON-NLS-1$
			it.appendNl("\\colorlet'{'SARLstring'}{'{0}'}'", colors.getStringColor()); //$NON-NLS-1$
			it.appendNl("\\colorlet'{'SARLkeyword'}{'{0}'}'", colors.getKeywordColor()); //$NON-NLS-1$
			it.appendNl("\\colorlet'{'SARLidentifier'}{'{0}'}'", colors.getIdentifierColor()); //$NON-NLS-1$
			it.appendNl("\\colorlet'{'SARLlinenumber'}{'{0}'}'", colors.getLineNumberColor()); //$NON-NLS-1$
		}

		final String langName = getLanguageSimpleName().toUpperCase();
		it.appendNl("\\lstdefinelanguage'{'{0}'}{'%", langName); //$NON-NLS-1$
		it.appendNl("   morecomment=[l]{//},"); //$NON-NLS-1$
		it.appendNl("   morecomment=[s]{/*}{*/},"); //$NON-NLS-1$
		it.appendNl("   morestring=[b]\","); //$NON-NLS-1$
		it.appendNl("   morekeywords='{'{0}'}',", Joiner.on(",").join(texKeywords)); //$NON-NLS-1$ //$NON-NLS-2$
		it.appendNl("}"); //$NON-NLS-1$

		it.appendNl("\\lstset{%"); //$NON-NLS-1$

		String floatBasicStyle = this.floatBasicStyle;
		if (floatBasicStyle == null) {
			floatBasicStyle = getEnableColors() ? DEFAULT_COLORIZED_FLOAT_BASIC_STYLE : DEFAULT_FLOAT_BASIC_STYLE;
		}
		floatBasicStyle = Strings.emptyIfNull(floatBasicStyle);
		it.appendNl("   basicstyle={0}, % the size of the fonts that are used for the code", floatBasicStyle); //$NON-NLS-1$

		it.appendNl("   breakatwhitespace=false, % sets if automatic breaks should only happen at whitespace"); //$NON-NLS-1$
		it.appendNl("   breaklines=true, % sets automatic line breaking"); //$NON-NLS-1$
		it.appendNl("   captionpos=b, % sets the caption-position to bottom"); //$NON-NLS-1$
		it.appendNl("   deletekeywords={filter}, % if you want to delete keywords from the given language"); //$NON-NLS-1$
		it.appendNl("   escapeinside={(*@}{@*)}, % if you want to add LaTeX within your code"); //$NON-NLS-1$
		it.append("   extendedchars=true, % lets you use non-ASCII characters; for 8-bits "); //$NON-NLS-1$
		it.appendNl("encodings only, does not work with UTF-8"); //$NON-NLS-1$
		it.appendNl("   frame=none, % no frame around the code"); //$NON-NLS-1$
		it.append("   keepspaces=true, % keeps spaces in text, useful for keeping "); //$NON-NLS-1$
		it.appendNl("indentation of code (possibly needs columns=flexible)"); //$NON-NLS-1$

		String identifierStyle = this.identifierStyle;
		if (identifierStyle == null) {
			identifierStyle = getEnableColors() ? DEFAULT_COLORIZED_IDENTIFIER_STYLE : DEFAULT_IDENTIFIER_STYLE;
		}
		identifierStyle = Strings.emptyIfNull(identifierStyle);
		it.appendNl("   identifierstyle={0},", identifierStyle); //$NON-NLS-1$

		String commentStyle = this.commentStyle;
		if (commentStyle == null) {
			commentStyle = getEnableColors() ? DEFAULT_COLORIZED_COMMENT_STYLE : DEFAULT_COMMENT_STYLE;
		}
		commentStyle = Strings.emptyIfNull(commentStyle);
		it.appendNl("   commentstyle={0},", commentStyle); //$NON-NLS-1$

		String stringStyle = this.stringStyle;
		if (stringStyle == null) {
			stringStyle = getEnableColors() ? DEFAULT_COLORIZED_STRING_STYLE : DEFAULT_STRING_STYLE;
		}
		stringStyle = Strings.emptyIfNull(stringStyle);
		it.appendNl("   stringstyle={0},", stringStyle); //$NON-NLS-1$

		String keywordStyle = this.keywordStyle;
		if (keywordStyle == null) {
			keywordStyle = getEnableColors() ? DEFAULT_COLORIZED_KEYWORD_STYLE : DEFAULT_KEYWORD_STYLE;
		}
		keywordStyle = Strings.emptyIfNull(keywordStyle);
		it.appendNl("   keywordstyle={0}, % keyword style", keywordStyle); //$NON-NLS-1$

		it.appendNl("   language={0}, % the default language of the code", langName); //$NON-NLS-1$

		it.append("   showspaces={0}, % show spaces everywhere adding particular ", this.showSpecialCharacters); //$NON-NLS-1$
		it.appendNl("underscores; it overrides 'showstringspaces'"); //$NON-NLS-1$
		it.appendNl("   showstringspaces={0}, % underline spaces within strings only", //$NON-NLS-1$
				this.showSpecialCharacters);
		it.appendNl("   showtabs={0}, % show tabs within strings adding particular underscores", //$NON-NLS-1$
				this.showSpecialCharacters);
		if (this.showLines) {
			it.appendNl("   numbers=left,% Numbers on left"); //$NON-NLS-1$
			it.appendNl("   firstnumber=1, % First line number"); //$NON-NLS-1$
			it.appendNl("   numberfirstline=false, %Start numbers at first line"); //$NON-NLS-1$
			it.append("   stepnumber={0}, % the step between two line-numbers. ", this.lineStep); //$NON-NLS-1$
			it.appendNl("If it's 1, each line will be numbered"); //$NON-NLS-1$
		}
		it.appendNl("   tabsize={0}, % sets default tabsize to 2 spaces", this.tabSize); //$NON-NLS-1$
		it.append("   title=\\lstname, % show the filename of files included with "); //$NON-NLS-1$
		it.appendNl("\\lstinputlisting; also try caption instead of title"); //$NON-NLS-1$
		it.appendNl("   frameround=fttt, % If framed, use this rounded corner style"); //$NON-NLS-1$
		it.appendNl("   xleftmargin=20pt,"); //$NON-NLS-1$
		it.append("   numberstyle="); //$NON-NLS-1$
		if (getEnableColors()) {
			it.append("\\color{SARLlinenumber}"); //$NON-NLS-1$
		}
		it.appendNl("\\tiny,"); //$NON-NLS-1$
		it.appendNl("}"); //$NON-NLS-1$

		String inlineBasicStyle = this.inlineBasicStyle;
		if (inlineBasicStyle == null) {
			inlineBasicStyle = getEnableColors() ? DEFAULT_COLORIZED_INLINE_BASIC_STYLE : DEFAULT_INLINE_BASIC_STYLE;
		}
		inlineBasicStyle = Strings.emptyIfNull(inlineBasicStyle);
		it.append("\\newcommand{\\code}[1]{"); //$NON-NLS-1$
		it.append("\\ifmmode\\text'{'\\lstinline[basicstyle={0}]'{'#1'}}'", inlineBasicStyle); //$NON-NLS-1$
		it.append("\\else\\lstinline[basicstyle={0}]'{'#1'}'", inlineBasicStyle); //$NON-NLS-1$
		it.appendNl("\\fi}"); //$NON-NLS-1$
		it.appendNl("\\newcommand{\\sarl}{\\mbox{SARL}\\xspace}"); //$NON-NLS-1$
		it.appendNl("\\newcommand'{'\\sarlversion'}{'{0}'}'", getLanguageVersion()); //$NON-NLS-1$

		generateExtension(it);

		it.appendNl("\\endinput"); //$NON-NLS-1$
	}

	/** Generate the package definition extensions.
	 *
	 * @param it the target.
	 * @since 0.6
	 */
	protected void generateExtension(IStyleAppendable it) {
		//
	}

	@Override
	protected Object getReadmeFileContent(String basename) {
		return null;
	}

	/** Generate the optional extensions.
	 *
	 * @param it the target.
	 * @return {@code true} if options are generated.
	 * @since 0.6
	 */
	protected boolean generateOptions(IStyleAppendable it) {
		if (getEnableColors()) {
			it.appendNl("\\newif\\ifusesarlcolors\\usesarlcolorstrue"); //$NON-NLS-1$
			it.appendNl("\\DeclareOption{sarlcolors}{\\global\\usesarlcolorstrue}"); //$NON-NLS-1$
			it.appendNl("\\DeclareOption{nosarlcolors}{\\global\\usesarlcolorsfalse}"); //$NON-NLS-1$
			return true;
		}
		return false;
	}

	/** Appendable for tex-based styles.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class TeXAppendable extends AbstractAppendable {

		/** Constructor.
		 *
		 * @param codeConfig the code configuration.
		 * @param languageName the language name.
		 * @param languageVersion the language version.
		 */
		protected TeXAppendable(CodeConfig codeConfig, String languageName, String languageVersion) {
			super(codeConfig, languageName, languageVersion);
		}

		@Override
		public void appendComment(String text, Object... parameters) {
			final String comment = applyFormat(text, parameters);
			for (final String line : comment.split("[\n\r]")) { //$NON-NLS-1$
				appendNl("% " + line.trim()); //$NON-NLS-1$
			}
		}

		@Override
		public void appendHeader() {
			final String[] header = Strings.emptyIfNull(getCodeConfig().getFileHeader()).split("[\n\r]+"); //$NON-NLS-1$
			for (final String headerLine : header) {
				appendNl(headerLine.replaceFirst("^\\s*[/]?[*][/]?", "%")); //$NON-NLS-1$//$NON-NLS-2$
			}
		}

	}

}

