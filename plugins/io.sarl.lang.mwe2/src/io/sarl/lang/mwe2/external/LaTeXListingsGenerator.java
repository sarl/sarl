/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.mwe2.external;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.ibm.icu.text.SimpleDateFormat;
import org.eclipse.xtext.generator.IGeneratorFragment;
import org.eclipse.xtext.util.Strings;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the Google prettify library.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class LaTeXListingsGenerator extends ExternalLanguageSpecificationGenerator {

	/** Default definition for the basic style for floating algorithms (without colors).
	 */
	public static final String DEFAULT_FLOAT_BASIC_STYLE = "\\normalcolor\\smaller\\smaller"; //$NON-NLS-1$

	/** Default definition for the basic style for floating algorithms (with colors).
	 */
	public static final String DEFAULT_COLORIZED_FLOAT_BASIC_STYLE = DEFAULT_FLOAT_BASIC_STYLE;

	/** Default definition for the basic style for inline code (without color).
	 */
	public static final String DEFAULT_INLINE_BASIC_STYLE = "\\normalcolor"; //$NON-NLS-1$

	/** Default definition for the basic style for inline code (with colors).
	 */
	public static final String DEFAULT_COLORIZED_INLINE_BASIC_STYLE = DEFAULT_INLINE_BASIC_STYLE;

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

	private String outputName;

	private String floatBasicStyle;

	private String identifierStyle;

	private String commentStyle;

	private String stringStyle;

	private String keywordStyle;

	private String inlineBasicStyle;

	private boolean useColors = true;
	
	private boolean showLines = true;

	private int lineStep = 2;

	private int tabSize = 2;

	private boolean showSpecialCharacters = false;

	private Collection<String> requirements;

	/** Construct the LaTeX generator.
	 */
	public LaTeXListingsGenerator() {
		//
	}

	/** Set the TeX style of the standard code in floats.
	 *
	 * @param style the TeX code that describes the style. If <code>null</code>
	 *     or empty, the default style is applied.
	 */
	public void setFloatBasicStyle(String style) {
		this.floatBasicStyle = style;
	}

	/** Set the TeX style of the inline standard code.
	 *
	 * @param style the TeX code that describes the style. If <code>null</code>
	 *     or empty, the default style is applied.
	 */
	public void setInlineBasicStyle(String style) {
		this.inlineBasicStyle = style;
	}

	/** Set the TeX style of the identifiers.
	 *
	 * @param style the TeX code that describes the style. If <code>null</code>
	 *     or empty, the default style is applied.
	 */
	public void setIdentifierStyle(String style) {
		this.identifierStyle = style;
	}

	/** Set the TeX style of the comments.
	 *
	 * @param style the TeX code that describes the style. If <code>null</code>
	 *     or empty, the default style is applied.
	 */
	public void setCommentStyle(String style) {
		this.commentStyle = style;
	}

	/** Set the TeX style of the strings of characters.
	 *
	 * @param style the TeX code that describes the style. If <code>null</code>
	 *     or empty, the default style is applied.
	 */
	public void setStringStyle(String style) {
		this.stringStyle = style;
	}

	/** Set the TeX style of the strings of keywords.
	 *
	 * @param style the TeX code that describes the style. If <code>null</code>
	 *     or empty, the default style is applied.
	 */
	public void setKeywordStyle(String style) {
		this.keywordStyle = style;
	}

	/** Set if the TeX style uses colors.
	 *
	 * @param useColors <code>true</code> for generating a style that uses colors.
	 */
	public void setUseColors(boolean useColors) {
		this.useColors = useColors;
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

	@Override
	protected String getHumanReadableSpecificationName() {
		return "LaTeX Listings style"; //$NON-NLS-1$
	}

	/** Set the basename of generated file that contains the Beamer style.
	 *
	 * @param name the basename of the file.
	 */
	public void setOutputName(String name) {
		if (!Strings.isEmpty(name)) {
			this.outputName = name;
		}
	}

	private static void append(List<String> buffer, String text, Object... parameters) {
		buffer.add(MessageFormat.format(text, parameters));
	}

	/** Compute the name of the sty file when it is not provided in the configuration.
	 *
	 * @param languageName the name of the language.
	 * @return the basename of the sty file.
	 */
	@SuppressWarnings("static-method")
	protected String computeDefaultStyBasename(String languageName) {
		return languageName.toLowerCase() + "-listing"; //$NON-NLS-1$
	}

	@Override
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	protected void generate(Set<String> literals, Set<String> keywords, Set<String> punctuation, Set<String> ignored) {
		Set<String> texKeywords = new TreeSet<>(keywords);
		texKeywords.addAll(literals);
		this.log.info(MessageFormat.format("\tKeywords: {0}", toString(texKeywords))); //$NON-NLS-1$

		List<String> sty = new ArrayList<>();

		String basename = this.outputName;
		if (Strings.isEmpty(basename)) {
			basename = computeDefaultStyBasename(getLanguage());
		}

		append(sty, "\\NeedsTeXFormat'{'LaTeX2e'}'[1995/12/01]"); //$NON-NLS-1$
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd"); //$NON-NLS-1$
		append(sty, "\\ProvidesPackage'{'{0}'}'[{1}]", basename, dateFormat.format(new Date())); //$NON-NLS-1$

		append(sty, "\\RequirePackage'{'algpseudocode'}'"); //$NON-NLS-1$
		append(sty, "\\RequirePackage'{'listings'}'"); //$NON-NLS-1$
		append(sty, "\\RequirePackage'{'xspace'}'"); //$NON-NLS-1$
		Collection<String> requirements = this.requirements;
		if (requirements == null) {
			requirements = Arrays.asList(DEFAULT_REQUIREMENTS);
		}
		for (String requirement : requirements) {
			append(sty, "\\RequirePackage'{'{0}'}'", requirement); //$NON-NLS-1$
		}
		if (this.useColors) {
			append(sty, "\\RequirePackage'{'xcolor'}'"); //$NON-NLS-1$
		}

		if (this.useColors) {
			append(sty, "\\definecolor'{'SARLblue'}{'RGB'}{'43,88,121'}'"); //$NON-NLS-1$
			append(sty, "\\definecolor'{'SARLlightblue'}{'RGB'}{'0,123,191'}'"); //$NON-NLS-1$
			append(sty, "\\definecolor'{'SARLlightestblue'}{'RGB'}{'223,239,247'}'"); //$NON-NLS-1$
			append(sty, "\\definecolor'{'SARLmagenta'}{'RGB'}{'153,30,102'}'"); //$NON-NLS-1$
			append(sty, "\\definecolor'{'SARLlightmagenta'}{'RGB'}{'204,122,170'}'"); //$NON-NLS-1$
			append(sty, "\\definecolor'{'SARLlightestmagenta'}{'RGB'}{'250,240,246'}'"); //$NON-NLS-1$
			append(sty, "\\definecolor'{'SARLgreen'}{'RGB'}{'0,128,0'}'"); //$NON-NLS-1$
			append(sty, "\\definecolor'{'SARLdarkgray'}{'RGB'}{'99,104,110'}'"); //$NON-NLS-1$
			append(sty, "\\colorlet'{'SARLcomment'}{'SARLgreen'}'"); //$NON-NLS-1$
			append(sty, "\\colorlet'{'SARLstring'}{'SARLmagenta'}'"); //$NON-NLS-1$
			append(sty, "\\colorlet'{'SARLkeyword'}{'SARLblue'}'"); //$NON-NLS-1$
			append(sty, "\\colorlet'{'SARLidentifier'}{'SARLdarkgray'}'"); //$NON-NLS-1$
		}

		String langName = getLanguage().toUpperCase();
		append(sty, "\\lstdefinelanguage'{'{0}'}{'%", langName); //$NON-NLS-1$
		append(sty, "   morecomment=[s]'{'/*'}{'*/'}',"); //$NON-NLS-1$
		append(sty, "   morestring=[b]\","); //$NON-NLS-1$
		append(sty, "   morekeywords='{'{0}'}',", toString(texKeywords, false)); //$NON-NLS-1$
		append(sty, "'}'"); //$NON-NLS-1$

		append(sty, "\\lstset'{'%"); //$NON-NLS-1$

		String floatBasicStyle = this.floatBasicStyle;
		if (floatBasicStyle == null) {
			floatBasicStyle = (this.useColors) ? DEFAULT_COLORIZED_FLOAT_BASIC_STYLE : DEFAULT_FLOAT_BASIC_STYLE;
		}
		floatBasicStyle = Strings.emptyIfNull(floatBasicStyle);
		append(sty, "   basicstyle={0}, % the size of the fonts that are used for the code", floatBasicStyle); //$NON-NLS-1$

		append(sty, "   breakatwhitespace=false, % sets if automatic breaks should only happen at whitespace"); //$NON-NLS-1$
		append(sty, "   breaklines=true, % sets automatic line breaking"); //$NON-NLS-1$
		append(sty, "   captionpos=b, % sets the caption-position to bottom"); //$NON-NLS-1$
		append(sty, "   deletekeywords='{'filter'}', % if you want to delete keywords from the given language"); //$NON-NLS-1$
		append(sty, "   escapeinside='{'(*@'}{'@*)'}', % if you want to add LaTeX within your code"); //$NON-NLS-1$
		append(sty, "   extendedchars=true, % lets you use non-ASCII characters; for 8-bits " //$NON-NLS-1$
				+ "encodings only, does not work with UTF-8"); //$NON-NLS-1$
		append(sty, "   frame=none, % no frame around the code"); //$NON-NLS-1$
		append(sty, "   keepspaces=true, % keeps spaces in text, useful for keeping " //$NON-NLS-1$
				+ "indentation of code (possibly needs columns=flexible)"); //$NON-NLS-1$

		String identifierStyle = this.identifierStyle;
		if (identifierStyle == null) {
			identifierStyle = (this.useColors) ? DEFAULT_COLORIZED_IDENTIFIER_STYLE : DEFAULT_IDENTIFIER_STYLE;
		}
		identifierStyle = Strings.emptyIfNull(identifierStyle);
		append(sty, "   identifierstyle={0},", identifierStyle); //$NON-NLS-1$

		String commentStyle = this.commentStyle;
		if (commentStyle == null) {
			commentStyle = (this.useColors) ? DEFAULT_COLORIZED_COMMENT_STYLE : DEFAULT_COMMENT_STYLE;
		}
		commentStyle = Strings.emptyIfNull(commentStyle);
		append(sty, "   commentstyle={0},", commentStyle); //$NON-NLS-1$

		String stringStyle = this.stringStyle;
		if (stringStyle == null) {
			stringStyle = (this.useColors) ? DEFAULT_COLORIZED_STRING_STYLE : DEFAULT_STRING_STYLE;
		}
		stringStyle = Strings.emptyIfNull(stringStyle);
		append(sty, "   stringstyle={0},", stringStyle); //$NON-NLS-1$

		String keywordStyle = this.keywordStyle;
		if (keywordStyle == null) {
			keywordStyle = (this.useColors) ? DEFAULT_COLORIZED_KEYWORD_STYLE : DEFAULT_KEYWORD_STYLE;
		}
		keywordStyle = Strings.emptyIfNull(keywordStyle);
		append(sty, "   keywordstyle={0}, % keyword style", keywordStyle); //$NON-NLS-1$

		append(sty, "   language={0}, % the default language of the code", langName); //$NON-NLS-1$

		append(sty, "   showspaces={0}, % show spaces everywhere adding particular " //$NON-NLS-1$
				+ "underscores; it overrides ''showstringspaces''", this.showSpecialCharacters); //$NON-NLS-1$
		append(sty, "   showstringspaces={0}, % underline spaces within strings only", //$NON-NLS-1$
				this.showSpecialCharacters);
		append(sty, "   showtabs={0}, % show tabs within strings adding particular underscores", //$NON-NLS-1$
				this.showSpecialCharacters);
		if (this.showLines) {
			append(sty, "   numbers=left,% Numbers on left"); //$NON-NLS-1$
			append(sty, "   firstnumber=1, % First line number"); //$NON-NLS-1$
			append(sty, "   numberfirstline=false, %Start numbers at first line"); //$NON-NLS-1$
			append(sty, "   stepnumber={0}, % the step between two line-numbers. " //$NON-NLS-1$
					+ "If it''s 1, each line will be numbered", this.lineStep); //$NON-NLS-1$
		}
		append(sty, "   tabsize={0}, % sets default tabsize to 2 spaces", this.tabSize); //$NON-NLS-1$
		append(sty, "   title=\\lstname, % show the filename of files included with " //$NON-NLS-1$
				+ "\\lstinputlisting; also try caption instead of title"); //$NON-NLS-1$
		append(sty, "   frameround=fttt, % If framed, use this rounded corner style"); //$NON-NLS-1$
		append(sty, "'}'"); //$NON-NLS-1$

		String inlineBasicStyle = this.inlineBasicStyle;
		if (inlineBasicStyle == null) {
			inlineBasicStyle = (this.useColors) ? DEFAULT_COLORIZED_INLINE_BASIC_STYLE : DEFAULT_INLINE_BASIC_STYLE;
		}
		inlineBasicStyle = Strings.emptyIfNull(inlineBasicStyle);
		append(sty, "\\newcommand'{'\\code'}'[1]'{{'\\lstinline[basicstyle={0}]'{'#1'}}}'", inlineBasicStyle); //$NON-NLS-1$
		append(sty, "\\newcommand'{'\\sarl'}{'\\mbox'{'SARL'}'\\xspace'}'"); //$NON-NLS-1$

		append(sty, "\\endinput"); //$NON-NLS-1$

		this.log.debug(sty.toString());

		basename += ".sty"; //$NON-NLS-1$

		for (String output : getOutputs()) {
			File directory = new File(output).getAbsoluteFile();
			try {
				this.log.info(MessageFormat.format("\twriting into {0}", directory.getAbsolutePath())); //$NON-NLS-1$
				directory.mkdirs();
				File outputFile = new File(directory, basename);
				Files.write(Paths.get(outputFile.getAbsolutePath()), sty);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}

}

