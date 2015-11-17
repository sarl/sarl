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

	private String outputName;

	private String floatBasicStyle;

	private String identifierStyle;

	private String commentStyle;

	private String stringStyle;

	private String keywordStyle;

	private String inlineBasicStyle;

	/** Construct the LaTeX generator.
	 */
	public LaTeXListingsGenerator() {
		setFloatBasicStyle("\\normalcolor\\scriptsize"); //$NON-NLS-1$
		setInlineBasicStyle("\\normalcolor\\normalsize"); //$NON-NLS-1$
		setIdentifierStyle("\\color{SARLidentifier}\\ttfamily"); //$NON-NLS-1$
		setCommentStyle("\\color{SARLcomment}\\tiny\\bfseries"); //$NON-NLS-1$
		setStringStyle("\\color{SARLstring}\ttfamily"); //$NON-NLS-1$
		setKeywordStyle("\\color{SARLkeyword}\\bfseries"); //$NON-NLS-1$
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
		append(sty, "\\RequirePackage'{'xcolor'}'"); //$NON-NLS-1$

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

		String langName = getLanguage().toUpperCase();
		append(sty, "\\lstdefinelanguage'{'{0}'}{'%", langName); //$NON-NLS-1$
		append(sty, "   morecomment=[s]'{'/*'}{'*/'}',"); //$NON-NLS-1$
		append(sty, "   morestring=[b]\","); //$NON-NLS-1$
		append(sty, "   morekeywords='{'{0}'}',", toString(texKeywords, false)); //$NON-NLS-1$
		append(sty, "'}'"); //$NON-NLS-1$

		append(sty, "\\lstset'{'%"); //$NON-NLS-1$
		append(sty, "   basicstyle={0}, % the size of the fonts that are used for the code", //$NON-NLS-1$
				Strings.emptyIfNull(this.floatBasicStyle));
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
		append(sty, "   identifierstyle={0},", Strings.emptyIfNull(this.identifierStyle)); //$NON-NLS-1$
		append(sty, "   commentstyle={0},", Strings.emptyIfNull(this.commentStyle)); //$NON-NLS-1$
		append(sty, "   stringstyle={0},", Strings.emptyIfNull(this.stringStyle)); //$NON-NLS-1$
		append(sty, "   keywordstyle={0}, % keyword style", Strings.emptyIfNull(this.keywordStyle)); //$NON-NLS-1$
		append(sty, "   language={0}, % the default language of the code", langName); //$NON-NLS-1$
		append(sty, "   showspaces=false, % show spaces everywhere adding particular " //$NON-NLS-1$
				+ "underscores; it overrides ''showstringspaces''"); //$NON-NLS-1$
		append(sty, "   showstringspaces=false, % underline spaces within strings only"); //$NON-NLS-1$
		append(sty, "   showtabs=false, % show tabs within strings adding particular underscores"); //$NON-NLS-1$
		append(sty, "   stepnumber=2, % the step between two line-numbers. If it''s 1, each line will be numbered"); //$NON-NLS-1$
		append(sty, "   tabsize=2, % sets default tabsize to 2 spaces"); //$NON-NLS-1$
		append(sty, "   title=\\lstname, % show the filename of files included with " //$NON-NLS-1$
				+ "\\lstinputlisting; also try caption instead of title"); //$NON-NLS-1$
		append(sty, "   frameround=fttt, % If framed, use this rounded corner style"); //$NON-NLS-1$
		append(sty, "'}'"); //$NON-NLS-1$

		append(sty, "\\newcommand'{'\\code'}'[1]'{{'\\lstinline[basicstyle={0}]'{'#1'}}}'", //$NON-NLS-1$
				Strings.emptyIfNull(this.inlineBasicStyle));

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

