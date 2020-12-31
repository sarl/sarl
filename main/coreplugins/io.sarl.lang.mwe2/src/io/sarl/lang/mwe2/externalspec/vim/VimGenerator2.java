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

package io.sarl.lang.mwe2.externalspec.vim;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.google.inject.Injector;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xtext.generator.CodeConfig;

import io.sarl.lang.mwe2.externalspec.AbstractExternalHighlightingFragment2;
import io.sarl.lang.mwe2.externalspec.IStyleAppendable;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the Vim editor.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 * @see "http://vimdoc.sourceforge.net/htmldoc/syntax.html"
 * @see "http://vim.wikia.com/wiki/Creating_your_own_syntax_files"
 */
public class VimGenerator2 extends AbstractExternalHighlightingFragment2<IStyleAppendable> {

	/** File extension.
	 */
	public static final String FILE_EXTENSION = ".vim"; //$NON-NLS-1$

	/** The default basename pattern for {@link MessageFormat}.
	 */
	public static final String BASENAME_PATTERN = "{0}" + FILE_EXTENSION; //$NON-NLS-1$

	/** Name of the folder that contains the syntax description.
	 */
	public static final String SYNTAX_FOLDER = "syntax"; //$NON-NLS-1$

	/** Name of the folder that contains the language detection script.
	 */
	public static final String FTDETECT_FOLDER = "ftdetect"; //$NON-NLS-1$

	private final Map<String, VimSyntaxGroup> highlights = new HashMap<>();

	@Override
	protected IStyleAppendable newStyleAppendable() {
		return new VimAppendable(getCodeConfig(), getLanguageSimpleName(), getLanguageVersion());
	}

	@Override
	public String toString() {
		return "Vim"; //$NON-NLS-1$
	}

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
		setBasenameTemplate(BASENAME_PATTERN);
		setOutputDirectoryFilter((folder, basename) -> {
			if (basename.endsWith(FILE_EXTENSION)) {
				return new File(folder, SYNTAX_FOLDER);
			}
			return folder;
		});
	}

	@SuppressWarnings({"checkstyle:parameternumber"})
	@Override
	protected void generate(IStyleAppendable it, Set<String> literals, Set<String> expressionKeywords,
			Set<String> modifiers, Set<String> primitiveTypes, Set<String> punctuation, Set<String> ignored,
			Set<String> specialKeywords, Set<String> typeDeclarationKeywords) {
		it.appendHeader();

		generatePreamble(it);

		generateComments(it);
		generateNumericConstants(it);
		generateStrings(it);

		generateAnnotations(it);

		generatePrimitiveTypes(it, primitiveTypes);

		generateKeywords(it, "sarlLiteral", VimSyntaxGroup.IDENTIFIER, literals); //$NON-NLS-1$
		generateKeywords(it, "sarlSpecial", VimSyntaxGroup.SPECIAL, specialKeywords); //$NON-NLS-1$
		generateKeywords(it, "sarlTypeDeclaration", VimSyntaxGroup.TYPE_DECLARATION, typeDeclarationKeywords); //$NON-NLS-1$
		generateKeywords(it, "sarlModifier", VimSyntaxGroup.STATEMENT, modifiers); //$NON-NLS-1$
		generateKeywords(it, "sarlKeyword", VimSyntaxGroup.STATEMENT, expressionKeywords); //$NON-NLS-1$

		generatePunctuation(it, punctuation);

		generatePostamble(it);
	}

	/** Generate the preamble of the Vim style.
	 *
	 * @param it the receiver of the generated elements.
	 */
	protected void generatePreamble(IStyleAppendable it) {
		clearHilights();
		final String nm = getLanguageSimpleName().toLowerCase();
		final String cmd = Strings.toFirstUpper(getLanguageSimpleName().toLowerCase()) + "HiLink"; //$NON-NLS-1$
		appendComment(it, "Quit when a syntax file was already loaded"); //$NON-NLS-1$
		appendCmd(it, false, "if !exists(\"main_syntax\")").increaseIndentation().newLine(); //$NON-NLS-1$
		appendCmd(it, false, "if exists(\"b:current_syntax\")").increaseIndentation().newLine(); //$NON-NLS-1$
		appendCmd(it, false, "finish").decreaseIndentation().newLine(); //$NON-NLS-1$
		appendCmd(it, "endif"); //$NON-NLS-1$
		appendComment(it, "we define it here so that included files can test for it"); //$NON-NLS-1$
		appendCmd(it, "let main_syntax='" + nm + "'"); //$NON-NLS-1$ //$NON-NLS-2$
		appendCmd(it, false, "syn region " + nm + "Fold start=\"{\" end=\"}\" transparent fold"); //$NON-NLS-1$ //$NON-NLS-2$
		it.decreaseIndentation().newLine();
		appendCmd(it, "endif"); //$NON-NLS-1$
		it.newLine();
		appendCmd(it, "let s:cpo_save = &cpo"); //$NON-NLS-1$
		appendCmd(it, "set cpo&vim"); //$NON-NLS-1$
		it.newLine();
		appendComment(it, "don't use standard HiLink, it will not work with included syntax files"); //$NON-NLS-1$
		appendCmd(it, false, "if version < 508").increaseIndentation().newLine(); //$NON-NLS-1$
		appendCmd(it, false, "command! -nargs=+ " + cmd + " hi link <args>").decreaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		appendCmd(it, false, "else").increaseIndentation().newLine(); //$NON-NLS-1$
		appendCmd(it, false, "command! -nargs=+ " + cmd + " hi def link <args>").decreaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		appendCmd(it, "endif"); //$NON-NLS-1$
		it.newLine();
		appendComment(it, "some characters that cannot be in a SARL program (outside a string)"); //$NON-NLS-1$
		appendMatch(it, nm + "Error", "[\\\\`]"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
	}

	/** Generate the postamble of the Vim style.
	 *
	 * @param it the receiver of the generated elements.
	 */
	protected void generatePostamble(IStyleAppendable it) {
		appendComment(it, "catch errors caused by wrong parenthesis"); //$NON-NLS-1$
		appendCmd(it, "syn region sarlParenT transparent matchgroup=sarlParen  start=\"(\" end=\")\" contains=@sarlTop,sarlParenT1"); //$NON-NLS-1$
		appendCmd(it, "syn region sarlParenT1 transparent matchgroup=sarlParen1 start=\"(\" end=\")\"" //$NON-NLS-1$
				+ " contains=@sarlTop,sarlParenT2 contained"); //$NON-NLS-1$
		appendCmd(it, "syn region sarlParenT2 transparent matchgroup=sarlParen2 start=\"(\" end=\")\"" //$NON-NLS-1$
				+ " contains=@sarlTop,sarlParenT contained"); //$NON-NLS-1$
		appendCmd(it, "syn match sarlParenError \")\""); //$NON-NLS-1$
		appendComment(it, "catch errors caused by wrong square parenthesis"); //$NON-NLS-1$
		appendCmd(it, "syn region sarlParenT transparent matchgroup=sarlParen  start=\"\\[\" end=\"\\]\"" //$NON-NLS-1$
				+ " contains=@sarlTop,sarlParenT1"); //$NON-NLS-1$
		appendCmd(it, "syn region sarlParenT1 transparent matchgroup=sarlParen1 start=\"\\[\" end=\"\\]\"" //$NON-NLS-1$
				+ " contains=@sarlTop,sarlParenT2 contained"); //$NON-NLS-1$
		appendCmd(it, "syn region sarlParenT2 transparent matchgroup=sarlParen2 start=\"\\[\" end=\"\\]\"" //$NON-NLS-1$
				+ " contains=@sarlTop,sarlParenT  contained"); //$NON-NLS-1$
		appendCmd(it, "syn match sarlParenError \"\\]\""); //$NON-NLS-1$
		it.newLine();
		appendCmd(it, "SarlHiLink sarlParenError sarlError"); //$NON-NLS-1$
		it.newLine();
		appendCmd(it, false, "if !exists(\"sarl_minlines\")").increaseIndentation().newLine(); //$NON-NLS-1$
		appendCmd(it, false, "let sarl_minlines = 10").decreaseIndentation().newLine(); //$NON-NLS-1$
		appendCmd(it, "endif"); //$NON-NLS-1$
		appendCmd(it, "exec \"syn sync ccomment sarlComment minlines=\" . sarl_minlines"); //$NON-NLS-1$
		it.newLine();
		appendComment(it, "The default highlighting."); //$NON-NLS-1$
		for (final Entry<String, VimSyntaxGroup> hilight : this.highlights.entrySet()) {
			appendCmd(it, "SarlHiLink " + hilight.getKey() + " " + hilight.getValue().getVimConstant()); //$NON-NLS-1$//$NON-NLS-2$
		}
		clearHilights();
		it.newLine();
		appendCmd(it, "delcommand SarlHiLink"); //$NON-NLS-1$
		it.newLine();
		appendCmd(it, "let b:current_syntax = \"" + getLanguageSimpleName().toLowerCase() + "\""); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		appendCmd(it, false, "if main_syntax == '" + getLanguageSimpleName().toLowerCase() + "'"); //$NON-NLS-1$ //$NON-NLS-2$
		it.increaseIndentation().newLine();
		appendCmd(it, false, "unlet main_syntax").decreaseIndentation().newLine(); //$NON-NLS-1$
		appendCmd(it, "endif"); //$NON-NLS-1$
		it.newLine();
		appendCmd(it, "let b:spell_options=\"contained\""); //$NON-NLS-1$
		appendCmd(it, "let &cpo = s:cpo_save"); //$NON-NLS-1$
		appendCmd(it, "unlet s:cpo_save"); //$NON-NLS-1$

	}

	/** Generate the keywords for the primitive types.
	 *
	 * @param it the receiver of the generated elements.
	 * @param punctuation the operators and punctuation.
	 */
	protected void generatePunctuation(IStyleAppendable it, Iterable<String> punctuation) {
		//
	}

	/** Generate the keywords for the primitive types.
	 *
	 * @param it the receiver of the generated elements.
	 * @param types the primitive types.
	 */
	protected void generatePrimitiveTypes(IStyleAppendable it, Iterable<String> types) {
		final Iterator<String> iterator = types.iterator();
		if (iterator.hasNext()) {
			appendComment(it, "primitive types."); //$NON-NLS-1$
			appendMatch(it, "sarlArrayDeclaration", "\\(\\s*\\[\\s*\\]\\)*", true); //$NON-NLS-1$//$NON-NLS-2$
			it.append("syn keyword sarlPrimitiveType"); //$NON-NLS-1$
			do {
				it.append(" "); //$NON-NLS-1$
				it.append(iterator.next());
			} while (iterator.hasNext());
			it.append(" nextgroup=sarlArrayDeclaration").newLine(); //$NON-NLS-1$
			appendCluster(it, "sarlPrimitiveType"); //$NON-NLS-1$
			hilight("sarlPrimitiveType", VimSyntaxGroup.SPECIAL); //$NON-NLS-1$
			hilight("sarlArrayDeclaration", VimSyntaxGroup.SPECIAL); //$NON-NLS-1$
			it.newLine();
		}
	}

	/** Generate the Vim comments.
	 *
	 * @param it the receiver of the generated elements.
	 */
	protected void generateComments(IStyleAppendable it) {
		appendComment(it, "comments"); //$NON-NLS-1$
		appendRegion(it, "sarlComment", "/\\*", "\\*/", "@Spell"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		appendCmd(it, "syn match sarlCommentStar contained \"^\\s*\\*[^/]\"me=e-1"); //$NON-NLS-1$
		appendMatch(it, "sarlCommentStar", "^\\s*\\*$", true); //$NON-NLS-1$ //$NON-NLS-2$
		appendMatch(it, "sarlLineComment", "//.*", "@Spell"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

		appendCluster(it, "sarlComment", "sarlLineComment"); //$NON-NLS-1$ //$NON-NLS-2$
		hilight("sarlComment", VimSyntaxGroup.COMMENT); //$NON-NLS-1$
		hilight("sarlLineComment", VimSyntaxGroup.COMMENT); //$NON-NLS-1$

		appendComment(it, "match the special comment /**/"); //$NON-NLS-1$
		appendMatch(it, "sarlComment", "/\\*\\*/"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
	}

	/** Generate the Vim strings of characters.
	 *
	 * @param it the receiver of the generated elements.
	 */
	protected void generateStrings(IStyleAppendable it) {
		appendComment(it, "Strings constants"); //$NON-NLS-1$
		appendMatch(it, "sarlSpecialError", "\\\\.", true); //$NON-NLS-1$ //$NON-NLS-2$
		appendMatch(it, "sarlSpecialCharError", "[^']", true); //$NON-NLS-1$ //$NON-NLS-2$
		appendMatch(it, "sarlSpecialChar", "\\\\\\([4-9]\\d\\|[0-3]\\d\\d\\|[\"\\\\'ntbrf]\\|u\\x\\{4\\}\\)", true); //$NON-NLS-1$ //$NON-NLS-2$
		appendRegion(it, true, "sarlString", "\"", "\"", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				"sarlSpecialChar", "sarlSpecialError", "@Spell"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		appendRegion(it, true, "sarlString", "'", "'", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				"sarlSpecialChar", "sarlSpecialError", "@Spell"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		appendCluster(it, "sarlString"); //$NON-NLS-1$
		hilight("sarlString", VimSyntaxGroup.CONSTANT); //$NON-NLS-1$
		it.newLine();
	}

	/** Generate the Vim numeric constants.
	 *
	 * @param it the receiver of the generated elements.
	 */
	protected void generateNumericConstants(IStyleAppendable it) {
		appendComment(it, "numerical constants"); //$NON-NLS-1$
		appendMatch(it, "sarlNumber", "[0-9][0-9]*\\.[0-9]\\+([eE][0-9]\\+)\\?[fFdD]\\?"); //$NON-NLS-1$ //$NON-NLS-2$
		appendMatch(it, "sarlNumber", "0[xX][0-9a-fA-F]\\+"); //$NON-NLS-1$ //$NON-NLS-2$
		appendMatch(it, "sarlNumber", "[0-9]\\+[lL]\\?"); //$NON-NLS-1$ //$NON-NLS-2$
		appendCluster(it, "sarlNumber"); //$NON-NLS-1$
		hilight("sarlNumber", VimSyntaxGroup.CONSTANT); //$NON-NLS-1$
		it.newLine();
	}

	/** Generate the annotations.
	 *
	 * @param it the receiver of the generated elements.
	 */
	protected void generateAnnotations(IStyleAppendable it) {
		appendComment(it, "annnotation"); //$NON-NLS-1$
		appendMatch(it, "sarlAnnotation", "@[_a-zA-Z][_0-9a-zA-Z]*\\([.$][_a-zA-Z][_0-9a-zA-Z]*\\)*"); //$NON-NLS-1$ //$NON-NLS-2$
		appendCluster(it, "sarlAnnotation"); //$NON-NLS-1$
		hilight("sarlAnnotation", VimSyntaxGroup.PRE_PROCESSOR); //$NON-NLS-1$
		it.newLine();
	}

	/** Generate the Vim keywords.
	 *
	 * @param it the receiver of the generated elements.
	 * @param family the name of the keyword family.
	 * @param color the color to be associated to the elements.
	 * @param keywords the keywords.
	 */
	protected void generateKeywords(IStyleAppendable it, String family, VimSyntaxGroup color, Iterable<String> keywords) {
		appendComment(it, "keywords for the '" + family + "' family."); //$NON-NLS-1$ //$NON-NLS-2$
		final Iterator<String> iterator = keywords.iterator();
		if (iterator.hasNext()) {
			it.append("syn keyword "); //$NON-NLS-1$
			it.append(family);
			do {
				it.append(" "); //$NON-NLS-1$
				it.append(iterator.next());
			} while (iterator.hasNext());
		}
		it.newLine();
		appendCluster(it, family);
		hilight(family, color);
		it.newLine();
	}

	/** Clear the hilight definitions.
	 */
	protected void clearHilights() {
		this.highlights.clear();
	}

	/** Hilight the group with the given name as the given Vim syntax group.
	 *
	 * @param name the group to hilight.
	 * @param group the Vim syntax group.
	 */
	protected void hilight(String name, VimSyntaxGroup group) {
		this.highlights.put(name, group);
	}

	/** Append elements to the Vim top cluster.
	 *
	 * @param it the receiver of the generated elements.
	 * @param element0 the first element to add into the cluster.
	 * @param elements the other elements to add into the cluster.
	 * @return {@code it}.
	 */
	protected IStyleAppendable appendCluster(IStyleAppendable it, String element0, String... elements) {
		return appendCluster(it, true, element0, elements);
	}

	/** Append elements to the Vim top cluster.
	 *
	 * @param it the receiver of the generated elements.
	 * @param addNewLine indicates if a new line must be appended.
	 * @param element0 the first element to add into the cluster.
	 * @param elements the other elements to add into the cluster.
	 * @return {@code it}.
	 */
	protected IStyleAppendable appendCluster(IStyleAppendable it, boolean addNewLine, String element0, String... elements) {
		it.append("syn cluster "); //$NON-NLS-1$
		it.append(getLanguageSimpleName().toLowerCase());
		it.append("Top add="); //$NON-NLS-1$
		it.append(element0);
		for (final String element : elements) {
			it.append(","); //$NON-NLS-1$
			it.append(element);
		}
		if (addNewLine) {
			it.newLine();
		}
		return it;
	}

	/** Append a Vim region.
	 *
	 * @param it the receiver of the generated elements.
	 * @param name the name of the pattern.
	 * @param start the start pattern.
	 * @param end the end pattern.
	 * @param contains the contained elements.
	 * @return {@code it}.
	 */
	protected IStyleAppendable appendRegion(IStyleAppendable it, String name, String start, String end, String... contains) {
		return appendRegion(it, true, name, start, end, contains);
	}

	/** Append a Vim region.
	 *
	 * @param it the receiver of the generated elements.
	 * @param addNewLine indicates if a new line must be appended.
	 * @param name the name of the pattern.
	 * @param start the start pattern.
	 * @param end the end pattern.
	 * @param contains the contained elements.
	 * @return {@code it}.
	 */
	protected IStyleAppendable appendRegion(IStyleAppendable it, boolean addNewLine, String name, String start, String end, String... contains) {
		return appendRegion(it, addNewLine, name, start, new String[] {end}, contains);
	}

	/** Append a Vim region.
	 *
	 * @param it the receiver of the generated elements.
	 * @param addNewLine indicates if a new line must be appended.
	 * @param name the name of the pattern.
	 * @param start the start pattern.
	 * @param end the end pattern.
	 * @param contains the contained elements.
	 * @return {@code it}.
	 */
	@SuppressWarnings("static-method")
	protected IStyleAppendable appendRegion(IStyleAppendable it, boolean addNewLine, String name, String start, String[] end, String... contains) {
		it.append("syn region  "); //$NON-NLS-1$
		it.append(name);
		it.append(" start="); //$NON-NLS-1$
		it.append(regexString(start));
		for (final String endPattern : end) {
			it.append(" end="); //$NON-NLS-1$
			it.append(regexString(endPattern));
		}
		if (contains.length > 0) {
			it.append(" contains=").append(contains[0]); //$NON-NLS-1$
			for (int i = 1; i < contains.length; ++i) {
				it.append(",").append(contains[i]); //$NON-NLS-1$
			}
		}
		if (addNewLine) {
			it.newLine();
		}
		return it;
	}

	/** Append a Vim pattern.
	 *
	 * @param it the receiver of the generated elements.
	 * @param name the name of the pattern.
	 * @param pattern the regular expression.
	 * @param contains the elements inside the matched region.
	 * @return {@code it}.
	 */
	protected IStyleAppendable appendMatch(IStyleAppendable it, String name, String pattern, String... contains) {
		return appendMatch(it, true, name, pattern, false, contains);
	}

	/** Append a Vim pattern.
	 *
	 * @param it the receiver of the generated elements.
	 * @param name the name of the pattern.
	 * @param pattern the regular expression.
	 * @param contained indicates if the pattern is matching when it is into a container.
	 * @param contains the elements inside the matched region.
	 * @return {@code it}.
	 */
	protected IStyleAppendable appendMatch(IStyleAppendable it, String name, String pattern, boolean contained, String... contains) {
		return appendMatch(it, true, name, pattern, contained, contains);
	}

	/** Append a Vim pattern.
	 *
	 * @param it the receiver of the generated elements.
	 * @param addNewLine indicates if a new line must be appended.
	 * @param name the name of the pattern.
	 * @param pattern the regular expression.
	 * @param contains the elements inside the matched region.
	 * @return {@code it}.
	 */
	protected IStyleAppendable appendMatch(IStyleAppendable it, boolean addNewLine, String name, String pattern, String... contains) {
		return appendMatch(it, addNewLine, name, pattern, false, contains);
	}

	/** Append a Vim pattern.
	 *
	 * @param it the receiver of the generated elements.
	 * @param addNewLine indicates if a new line must be appended.
	 * @param name the name of the pattern.
	 * @param pattern the regular expression.
	 * @param contained indicates if the pattern is matching when it is into a container.
	 * @param contains the elements inside the matched region.
	 * @return {@code it}.
	 */
	@SuppressWarnings("static-method")
	protected IStyleAppendable appendMatch(IStyleAppendable it, boolean addNewLine, String name,
			String pattern, boolean contained, String... contains) {
		it.append("syn match "); //$NON-NLS-1$
		it.append(name);
		if (contained) {
			it.append(" contained"); //$NON-NLS-1$
		}
		it.append(" "); //$NON-NLS-1$
		it.append(regexString(pattern));
		if (contains.length > 0) {
			it.append(" contains=").append(contains[0]); //$NON-NLS-1$
			for (int i = 1; i < contains.length; ++i) {
				it.append(",").append(contains[i]); //$NON-NLS-1$
			}
		}
		if (addNewLine) {
			it.newLine();
		}
		return it;
	}

	/** Append a Vim comment.
	 *
	 * @param it the receiver of the generated elements.
	 * @param text the text of the comment.
	 * @return {@code it}.
	 */
	protected IStyleAppendable appendComment(IStyleAppendable it, String text) {
		return appendComment(it, true, text);
	}

	/** Append a Vim comment.
	 *
	 * @param it the receiver of the generated elements.
	 * @param addNewLine indicates if a new line must be appended.
	 * @param text the text of the comment.
	 * @return {@code it}.
	 */
	@SuppressWarnings("static-method")
	protected IStyleAppendable appendComment(IStyleAppendable it, boolean addNewLine, String text) {
		it.append("\" "); //$NON-NLS-1$
		it.append(text);
		if (addNewLine) {
			it.newLine();
		}
		return it;
	}

	/** Append a Vim command.
	 *
	 * @param it the receiver of the generated elements.
	 * @param text the text of the command.
	 * @return {@code it}.
	 */
	protected IStyleAppendable appendCmd(IStyleAppendable it, String text) {
		return appendCmd(it, true, text);
	}

	/** Append a Vim command.
	 *
	 * @param it the receiver of the generated elements.
	 * @param addNewLine indicates if a new line must be appended.
	 * @param text the text of the command.
	 * @return {@code it}.
	 */
	@SuppressWarnings("static-method")
	protected IStyleAppendable appendCmd(IStyleAppendable it, boolean addNewLine, String text) {
		it.append(text);
		if (addNewLine) {
			it.newLine();
		}
		return it;
	}

	@Override
	protected Object getReadmeFileContent(String basename) {
		return concat(
				"1. MANUAL INSTALLATION", //$NON-NLS-1$
				"", //$NON-NLS-1$
				"For Unix:", //$NON-NLS-1$
				"* Copy the content of the 'syntax' folder into $HOME/.vim/syntax", //$NON-NLS-1$
				"* Copy the content of the 'ftdetect' folder into $HOME/.vim/ftdetect", //$NON-NLS-1$
				"", //$NON-NLS-1$
				"For Windows:", //$NON-NLS-1$
				"* Copy the content of the 'syntax' folder into C:\\Document and Settings\\User\\vimfiles\\syntax", //$NON-NLS-1$
				"* Copy the content of the 'ftdetect' folder into C:\\Document and Settings\\User\\vimfiles\\ftdetect"); //$NON-NLS-1$
	}

	@Override
	protected void generateAdditionalFiles(String basename, IStyleAppendable appendable) {
		generateFileTypeDetectionScript(basename);
	}

	/** Generate the script for detecting the SARL files.
	 *
	 * @param basename the name of the file to create.
	 * @see #getFileTypeDetectionScript()
	 */
	protected void generateFileTypeDetectionScript(String basename) {
		final CharSequence scriptContent = getFileTypeDetectionScript();
		if (scriptContent != null) {
			final String textualContent = scriptContent.toString();
			if (!Strings.isEmpty(textualContent)) {
				final byte[] bytes = textualContent.getBytes();
				for (final String output : getOutputs()) {
					final File directory = new File(output, FTDETECT_FOLDER).getAbsoluteFile();
					try {
						final File outputFile = new File(directory, basename);
						outputFile.getParentFile().mkdirs();
						Files.write(Paths.get(outputFile.getAbsolutePath()), bytes);
					} catch (IOException e) {
						throw new RuntimeException(e);
					}
				}
			}
		}
	}

	/** Replies the script for detecting the SARL files.
	 *
	 * @return the content of the "ftdetect" script.
	 */
	protected CharSequence getFileTypeDetectionScript() {
		return concat(
				"\" Vim filetype-detection file", //$NON-NLS-1$
				"\" Language: " + getLanguageSimpleName(), //$NON-NLS-1$
				"\" Version: " + getLanguageVersion(), //$NON-NLS-1$
				"", //$NON-NLS-1$
				"au BufRead,BufNewFile *." + getLanguage().getFileExtensions().get(0) //$NON-NLS-1$
				+ " set filetype=" + getLanguageSimpleName().toLowerCase()); //$NON-NLS-1$
	}

	private static String regexString(String expression) {
		if (expression.contains("\"")) { //$NON-NLS-1$
			if (expression.contains("'")) { //$NON-NLS-1$
				if (expression.contains("+")) { //$NON-NLS-1$
					throw new IllegalStateException();
				}
				return "+" + expression + "+"; //$NON-NLS-1$ //$NON-NLS-2$
			}
			return "'" + expression + "'"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		return "\"" + expression + "\""; //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Appendable for Vim styles.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class VimAppendable extends AbstractAppendable {

		/** Constructor.
		 *
		 * @param codeConfig the code configuration.
		 * @param languageName the language name.
		 * @param languageVersion the language version.
		 */
		protected VimAppendable(CodeConfig codeConfig, String languageName, String languageVersion) {
			super("  ", codeConfig, languageName, languageVersion); //$NON-NLS-1$
		}

		@Override
		public void appendComment(String text, Object... parameters) {
			final String comment = applyFormat(text, parameters);
			for (final String line : comment.split("[\n\r]")) { //$NON-NLS-1$
				appendNl("\" " + line.trim()); //$NON-NLS-1$
			}
		}

		@Override
		public void appendHeader() {
			appendNl("\" Vim syntax file"); //$NON-NLS-1$
			appendNl("\" Language: " + getLanguageSimpleName()); //$NON-NLS-1$
			appendNl("\" Version: " + getLanguageVersion()); //$NON-NLS-1$
			final String[] header = Strings.emptyIfNull(getCodeConfig().getFileHeader()).split("[\n\r]+"); //$NON-NLS-1$
			for (final String headerLine : header) {
				appendNl(headerLine.replaceFirst("^\\s*[/]?[*][/]?", "\" ")); //$NON-NLS-1$//$NON-NLS-2$
			}
			newLine().newLine();
		}

	}

	/** Syntax groups in vim.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected enum VimSyntaxGroup {
		/** Normal text.
		 */
		NORMAL("Normal"), //$NON-NLS-1$
		/** Comment.
		 */
		COMMENT("Comment"), //$NON-NLS-1$
		/** Constant.
		 */
		CONSTANT("Constant"), //$NON-NLS-1$
		/** Special word.
		 */
		SPECIAL("Special"), //$NON-NLS-1$
		/** Identifier.
		 */
		IDENTIFIER("Identifier"), //$NON-NLS-1$
		/** Statement.
		 */
		STATEMENT("Statement"), //$NON-NLS-1$
		/** Pre-processor.
		 */
		PRE_PROCESSOR("PreProc"), //$NON-NLS-1$
		/** Type declaration.
		 */
		TYPE_DECLARATION("Type"), //$NON-NLS-1$
		/** Underlined text.
		 */
		UNDERLINED("Underlined"), //$NON-NLS-1$
		/** To-Do.
		 */
		TODO("Todo"), //$NON-NLS-1$
		/** Operator.
		 */
		OPERATOR("Operator"); //$NON-NLS-1$

		private final String vimConstant;

		VimSyntaxGroup(String vimConstant) {
			this.vimConstant = vimConstant;
		}

		/** Replies the VIM constant for the syntax group.
		 *
		 * @return the VIM constant.
		 */
		public String getVimConstant() {
			return this.vimConstant;
		}

	}

}

