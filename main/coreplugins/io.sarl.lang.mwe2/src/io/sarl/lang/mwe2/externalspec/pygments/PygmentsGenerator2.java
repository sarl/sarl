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

package io.sarl.lang.mwe2.externalspec.pygments;

import java.text.MessageFormat;
import java.util.Set;

import com.google.inject.Injector;

import io.sarl.lang.mwe2.externalspec.AbstractScriptHighlightingFragment2;
import io.sarl.lang.mwe2.externalspec.IStyleAppendable;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the Python syntax highlighted, aka. Pygments.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://pygments.org/"
 */
public class PygmentsGenerator2 extends AbstractScriptHighlightingFragment2 {

	/** The default basename pattern for {@link MessageFormat}.
	 */
	public static final String BASENAME_PATTERN = "{0}.py"; //$NON-NLS-1$

	private String url;

	/** Replies the URL of the language.
	 *
	 * @return the url.
	 */
	public String getUrl() {
		return this.url;
	}

	/** Set the URL of the language.
	 *
	 * @param url the url.
	 */
	public void setUrl(String url) {
		this.url = url;
	}

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
		setBasenameTemplate(BASENAME_PATTERN);
	}

	@Override
	public String toString() {
		return "Pygments"; //$NON-NLS-1$
	}

	@SuppressWarnings({"checkstyle:parameternumber", "checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	@Override
	protected void generate(IStyleAppendable it, Set<String> literals, Set<String> expressionKeywords,
			Set<String> modifiers, Set<String> primitiveTypes, Set<String> punctuation, Set<String> ignored,
			Set<String> specialKeywords, Set<String> typeDeclarationKeywords) {
		it.appendNl("# -*- coding: {0} -*-", getCodeConfig().getEncoding().toLowerCase()); //$NON-NLS-1$
		it.appendHeader();
		it.newLine();
		it.appendNl("import re"); //$NON-NLS-1$
		it.newLine();
		it.appendNl("from pygments.lexer import Lexer, RegexLexer, include, bygroups, using, this"); //$NON-NLS-1$
		it.appendNl("from pygments.token import Text, Comment, Operator, Keyword, Name, String, Number, Punctuation"); //$NON-NLS-1$
		it.newLine();
		it.appendNl("class SarlLexer(RegexLexer):"); //$NON-NLS-1$
		it.appendNl("\t\"\"\""); //$NON-NLS-1$
		it.appendNl("\tFor `{0} <{1}>`_ source code.", getLanguageSimpleName(), getUrl()); //$NON-NLS-1$
		it.appendNl("\t"); //$NON-NLS-1$
		it.appendNl("\t.. versionadded:: {0}", getLanguageVersion()); //$NON-NLS-1$
		it.appendNl("\t\"\"\""); //$NON-NLS-1$
		it.appendNl("\t"); //$NON-NLS-1$
		it.appendNl("\tname = ''{0}''", getLanguageSimpleName()); //$NON-NLS-1$
		it.appendNl("\taliases = [''{0}'']", getLanguageSimpleName().toLowerCase()); //$NON-NLS-1$
		final StringBuilder pyFileExtensions = new StringBuilder();
		for (final String fileExtension : getLanguage().getFileExtensions()) {
			if (pyFileExtensions.length() > 0) {
				pyFileExtensions.append(","); //$NON-NLS-1$
			}
			pyFileExtensions.append("'*.").append(fileExtension).append("'"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		it.appendNl("\tfilenames = [{0}]", pyFileExtensions); //$NON-NLS-1$
		final StringBuilder pyMimes = new StringBuilder();
		for (final String mimeType : getMimeTypes()) {
			if (pyMimes.length() > 0) {
				pyMimes.append(","); //$NON-NLS-1$
			}
			pyMimes.append("'").append(mimeType).append("'"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		it.appendNl("\tmimetypes = [{0}]", pyMimes); //$NON-NLS-1$
		it.appendNl("\t"); //$NON-NLS-1$
		it.appendNl("\tflags = re.MULTILINE | re.DOTALL"); //$NON-NLS-1$
		it.appendNl("\t"); //$NON-NLS-1$
		it.appendNl("\ttokens = {"); //$NON-NLS-1$
		it.appendNl("\t\t'root': ["); //$NON-NLS-1$
		it.appendNl("\t\t\t# method names"); //$NON-NLS-1$
		it.appendNl("\t\t\t(r'^(\\s*(?:[a-zA-Z_][\\w.\\[\\]]*\\s+)+?)'  # return arguments"); //$NON-NLS-1$
		it.appendNl("\t\t\t r'([a-zA-Z_$][\\w$]*)'                      # method name"); //$NON-NLS-1$
		it.appendNl("\t\t\t r'(\\s*)(\\()',                             # signature start"); //$NON-NLS-1$
		it.appendNl("\t\t\t bygroups(using(this), Name.Function, Text, Operator)),"); //$NON-NLS-1$
		it.appendNl("\t\t\t(r'[^\\S\\n]+', Text),"); //$NON-NLS-1$
		it.appendNl("\t\t\t(r'//.*?\\n', Comment.Single),"); //$NON-NLS-1$
		it.appendNl("\t\t\t(r'/\\*.*?\\*/', Comment.Multiline),"); //$NON-NLS-1$
		it.appendNl("\t\t\t(r'@[a-zA-Z_][\\w.]*', Name.Decorator),"); //$NON-NLS-1$

		final StringBuilder rawKeywords = new StringBuilder();
		for (final String keyword : expressionKeywords) {
			if (rawKeywords.length() > 0) {
				rawKeywords.append("|"); //$NON-NLS-1$
			}
			rawKeywords.append(keyword);
		}
		it.appendNl("\t\t\t(r''({0})\\b'',", rawKeywords); //$NON-NLS-1$
		it.appendNl("\t\t\t Keyword),"); //$NON-NLS-1$

		final StringBuilder rawModifiers = new StringBuilder();
		for (final String modifier : modifiers) {
			if (rawModifiers.length() > 0) {
				rawModifiers.append("|"); //$NON-NLS-1$
			}
			rawModifiers.append(modifier);
		}
		it.appendNl("\t\t\t(r''({0})\\b'', Keyword.Declaration),", rawModifiers); //$NON-NLS-1$

		final StringBuilder rawPrimitives = new StringBuilder();
		for (final String primitive : primitiveTypes) {
			if (rawPrimitives.length() > 0) {
				rawPrimitives.append("|"); //$NON-NLS-1$
			}
			rawPrimitives.append(primitive);
		}
		it.appendNl("\t\t\t (r''({0})\\b'',", rawPrimitives); //$NON-NLS-1$
		it.appendNl("\t\t\t  Keyword.Type),"); //$NON-NLS-1$

		// Special keyword
		final StringBuilder rawSpecials = new StringBuilder();
		for (final String special : specialKeywords) {
			if (!"import".equals(special)) { //$NON-NLS-1$
				if (rawSpecials.length() > 0) {
					rawSpecials.append("|"); //$NON-NLS-1$
				}
				rawSpecials.append(special);
			}
		}
		it.appendNl("\t\t\t (r''({0})(\\s+)'', bygroups(Keyword.Namespace, Text)),", rawSpecials); //$NON-NLS-1$

		// Literals
		final StringBuilder rawLiterals = new StringBuilder();
		for (final String literal : literals) {
			if (rawLiterals.length() > 0) {
				rawLiterals.append("|"); //$NON-NLS-1$
			}
			rawLiterals.append(literal);
		}
		it.appendNl("\t\t\t (r''({0})\\b'', Keyword.Constant),", rawLiterals); //$NON-NLS-1$

		// typeDeclarationKeywords
		final StringBuilder rawTypeDeclarations = new StringBuilder();
		for (final String typeDeclaration : typeDeclarationKeywords) {
			if (rawTypeDeclarations.length() > 0) {
				rawTypeDeclarations.append("|"); //$NON-NLS-1$
			}
			rawTypeDeclarations.append(typeDeclaration);
		}
		it.appendNl("\t\t\t (r''({0})(\\s+)'', bygroups(Keyword.Declaration, Text),", rawTypeDeclarations); //$NON-NLS-1$
		it.appendNl("\t\t\t  'class'),"); //$NON-NLS-1$

		// Special keyword
		final StringBuilder rawImports = new StringBuilder();
		for (final String special : specialKeywords) {
			if ("import".equals(special)) { //$NON-NLS-1$
				if (rawImports.length() > 0) {
					rawImports.append("|"); //$NON-NLS-1$
				}
				rawImports.append(special);
			}
		}
		it.appendNl("\t\t\t (r''({0})(\\s+)'', bygroups(Keyword.Namespace, Text), ''import''),", rawImports); //$NON-NLS-1$

		//it.appendNl("\t\t\t (r\"(''')\", String, 'template'),"); //$NON-NLS-1$
		//it.appendNl("\t\t\t (u'(\\u00BB)', String, 'template'),"); //$NON-NLS-1$
		it.appendNl("\t\t\t (r'\"(\\\\\\\\|\\\\\"|[^\"])*\"', String),"); //$NON-NLS-1$
		it.appendNl("\t\t\t (r\"'(\\\\\\\\|\\\\'|[^'])*'\", String),"); //$NON-NLS-1$
		it.appendNl("\t\t\t (r'[a-zA-Z_]\\w*:', Name.Label),"); //$NON-NLS-1$
		it.appendNl("\t\t\t (r'[a-zA-Z_$]\\w*', Name),"); //$NON-NLS-1$

		// Punctuation
		it.appendNl("\t\t\t (r'[~^*!%&\\[\\](){}<>\\|+=:;,./?-]', Operator),"); //$NON-NLS-1$

		it.appendNl("\t\t\t (r'[0-9][0-9]*\\.[0-9]+([eE][0-9]+)?[fFdD]?', Number.Float),"); //$NON-NLS-1$
		it.appendNl("\t\t\t (r'0[xX][0-9a-fA-F]+', Number.Hex),"); //$NON-NLS-1$
		it.appendNl("\t\t\t (r'[0-9]+[lL]?', Number.Integer),"); //$NON-NLS-1$
		it.appendNl("\t\t\t (r'\\n', Text)"); //$NON-NLS-1$
		it.appendNl("\t\t],"); //$NON-NLS-1$

		it.appendNl("\t\t'class': ["); //$NON-NLS-1$
		it.appendNl("\t\t\t(r'[a-zA-Z_]\\w*', Name.Class, '#pop')"); //$NON-NLS-1$
		it.appendNl("\t\t],"); //$NON-NLS-1$
		it.appendNl("\t\t'import': ["); //$NON-NLS-1$
		it.appendNl("\t\t\t(r'[\\w.]+\\*?', Name.Namespace, '#pop')"); //$NON-NLS-1$
		it.appendNl("\t\t],"); //$NON-NLS-1$
		//it.appendNl("\t\t'template': ["); //$NON-NLS-1$
		//it.appendNl("\t\t\t(r\"'''\", String, '#pop'),"); //$NON-NLS-1$
		//it.appendNl("\t\t\t(u'\\u00AB', String, '#pop'),"); //$NON-NLS-1$
		//it.appendNl("\t\t\t(r'.', String)"); //$NON-NLS-1$
		//it.appendNl("\t\t],"); //$NON-NLS-1$
		it.appendNl("\t}"); //$NON-NLS-1$
		it.newLine();
	}

	@Override
	protected void generateAdditionalFiles(String basename, IStyleAppendable writtenAppendable) {
		IStyleAppendable appendable;

		appendable = newStyleAppendable();
		generatePythonPackage(appendable, basename);
		writeFile("__init__.py", appendable); //$NON-NLS-1$

		appendable = newStyleAppendable();
		generatePythonSetup(appendable, basename);
		writeFile("setup.py", appendable, (folder, bname) -> folder.getParentFile()); //$NON-NLS-1$
	}

	/** Create the content of the "setup.py" file.
	 *
	 * @param it the content.
	 * @param basename the basename.
	 */
	protected void generatePythonSetup(IStyleAppendable it, String basename) {
		it.appendNl("# -*- coding: {0} -*-", getCodeConfig().getEncoding().toLowerCase()); //$NON-NLS-1$
		it.appendHeader();
		it.newLine();
		it.append("from setuptools import setup"); //$NON-NLS-1$
		it.newLine().newLine();
		it.append("setup ("); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("name='").append(basename).append("lexer',"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("version='").append(getLanguageVersion()).append("',"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("packages=['").append(basename).append("lexer'],"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("entry_points ="); //$NON-NLS-1$
		it.newLine();
		it.append("\"\"\""); //$NON-NLS-1$
		it.newLine();
		it.append("[pygments.lexers]"); //$NON-NLS-1$
		it.newLine();
		it.append("sarllexer = ").append(basename).append("lexer.").append(basename); //$NON-NLS-1$ //$NON-NLS-2$
		it.append(":SarlLexer"); //$NON-NLS-1$
		it.newLine();
		it.append("\"\"\","); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append(")"); //$NON-NLS-1$
		it.newLine();
	}

	/** Create the content of the "__init__.py" file.
	 *
	 * @param it the content.
	 * @param basename the basename.
	 */
	protected void generatePythonPackage(IStyleAppendable it, String basename) {
		it.appendNl("# -*- coding: {0} -*-", getCodeConfig().getEncoding().toLowerCase()); //$NON-NLS-1$
		it.appendHeader();
		it.newLine();
		it.append("__all__ = [ ]"); //$NON-NLS-1$
		it.newLine();
	}

	@Override
	protected Object getReadmeFileContent(String basename) {
		return null;
	}

}

