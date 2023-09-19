/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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
 *
 *------- FORKED SOURCE CODE:
 *
 * THIS CODE IS FORKED FROM JDK.JAVADOC INTERNAL PACKAGE AND ADAPTED TO THE SARL PURPOSE.
 * THE FORK WAS NECESSARY BECAUSE IT IS IMPOSSIBLE TO SUBCLASS THE TYPES FOR THE.
 * STANDARD HTML DOCLET THAT IS PROVIDED BY JDK.JAVADOC MODULE.
 *
 * Copyright (c) 2003, 2021, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package io.sarl.docs.doclet2.html.summaries;

import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.Collection;

import javax.tools.Diagnostic.Kind;

import jdk.javadoc.doclet.Reporter;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.AbstractDocumentationGenerator;
import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.Navigation;

/** Abstract implementation of a generator summary.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.13.0 20230919-093059
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public abstract class AbstractSummaryGenerator extends AbstractDocumentationGenerator {

	private String title;

	/** Constructor.
	 */
	public AbstractSummaryGenerator() {
		this.title = null;
	}

	/** Constructor.
	 *
	 * @param title the title of the page.
	 */
	public AbstractSummaryGenerator(String title) {
		this.title = title;
	}

	/** Change the default title.
	 *
	 * @param title the title of the page.
	 */
	protected void setDefaultTitle(String title) {
		this.title = title;
	}
	
	/** Generate the summary.
	 *
	 * @param logMessage the initial log message.
	 * @param basename the basename of the file of the summary
	 * @param cssStylesheets the list of available CSS style sheets.
	 * @param jsScripts the list of available Javascript scripts.
	 * @param environment the generation environment.
	 * @param cliOptions the options provided on the CLI.
	 * @param reporter the tool for reporting errors, warnings and notes.
	 * @throws Exception error during generation.
	 */
	protected void generate(String logMessage, String basename, Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		initGenerator(cssStylesheets, jsScripts, reporter, environment, cliOptions);
		getReporter().print(Kind.NOTE, logMessage);
		computePaths(basename, false);
		generate();
	}

	/** Generate the summary.
	 *
	 * @param logMessage the initial log message.
	 * @param filename relative path to the generated file.
	 * @param cssStylesheets the list of available CSS style sheets.
	 * @param jsScripts the list of available Javascript scripts.
	 * @param environment the generation environment.
	 * @param cliOptions the options provided on the CLI.
	 * @param reporter the tool for reporting errors, warnings and notes.
	 * @throws Exception error during generation.
	 */
	protected void generate(String logMessage, Path filename, Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		initGenerator(cssStylesheets, jsScripts, reporter, environment, cliOptions);
		getReporter().print(Kind.NOTE, logMessage);
		computePaths(filename, true);
		generate();
	}

	private void generate() throws Exception {
		final Path outputPath = getDocletOptions().getOutputDirectory().resolve(getRelativePath());
		//
		final Document document = getHtmlFactory().createDocument(getDocletOptions().getCharset(), this);
		final String title = getDocumentTitleFor(null);
		setLastTitle(title);
		final Element htmlTag = getHtmlAccessor().getRootElement(document);
		//
		generateHtmlHeader(htmlTag);
		generateHtmlBody(htmlTag);
		//
		if (!getDocletOptions().isFakeOutput()) {
			getReporter().print(Kind.NOTE, MessageFormat.format(Messages.AbstractSummaryGenerator_0, outputPath.toString()));
			writeDocument(outputPath, document);
		}
	}

	/** Generate the HTML header.
	 *
	 * @param htmlTag the container.
	 * @return the header.
	 */
	protected Element generateHtmlHeader(Element htmlTag) {
		final Element headerTree = getHtmlFactory().createHeadTag(htmlTag);
		getHtmlFactory().createTitleTag(headerTree, getLastTitle());
		final Path pathToRoot = getPathToRoot();
		for (final Path cssStyle : getCssStylesheets()) {
			getHtmlFactory().createCssLinkTag(headerTree, pathToRoot.resolve(cssStyle));
		}
		for (final Path jsScript : getJsScripts()) {
			getHtmlFactory().createJsLinkTag(headerTree, pathToRoot.resolve(jsScript));
		}
		return headerTree;
	}

	/** Generate the body of the documentation.
	 *
	 * @param htmlTag the container.
	 * @return the body.
	 */
	protected Element generateHtmlBody(Element htmlTag) {
		final Element bodyTag = getHtmlFactory().createBodyTag(htmlTag);
		generateBodyHeader(bodyTag);
		final Element contentTag = getHtmlFactory().createDivTag(bodyTag, CssStyles.CONTENT);
		generateBodyContent(contentTag);
		generateBodyFooter(bodyTag);
		generateNavigationBar();
		return bodyTag;
	}

	/** Generate the navigation bar.
	 */
	protected abstract void generateNavigationBar();

	/** Generate the visible content.
	 *
	 * @param parent the container.
	 */
	protected abstract void generateBodyContent(Element parent);

	/** Generate the visible header in the body.
	 *
	 * @param parent the container.
	 */
	protected void generateBodyHeader(Element parent) {
		Element divTag = getHtmlFactory().createDivTag(parent, CssStyles.HEADER);
		getNavigation().createNavigationBar(divTag);
	}

	/** Generate the visible footer in the body.
	 *
	 * @param parent the container.
	 */
	protected void generateBodyFooter(Element parent) {
		Element divTag = getHtmlFactory().createDivTag(parent, CssStyles.FOOTER);
		getNavigation().createNavigationBar(divTag);
		createCopyrightBox(divTag);
	}

	@Override
	protected void initNavigation(Navigation navigation) {
		navigation.setKind(null);
	}


	@Override
	protected String getDocumentTitleFor(String elementName) {
		return this.title;
	}

}
