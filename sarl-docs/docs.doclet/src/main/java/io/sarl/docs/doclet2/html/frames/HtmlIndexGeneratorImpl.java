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

package io.sarl.docs.doclet2.html.frames;

import static io.sarl.docs.doclet2.html.framework.HtmlTags.BOOLEAN_YES_ATTR_VALUE;
import static io.sarl.docs.doclet2.html.framework.HtmlTags.COLS_ATTR;
import static io.sarl.docs.doclet2.html.framework.HtmlTags.NAME_ATTR;
import static io.sarl.docs.doclet2.html.framework.HtmlTags.ONLOAD_ATTR;
import static io.sarl.docs.doclet2.html.framework.HtmlTags.ROWS_ATTR;
import static io.sarl.docs.doclet2.html.framework.HtmlTags.SCROLLING_ATTR;
import static io.sarl.docs.doclet2.html.framework.HtmlTags.SRC_ATTR;
import static io.sarl.docs.doclet2.html.framework.HtmlTags.TITLE_ATTR;

import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.Collection;

import javax.tools.Diagnostic.Kind;

import jdk.javadoc.doclet.Reporter;
import org.jsoup.nodes.Element;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.AbstractDocumentationGenerator;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.Navigation;
import io.sarl.docs.doclet2.html.framework.Navigation.NavigationKind;

/** Generate the main HTML index file.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.15.0 20250909-115750
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public class HtmlIndexGeneratorImpl extends AbstractDocumentationGenerator implements HtmlIndexGenerator {

	/** Name of the main frame for package content.
	 */
	public static final String MAIN_PACKAGE_FRAME_NAME = "packageFrame"; //$NON-NLS-1$
	
	/** Name of the main frame for type content.
	 */
	public static final String MAIN_TYPE_FRAME_NAME = "typeFrame"; //$NON-NLS-1$

	private static final String SCRIPT_0 = "    tmpTargetPage = \"\" + window.location.search;\n" //$NON-NLS-1$
			+ "    if (tmpTargetPage != \"\" && tmpTargetPage != \"undefined\")\n" //$NON-NLS-1$
			+ "        tmpTargetPage = tmpTargetPage.substring(1);\n" //$NON-NLS-1$
			+ "    if (tmpTargetPage.indexOf(\":\") != -1 || (tmpTargetPage != \"\" && !validURL(tmpTargetPage)))\n" //$NON-NLS-1$
			+ "        tmpTargetPage = \"undefined\";\n" //$NON-NLS-1$
			+ "    targetPage = tmpTargetPage;\n" //$NON-NLS-1$
			+ "    function validURL(url) {\n" //$NON-NLS-1$
			+ "        try {\n" //$NON-NLS-1$
			+ "            url = decodeURIComponent(url);\n" //$NON-NLS-1$
			+ "        }\n" //$NON-NLS-1$
			+ "        catch (error) {\n" //$NON-NLS-1$
			+ "            return false;\n" //$NON-NLS-1$
			+ "        }\n" //$NON-NLS-1$
			+ "        var pos = url.indexOf(\".html\");\n" //$NON-NLS-1$
			+ "        if (pos == -1 || pos != url.length - 5)\n" //$NON-NLS-1$
			+ "            return false;\n" //$NON-NLS-1$
			+ "        var allowNumber = false;\n" //$NON-NLS-1$
			+ "        var allowSep = false;\n" //$NON-NLS-1$
			+ "        var seenDot = false;\n" //$NON-NLS-1$
			+ "        for (var i = 0; i < url.length - 5; i++) {\n" //$NON-NLS-1$
			+ "            var ch = url.charAt(i);\n" //$NON-NLS-1$
			+ "            if ('a' <= ch && ch <= 'z' ||\n" //$NON-NLS-1$
			+ "                    'A' <= ch && ch <= 'Z' ||\n" //$NON-NLS-1$
			+ "                    ch == '$' ||\n" //$NON-NLS-1$
			+ "                    ch == '_' ||\n" //$NON-NLS-1$
			+ "                    ch.charCodeAt(0) > 127) {\n" //$NON-NLS-1$
			+ "                allowNumber = true;\n" //$NON-NLS-1$
			+ "                allowSep = true;\n" //$NON-NLS-1$
			+ "            } else if ('0' <= ch && ch <= '9'\n" //$NON-NLS-1$
			+ "                    || ch == '-') {\n" //$NON-NLS-1$
			+ "                if (!allowNumber)\n" //$NON-NLS-1$
			+ "                     return false;\n" //$NON-NLS-1$
			+ "            } else if (ch == '/' || ch == '.') {\n" //$NON-NLS-1$
			+ "                if (!allowSep)\n" //$NON-NLS-1$
			+ "                    return false;\n" //$NON-NLS-1$
			+ "                allowNumber = false;\n" //$NON-NLS-1$
			+ "                allowSep = false;\n" //$NON-NLS-1$
			+ "                if (ch == '.')\n" //$NON-NLS-1$
			+ "                     seenDot = true;\n" //$NON-NLS-1$
			+ "                if (ch == '/' && seenDot)\n" //$NON-NLS-1$
			+ "                     return false;\n" //$NON-NLS-1$
			+ "            } else {\n" //$NON-NLS-1$
			+ "                return false;\n" //$NON-NLS-1$
			+ "            }\n" //$NON-NLS-1$
			+ "        }\n" //$NON-NLS-1$
			+ "        return true;\n" //$NON-NLS-1$
			+ "    }\n" //$NON-NLS-1$
			+ "    function loadFrames() {\n" //$NON-NLS-1$
			+ "        if (targetPage != \"\" && targetPage != \"undefined\")\n" //$NON-NLS-1$
			+ "             top.classFrame.location = top.targetPage;\n" //$NON-NLS-1$
			+ "    }"; //$NON-NLS-1$
	
	/** Constructor.
	 */
	public HtmlIndexGeneratorImpl() {
		//
	}

	@Override
	protected String getDocumentTitleFor(String elementName) {
		return getDocumentationTitle();
	}

	@Override
	protected void initNavigation(Navigation navigation) {
		navigation.setKind(NavigationKind.OVERVIEW);
	}

	@Override
	public void generate(Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		initGenerator(cssStylesheets, jsScripts, reporter, environment, cliOptions);
		getReporter().print(Kind.NOTE, Messages.HtmlIndexGeneratorImpl_0);
		computePaths(getPathBuilder().htmlIndexFile(), true);
		//
		final var outputPath = getDocletOptions().getOutputDirectory().resolve(getRelativePath());
		//
		final var docType = getHtmlFactory().createDocumentType(
				"html", //$NON-NLS-1$
				"-//W3C//DTD XHTML 1.0 Frameset//EN", //$NON-NLS-1$
				"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"); //$NON-NLS-1$
		final var document = getHtmlFactory().createDocument(docType, cliOptions.getCharset(), this);
		final var title = getDocumentTitleFor(null);
		setLastTitle(title);
		final var htmlTag = getHtmlAccessor().getRootElement(document);
		//
		generateContentHead(htmlTag);
		generateContentBody(htmlTag);
		//
		if (!getDocletOptions().isFakeOutput()) {
			getReporter().print(Kind.NOTE, MessageFormat.format(Messages.HtmlIndexGeneratorImpl_1, outputPath.toString()));
			writeDocument(outputPath, document);
		}
	}

	/** Generate the content head of the main index file.
	 *
	 * @param content the content.
	 */
	protected void generateContentHead(Element content) {
		final var head = getHtmlFactory().createHeadTag(content);
		getHtmlFactory().createTitleTag(head, getDocumentationTitle());
		final var script = getHtmlFactory().createScriptTag(head, "text/javascript"); //$NON-NLS-1$
		script.append(SCRIPT_0);
	}

	/** Generate the content body of the main index file.
	 *
	 * @param content the content.
	 */
	protected void generateContentBody(Element content) {
		final var bodyTag = getHtmlFactory().createBodyTag(content);
		bodyTag.remove();
		//
		final var docFrame = getHtmlFactory().createFramesetTag(content);
		docFrame.attr(COLS_ATTR, "20%,80%"); //$NON-NLS-1$
		docFrame.attr(TITLE_ATTR, Messages.HtmlIndexGeneratorImpl_2);
		docFrame.attr(ONLOAD_ATTR, "top.loadFrames()"); //$NON-NLS-1$
		//
		final var leftFrames = getHtmlFactory().createFramesetTag(docFrame);
		leftFrames.attr(ROWS_ATTR, "30%,70%"); //$NON-NLS-1$
		leftFrames.attr(TITLE_ATTR, Messages.HtmlIndexGeneratorImpl_3);
		leftFrames.attr(ONLOAD_ATTR, "top.loadFrames()"); //$NON-NLS-1$
		//
		final var leftOverviewFrame = getHtmlFactory().createFrameTag(leftFrames);
		Path path = getPathToRoot().resolve(getPathBuilder().overviewFrame());
		leftOverviewFrame.attr(SRC_ATTR, getHtmlFactory().path2UrlPath(path));
		leftOverviewFrame.attr(NAME_ATTR, "packageListFrame"); //$NON-NLS-1$
		leftOverviewFrame.attr(TITLE_ATTR, Messages.HtmlIndexGeneratorImpl_7);
		//
		final var allClassFrame = getHtmlFactory().createFrameTag(leftFrames);
		path = getPathToRoot().resolve(getPathBuilder().allTypesFrame());
		allClassFrame.attr(SRC_ATTR, getHtmlFactory().path2UrlPath(path));
		allClassFrame.attr(NAME_ATTR, MAIN_PACKAGE_FRAME_NAME);
		allClassFrame.attr(TITLE_ATTR, Messages.HtmlIndexGeneratorImpl_8);
		//
		final var typeFrame = getHtmlFactory().createFrameTag(docFrame);
		path = getPathToRoot().resolve(getPathBuilder().overviewSummary());
		typeFrame.attr(SRC_ATTR, getHtmlFactory().path2UrlPath(path));
		typeFrame.attr(NAME_ATTR, MAIN_TYPE_FRAME_NAME);
		typeFrame.attr(TITLE_ATTR, Messages.HtmlIndexGeneratorImpl_9);
		typeFrame.attr(SCROLLING_ATTR, BOOLEAN_YES_ATTR_VALUE);
		//
		final var noframes = getHtmlFactory().createNoFramesTag(docFrame);
		final var noscript = getHtmlFactory().createNoScriptTag(noframes);
		final var noscriptDiv = getHtmlFactory().createDivTag(noscript, null);
		noscriptDiv.appendText(Messages.HtmlIndexGeneratorImpl_4);
		final var title = getHtmlFactory().createDivTag(noframes, null);
		title.appendText(Messages.HtmlIndexGeneratorImpl_5);
		final var alert = getHtmlFactory().createDivTag(noframes, null);
		alert.append(Messages.HtmlIndexGeneratorImpl_6);
	}

}
