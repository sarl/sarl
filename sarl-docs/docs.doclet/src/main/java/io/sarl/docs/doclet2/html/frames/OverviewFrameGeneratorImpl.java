/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.Collection;

import javax.tools.Diagnostic.Kind;

import jdk.javadoc.doclet.Reporter;
import org.jsoup.nodes.Element;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.AbstractDocumentationGenerator;
import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.Navigation;
import io.sarl.docs.doclet2.html.framework.Navigation.NavigationKind;

/** Generate the overview frame.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class OverviewFrameGeneratorImpl extends AbstractDocumentationGenerator implements OverviewFrameGenerator {

	/** Constructor.
	 */
	public OverviewFrameGeneratorImpl() {
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
		getReporter().print(Kind.NOTE, Messages.OverviewFrameGeneratorImpl_0);
		computePaths(getPathBuilder().overviewFrame(), true);
		//
		final var outputPath = getDocletOptions().getOutputDirectory().resolve(getRelativePath());
		//
		final var document = getHtmlFactory().createDocument(cliOptions.getCharset(), this);
		final var title = getDocumentTitleFor(null);
		setLastTitle(title);
		final var htmlTag = getHtmlAccessor().getRootElement(document);
		//
		generateContentHead(htmlTag);
		generateContentBody(htmlTag);
		//
		if (!getDocletOptions().isFakeOutput()) {
			getReporter().print(Kind.NOTE, MessageFormat.format(Messages.OverviewFrameGeneratorImpl_3, outputPath.toString()));
			writeDocument(outputPath, document);
		}
	}

	/** Generate the content head of the frame file.
	 *
	 * @param content the content.
	 */
	protected void generateContentHead(Element content) {
		final var head = getHtmlFactory().createHeadTag(content);
		getHtmlFactory().createTitleTag(head, getDocumentationTitle());
		final var pathToRoot = getPathToRoot();
		for (final var cssStyle : getCssStylesheets()) {
			getHtmlFactory().createCssLinkTag(head, pathToRoot.resolve(cssStyle));
		}
		for (final var jsScript : getJsScripts()) {
			getHtmlFactory().createJsLinkTag(head, pathToRoot.resolve(jsScript));
		}
	}

	/** Generate the content body of the frame file.
	 *
	 * @param content the content.
	 */
	protected void generateContentBody(Element content) {
		final var body = getHtmlFactory().createBodyTag(content);
		body.addClass(CssStyles.FRAME.getCssClassname());
		final var allTypes = getHtmlFactory().createDivTag(body, null);
		final var path = getPathToRoot().resolve(getPathBuilder().allTypesFrame());
		var link = getHtmlFactory().createLink(path, Messages.OverviewFrameGeneratorImpl_1, null);
		getHtmlFactory().addLinkTargetFrame(link, HtmlIndexGeneratorImpl.MAIN_PACKAGE_FRAME_NAME);
		allTypes.appendChildren(link);
		//
		final var packageListBlock = getHtmlFactory().createDivTag(body, null);
		final var packageListTitle = getHtmlFactory().createH2Tag(packageListBlock, CssStyles.HEADER_SMALL_TITLE);
		packageListTitle.appendText(Messages.OverviewFrameGeneratorImpl_2);
		final var packageList = getHtmlFactory().createUlTag(packageListBlock, CssStyles.BULLET_LESS_LIST);
		for (final var pkg : getTypeRepository().getPackages()) {
			final var pkgLine = getHtmlFactory().createLiTag(packageList, CssStyles.BULLET_LESS_LIST);
			link = getHtmlFactory().createPackageLink(pkg, pkg.getQualifiedName().toString(), null, this);
			getHtmlFactory().addLinkTargetFrame(link, HtmlIndexGeneratorImpl.MAIN_PACKAGE_FRAME_NAME);
			pkgLine.appendChildren(link);
		}
	}

}
