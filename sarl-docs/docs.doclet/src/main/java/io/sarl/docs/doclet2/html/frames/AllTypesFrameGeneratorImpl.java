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
 */

package io.sarl.docs.doclet2.html.frames;

import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic.Kind;

import jdk.javadoc.doclet.Reporter;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.AbstractDocumentationGenerator;
import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.Navigation;
import io.sarl.docs.doclet2.html.framework.Navigation.NavigationKind;

/** Generate the all-types overview frame.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class AllTypesFrameGeneratorImpl extends AbstractDocumentationGenerator implements AllTypesFrameGenerator {

	/** Constructor.
	 */
	public AllTypesFrameGeneratorImpl() {
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
		getReporter().print(Kind.NOTE, Messages.AllTypesFrameGeneratorImpl_0);
		computePaths(getPathBuilder().allTypesFrame(), true);
		//
		final Path outputPath = getDocletOptions().getOutputDirectory().resolve(getRelativePath());
		//
		final Document document = getHtmlFactory().createDocument(cliOptions.getCharset(), this);
		final String title = getDocumentTitleFor(null);
		setLastTitle(title);
		final Element htmlTag = getHtmlAccessor().getRootElement(document);
		//
		generateContentHead(htmlTag);
		generateContentBody(htmlTag);
		//
		if (!getDocletOptions().isFakeOutput()) {
			getReporter().print(Kind.NOTE, MessageFormat.format(Messages.AllTypesFrameGeneratorImpl_1, outputPath.toString()));
			writeDocument(outputPath, document);
		}
	}

	/** Generate the content head of the frame file.
	 *
	 * @param content the content.
	 */
	protected void generateContentHead(Element content) {
		final Element head = getHtmlFactory().createHeadTag(content);
		getHtmlFactory().createTitleTag(head, getDocumentationTitle());
		final Path pathToRoot = getPathToRoot();
		for (final Path cssStyle : getCssStylesheets()) {
			getHtmlFactory().createCssLinkTag(head, pathToRoot.resolve(cssStyle));
		}
		for (final Path jsScript : getJsScripts()) {
			getHtmlFactory().createJsLinkTag(head, pathToRoot.resolve(jsScript));
		}
	}

	/** Generate the content body of the frame file.
	 *
	 * @param content the content.
	 */
	protected void generateContentBody(Element content) {
		final Element body = getHtmlFactory().createBodyTag(content);
		body.addClass(CssStyles.FRAME.getCssClassname());
		//
		final Element typeListBlock = getHtmlFactory().createDivTag(body, null);
		final Element typeListTitle = getHtmlFactory().createH2Tag(typeListBlock, CssStyles.HEADER_SMALL_TITLE);
		typeListTitle.appendText(Messages.AllTypesFrameGeneratorImpl_2);
		final Element typeList = getHtmlFactory().createUlTag(typeListBlock, CssStyles.BULLET_LESS_LIST);
		//
		final SortedSet<TypeElement> list = new TreeSet<>(getElementUtils().getTypeElementBasenameComparator());
		list.addAll(getTypeRepository().getTypes());
		for (final TypeElement type : list) {
			final Element typeLine = getHtmlFactory().createLiTag(typeList, CssStyles.BULLET_LESS_LIST);
			final List<? extends Node> link = getHtmlFactory().createTypeLink(type, true, null, this);
			getHtmlFactory().addLinkTargetFrame(link, HtmlIndexGeneratorImpl.MAIN_TYPE_FRAME_NAME);
			typeLine.appendChildren(link);
		}
	}

}
