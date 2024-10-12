/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.doclet2.html.indexes;

import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
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

/** Generate the global index.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class IndexGeneratorImpl extends AbstractDocumentationGenerator implements IndexGenerator {

	private static final Comparator<javax.lang.model.element.Element> COMPARATOR = (a, b) -> {
		if (a == b) {
			return 0;
		}
		if (a == null) {
			return Integer.MIN_VALUE;
		}
		if (b == null) {
			return Integer.MAX_VALUE;
		}
		final String abn = a.getSimpleName().toString();
		final String bbn = b.getSimpleName().toString();
		final int cmp = abn.compareTo(bbn);
		if (cmp != 0) {
			return cmp;
		}
		return a.toString().compareTo(b.toString());
	};

	/** Constructor.
	 */
	public IndexGeneratorImpl() {
		//
	}

	@Override
	protected String getDocumentTitleFor(String elementName) {
		return Messages.IndexGeneratorImpl_1;
	}

	@Override
	protected void initNavigation(Navigation navigation) {
		navigation.setKind(NavigationKind.INDEX);
	}

	@Override
	public void generate(Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		initGenerator(cssStylesheets, jsScripts, reporter, environment, cliOptions);
		getReporter().print(Kind.NOTE, Messages.IndexGeneratorImpl_0);
		computePaths(getPathBuilder().index(), true);
		//
		final Path outputPath = getDocletOptions().getOutputDirectory().resolve(getRelativePath());
		//
		final Document document = getHtmlFactory().createDocument(cliOptions.getCharset(), this);
		final String title = getDocumentTitleFor(null);
		setLastTitle(title);
		final Element htmlTag = getHtmlAccessor().getRootElement(document);
		//
		generateHtmlHeader(htmlTag);
		generateHtmlBody(htmlTag);
		//
		if (!getDocletOptions().isFakeOutput()) {
			getReporter().print(Kind.NOTE, MessageFormat.format(Messages.IndexGeneratorImpl_2, outputPath.toString()));
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

	/** Generate the navigation bar.
	 */
	protected void generateNavigationBar() {
		getNavigation().generateNavigationBars((TypeElement) null, this);
	}

	@SuppressWarnings("static-method")
	private void addEntry(Map<String, SortedSet<javax.lang.model.element.Element>> index, javax.lang.model.element.Element element) {
		final String name = element.getSimpleName().toString();
		final String letter = Character.valueOf(name.charAt(0)).toString().toUpperCase();
		SortedSet<javax.lang.model.element.Element> entries = index.computeIfAbsent(letter, it -> new TreeSet<>(COMPARATOR));
		entries.add(element);
	}
	
	/** Build the full index.
	 *
	 * @return the map with keys as letters, and values as entries.
	 */
	protected Map<String, SortedSet<javax.lang.model.element.Element>> buildIndex() {
		final Map<String, SortedSet<javax.lang.model.element.Element>> index = new TreeMap<>();
		final SortedSet<TypeElement> types = getTypeRepository().getTypes();
		for (final TypeElement type : types) {
			if (getEnvironment().isIncluded(type)) {
				addEntry(index, type);
				
				for (final javax.lang.model.element.Element member : type.getEnclosedElements()) {
					if (getEnvironment().isIncluded(member)) {
						switch (member.getKind()) {
						case FIELD:
							addEntry(index, member);
							break;
						case METHOD:
							addEntry(index, member);
							break;
						case ENUM_CONSTANT:
							addEntry(index, member);
							break;
						default:
							//
						}
					}
				}
			}
		}
		return index;
	}
	
	/** Generate the index.
	 *
	 * @param parent the container of the generated elements.
	 * @param index is the map with keys as letters, and values as entries.
	 */
	protected void generateIndex(Element parent, Map<String, SortedSet<javax.lang.model.element.Element>> index) {
		final Element dlTag = getHtmlFactory().createDlTag(parent, CssStyles.INDEX);		
		for (final Entry<String, SortedSet<javax.lang.model.element.Element>> entry : index.entrySet()) {
			final Element dtTag = getHtmlFactory().createDtTag(dlTag, CssStyles.INDEX);
			dtTag.appendText(entry.getKey());
			final Element ddTag = getHtmlFactory().createDdTag(dlTag, CssStyles.INDEX);
			final Element ulTag = getHtmlFactory().createUlTag(ddTag, CssStyles.INDEX);
			for (final javax.lang.model.element.Element element : entry.getValue()) {
				final Element liTag = getHtmlFactory().createLiTag(null, null);
				if (generateSingleEntry(liTag, element)) {
					ulTag.appendChild(liTag);
				}
			}
		}
	}

	/** Generate an entry of the index.
	 *
	 * @param parent the container of the generated elements.
	 * @param element the element to add.
	 * @return {@code true} if an entry is added.
	 */
	protected boolean generateSingleEntry(Element parent, javax.lang.model.element.Element element) {
		final List<Node> link;
		if (element instanceof TypeElement) {
			link = getHtmlFactory().createTypeLink((TypeElement) element, true, null, this);
		} else if (element instanceof ExecutableElement) {
			final ExecutableElement ee = (ExecutableElement) element;
			final List<Node> label = getHtmlFactory().getExecutablePrototype(ee, ee.getSimpleName().toString(), this);
			link = getHtmlFactory().createExecutableLink(ee, label, null, this);
		} else if (element instanceof VariableElement) {
			final VariableElement ve = (VariableElement) element;
			link = getHtmlFactory().createVariableLink(ve, ve.getSimpleName().toString(), null, this);
		} else {
			return false;
		}
		parent.appendChildren(link);
		getHtmlFactory().createUnsecableSpace(parent);
		parent.appendText("-"); //$NON-NLS-1$
		getHtmlFactory().createUnsecableSpace(parent);
		final List<Node> description = new ArrayList<>();
		createFirstSentence(element, description, false, false);
		createShortDeprecationMessage(element, description, true);
		parent.appendChildren(description);
		return true;
	}

	/** Generate the visible content.
	 *
	 * @param parent the container.
	 */
	protected void generateBodyContent(Element parent) {
		final Map<String, SortedSet<javax.lang.model.element.Element>> fullIndex = buildIndex();
		generateIndex(parent, fullIndex);
	}

}
