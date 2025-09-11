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

package io.sarl.docs.doclet2.html.framework;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;

import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;

import com.google.common.base.Strings;
import com.google.inject.Inject;

import io.sarl.docs.doclet2.framework.ElementUtils;
import io.sarl.docs.doclet2.framework.TypeRepository;

/** Represent the navigation bar.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.15.1 20250911-224827
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public class NavigationImpl implements Navigation {

	private final static ResourceBundle BUNDLE = ResourceBundle.getBundle(NavigationImpl.class.getPackageName() + ".messages2"); //$NON-NLS-1$
	
	private List<? extends Node> moduleLink = Collections.emptyList();

	private final List<Element> navigationBars = new ArrayList<>();

	private final Set<String> summaryBoxAnchors = new TreeSet<>();

	private final Set<String> detailBoxAnchors = new TreeSet<>();

	private HtmlFactory htmlFactory;
	
	private PathBuilder pathBuilder;

	private TypeRepository typeRepository;
	
	private ElementUtils elementUtils;

	private NavigationKind kind;

	/** Change the SARL specific element utilities.
	 *
	 * @param utils the utilities.
	 */
	@Inject
	public void setElementUtils(ElementUtils utils) {
		this.elementUtils = utils;
	}

	/** Replies the SARL specific element utilities.
	 *
	 * @return the utilities.
	 */
	public ElementUtils getElementUtils() {
		return this.elementUtils;
	}

	/** Change the type repository.
	 *
	 * @param repository the manager of type repository.
	 */
	@Inject
	public void setTypeRepository(TypeRepository repository) {
		this.typeRepository = repository;
	}

	/** Replies the type repository.
	 *
	 * @return the repository.
	 */
	public TypeRepository getTypeRepository() {
		return this.typeRepository;
	}

	/** Change the HTML factory.
	 *
	 * @param factory the factory.
	 */
	@Inject
	public void setHtmlFactory(HtmlFactory factory) {
		this.htmlFactory = factory;
	}

	/** Replies the HTML factory.
	 *
	 * @return the factory.
	 */
	public HtmlFactory getHtmlFactory() {
		return this.htmlFactory;
	}

	/** Change the HTML path builder.
	 *
	 * @param builder the builder.
	 */
	@Inject
	public void setPathBuilder(PathBuilder builder) {
		this.pathBuilder = builder;
	}

	/** Replies the HTML path builder.
	 *
	 * @return the builder.
	 */
	public PathBuilder getPathBuilder() {
		return this.pathBuilder;
	}

	@Override
	public NavigationKind getKind() {
		return this.kind;
	}
	
	@Override
	public void setKind(NavigationKind kind) {
		this.kind = kind;
	}

	@Override
	public void setModuleLink(List<? extends Node> link) {
		if (link == null) {
			this.moduleLink = Collections.emptyList();
		} else {
			this.moduleLink = link;
		}
	}

	@Override
	public List<? extends Node> getModuleLink() {
		return this.moduleLink;
	}

	@Override
	public Node createNavigationBar(Element parent) {
		final Element bar = getHtmlFactory().createDivTag(parent, CssStyles.NAVIGATION);
		this.navigationBars.add(bar);
		return bar;
	}

	@Override
	public void generateNavigationBars(TypeElement currentType, HtmlFactoryContext context) {
		// Search for previous and next types.
		TypeElement previousType = null;
		TypeElement nextType = null;
		if (currentType != null) {
			final SortedSet<TypeElement> packageElements = getTypeRepository().getTypesInPackage(
					context.getEnvironment().getElementUtils().getPackageOf(currentType));
			final Comparator<? super TypeElement> cmp = getElementUtils().getTypeElementComparator();
			final Iterator<TypeElement> iterator = packageElements.iterator();
			boolean found = false;
			while (iterator.hasNext() && !found) {
				final TypeElement c = iterator.next();
				if (cmp.compare(c, currentType) == 0) {
					found = true;
				} else {
					previousType = c;
				}
			}
			if (found) {
				if (iterator.hasNext()) {
					nextType = iterator.next();
				}
			} else {
				previousType = null;
			}
		}
		// Generate the navigation bars
		for (final Element bar : this.navigationBars) {
			generateGlobalNavigationBar(bar, currentType, context);
			if (currentType != null) {
				generateLocalNavigationBar(bar, previousType, currentType, nextType, context);
			}
		}
	}

	@Override
	public void generateNavigationBars(PackageElement currentPackage, HtmlFactoryContext context) {
		for (final Element bar : this.navigationBars) {
			generateGlobalNavigationBar(bar, currentPackage, context);
		}
	}

	@Override
	public void generateNavigationBars(ModuleElement currentModule, HtmlFactoryContext context) {
		for (final Element bar : this.navigationBars) {
			generateGlobalNavigationBar(bar, currentModule, context);
		}
	}

	private void createLink(Element receiver, Path path, String label, HtmlFactoryContext context) {
		final Path linkModulePath = context.getPathToRoot().resolve(path);
		final List<Node> link = getHtmlFactory().createLink(linkModulePath, label, null);
		receiver.appendChildren(link);
	}

	private void createMainNavButton(Element parent, String label, Path relativePath, HtmlFactoryContext context, NavigationKind kind) {
		final Element navItem = getHtmlFactory().createLiTag(parent, null);
		if (getKind() == kind) {
			navItem.appendText(label);
			navItem.addClass(CssStyles.ACTIVE.getCssClassname());
		} else if (relativePath != null) {
			createLink(navItem, relativePath, label, context);
		} else {
			navItem.appendText(label);
		}
	}

	/** Generate the navigation bar that is common to all types.
	 *
	 * @param parent the container.
	 * @param currentType the current type.
	 * @param context the HTML factory context.
	 */
	protected void generateGlobalNavigationBar(Element parent, TypeElement currentType, HtmlFactoryContext context) {
		final ModuleElement moduleElement;
		final PackageElement packageElement; 
		if (currentType != null) {
			moduleElement = context.getEnvironment().getElementUtils().getModuleOf(currentType);
			packageElement = context.getEnvironment().getElementUtils().getPackageOf(currentType);
		} else {
			moduleElement = null;
			packageElement = null;
		}
		generateGlobalNavigationBar(parent, moduleElement, packageElement, currentType, context);
	}

	/** Generate the navigation bar that is common to all types.
	 *
	 * @param parent the container.
	 * @param currentPackage the current package.
	 * @param context the HTML factory context.
	 */
	protected void generateGlobalNavigationBar(Element parent, PackageElement currentPackage, HtmlFactoryContext context) {
		final ModuleElement moduleElement;
		if (currentPackage != null) {
			moduleElement = context.getEnvironment().getElementUtils().getModuleOf(currentPackage);
		} else {
			moduleElement = null;
		}
		generateGlobalNavigationBar(parent, moduleElement, currentPackage, null, context);
	}

	/** Generate the navigation bar that is common to all types.
	 *
	 * @param parent the container.
	 * @param currentModule the current module.
	 * @param context the HTML factory context.
	 */
	protected void generateGlobalNavigationBar(Element parent, ModuleElement currentModule, HtmlFactoryContext context) {
		generateGlobalNavigationBar(parent, currentModule, null, null, context);
	}

	/** Generate the navigation bar that is common to all types.
	 *
	 * @param parent the container.
	 * @param moduleElement the module name.
	 * @param packageElement the package name.
	 * @param currentType the current type.
	 * @param context the HTML factory context.
	 */
	protected void generateGlobalNavigationBar(Element parent, ModuleElement moduleElement,
			PackageElement packageElement, TypeElement currentType, HtmlFactoryContext context) {
		final Element bar = getHtmlFactory().createDivTag(parent, CssStyles.GLOBAL_NAVIGATION);
		final Element nav = getHtmlFactory().createUlTag(bar, CssStyles.NAVIGATION_MAIN_LIST);

		createMainNavButton(nav, Messages.NavigationImpl_5, getPathBuilder().overviewSummary(), context, NavigationKind.OVERVIEW);

		createMainNavButton(nav, Messages.NavigationImpl_6, getPathBuilder().moduleSummary(moduleElement), context, NavigationKind.MODULE);

		createMainNavButton(nav, Messages.NavigationImpl_7, getPathBuilder().packageSummary(packageElement), context, NavigationKind.PACKAGE);

		createMainNavButton(nav, Messages.NavigationImpl_8, null, context, NavigationKind.TYPE);

		final Path treePath;
		if (packageElement != null) {
			treePath = getPathBuilder().packageTypeHierarchy(packageElement);
		} else {
			treePath = getPathBuilder().typeHierarchy();			
		}
		createMainNavButton(nav, Messages.NavigationImpl_10, treePath, context, NavigationKind.TREE);

		createMainNavButton(nav, Messages.NavigationImpl_11, getPathBuilder().deprecatedIndex(), context, NavigationKind.DEPRECATED);

		createMainNavButton(nav, Messages.NavigationImpl_12, getPathBuilder().index(), context, NavigationKind.INDEX);
	}

	/** Generate the navigation bar that is deciated to the local type.
	 *
	 * @param parent the container.
	 * @param previousType the previous type.
	 * @param currentType the current type.
	 * @param nextType the next type.
	 * @param context the HTML factory context.
	 */
	protected void generateLocalNavigationBar(Element parent, TypeElement previousType, TypeElement currentType,
			TypeElement nextType, HtmlFactoryContext context) {
		final Element bar = getHtmlFactory().createDivTag(parent, CssStyles.LOCAL_NAVIGATION);
		
		final Element classNav = getHtmlFactory().createUlTag(bar, CssStyles.NAVIGATION_LIST);
		Element classNavItem = getHtmlFactory().createLiTag(classNav, null);
		Element emphSpan = getHtmlFactory().createSpanTag(classNavItem, CssStyles.EMPH);
		if (previousType != null) {
			emphSpan.appendChildren(getHtmlFactory().createTypeLink(previousType, Messages.NavigationImpl_0, null, context));
		} else {
			emphSpan.appendText(Messages.NavigationImpl_0);
		}
		classNavItem = getHtmlFactory().createLiTag(classNav, null);
		emphSpan = getHtmlFactory().createSpanTag(classNavItem, CssStyles.EMPH);
		if (nextType != null) {
			emphSpan.appendChildren(getHtmlFactory().createTypeLink(nextType, Messages.NavigationImpl_1, null, context));
		} else {
			emphSpan.appendText(Messages.NavigationImpl_1);
		}

		final Element indexNav = getHtmlFactory().createUlTag(bar, CssStyles.NAVIGATION_LIST);
		Element indexNavItem = getHtmlFactory().createLiTag(indexNav, null);
		final Path allTypeIndexPath = getPathBuilder().allTypesIndex();
		createLink(indexNavItem, allTypeIndexPath, Messages.NavigationImpl_2, context);

		bar.appendChild(getHtmlFactory().createNewLineTag());

		if (!this.summaryBoxAnchors.isEmpty()) {
			final Element summaryNav = getHtmlFactory().createUlTag(bar, CssStyles.NAVIGATION_SUBLIST);
			Element summaryNavItem = getHtmlFactory().createLiTag(summaryNav, null);
			summaryNavItem.appendText(Messages.NavigationImpl_3);
			final Iterator<String> anchorIterator = this.summaryBoxAnchors.iterator();
			while (anchorIterator.hasNext()) {
				final String anchor = anchorIterator.next();
				summaryNavItem = getHtmlFactory().createLiTag(summaryNav, null);
				final List<Node> link = getHtmlFactory().createLink((Path) null, anchor, mapBoxAnchorIdToName(anchor), null);
				summaryNavItem.appendChildren(link);
				if (anchorIterator.hasNext()) {
					getHtmlFactory().createUnsecableSpace(summaryNavItem);
					summaryNavItem.appendText("|"); //$NON-NLS-1$
				}
			}
		}

		if (!this.detailBoxAnchors.isEmpty()) {
			final Element detailsNav = getHtmlFactory().createUlTag(bar, CssStyles.NAVIGATION_SUBLIST);
			Element detailsNavItem = getHtmlFactory().createLiTag(detailsNav, null);
			detailsNavItem.appendText(Messages.NavigationImpl_4);
			final Iterator<String> anchorIterator = this.detailBoxAnchors.iterator();
			while (anchorIterator.hasNext()) {
				final String anchor = anchorIterator.next();
				detailsNavItem = getHtmlFactory().createLiTag(detailsNav, null);
				final List<Node> link = getHtmlFactory().createLink((Path) null, anchor, mapBoxAnchorIdToName(anchor), null);
				detailsNavItem.appendChildren(link);
				if (anchorIterator.hasNext()) {
					getHtmlFactory().createUnsecableSpace(detailsNavItem);
					detailsNavItem.appendText("|"); //$NON-NLS-1$
				}
			}
		}
	}

	/** Map the anchor identifier of a box to its name in the navigation bar.
	 *
	 * @param anchorId the box's anchor identifier.
	 * @return the box name.
	 */
	@SuppressWarnings("static-method")
	protected String mapBoxAnchorIdToName(String anchorId) {
		try {
			final String value = BUNDLE.getString(anchorId);
			if (!Strings.isNullOrEmpty(value)) {
				return value;
			}
		} catch (Throwable ex) {
			//
		}
		return anchorId;
	}

	@Override
	public void addSummaryBoxAnchor(String anchor) {
		this.summaryBoxAnchors.add(anchor);
	}

	@Override
	public void addDetailBoxAnchor(String anchor) {
		this.detailBoxAnchors.add(anchor);
	}

}
