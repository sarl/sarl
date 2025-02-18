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

package io.sarl.docs.doclet2.html.summaries;

import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;

import javax.lang.model.element.ElementKind;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;

import com.google.common.collect.Iterables;
import jdk.javadoc.doclet.Reporter;
import jdk.javadoc.doclet.Taglet.Location;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.Navigation;
import io.sarl.docs.doclet2.html.framework.Navigation.NavigationKind;

/** Generator of the description for a specific package.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class PackageSummaryGeneratorImpl extends AbstractSummaryGenerator implements PackageSummaryGenerator {

	private PackageElement packageElement;
	
	@Override
	protected void initNavigation(Navigation navigation) {
		navigation.setKind(NavigationKind.PACKAGE);
	}

	@Override
	protected void generateNavigationBar() {
		getNavigation().generateNavigationBars(this.packageElement, this);
	}

	@Override
	public void generate(PackageElement packageElement, Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		this.packageElement = packageElement;
		final var packageName = getElementUtils().getElementName(packageElement);
		setDefaultTitle(MessageFormat.format(Messages.PackageSummaryGeneratorImpl_1, packageName));
		generate(
				MessageFormat.format(Messages.PackageSummaryGeneratorImpl_0, packageName),
				getPathBuilder().packageSummary(packageElement),
				cssStylesheets, jsScripts, environment, cliOptions, reporter);
	}

	/** Generate the title in the body.
	 *
	 * @param packageElement the package for which the documentation is generated.
	 * @param parent the container.
	 */
	protected void generateBodyTitle(PackageElement packageElement, Element parent) {
		final var nameDiv = getHtmlFactory().createDivTag(parent, CssStyles.HEADER_TYPE_NAME);
		nameDiv.appendText(getLastTitle());
	}

	/** Generate the description of the package.
	 *
	 * @param packageElement the module for which the documentation is generated.
	 * @param parent the container.
	 */
	protected void generatePackageDescription(PackageElement packageElement, Element parent) {
		final var description = new ArrayList<Node>();
		createFullDescriptionBody(packageElement, description, false, true);
		final var descriptionDiv = getHtmlFactory().createDivTag(parent, CssStyles.PACKAGE_DESCRIPTION);
		descriptionDiv.appendChildren(description);
	}

	/** Generate the introduction of the package.
	 *
	 * @param packageElement the module for which the documentation is generated.
	 * @param parent the container.
	 */
	protected void generatePackageIntroduction(PackageElement packageElement, Element parent) {
		final var description = new ArrayList<Node>();
		createFirstSentence(packageElement, description, false, true);
		createBlockTagsFor(packageElement, description, Location.PACKAGE, CssStyles.PACKAGE_TAG_INFO);
		final var descriptionDiv = getHtmlFactory().createDivTag(parent, CssStyles.PACKAGE_DESCRIPTION);
		descriptionDiv.appendChildren(description);
	}

	/** Generate the list of agent-oriented programming types.
	 *
	 * @param packageElement the package for which the documentation is generated.
	 * @param parent the container.
	 */
	protected void generateAopList(PackageElement packageElement, Element parent) {
		final var allElements = new LinkedHashMap<String, Iterable<? extends TypeElement>>();
		//
		final var allTypes = getTypeRepository().getTypesInPackage(packageElement);
		//
		final var aop = Iterables.filter(allTypes, it -> getElementUtils().isAopElement(it));
		allElements.put(Messages.PackageSummaryGeneratorImpl_3, aop);
		//
		final var agents = Iterables.filter(allTypes, it -> getElementUtils().isSarlAgent(it));
		allElements.put(Messages.PackageSummaryGeneratorImpl_10, agents);
		//
		final var behaviors = Iterables.filter(allTypes, it -> getElementUtils().isSarlBehavior(it));
		allElements.put(Messages.PackageSummaryGeneratorImpl_11, behaviors);
		//
		final var capacities = Iterables.filter(allTypes, it -> getElementUtils().isSarlCapacity(it));
		allElements.put(Messages.PackageSummaryGeneratorImpl_12, capacities);
		//
		final var skills = Iterables.filter(allTypes, it -> getElementUtils().isSarlSkill(it));
		allElements.put(Messages.PackageSummaryGeneratorImpl_13, skills);
		//
		final var events = Iterables.filter(allTypes, it -> getElementUtils().isSarlEvent(it));
		allElements.put(Messages.PackageSummaryGeneratorImpl_14, events);
		//
		createSummaryBox2(Messages.PackageSummaryGeneratorImpl_9, Messages.PackageSummaryGeneratorImpl_8,  Messages.PackageSummaryGeneratorImpl_15,
				null, parent, allElements,
				getElementUtils().getTypeElementBasenameComparator(),
				element -> {
					final var nodes = new ArrayList<Node>();
					nodes.addAll(getHtmlFactory().createTypeLink(element, false, null, this));
					return nodes;
				},
				element -> {
					final var nodes = new ArrayList<Node>();
					createFirstSentence(element, nodes, false, false);
					createShortDeprecationMessage(element, nodes, true);
					return nodes;
				},
				null);
	}

	/** Generate the list of object-oriented programming types.
	 *
	 * @param packageElement the package for which the documentation is generated.
	 * @param parent the container.
	 */
	protected void generateOopList(PackageElement packageElement, Element parent) {
		final var allElements = new LinkedHashMap<String, Iterable<? extends TypeElement>>();
		//
		final var allTypes = getTypeRepository().getTypesInPackage(packageElement);
		//
		final var oop = Iterables.filter(allTypes, it -> !getElementUtils().isAopElement(it));
		allElements.put(Messages.PackageSummaryGeneratorImpl_3, oop);
		//
		final var classes = Iterables.filter(allTypes, it -> it.getKind() == ElementKind.CLASS && !getElementUtils().isAopElement(it));
		allElements.put(Messages.PackageSummaryGeneratorImpl_4, classes);
		//
		final var interfaces = Iterables.filter(allTypes, it -> it.getKind() == ElementKind.INTERFACE && !getElementUtils().isAopElement(it));
		allElements.put(Messages.PackageSummaryGeneratorImpl_5, interfaces);
		//
		final var enumerations = Iterables.filter(allTypes, it -> it.getKind() == ElementKind.ENUM && !getElementUtils().isAopElement(it));
		allElements.put(Messages.PackageSummaryGeneratorImpl_6, enumerations);
		//
		final var annotations = Iterables.filter(allTypes, it -> it.getKind() == ElementKind.ANNOTATION_TYPE && !getElementUtils().isAopElement(it));
		allElements.put(Messages.PackageSummaryGeneratorImpl_7, annotations);
		//
		createSummaryBox2(Messages.PackageSummaryGeneratorImpl_2, Messages.PackageSummaryGeneratorImpl_8, Messages.PackageSummaryGeneratorImpl_15,
				null, parent, allElements,
				getElementUtils().getTypeElementBasenameComparator(),
				element -> {
					final var nodes = new ArrayList<Node>();
					nodes.addAll(getHtmlFactory().createTypeLink(element, false, null, this));
					return nodes;
				},
				element -> {
					final var nodes = new ArrayList<Node>();
					createFirstSentence(element, nodes, false, false);
					createShortDeprecationMessage(element, nodes, true);
					return nodes;
				},
				null);
	}

	@Override
	protected void generateBodyContent(Element parent) {
		generateBodyTitle(this.packageElement, parent);
		generatePackageIntroduction(this.packageElement, parent);
		generateAopList(this.packageElement, parent);
		generateOopList(this.packageElement, parent);
		generatePackageDescription(this.packageElement, parent);
	}

}
