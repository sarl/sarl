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

package io.sarl.docs.doclet2.html.summaries;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;

import com.google.common.collect.Iterables;
import jdk.javadoc.doclet.Reporter;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.DocPaths;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.Navigation;
import io.sarl.docs.doclet2.html.framework.Navigation.NavigationKind;

/** Generate the all-type summary.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class DeprecatedListGeneratorImpl extends AbstractSummaryGenerator implements DeprecatedListGenerator {

	/** Constructor.
	 */
	public DeprecatedListGeneratorImpl() {
		super(Messages.DeprecatedListGeneratorImpl_1);
	}

	@Override
	protected void generateNavigationBar() {
		getNavigation().generateNavigationBars((TypeElement) null, this);
	}

	@Override
	protected void initNavigation(Navigation navigation) {
		navigation.setKind(NavigationKind.DEPRECATED);
	}

	@Override
	public void generate(Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		generate(
				Messages.DeprecatedListGeneratorImpl_0, DocPaths.DEPRECATED_INDEX_HTML,
				cssStylesheets, jsScripts, environment, cliOptions, reporter);
	}

	/** Generate the title in the body.
	 *
	 * @param parent the container.
	 */
	protected void generateBodyTitle(Element parent) {
		final Element nameDiv = getHtmlFactory().createDivTag(parent, CssStyles.HEADER_TYPE_NAME);
		nameDiv.appendText(getLastTitle());
	}

	/** Generate the list of deprecated AOP types.
	 *
	 * @param parent the container.
	 */
	protected void generateAopList(Element parent) {
		final Map<String, Iterable<? extends TypeElement>> allElements = new LinkedHashMap<>();
		//
		final Iterable<TypeElement> allDeprecatedTypes = getTypeRepository().getDeprecatedTypes();
		//
		final Iterable<TypeElement> aop = Iterables.filter(allDeprecatedTypes, it -> getElementUtils().isAopElement(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_2, aop);
		//
		final Iterable<TypeElement> agents = Iterables.filter(allDeprecatedTypes, it -> getElementUtils().isSarlAgent(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_3, agents);
		//
		final Iterable<TypeElement> behaviors = Iterables.filter(allDeprecatedTypes, it -> getElementUtils().isSarlBehavior(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_4, behaviors);
		//
		final Iterable<TypeElement> capacities = Iterables.filter(allDeprecatedTypes, it -> getElementUtils().isSarlCapacity(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_5, capacities);
		//
		final Iterable<TypeElement> skills = Iterables.filter(allDeprecatedTypes, it -> getElementUtils().isSarlSkill(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_6, skills);
		//
		final Iterable<TypeElement> events = Iterables.filter(allDeprecatedTypes, it -> getElementUtils().isSarlEvent(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_7, events);
		//
		createSummaryBox2(Messages.DeprecatedListGeneratorImpl_8, Messages.DeprecatedListGeneratorImpl_9,  Messages.DeprecatedListGeneratorImpl_10,
				null, parent, allElements,
				getElementUtils().getTypeElementBasenameComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					nodes.addAll(getHtmlFactory().createTypeLink(element, false, null, this));
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					createShortDeprecationMessage(element, nodes, false);
					return nodes;
				},
				null);
	}

	/** Generate the list of deprecated OOP types.
	 *
	 * @param parent the container.
	 */
	protected void generateOopList(Element parent) {
		final Map<String, Iterable<? extends TypeElement>> allElements = new LinkedHashMap<>();
		//
		final Iterable<TypeElement> allDeprecatedTypes = getTypeRepository().getDeprecatedTypes();
		//
		final Iterable<TypeElement> oop = Iterables.filter(allDeprecatedTypes, it -> !getElementUtils().isAopElement(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_2, oop);
		//
		final Iterable<TypeElement> classes = Iterables.filter(allDeprecatedTypes, it -> it.getKind() == ElementKind.CLASS && !getElementUtils().isAopElement(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_12, classes);
		//
		final Iterable<TypeElement> interfaces = Iterables.filter(allDeprecatedTypes, it -> it.getKind() == ElementKind.INTERFACE && !getElementUtils().isAopElement(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_13, interfaces);
		//
		final Iterable<TypeElement> enumerations = Iterables.filter(allDeprecatedTypes, it -> it.getKind() == ElementKind.ENUM && !getElementUtils().isAopElement(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_14, enumerations);
		//
		final Iterable<TypeElement> annotations = Iterables.filter(allDeprecatedTypes, it -> it.getKind() == ElementKind.ANNOTATION_TYPE && !getElementUtils().isAopElement(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_15, annotations);
		//
		createSummaryBox2(Messages.DeprecatedListGeneratorImpl_11, Messages.DeprecatedListGeneratorImpl_9,  Messages.DeprecatedListGeneratorImpl_10,
				null, parent, allElements,
				getElementUtils().getTypeElementBasenameComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					nodes.addAll(getHtmlFactory().createTypeLink(element, false, null, this));
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					createShortDeprecationMessage(element, nodes, false);
					return nodes;
				},
				null);
	}

	/** Generate the list of deprecated fields.
	 *
	 * @param parent the container.
	 */
	protected void generateFieldList(Element parent) {
		final Map<String, Iterable<? extends VariableElement>> allElements = new LinkedHashMap<>();
		//
		final Iterable<VariableElement> allDeprecatedFields = getTypeRepository().getDeprecatedFields();
		allElements.put(Messages.DeprecatedListGeneratorImpl_17, allDeprecatedFields);
		//
		final Iterable<VariableElement> staticFields = Iterables.filter(allDeprecatedFields, it -> getElementUtils().isStatic(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_20, staticFields);
		//
		final Iterable<VariableElement> concreteFields = Iterables.filter(allDeprecatedFields, it -> !getElementUtils().isStatic(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_21, concreteFields);
		//
		createSummaryBox2(Messages.DeprecatedListGeneratorImpl_16, Messages.DeprecatedListGeneratorImpl_9,  Messages.DeprecatedListGeneratorImpl_10,
				null, parent, allElements,
				getElementUtils().getVariableElementComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String label = element.getEnclosingElement().getSimpleName().toString() + "." + element.getSimpleName().toString(); //$NON-NLS-1$
					nodes.addAll(getHtmlFactory().createVariableLink(element, label, null, this));
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					createShortDeprecationMessage(element, nodes, false);
					return nodes;
				},
				null);
	}

	/** Generate the list of deprecated constructors.
	 *
	 * @param parent the container.
	 */
	protected void generateConstructorList(Element parent) {
		final Map<String, Iterable<? extends ExecutableElement>> allElements = new LinkedHashMap<>();
		//
		final Iterable<ExecutableElement> allDeprecatedExecutables = Iterables.filter(getTypeRepository().getDeprecatedExecutables(),
				it -> it.getKind() == ElementKind.CONSTRUCTOR);
		allElements.put(Messages.DeprecatedListGeneratorImpl_23, allDeprecatedExecutables);
		//
		final String constructorName = getSARLGrammarKeywordAccess().getNewKeyword();
		createSummaryBox2(Messages.DeprecatedListGeneratorImpl_22, Messages.DeprecatedListGeneratorImpl_9,  Messages.DeprecatedListGeneratorImpl_10,
				null, parent, allElements,
				getElementUtils().getVariableElementComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String label = element.getEnclosingElement().getSimpleName().toString() + "." + constructorName; //$NON-NLS-1$
					final List<Node> constructorPrototype = getHtmlFactory().getExecutablePrototype(element, label, this);
					final List<Node> elementLink = getHtmlFactory().createExecutableLink(element, constructorPrototype, null, this);
					if (elementLink != null) {
						final Element emphLink = getHtmlFactory().createSpanTag(null, null);
						emphLink.appendChildren(elementLink);
						nodes.add(emphLink);
					}
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					createShortDeprecationMessage(element, nodes, false);
					return nodes;
				},
				null);
	}

	/** Generate the list of deprecated actions.
	 *
	 * @param parent the container.
	 */
	protected void generateActionList(Element parent) {
		final Map<String, Iterable<? extends ExecutableElement>> allElements = new LinkedHashMap<>();
		//
		final Iterable<ExecutableElement> allDeprecatedActions = Iterables.filter(getTypeRepository().getDeprecatedExecutables(),
				it -> it.getKind() == ElementKind.METHOD);
		allElements.put(Messages.DeprecatedListGeneratorImpl_25, allDeprecatedActions);
		//
		final Iterable<ExecutableElement> staticActions = Iterables.filter(allDeprecatedActions, it -> getElementUtils().isStatic(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_26, staticActions);
		//
		final Iterable<ExecutableElement> abstractActions = Iterables.filter(allDeprecatedActions, it -> !getElementUtils().isStatic(it) && getElementUtils().isAbstract(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_27, abstractActions);
		//
		final Iterable<ExecutableElement> concreteActions = Iterables.filter(allDeprecatedActions, it -> !getElementUtils().isStatic(it) && !getElementUtils().isAbstract(it));
		allElements.put(Messages.DeprecatedListGeneratorImpl_28, concreteActions);
		//
		createSummaryBox2(Messages.DeprecatedListGeneratorImpl_24, Messages.DeprecatedListGeneratorImpl_9,  Messages.DeprecatedListGeneratorImpl_10,
				null, parent, allElements,
				getElementUtils().getExecutableElementComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String methodName = element.getEnclosingElement().getSimpleName().toString() + "." + element.getSimpleName().toString(); //$NON-NLS-1$
					final List<Node> methodPrototype = getHtmlFactory().getExecutablePrototype(element, methodName, this);
					final List<? extends Node> elementLink = getHtmlFactory().createExecutableLink(element, methodPrototype, null, this);
					if (elementLink != null) {
						final Element emphLink = getHtmlFactory().createSpanTag(null, null);
						emphLink.appendChildren(elementLink);
						nodes.add(emphLink);
					}
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					createShortDeprecationMessage(element, nodes, false);
					return nodes;
				},
				null);
	}

	@Override
	protected void generateBodyContent(Element parent) {
		generateBodyTitle(parent);
		generateAopList(parent);
		generateOopList(parent);
		generateFieldList(parent);
		generateConstructorList(parent);
		generateActionList(parent);
	}

}
