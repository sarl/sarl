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

import java.util.List;
import java.util.SortedSet;

import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;

import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;

import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.Navigation;
import io.sarl.docs.doclet2.html.framework.Navigation.NavigationKind;

/** Abstract generator of the type hierarchy.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public abstract class AbstractTreeSummaryGenerator extends AbstractSummaryGenerator {

	private Element agentHierarchy;
	
	private Element behaviorHierarchy;

	private Element capacityHierarchy;

	private Element skillHierarchy;

	private Element eventHierarchy;

	private Element classHierarchy;

	private Element interfaceHierarchy;

	/** Constructor.
	 *
	 * @param title the title of the page.
	 */
	public AbstractTreeSummaryGenerator(String title) {
		super(title);
	}

	@Override
	protected void initNavigation(Navigation navigation) {
		navigation.setKind(NavigationKind.TREE);
	}

	/** Build the type hierarchies if needed.
	 */
	protected void buildHierarchies() {
		this.agentHierarchy = getHtmlFactory().createUlTag(null, null);
		this.behaviorHierarchy = getHtmlFactory().createUlTag(null, null);
		this.capacityHierarchy = getHtmlFactory().createUlTag(null, null);
		this.skillHierarchy = getHtmlFactory().createUlTag(null, null);
		this.eventHierarchy = getHtmlFactory().createUlTag(null, null);
		this.classHierarchy = getHtmlFactory().createUlTag(null, null);
		this.interfaceHierarchy = getHtmlFactory().createUlTag(null, null);
		updateClasses(null, null, getTypeHierarchy().getBaseClasses());
		updateInterfaces(null, null, getTypeHierarchy().getBaseInterfaces());
	}

	/** Replies if the given type is visible in the generated documentation.
	 *
	 * @param type the type to test.
	 * @return {@code true} if the type is visible.
	 */
	protected boolean isVisible(TypeElement type) {
		return true;
	}

	private void updateClasses(Element receiver, Element root, SortedSet<? extends TypeElement> types) {
		for (final TypeElement type : types) {
			final Element expectedRoot;
			if (getElementUtils().isSarlAgent(type)) {
				expectedRoot = this.agentHierarchy;
			} else if (getElementUtils().isSarlBehavior(type)) {
				expectedRoot = this.behaviorHierarchy;
			} else if (getElementUtils().isSarlSkill(type)) {
				expectedRoot = this.skillHierarchy;
			} else if (getElementUtils().isSarlEvent(type)) {
				expectedRoot = this.eventHierarchy;
			} else {
				expectedRoot = this.classHierarchy;
			}

			Element parent = receiver;
			if (parent == null || expectedRoot != root) {
				parent = expectedRoot;
			}

			if (isVisible(type)) {
				final Element typeElement = getHtmlFactory().createLiTag(parent, null);
				generateClassLink(typeElement, type);
				final SortedSet<? extends TypeElement> subTypes = getTypeHierarchy().getDirectSubTypes(type);
				if (!subTypes.isEmpty()) {
					final Element container = getHtmlFactory().createUlTag(null, null);
					updateClasses(container, expectedRoot, subTypes);
					if (container.childNodeSize() > 0) {
						typeElement.appendChild(container);
					}
				}
			}
			
		}
	}

	private void updateInterfaces(Element receiver, Element root, SortedSet<? extends TypeElement> types) {
		for (final TypeElement type : types) {
			final Element expectedRoot;
			if (getElementUtils().isSarlCapacity(type)) {
				expectedRoot = this.capacityHierarchy;
			} else {
				expectedRoot = this.interfaceHierarchy;
			}

			Element parent = receiver;
			if (parent == null || expectedRoot != root) {
				parent = expectedRoot;
			}

			if (isVisible(type)) {
				final Element typeElement = getHtmlFactory().createLiTag(parent, null);
				generateInterfaceLink(typeElement, type);
				
				final SortedSet<? extends TypeElement> subTypes = getTypeHierarchy().getDirectSubTypes(type);
				if (!subTypes.isEmpty()) {
					final Element container = getHtmlFactory().createUlTag(null, null);
					updateClasses(container, expectedRoot, subTypes);
					if (container.childNodeSize() > 0) {
						typeElement.appendChild(container);
					}
				}
			}
		}
	}

	/** Generate the link to a class.
	 *
	 * @param parent the container.
	 * @param type the type to link. 
	 */
	protected void generateClassLink(Element parent, TypeElement type) {
		final PackageElement pkg = getEnvironment().getElementUtils().getPackageOf(type);
		if (!pkg.isUnnamed()) {
			parent.appendText(pkg.getQualifiedName().toString());
			parent.appendText(getSARLGrammarKeywordAccess().getFullStopKeyword());
		}
		final List<Node> link = getHtmlFactory().createTypeLink(type, false, null, this);
		parent.appendChildren(link);
	}

	/** Generate the link to an interface.
	 *
	 * @param parent the container.
	 * @param type the type to link. 
	 */
	protected void generateInterfaceLink(Element parent, TypeElement type) {
		final PackageElement pkg = getEnvironment().getElementUtils().getPackageOf(type);
		if (!pkg.isUnnamed()) {
			parent.appendText(pkg.getQualifiedName().toString());
			parent.appendText(getSARLGrammarKeywordAccess().getFullStopKeyword());
		}
		final List<Node> link = getHtmlFactory().createTypeLink(type, false, null, this);
		parent.appendChildren(link);
	}


	/** Generate the hierarchy of agents.
	 *
	 * @param parent the container.
	 */
	protected void generateAgentHierarchy(Element parent, Element hierarchy) {
		if (hierarchy.childNodeSize() > 0) {
			final Element box = getHtmlFactory().createDivTag(parent, null);
			final Element nameDiv = getHtmlFactory().createDivTag(box, CssStyles.HEADER_TYPE_NAME);
			nameDiv.appendText(Messages.AbstractTreeSummaryGeneratorImpl_0);
			box.appendChild(hierarchy);
		}
	}

	/** Generate the hierarchy of behaviors.
	 *
	 * @param parent the container.
	 */
	protected void generateBehaviorHierarchy(Element parent, Element hierarchy) {
		if (hierarchy.childNodeSize() > 0) {
			final Element box = getHtmlFactory().createDivTag(parent, null);
			final Element nameDiv = getHtmlFactory().createDivTag(box, CssStyles.HEADER_TYPE_NAME);
			nameDiv.appendText(Messages.AbstractTreeSummaryGeneratorImpl_1);
			box.appendChild(hierarchy);
		}
	}

	/** Generate the hierarchy of capacities.
	 *
	 * @param parent the container.
	 */
	protected void generateCapacityHierarchy(Element parent, Element hierarchy) {
		if (hierarchy.childNodeSize() > 0) {
			final Element box = getHtmlFactory().createDivTag(parent, null);
			final Element nameDiv = getHtmlFactory().createDivTag(box, CssStyles.HEADER_TYPE_NAME);
			nameDiv.appendText(Messages.AbstractTreeSummaryGeneratorImpl_2);
			box.appendChild(hierarchy);
		}
	}

	/** Generate the hierarchy of skills.
	 *
	 * @param parent the container.
	 */
	protected void generateSkillHierarchy(Element parent, Element hierarchy) {
		if (hierarchy.childNodeSize() > 0) {
			final Element box = getHtmlFactory().createDivTag(parent, null);
			final Element nameDiv = getHtmlFactory().createDivTag(box, CssStyles.HEADER_TYPE_NAME);
			nameDiv.appendText(Messages.AbstractTreeSummaryGeneratorImpl_3);
			box.appendChild(hierarchy);
		}
	}

	/** Generate the hierarchy of events.
	 *
	 * @param parent the container.
	 */
	protected void generateEventHierarchy(Element parent, Element hierarchy) {
		if (hierarchy.childNodeSize() > 0) {
			final Element box = getHtmlFactory().createDivTag(parent, null);
			final Element nameDiv = getHtmlFactory().createDivTag(box, CssStyles.HEADER_TYPE_NAME);
			nameDiv.appendText(Messages.AbstractTreeSummaryGeneratorImpl_4);
			box.appendChild(hierarchy);
		}
	}

	/** Generate the hierarchy of classes.
	 *
	 * @param parent the container.
	 */
	protected void generateClassHierarchy(Element parent, Element hierarchy) {
		if (hierarchy.childNodeSize() > 0) {
			final Element box = getHtmlFactory().createDivTag(parent, null);
			final Element nameDiv = getHtmlFactory().createDivTag(box, CssStyles.HEADER_TYPE_NAME);
			nameDiv.appendText(Messages.AbstractTreeSummaryGeneratorImpl_5);
			box.appendChild(hierarchy);
		}
	}

	/** Generate the hierarchy of interfaces.
	 *
	 * @param parent the container.
	 */
	protected void generateInterfaceHierarchy(Element parent, Element hierarchy) {
		if (hierarchy.childNodeSize() > 0) {
			final Element box = getHtmlFactory().createDivTag(parent, null);
			final Element nameDiv = getHtmlFactory().createDivTag(box, CssStyles.HEADER_TYPE_NAME);
			nameDiv.appendText(Messages.AbstractTreeSummaryGeneratorImpl_6);
			box.appendChild(hierarchy);
		}
	}

	@Override
	protected void generateBodyContent(Element parent) {
		buildHierarchies();
		generateAgentHierarchy(parent, this.agentHierarchy);
		generateBehaviorHierarchy(parent, this.behaviorHierarchy);
		generateCapacityHierarchy(parent, this.capacityHierarchy);
		generateSkillHierarchy(parent, this.skillHierarchy);
		generateEventHierarchy(parent, this.eventHierarchy);
		generateClassHierarchy(parent, this.classHierarchy);
		generateInterfaceHierarchy(parent, this.interfaceHierarchy);
	}

}