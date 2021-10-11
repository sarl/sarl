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

package io.sarl.docs.doclet.j11.writers;

import static io.sarl.docs.doclet.j11.utils.GeneralUtils.getAgentSelector;
import static io.sarl.docs.doclet.j11.utils.GeneralUtils.getBehaviorSelector;
import static io.sarl.docs.doclet.j11.utils.GeneralUtils.getCapacitySelector;
import static io.sarl.docs.doclet.j11.utils.GeneralUtils.getEventSelector;
import static io.sarl.docs.doclet.j11.utils.GeneralUtils.getOopSelector;
import static io.sarl.docs.doclet.j11.utils.GeneralUtils.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.function.Predicate;

import javax.lang.model.element.TypeElement;

import jdk.javadoc.internal.doclets.formats.html.HtmlConfiguration;
import jdk.javadoc.internal.doclets.formats.html.TreeWriter;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlConstants;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTag;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTree;
import jdk.javadoc.internal.doclets.formats.html.markup.StringContent;
import jdk.javadoc.internal.doclets.toolkit.Content;
import jdk.javadoc.internal.doclets.toolkit.util.ClassTree;
import jdk.javadoc.internal.doclets.toolkit.util.DocFileIOException;
import jdk.javadoc.internal.doclets.toolkit.util.DocPath;

/** Writer of the class hierarchy page for all the classes.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Create specific sections for the SARL concepts (agent, behaviors, etc.)</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlTreeWriter extends TreeWriter implements io.sarl.docs.doclet.j11.factories.TreeWriter {

	private volatile Predicate<TypeElement> expectedType = null;

	/** Constructor.
	 *
	 * @param configuration the current configuration of the doclet.
	 * @param filename the name of the file to generate.
	 * @param classtree the tree being built.
	 */
	public SarlTreeWriter(HtmlConfiguration configuration, DocPath filename, ClassTree classtree) {
		super(configuration, filename, classtree);
	}

	@Override
	public void generatePage() {
		try {
			generateTreeFile();
		} catch (DocFileIOException exception) {
			throw new RuntimeException(exception);
		}
	}

	private void internalAddTree(SortedSet<TypeElement> sset, String heading, HtmlTree div, Predicate<TypeElement> expectedType) {
		// This function is a copy of the super.addDeprecatedAPI with a different treatment of
		// the heading parameter (that is the label here, and the resource key of the label into
		// the super method).
		if (!sset.isEmpty()) {
			this.expectedType = expectedType;
			try {
				TypeElement firstTypeElement = sset.first();
				Content headingContent = new StringContent(heading);
				Content sectionHeading = HtmlTree.HEADING(HtmlConstants.CONTENT_HEADING, true,
						headingContent);
				HtmlTree htmlTree;
				if (this.configuration.allowTag(HtmlTag.SECTION)) {
					htmlTree = HtmlTree.SECTION(sectionHeading);
				} else {
					div.addContent(sectionHeading);
					htmlTree = div;
				}
				final int osize = htmlTree.charCount();
				addLevelInfo(!this.utils.isInterface(firstTypeElement) ? firstTypeElement : null,
						sset, false, htmlTree);
				if (this.configuration.allowTag(HtmlTag.SECTION) && osize != htmlTree.charCount()) {
					div.addContent(htmlTree);
				}
			} finally {
				this.expectedType = null;
			}
		}
	}

	@Override
	protected void addTree(SortedSet<TypeElement> sset, String heading, HtmlTree div) {
		if ("doclet.Class_Hierarchy".equals(heading)) {
			internalAddTree(sset, getText("doclet.Agents.hierarchy", this.configuration.getLocale()), div, getAgentSelector());
			internalAddTree(sset, getText("doclet.Events.hierarchy", this.configuration.getLocale()), div, getEventSelector());
			internalAddTree(sset, getText("doclet.Behaviors.hierarchy", this.configuration.getLocale()), div, getBehaviorSelector());
			internalAddTree(sset, getText("doclet.Skills.hierarchy", this.configuration.getLocale()), div, getSkillSelector());
			internalAddTree(sset, this.contents.getContent("doclet.Class_Hierarchy").toString(), div, getOopSelector());
		} else if ("doclet.Interface_Hierarchy".equals(heading)) {
			internalAddTree(sset, getText("doclet.Capacities.hierarchy", this.configuration.getLocale()), div, getCapacitySelector());
			internalAddTree(sset, this.contents.getContent("doclet.Interface_Hierarchy").toString(), div, getOopSelector());
		} else {
			super.addTree(sset, heading, div);
		}
	}

	private void internalLevelInfo(TypeElement parent, Collection<TypeElement> collection, boolean isEnum,
			List<Content> roots) {
		for (final TypeElement local : collection) {
			final boolean isValid = this.expectedType.test(local);
			final SortedSet<TypeElement> subtypes = this.classtree.directSubClasses(local, isEnum);
			if (isValid) {
				final HtmlTree li = new HtmlTree(HtmlTag.LI);
				addPartialInfo(local, li);
				addExtendsImplements(parent, local, li);
				super.addLevelInfo(local, subtypes, isEnum, li);
				roots.add(li);
			} else {
				internalLevelInfo(local, subtypes, isEnum, roots);
			}
		}
	}

	@Override
	protected void addLevelInfo(TypeElement parent, Collection<TypeElement> collection, boolean isEnum,
			Content contentTree) {
		if (!collection.isEmpty()) {
			if (this.expectedType != null) {
				final List<Content> roots = new ArrayList<>();
				internalLevelInfo(parent, collection, isEnum, roots);
				if (!roots.isEmpty()) {
		            final Content ul = new HtmlTree(HtmlTag.UL);
		            for (final Content li : roots) {
		            	ul.addContent(li);
		            }
					contentTree.addContent(ul);
				}
			} else {
				super.addLevelInfo(parent, collection, isEnum, contentTree);
			}
		}
	}

}
