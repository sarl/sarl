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

import java.util.List;
import java.util.SortedSet;

import javax.lang.model.element.Element;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;

import com.google.common.collect.Sets;
import com.sun.source.doctree.DocTree;
import jdk.javadoc.internal.doclets.formats.html.DeprecatedListWriter;
import jdk.javadoc.internal.doclets.formats.html.HtmlConfiguration;
import jdk.javadoc.internal.doclets.formats.html.markup.ContentBuilder;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlConstants;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlStyle;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTag;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTree;
import jdk.javadoc.internal.doclets.formats.html.markup.StringContent;
import jdk.javadoc.internal.doclets.formats.html.markup.Table;
import jdk.javadoc.internal.doclets.formats.html.markup.TableHeader;
import jdk.javadoc.internal.doclets.toolkit.Content;
import jdk.javadoc.internal.doclets.toolkit.util.DeprecatedAPIListBuilder;
import jdk.javadoc.internal.doclets.toolkit.util.DeprecatedAPIListBuilder.DeprElementKind;
import jdk.javadoc.internal.doclets.toolkit.util.DocFileIOException;
import jdk.javadoc.internal.doclets.toolkit.util.DocPath;

import io.sarl.docs.doclet.j11.utils.GeneralUtils;
import io.sarl.docs.doclet.j11.utils.Reflect;
import io.sarl.lang.sarl.SarlPackage;

/** Writer of list of deprecated features dedicated to the SARL doclet.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Add sections for agents, behaviors, events, etc.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlDeprecatedListWriter extends DeprecatedListWriter implements io.sarl.docs.doclet.j11.factories.DeprecatedListWriter {

	private final HtmlConfiguration configuration;

	/** Constructor.
	 *
	 * @param writer the writer for the class that the methods belong to.
	 * @param filename the file to be generated
	 */
	public SarlDeprecatedListWriter(HtmlConfiguration configuration, DocPath filename) {
		super(configuration, filename);
		this.configuration = configuration;
	}

	@Override
	public void generatePage() {
		try {
			generateDeprecatedListFile(new DeprecatedAPIListBuilder(this.configuration));
		} catch (DocFileIOException exception) {
			throw new RuntimeException(exception);
		}
	}

	private void addAnchor(String anchorName, Content htmlTree) {
		htmlTree.addContent(this.links.createAnchor(anchorName));
	}

	private void internalAddDeprecatedAPI(SortedSet<Element> deprList, String anchorName, String heading,
			String tableSummary, TableHeader tableHeader, Content contentTree) {
		// This function is a copy of the super.addDeprecatedAPI with a different treatment of
		// the heading parameter (that is the label here, and the resource key of the label into
		// the super method).
		if (deprList.size() > 0) {
			// The following is added to insert special anchors
			addAnchor(anchorName, contentTree);
			Content caption = new StringContent(heading);
			Table table = new Table(configuration.htmlVersion, HtmlStyle.deprecatedSummary)
					.setSummary(tableSummary)
					.setCaption(caption)
					.setHeader(tableHeader)
					.setColumnStyles(HtmlStyle.colDeprecatedItemName, HtmlStyle.colLast);
			for (Element e : deprList) {
				Content link;
				switch (e.getKind()) {
				case MODULE:
					ModuleElement m = (ModuleElement) e;
					link = getModuleLink(m, new StringContent(m.getQualifiedName()));
					break;
				case PACKAGE:
					PackageElement pkg = (PackageElement) e;
					link = getPackageLink(pkg, getPackageName(pkg));
					break;
				default:
					link = getDeprecatedLink(e);
				}
				Content desc = new ContentBuilder();
				List<? extends DocTree> tags = utils.getDeprecatedTrees(e);
				if (!tags.isEmpty()) {
					addInlineDeprecatedComment(e, tags.get(0), desc);
				} else {
					desc.addContent(HtmlTree.EMPTY);
				}
				table.addRow(link, desc);
			}
			Content li = HtmlTree.LI(HtmlStyle.blockList, table.toContent());
			Content ul = HtmlTree.UL(HtmlStyle.blockList, li);
			contentTree.addContent(ul);
		}
	}

	private SortedSet<Element> getOopElements(SortedSet<Element> elements) {
		return Sets.filter(elements, it -> !GeneralUtils.isOneOf(it,
				SarlPackage.SARL_AGENT, SarlPackage.SARL_BEHAVIOR,
				SarlPackage.SARL_SKILL, SarlPackage.SARL_EVENT,
				SarlPackage.SARL_CAPACITY));
	}

	private SortedSet<Element> getAgentElements(SortedSet<Element> elements) {
		return Sets.filter(elements, it -> GeneralUtils.isOneOf(it, SarlPackage.SARL_AGENT));
	}

	private SortedSet<Element> getCapacityElements(SortedSet<Element> elements) {
		return Sets.filter(elements, it -> GeneralUtils.isOneOf(it, SarlPackage.SARL_CAPACITY));
	}

	private SortedSet<Element> getEventElements(SortedSet<Element> elements) {
		return Sets.filter(elements, it -> GeneralUtils.isOneOf(it, SarlPackage.SARL_EVENT));
	}

	private SortedSet<Element> getBehaviorElements(SortedSet<Element> elements) {
		return Sets.filter(elements, it -> GeneralUtils.isOneOf(it, SarlPackage.SARL_BEHAVIOR));
	}

	private SortedSet<Element> getSkillElements(SortedSet<Element> elements) {
		return Sets.filter(elements, it -> GeneralUtils.isOneOf(it, SarlPackage.SARL_SKILL));
	}

	@Override
	protected void addDeprecatedAPI(SortedSet<Element> deprList, String headingKey, String tableSummary,
			TableHeader tableHeader, Content contentTree) {
		super.addDeprecatedAPI(getOopElements(deprList), headingKey, tableSummary, tableHeader, contentTree);
		final StringContent typeLabel = new StringContent(GeneralUtils.getText(GeneralUtils.DOCLET_TYPE_NAME, this.configuration.getLocale()));
		internalAddDeprecatedAPI(getAgentElements(deprList),
				"agent",
				GeneralUtils.getText("doclet.Agents", this.configuration.getLocale()),
				GeneralUtils.getText("doclet.Agents.tableSummary", this.configuration.getLocale()),
				new TableHeader(typeLabel, this.contents.descriptionLabel),
				contentTree);
		internalAddDeprecatedAPI(getCapacityElements(deprList),
				"capacity",
				GeneralUtils.getText("doclet.Capacities", this.configuration.getLocale()),
				GeneralUtils.getText("doclet.Capacities.tableSummary", this.configuration.getLocale()),
				new TableHeader(typeLabel, this.contents.descriptionLabel),
				contentTree);
		internalAddDeprecatedAPI(getEventElements(deprList),
				"event",
				GeneralUtils.getText("doclet.Events", this.configuration.getLocale()),
				GeneralUtils.getText("doclet.Events.tableSummary", this.configuration.getLocale()),
				new TableHeader(typeLabel, this.contents.descriptionLabel),
				contentTree);
		internalAddDeprecatedAPI(getBehaviorElements(deprList),
				"behavior",
				GeneralUtils.getText("doclet.Behaviors", this.configuration.getLocale()),
				GeneralUtils.getText("doclet.Behaviors.tableSummary", this.configuration.getLocale()),
				new TableHeader(typeLabel, this.contents.descriptionLabel),
				contentTree);
		internalAddDeprecatedAPI(getSkillElements(deprList),
				"skill",
				GeneralUtils.getText("doclet.Skills", this.configuration.getLocale()),
				GeneralUtils.getText("doclet.Skills.tableSummary", this.configuration.getLocale()),
				new TableHeader(typeLabel, this.contents.descriptionLabel),
				contentTree);
	}

	private void addIndexLink(SortedSet<Element> elements, String anchorName, String label,
			Content contentTree) {
		if (!elements.isEmpty()) {
			Content li = HtmlTree.LI(this.links.createLink(anchorName, new StringContent(label)));
            contentTree.addContent(li);
		}
	}
	
	@Override
	public Content getContentsList(DeprecatedAPIListBuilder deprapi) {
		Content headContent = contents.deprecatedAPI;
		Content heading = HtmlTree.HEADING(HtmlConstants.TITLE_HEADING, true,
				HtmlStyle.title, headContent);
		Content div = HtmlTree.DIV(HtmlStyle.header, heading);
		Content headingContent = contents.contentsHeading;
		div.addContent(HtmlTree.HEADING(HtmlConstants.CONTENT_HEADING, true,
				headingContent));
		Content ul = new HtmlTree(HtmlTag.UL);
		for (DeprElementKind kind : DeprElementKind.values()) {
			if (kind == DeprElementKind.CLASS) {
				final SortedSet<Element> elements = deprapi.getSet(kind);
				addIndexLink(getAgentElements(elements),
						"agent",
						GeneralUtils.getText("doclet.Agents", this.configuration.getLocale()),
						ul);
				addIndexLink(getEventElements(elements),
						"event",
						GeneralUtils.getText("doclet.Events", this.configuration.getLocale()),
						ul);
				addIndexLink(getBehaviorElements(elements),
						"behavior",
						GeneralUtils.getText("doclet.Behaviors", this.configuration.getLocale()),
						ul);
				addIndexLink(getSkillElements(elements),
						"skill",
						GeneralUtils.getText("doclet.Skills", this.configuration.getLocale()),
						ul);
			} else if (kind == DeprElementKind.INTERFACE) {
				final SortedSet<Element> elements = deprapi.getSet(kind);
				addIndexLink(getCapacityElements(elements),
						"capacity",
						GeneralUtils.getText("doclet.Capacities", this.configuration.getLocale()),
						ul);
				addIndexLink(getOopElements(elements),
						"interface",
						this.contents.getContent("doclet.Interfaces").toString(),
						ul);
			} else {
				Reflect.callProc(this, DeprecatedListWriter.class, "addIndexLink",
						new Class[] {DeprecatedAPIListBuilder.class, DeprElementKind.class, Content.class},
						deprapi, kind, ul);
			}
		}
		div.addContent(ul);
		return div;
	}

}
