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

import java.util.SortedSet;

import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;

import com.google.common.collect.Sets;
import jdk.javadoc.internal.doclets.formats.html.HtmlConfiguration;
import jdk.javadoc.internal.doclets.formats.html.PackageWriterImpl;
import jdk.javadoc.internal.doclets.formats.html.markup.StringContent;
import jdk.javadoc.internal.doclets.formats.html.markup.TableHeader;
import jdk.javadoc.internal.doclets.toolkit.Content;

import io.sarl.docs.doclet.j11.utils.GeneralUtils;
import io.sarl.lang.sarl.SarlPackage;

/** Writer of package summary dedicated to the SARL doclet.
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
public class SarlPackageWriter extends PackageWriterImpl {

	/** Constructor.
	 *
	 * @param writer the writer for the class that the methods belong to.
	 * @param packageElement the package being documented.
	 */
	public SarlPackageWriter(HtmlConfiguration configuration, PackageElement packageElement) {
		super(configuration, packageElement);
	}

	/** Add agent summary.
	 *
	 * @param agents the agents.
	 * @param summaryContentTree the receiver.
	 */
	public void addAgentSummary(SortedSet<TypeElement> agents, Content summaryContentTree) {
		final TableHeader tableHeader = new TableHeader(
				new StringContent(GeneralUtils.getText(GeneralUtils.DOCLET_TYPE_NAME, this.configuration.getLocale())),
				contents.descriptionLabel);
		addClassesSummary(agents, "Agent Summary", null, tableHeader, summaryContentTree);
	}

	/** Add behavior summary.
	 *
	 * @param behaviors the behaviors.
	 * @param summaryContentTree the receiver.
	 */
	public void addBehaviorSummary(SortedSet<TypeElement> behaviors, Content summaryContentTree) {
		final TableHeader tableHeader = new TableHeader(
				new StringContent(GeneralUtils.getText(GeneralUtils.DOCLET_TYPE_NAME, this.configuration.getLocale())),
				contents.descriptionLabel);
		addClassesSummary(behaviors, "Behavior Summary", null, tableHeader, summaryContentTree);
	}

	/** Add skill summary.
	 *
	 * @param skills the skills.
	 * @param summaryContentTree the receiver.
	 */
	public void addSkillSummary(SortedSet<TypeElement> skills, Content summaryContentTree) {
		final TableHeader tableHeader = new TableHeader(
				new StringContent(GeneralUtils.getText(GeneralUtils.DOCLET_TYPE_NAME, this.configuration.getLocale())),
				contents.descriptionLabel);
		addClassesSummary(skills, "Skill Summary", null, tableHeader, summaryContentTree);
	}

	/** Add event summary.
	 *
	 * @param events the events.
	 * @param summaryContentTree the receiver.
	 */
	public void addEventSummary(SortedSet<TypeElement> events, Content summaryContentTree) {
		final TableHeader tableHeader = new TableHeader(
				new StringContent(GeneralUtils.getText(GeneralUtils.DOCLET_TYPE_NAME, this.configuration.getLocale())),
				contents.descriptionLabel);
		addClassesSummary(events, "Event Summary", null, tableHeader, summaryContentTree);
	}

	@Override
	public void addClassSummary(SortedSet<TypeElement> classes, Content summaryContentTree) {
		addAgentSummary(
				Sets.filter(classes, it -> GeneralUtils.isOneOf(it,
						SarlPackage.SARL_AGENT)), summaryContentTree);
		addBehaviorSummary(
				Sets.filter(classes, it -> GeneralUtils.isOneOf(it,
						SarlPackage.SARL_BEHAVIOR)), summaryContentTree);
		addSkillSummary(
				Sets.filter(classes, it -> GeneralUtils.isOneOf(it,
						SarlPackage.SARL_SKILL)), summaryContentTree);
		addEventSummary(
				Sets.filter(classes, it -> GeneralUtils.isOneOf(it,
						SarlPackage.SARL_EVENT)), summaryContentTree);
		final SortedSet<TypeElement> rest = Sets.filter(classes, it -> !GeneralUtils.isOneOf(it,
				SarlPackage.SARL_AGENT, SarlPackage.SARL_BEHAVIOR,
				SarlPackage.SARL_SKILL, SarlPackage.SARL_EVENT));
		super.addClassSummary(rest, summaryContentTree);
	}

	/** Add capacity summary.
	 *
	 * @param capacities the capacities.
	 * @param summaryContentTree the receiver.
	 */
	public void addCapacitySummary(SortedSet<TypeElement> capacities, Content summaryContentTree) {
		final TableHeader tableHeader = new TableHeader(
				new StringContent(GeneralUtils.getText(GeneralUtils.DOCLET_TYPE_NAME, this.configuration.getLocale())),
				contents.descriptionLabel);
		addClassesSummary(capacities, "Capacity Summary", null, tableHeader, summaryContentTree);
	}

	@Override
	public void addInterfaceSummary(SortedSet<TypeElement> interfaces, Content summaryContentTree) {
		addCapacitySummary(
				Sets.filter(interfaces, it -> GeneralUtils.isOneOf(it, SarlPackage.SARL_CAPACITY)), summaryContentTree);
		super.addInterfaceSummary(
				Sets.filter(interfaces, it -> !GeneralUtils.isOneOf(it, SarlPackage.SARL_CAPACITY)), summaryContentTree);
	}

}
