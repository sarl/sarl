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
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;

import jdk.javadoc.doclet.Reporter;
import jdk.javadoc.doclet.Taglet.Location;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.Navigation;
import io.sarl.docs.doclet2.html.framework.Navigation.NavigationKind;

/** Generator of the description for a specific module.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class ModuleSummaryGeneratorImpl extends AbstractSummaryGenerator implements ModuleSummaryGenerator {

	private ModuleElement moduleElement;
	
	@Override
	protected void initNavigation(Navigation navigation) {
		navigation.setKind(NavigationKind.MODULE);
	}

	@Override
	protected void generateNavigationBar() {
		getNavigation().generateNavigationBars(this.moduleElement, this);
	}

	@Override
	public void generate(ModuleElement moduleElement, Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		this.moduleElement = moduleElement;
		final String moduleName = getElementUtils().getElementName(moduleElement);
		setDefaultTitle(MessageFormat.format(Messages.ModuleSummaryGeneratorImpl_1, moduleName));
		generate(
				MessageFormat.format(Messages.ModuleSummaryGeneratorImpl_0, moduleName),
				getPathBuilder().moduleSummary(moduleElement),
				cssStylesheets, jsScripts, environment, cliOptions, reporter);
	}

	/** Generate the title in the body.
	 *
	 * @param moduleElement the module for which the documentation is generated.
	 * @param parent the container.
	 */
	protected void generateBodyTitle(ModuleElement moduleElement, Element parent) {
		final Element nameDiv = getHtmlFactory().createDivTag(parent, CssStyles.HEADER_TYPE_NAME);
		nameDiv.appendText(getLastTitle());
	}

	/** Generate the list of packages.
	 *
	 * @param moduleElement the module for which the documentation is generated.
	 * @param parent the container.
	 */
	protected void generatePackageList(ModuleElement moduleElement, Element parent) {
		final Iterable<PackageElement> packages = getTypeRepository().getPackagesFor(moduleElement);
		createSummaryBox1(Messages.ModuleSummaryGeneratorImpl_2,
				Messages.ModuleSummaryGeneratorImpl_3, Messages.ModuleSummaryGeneratorImpl_4,
				null, parent, packages,
				getElementUtils().getPackageElementComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					nodes.addAll(getHtmlFactory().createPackageLink(element, element.getQualifiedName().toString(), null, this));
					createFirstSentence(element, nodes, true, false);
					createShortDeprecationMessage(element, nodes, true);
					return nodes;
				});
	}

	/** Generate the description of the module.
	 *
	 * @param moduleElement the module for which the documentation is generated.
	 * @param parent the container.
	 */
	protected void generateModuleDescription(ModuleElement moduleElement, Element parent) {
		final List<Node> description = new ArrayList<>();
		createFullDescriptionBody(moduleElement, description, false, true);
		final Element descriptionDiv = getHtmlFactory().createDivTag(parent, CssStyles.MODULE_DESCRIPTION);
		descriptionDiv.appendChildren(description);
	}

	/** Generate the introduction of the module.
	 *
	 * @param moduleElement the module for which the documentation is generated.
	 * @param parent the container.
	 */
	protected void generateModuleIntroduction(ModuleElement moduleElement, Element parent) {
		final List<Node> description = new ArrayList<>();
		createFirstSentence(moduleElement, description, false, true);
		createBlockTagsFor(moduleElement, description, Location.MODULE, CssStyles.MODULE_TAG_INFO);
		final Element descriptionDiv = getHtmlFactory().createDivTag(parent, CssStyles.MODULE_DESCRIPTION);
		descriptionDiv.appendChildren(description);		
	}

	@Override
	protected void generateBodyContent(Element parent) {
		generateBodyTitle(this.moduleElement, parent);
		generateModuleIntroduction(this.moduleElement, parent);
		generatePackageList(this.moduleElement, parent);
		generateModuleDescription(this.moduleElement, parent);
	}

}