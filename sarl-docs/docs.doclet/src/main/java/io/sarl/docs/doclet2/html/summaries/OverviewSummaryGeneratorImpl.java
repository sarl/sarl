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
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;

import jdk.javadoc.doclet.Reporter;
import org.eclipse.xtext.xbase.lib.Pair;
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
public class OverviewSummaryGeneratorImpl extends AbstractSummaryGenerator implements OverviewSummaryGenerator {

	public OverviewSummaryGeneratorImpl() {
		super(Messages.OverviewSummaryGeneratorImpl_1);
	}

	@Override
	protected void generateNavigationBar() {
		getNavigation().generateNavigationBars((TypeElement) null, this);
	}

	@Override
	protected void initNavigation(Navigation navigation) {
		navigation.setKind(NavigationKind.OVERVIEW);
	}

	@Override
	public void generate(Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		generate(
				Messages.OverviewSummaryGeneratorImpl_0, DocPaths.OVERVIEW_SUMMARY_HTML,
				cssStylesheets, jsScripts, environment, cliOptions, reporter);
	}

	/** Generate the title in the body.
	 *
	 * @param parent the container.
	 */
	protected void generateBodyTitle(Element parent) {
		final Element nameDiv = getHtmlFactory().createDivTag(parent, CssStyles.HEADER_TYPE_NAME);
		nameDiv.appendText(getDocumentationTitle());
	}

	/** Generate the list of modules.
	 *
	 * @param parent the container.
	 */
	protected void generateModuleList(Element parent) {
		final Iterable<ModuleElement> modules = getTypeRepository().getModules();
		createSummaryBox1(Messages.OverviewSummaryGeneratorImpl_2,
				Messages.OverviewSummaryGeneratorImpl_3, Messages.OverviewSummaryGeneratorImpl_4,
				null, parent, modules,
				getElementUtils().getModuleElementComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					nodes.addAll(getHtmlFactory().createModuleLink(element, element.getQualifiedName().toString(), null, this));
					createFirstSentence(element, nodes, true, false);
					createShortDeprecationMessage(element, nodes, true);
					return nodes;
				});
	}

	/** Generate the list of packages.
	 *
	 * @param parent the container.
	 * @param packages the list of packages to be included.
	 */
	protected void generatePackageList(Element parent, Collection<PackageElement> packages) {
		createSummaryBox1(Messages.OverviewSummaryGeneratorImpl_5,
				Messages.OverviewSummaryGeneratorImpl_6, Messages.OverviewSummaryGeneratorImpl_4,
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

	private SortedSet<PackageElement> selectPackages(SortedSet<PackageElement> remainingPackages, Pattern pattern) {
		final SortedSet<PackageElement> selectedPackages = new TreeSet<>(getElementUtils().getPackageElementComparator());
		final Iterator<PackageElement> iterator = remainingPackages.iterator();
		while (iterator.hasNext()) {
			final PackageElement candidate = iterator.next();
			final Matcher matcher = pattern.matcher(candidate.getQualifiedName().toString());
			if (matcher.matches()) {
				iterator.remove();
				selectedPackages.add(candidate);
			}
		}
		return selectedPackages;
	}
	
	/** Generate the groups of packages.
	 *
	 * @param parent the container.
	 * @param packages the list of packages to be included.
	 * @return the {@code packages} that are not a member of a group.
	 */
	protected Collection<PackageElement> generateGroups(Element parent, Collection<PackageElement> packages) {
		final SortedSet<PackageElement> remainingPackages = new TreeSet<>(getElementUtils().getPackageElementComparator());
		remainingPackages.addAll(packages);
		for (final Pair<String, Pattern> entry : getCliOptions().getGroups()) {
			final SortedSet<PackageElement> selectedPackages = selectPackages(remainingPackages, entry.getValue());
			if (!selectedPackages.isEmpty()) {
				createSummaryBox1(entry.getKey(),
						Messages.OverviewSummaryGeneratorImpl_6, Messages.OverviewSummaryGeneratorImpl_4,
						null, parent, selectedPackages,
						getElementUtils().getPackageElementComparator(),
						element -> {
							final List<Node> nodes = new ArrayList<>();
							nodes.addAll(getHtmlFactory().createPackageLink(element, element.getQualifiedName().toString(), null, this));
							createFirstSentence(element, nodes, true, false);
							createShortDeprecationMessage(element, nodes, true);
							return nodes;
						});
			}
		}
		return remainingPackages;
	}

	@Override
	protected void generateBodyContent(Element parent) {
		generateBodyTitle(parent);
		final Collection<PackageElement> packages = generateGroups(parent, new ArrayList<>(getTypeRepository().getPackages()));
		generateModuleList(parent);
		generatePackageList(parent, packages);
	}

}