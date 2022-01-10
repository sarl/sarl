/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2022 the original authors or authors.
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

package io.sarl.docs.doclet2.html.framework;

import java.util.List;

import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;

import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;

/** Represent the navigation bar.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public interface Navigation {

	/** Replies the kind of navigation.
	 *
	 * @return the kind.
	 */
	NavigationKind getKind();
	
	/** Change the kind of navigation.
	 *
	 * @param kind the kind.
	 */
	void setKind(NavigationKind kind);

	/** Set the link to the module.
	 *
	 * @param link the link.
	 */
	void setModuleLink(List<? extends Node> link);

	/** Replies the link to the module.
	 *
	 * @return the link.
	 */
	List<? extends Node> getModuleLink();

	/** Create a placeholder for a navigation bar.
	 *
	 * @param parent the container.
	 * @return the element.
	 */
	Node createNavigationBar(Element parent);

	/** Generate the content of the navigation bar and place it into all the place holders created with
	 * {@link #createNavigationBar(org.jsoup.nodes.Element)}
	 *
	 * @param currentType the type for which the navigation bars are generated.
	 * @param context the html factory context.
	 */
	void generateNavigationBars(TypeElement currentType, HtmlFactoryContext context);

	/** Generate the content of the navigation bar and place it into all the place holders created with
	 * {@link #createNavigationBar(org.jsoup.nodes.Element)}
	 *
	 * @param currentPackage the package for which the navigation bars are generated.
	 * @param context the html factory context.
	 */
	void generateNavigationBars(PackageElement currentPackage, HtmlFactoryContext context);

	/** Generate the content of the navigation bar and place it into all the place holders created with
	 * {@link #createNavigationBar(org.jsoup.nodes.Element)}
	 *
	 * @param currentModule the module for which the navigation bars are generated.
	 * @param context the html factory context.
	 */
	void generateNavigationBars(ModuleElement currentModule, HtmlFactoryContext context);

	/** Register the given anchor to be one for a summary box.
	 *
	 * @param anchor the id.
	 */
	void addSummaryBoxAnchor(String anchor);

	/** Register the given anchor to be one for a detail box.
	 *
	 * @param anchor the id.
	 */
	void addDetailBoxAnchor(String anchor);

	/** Kind of navigation.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	public enum NavigationKind {

		/** Overview.
		 */
		OVERVIEW,
		
		/** Module.
		 */
		MODULE,

		/** Package.
		 */
		PACKAGE,
		
		/** Type.
		 */
		TYPE,

		/** TREE.
		 */
		TREE,

		/** Deprecated.
		 */
		DEPRECATED,

		/** Index.
		 */
		INDEX;

	}

}
