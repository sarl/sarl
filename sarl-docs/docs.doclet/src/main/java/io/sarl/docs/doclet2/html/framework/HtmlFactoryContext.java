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

package io.sarl.docs.doclet2.html.framework;

import java.nio.file.Path;

import javax.lang.model.element.Element;

import io.sarl.docs.doclet2.framework.DocUtils;
import io.sarl.docs.doclet2.framework.ExternalLinkManager.ExternalLinkManagerContext;
import io.sarl.docs.doclet2.framework.QualifiedNameSetBuilder;
import io.sarl.docs.doclet2.framework.TagletManager;

/** Context for an HTML factory.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public interface HtmlFactoryContext extends ExternalLinkManagerContext {

	/** Replies the last computed base URI.
	 *
	 * @return the base URI.
	 */
	String getBaseUri();

	/** Replies the relative path to the documentation root.
	 *
	 * @return the path to root.
	 */
	Path getPathToRoot();

	/** Replies the CLI options.
	 *
	 * @return the options.
	 */
	DocletOptions getCliOptions();

	/** Replies the manager of taglets.
	 *
	 * @return the manager of taglets
	 */
	TagletManager getTagletManager();

	/** Replies the utilities for the documentation.
	 *
	 * @return the utilities for the documentation.
	 */
	DocUtils getDocUtils();

	/** Replies the finder of fully qualified names for types.
	 *
	 * @pazram element the current element.
	 * @return the fully qualified name finder.
	 */
	QualifiedNameSetBuilder getQualifiedNameSetBuilder(Element element); 

}
