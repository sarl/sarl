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

package io.sarl.docs.doclet2.html.framework;

import java.nio.file.Path;

import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;

/** Builder of HTML paths.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.15.1 20250911-224827
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public interface PathBuilder {

	/** Create a relative path to the summary of the given module.
	 * Path is relative to the root of the documentation.
	 *
	 * @param module the module for which the path must be computed.
	 * @return the path.
	 */
	Path moduleSummary(ModuleElement module);

	/** Create a relative path to the summary of the given package.
	 * Path is relative to the root of the documentation.
	 *
	 * @param pkg the package for which the path must be computed.
	 * @return the path.
	 */
	Path packageSummary(PackageElement pkg);

	/** Replies a relative path for the given type.
	 *
	 * @param type the type to analyze.
	 * @return the path.
	 */
	Path typeIndex(TypeElement type);

	/** Replies a relative path to the index of all the types.
	 *
	 * @return the path.
	 */
	Path allTypesIndex();

	/** Replies a relative path to the general index.
	 *
	 * @return the path.
	 */
	Path index();

	/** Replies a relative path to the HTML index file.
	 *
	 * @return the path.
	 */
	Path htmlIndexFile();

	/** Replies a relative path to the raw package list.
	 *
	 * @return the path.
	 */
	Path rawPackageList();

	/** Replies a relative path to the raw module list.
	 *
	 * @return the path.
	 */
	Path rawModuleList();

	/** Replies a relative path to the deprecated index.
	 *
	 * @return the path.
	 */
	Path deprecatedIndex();

	/** Replies a relative path to the type hierarchy for a package.
	 *
	 * @param packageElement the package.
	 * @return the path.
	 */
	Path packageTypeHierarchy(PackageElement packageElement);

	/** Replies a relative path to the general type hierarchy.
	 *
	 * @return the path.
	 */
	Path typeHierarchy();

	/** Replies a relative path to the type hierarchy for all packages.
	 *
	 * @return the path.
	 */
	Path overviewTree();

	/** Replies a relative path to the use index for the given type.
	 *
	 * @param type the type for which to reply the use index.
	 * @return the path.
	 */
	Path useIndex(TypeElement type);

	/** Replies a relative path to the overview index.
	 *
	 * @return the path.
	 */
	Path overviewSummary();

	/** Replies a relative path to the overview frame.
	 *
	 * @return the path.
	 */
	Path overviewFrame();

	/** Replies a relative path to the all-types frame.
	 *
	 * @return the path.
	 */
	Path allTypesFrame();

}
