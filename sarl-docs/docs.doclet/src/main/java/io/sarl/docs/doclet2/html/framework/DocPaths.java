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

/** List of paths for documentation.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.15.0 20250909-115750
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public interface DocPaths {

	/** Basename for the summary pages.
	 */
	String SUMMARY_HTML = "summary.html"; //$NON-NLS-1$

	/** Basename for the package summary pages.
	 */
	String PACKAGE_SUMMARY_HTML = "package-summary.html"; //$NON-NLS-1$

	/** Basename for the default package summary pages.
	 */
	String DEFAULT_PACKAGE_SUMMARY_HTML = "default-package-summary.html"; //$NON-NLS-1$

	/** Basename for the index of all types.
	 */
	String ALL_TYPE_HTML = "all-types.html"; //$NON-NLS-1$

	/** Basename for the global index.
	 */
	String INDEX_HTML = "index-all.html"; //$NON-NLS-1$

	/** Basename for the HTML index file.
	 */
	String HTML_INDEX_HTML = "index.html"; //$NON-NLS-1$

	/** Basename for the raw package list.
	 */
	String RAW_PACKAGE_LIST = "package-list"; //$NON-NLS-1$

	/** Basename for the raw module list.
	 */
	String RAW_MODULE_LIST = "module-list"; //$NON-NLS-1$

	/** Basename for the deprecated index.
	 */
	String DEPRECATED_INDEX_HTML = "deprecated-list.html"; //$NON-NLS-1$

	/** Basename for the package tree.
	 */
	String PACKAGE_TREE_HTML = "package-tree.html"; //$NON-NLS-1$
	
	/** Basename for the general tree.
	 */
	String TREE_HTML = "overview-tree.html"; //$NON-NLS-1$

	/** Basename for the overview tree.
	 */
	String OVERVIEW_TREE_HTML = "overview-tree.html"; //$NON-NLS-1$

	/** Basename for the use folder name.
	 */
	String USE_FOLDER = "class-use"; //$NON-NLS-1$
	
	/** Basename for the overview index.
	 */
	String OVERVIEW_SUMMARY_HTML = "overview-summary.html"; //$NON-NLS-1$

	/** Basename for the overview frame.
	 */
	String OVERVIEW_FRAME_HTML = "overview-frame.html"; //$NON-NLS-1$

	/** Basename for the overview frame.
	 */
	String ALL_TYPES_FRAME_HTML = "alltypes-frame.html"; //$NON-NLS-1$

}
