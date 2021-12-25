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

package io.sarl.docs.doclet2.html.framework;

/** List of paths for documentation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public interface DocPaths {

	/** Basename for the summary pages.
	 */
	String SUMMARY_HTML = "summary.html";

	/** Basename for the package summary pages.
	 */
	String PACKAGE_SUMMARY_HTML = "package-summary.html";

	/** Basename for the default package summary pages.
	 */
	String DEFAULT_PACKAGE_SUMMARY_HTML = "default-package-summary.html";

	/** Basename for the index of all types.
	 */
	String ALL_TYPE_HTML = "all-types.html";

	/** Basename for the global index.
	 */
	String INDEX_HTML = "index-all.html";

	/** Basename for the HTML index file.
	 */
	String HTML_INDEX_HTML = "index.html";

	/** Basename for the raw package list.
	 */
	String RAW_PACKAGE_LIST = "package-list";

	/** Basename for the raw module list.
	 */
	String RAW_MODULE_LIST = "module-list";

	/** Basename for the deprecated index.
	 */
	String DEPRECATED_INDEX_HTML = "deprecated-list.html";

	/** Basename for the package tree.
	 */
	String PACKAGE_TREE_HTML = "package-tree.html";
	
	/** Basename for the general tree.
	 */
	String TREE_HTML = "overview-tree.html";

	/** Basename for the overview tree.
	 */
	String OVERVIEW_TREE_HTML = "overview-tree.html";

	/** Basename for the use folder name.
	 */
	String USE_FOLDER = "class-use";
	
	/** Basename for the overview index.
	 */
	String OVERVIEW_SUMMARY_HTML = "overview-summary.html";

	/** Basename for the overview frame.
	 */
	String OVERVIEW_FRAME_HTML = "overview-frame.html";

	/** Basename for the overview frame.
	 */
	String ALL_TYPES_FRAME_HTML = "alltypes-frame.html";

}
