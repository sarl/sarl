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

import static io.sarl.docs.doclet2.html.framework.DocPaths.ALL_TYPES_FRAME_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.ALL_TYPE_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.DEFAULT_PACKAGE_SUMMARY_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.DEPRECATED_INDEX_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.HTML_INDEX_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.INDEX_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.OVERVIEW_FRAME_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.OVERVIEW_SUMMARY_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.OVERVIEW_TREE_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.PACKAGE_SUMMARY_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.PACKAGE_TREE_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.RAW_MODULE_LIST;
import static io.sarl.docs.doclet2.html.framework.DocPaths.RAW_PACKAGE_LIST;
import static io.sarl.docs.doclet2.html.framework.DocPaths.SUMMARY_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.TREE_HTML;
import static io.sarl.docs.doclet2.html.framework.DocPaths.USE_FOLDER;

import java.nio.file.Path;
import java.util.regex.Pattern;

import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;

import com.google.common.base.Strings;

/** Builder of HTML paths.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class PathBuilderImpl implements PathBuilder {

	/** Create a module path.
	 *
	 * @param module the module.
	 * @param basename the basename.
	 * @return the path.
	 */
	protected Path createModulePath(ModuleElement module, String basename) {
		final String name;
		if (module.isUnnamed()) {
			name = "unnamed-module-" + basename;
		} else {
			name = module.getQualifiedName() + "-" + basename;
		}
		return Path.of(name);
	}

	@Override
	public Path moduleSummary(ModuleElement module) {
		if (module == null) {
			return null;
		}
		return createModulePath(module, SUMMARY_HTML);
	}

	/** Create a package path.
	 *
	 * @param pkg the package.
	 * @param basename the basename. If {@code null}, consider {@link DocPaths#PACKAGE_SUMMARY_HTML}.
	 * @return the path.
	 */
	protected Path createPackagePath(PackageElement pkg, String basename) {
		final String[] elements = pkg.getQualifiedName().toString().split(Pattern.quote("."));
		Path pt = null;
		for (int i = 0 ; i < elements.length; ++i) {
			final String elt = elements[i];
			final Path cpath = Path.of(elt);
			if (pt == null) {
				pt = cpath;
			} else {
				pt = pt.resolve(cpath);
			}
		}
		String name = basename;
		if (Strings.isNullOrEmpty(name)) {
			if (pkg.isUnnamed()) {
				name = DEFAULT_PACKAGE_SUMMARY_HTML;
			} else {
				name = PACKAGE_SUMMARY_HTML;
			}
		}
		if (pt == null) {
			pt = Path.of(name);
		} else {
			pt = pt.resolve(name);
		}
		return pt;
	}

	@Override
	public Path packageSummary(PackageElement pkg) {
		if (pkg == null) {
			return null;
		}
		return createPackagePath(pkg, null);
	}

	/** Create a type path.
	 *
	 * @param type the type.
	 * @param basename the basename.
	 * @return the path.
	 */
	protected Path createTypePath(TypeElement type, String basename) {
		String[] elements = type.getQualifiedName().toString().split(Pattern.quote("."));
		Path pt = null;
		for (int i = 0 ; i < elements.length - 1; ++i) {
			final String elt = elements[i];
			final Path cpath = Path.of(elt);
			if (pt == null) {
				pt = cpath;
			} else {
				pt = pt.resolve(cpath);
			}
		}
		if (pt == null) {
			if (basename != null) {
				pt = Path.of(basename);
			} else {
				pt = Path.of(elements[elements.length - 1] + ".html");
			}
		} else if (basename != null) {
			pt = pt.resolve(basename);
		} else {
			pt = pt.resolve(elements[elements.length - 1] + ".html");
		}
		return pt;
	}

	@Override
	public Path typeIndex(TypeElement type) {
		if (type == null) {
			return null;
		}
		return createTypePath(type, null);
	}

	@Override
	public Path allTypesIndex() {
		return Path.of(ALL_TYPE_HTML);
	}

	@Override
	public Path index() {
		return Path.of(INDEX_HTML);
	}

	@Override
	public Path htmlIndexFile() {
		return Path.of(HTML_INDEX_HTML);
	}

	@Override
	public Path rawPackageList() {
		return Path.of(RAW_PACKAGE_LIST);
	}

	@Override
	public Path rawModuleList() {
		return Path.of(RAW_MODULE_LIST);
	}

	@Override
	public Path deprecatedIndex() {
		return Path.of(DEPRECATED_INDEX_HTML);
	}

	@Override
	public Path packageTypeHierarchy(PackageElement packageElement) {
		if (packageElement == null) {
			return null;
		}
		return createPackagePath(packageElement, PACKAGE_TREE_HTML);
	}

	@Override
	public Path typeHierarchy() {
		return Path.of(TREE_HTML);
	}

	@Override
	public Path overviewTree() {
		return Path.of(OVERVIEW_TREE_HTML);
	}

	@Override
	public Path useIndex(TypeElement type) {
		if (type == null) {
			return null;
		}
		final Path typeIndex = typeIndex(type);
		final Path parent = typeIndex.getParent();
		final Path basename = typeIndex.getFileName();
		return parent.resolve(USE_FOLDER).resolve(basename);
	}

	@Override
	public Path overviewSummary() {
		return Path.of(OVERVIEW_SUMMARY_HTML);
	}

	@Override
	public Path overviewFrame() {
		return Path.of(OVERVIEW_FRAME_HTML);
	}

	@Override
	public Path allTypesFrame() {
		return Path.of(ALL_TYPES_FRAME_HTML);
	}

}
