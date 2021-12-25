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

package io.sarl.docs.doclet2.framework;

import java.util.LinkedHashSet;
import java.util.Set;

import javax.lang.model.element.PackageElement;

import org.eclipse.xtext.util.Strings;

/** A standard implementation of a tool for finding the qualified name of an element in an expected search path.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class StandardQualifiedNameSetBuilder implements QualifiedNameSetBuilder {

	private final PackageElement currentPackage;
	private final Set<String> importedPackages;
	
	/** Constructor.
	 *
	 * @param currentPackage the current (default) package.
	 * @param importedPackages the set of packages that must be considered into the search path.
	 */
	public StandardQualifiedNameSetBuilder(PackageElement currentPackage, Set<String> importedPackages) {
		this.currentPackage = currentPackage;
		this.importedPackages = importedPackages;
	}

	private static String qualifiedName(String packageName, String elementName) {
		if (Strings.isEmpty(packageName)) {
			return elementName;
		}
		return packageName + '.' + elementName;
	}

	@Override
	public Set<String> buildCandidateList(String basename) {
		final Set<String> candidates = new LinkedHashSet<>(importedPackages.size() + 3);

		// Add the imports that were define into the element's source code
		for (final String importedPkg : this.importedPackages) {
			candidates.add(qualifiedName(importedPkg, basename));
		}
		
		// Add as candidate the package of the element
		candidates.add(qualifiedName(currentPackage.getQualifiedName().toString(), basename));
		
		// Add as candidate the java default
		candidates.add(qualifiedName(String.class.getPackageName(), basename));
		return candidates;
	}

}
