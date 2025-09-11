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

package io.sarl.docs.doclet2.framework;

import java.util.LinkedHashSet;
import java.util.Set;

import javax.lang.model.element.PackageElement;

import org.eclipse.xtext.util.Strings;

/** A standard implementation of a tool for finding the qualified name of an element in an expected search path.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
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
		final var candidates = new LinkedHashSet<String>(this.importedPackages.size() + 3);

		// Add the imports that were define into the element's source code
		for (final var importedPkg : this.importedPackages) {
			candidates.add(qualifiedName(importedPkg, basename));
		}
		
		// Add as candidate the package of the element
		candidates.add(qualifiedName(this.currentPackage.getQualifiedName().toString(), basename));
		
		// Add as candidate the java default
		candidates.add(qualifiedName(String.class.getPackageName(), basename));

		return candidates;
	}

}
