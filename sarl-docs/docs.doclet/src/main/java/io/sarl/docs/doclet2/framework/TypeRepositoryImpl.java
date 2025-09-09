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

import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.util.Elements;

import com.google.inject.Inject;

/** Store the types for all the types.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.15.0 20250909-115750
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public class TypeRepositoryImpl implements TypeRepository {

	private SortedSet<TypeElement> types;

	private Map<ModuleElement, SortedSet<TypeElement>> typesPerModule;
	
	private Map<PackageElement, SortedSet<TypeElement>> typesPerPackage;

	private Map<ModuleElement, SortedSet<PackageElement>> packagesPerModule;

	private SortedSet<TypeElement> deprecatedTypes;

	private SortedSet<VariableElement> deprecatedFields;

	private SortedSet<ExecutableElement> deprecatedExecutables;

	private ElementUtils elementUtils;

	/** Change the SARL-specific element utilities.
	 *
	 * @param utils the element utilities.
	 */
	@Inject
	public void setElementUtils(ElementUtils utils) {
		this.elementUtils = utils;
	}

	/** Replies the SARL-specific element utilities.
	 *
	 * @return the utilities.
	 */
	public ElementUtils getElementUtils() {
		return this.elementUtils;
	}
	
	@Override
	public void buildRepository(Iterable<? extends TypeElement> typeElements, SarlDocletEnvironment environment) {
		final var typeComparator = getElementUtils().getTypeElementComparator();
		final var packageComparator = getElementUtils().getPackageElementComparator();
		this.types = new TreeSet<>(typeComparator);
		this.typesPerModule = new TreeMap<>(getElementUtils().getModuleElementComparator());
		this.typesPerPackage = new TreeMap<>(packageComparator);
		this.packagesPerModule = new TreeMap<>(getElementUtils().getModuleElementComparator());
		this.deprecatedTypes = new TreeSet<>(typeComparator);
		this.deprecatedFields = new TreeSet<>(getElementUtils().getVariableElementComparator());
		this.deprecatedExecutables = new TreeSet<>(getElementUtils().getExecutableElementComparator());
		for (final var element : typeElements) {
			this.types.add(element);
			final var moduleElement = environment.getElementUtils().getModuleOf(element);
			if (moduleElement != null) {
				final var list0 = this.typesPerModule.computeIfAbsent(moduleElement, it -> new TreeSet<>(typeComparator));
				list0.add(element);
			}
			final var packageElement = environment.getElementUtils().getPackageOf(element);
			if (packageElement != null) {
				final var list0 = this.typesPerPackage.computeIfAbsent(packageElement, it -> new TreeSet<>(typeComparator));
				list0.add(element);
				final var list1 = this.packagesPerModule.computeIfAbsent(moduleElement, it -> new TreeSet<>(packageComparator));
				list1.add(packageElement);
			}
			if (environment.getElementUtils().isDeprecated(element)) {
				if (this.deprecatedTypes.add(element)) {
					deprecate(element.getEnclosedElements());
				}
			} else {
				discoverDeprecatedMembers(element.getEnclosedElements(), environment.getElementUtils());
			}
		}
	}

	private void deprecate(Iterable<? extends Element> elements) {
		for (final var element : elements) {
			switch (element.getKind()) {
			case FIELD:
				this.deprecatedFields.add((VariableElement) element);
				break;
			case METHOD:
			case CONSTRUCTOR:
				this.deprecatedExecutables.add((ExecutableElement) element);
				break;
			case CLASS:
			case INTERFACE:
			case ENUM:
			case ANNOTATION_TYPE:
				if (this.deprecatedTypes.add((TypeElement) element)) {
					deprecate(element.getEnclosedElements());
				}
				break;
			default:
				// Ignore the element
			}
		}
	}

	private void discoverDeprecatedMembers(Iterable<? extends Element> elements, Elements elementUtils) {
		for (final var element : elements) {
			if (elementUtils.isDeprecated(element)) {
				switch (element.getKind()) {
				case FIELD:
					this.deprecatedFields.add((VariableElement) element);
					break;
				case METHOD:
				case CONSTRUCTOR:
					this.deprecatedExecutables.add((ExecutableElement) element);
					break;
				default:
					// Ignore the element
				}
			}
		}
	}

	@Override
	public Set<ModuleElement> getModules() {
		return Collections.unmodifiableSet(this.typesPerModule.keySet());
	}

	@Override
	public Set<PackageElement> getPackages() {
		return Collections.unmodifiableSet(this.typesPerPackage.keySet());
	}

	@Override
	public SortedSet<TypeElement> getTypesInModule(ModuleElement moduleName) {
		var data = this.typesPerModule.get(moduleName);
		if (data == null) {
			return Collections.emptySortedSet();
		}
		return Collections.unmodifiableSortedSet(data);
	}

	@Override
	public SortedSet<TypeElement> getTypesInPackage(PackageElement packageName) {
		var data = this.typesPerPackage.get(packageName);
		if (data == null) {
			return Collections.emptySortedSet();
		}
		return Collections.unmodifiableSortedSet(data);
	}

	@Override
	public Iterable<PackageElement> getPackagesFor(ModuleElement moduleName) {
		var data = this.packagesPerModule.get(moduleName);
		if (data == null) {
			return Collections.emptySortedSet();
		}
		return Collections.unmodifiableSortedSet(data);
	}

	@Override
	public SortedSet<TypeElement> getTypes() {
		if (this.types == null) {
			return Collections.emptySortedSet();
		}
		return Collections.unmodifiableSortedSet(this.types);
	}
	
	@Override
	public boolean hasDeprecatedFeatures() {
		return (this.deprecatedTypes != null && !this.deprecatedTypes.isEmpty())
				|| (this.deprecatedFields != null && !this.deprecatedFields.isEmpty())
				|| (this.deprecatedExecutables != null && !this.deprecatedExecutables.isEmpty());
	}

	@Override
	public SortedSet<TypeElement> getDeprecatedTypes() {
		if (this.deprecatedTypes == null) {
			return Collections.emptySortedSet();
		}
		return Collections.unmodifiableSortedSet(this.deprecatedTypes);
	}

	@Override
	public SortedSet<VariableElement> getDeprecatedFields() {
		if (this.deprecatedFields == null) {
			return Collections.emptySortedSet();
		}
		return Collections.unmodifiableSortedSet(this.deprecatedFields);
	}

	@Override
	public SortedSet<ExecutableElement> getDeprecatedExecutables() {
		if (this.deprecatedExecutables == null) {
			return Collections.emptySortedSet();
		}
		return Collections.unmodifiableSortedSet(this.deprecatedExecutables);
	}

}
