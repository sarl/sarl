/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Predicate;

import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;

import com.google.inject.Inject;

/** Store the type hierarchy for all the types.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class TypeHierarchyImpl implements TypeHierarchy {

	/**
	 * List of base classes.
	 */
	private final SortedSet<TypeElement> baseClasses = new TreeSet<>((a, b) -> a.getQualifiedName().toString().compareTo(b.getQualifiedName().toString()));

	/**
	 * List of base interfaces.
	 */
	private final SortedSet<TypeElement> baseInterfaces = new TreeSet<>((a, b) -> a.getQualifiedName().toString().compareTo(b.getQualifiedName().toString()));

	/**
	 * Mapping for each type with types who implement it.
	 */
	private final Map<TypeElement, SortedSet<TypeElement>> subTypes = new HashMap<>();

	/**
	 * Mapping for each Interface with classes who implement it.
	 */
	private final Map<TypeElement, SortedSet<TypeElement>> implementingClasses = new HashMap<>();

	private ElementUtils elementUtils;

	/** Change the utilities for elements.
	 *
	 * @param utils the utility.
	 */
	@Inject
	public void setElementUtils(ElementUtils utils) {
		this.elementUtils = utils;
	}

	/** Replies the utilities for elements.
	 *
	 * @return the utility.
	 */
	public ElementUtils getElementUtils() {
		return this.elementUtils;
	}

	@Override
	public void buildTree(Iterable<? extends TypeElement> typeElements, SarlDocletEnvironment environment) {
		this.baseClasses.clear();
		this.baseInterfaces.clear();
		this.implementingClasses.clear();
		this.subTypes.clear();
		//
		for (var typeElement : typeElements) {
			final var type = typeElement.getKind();
			if (type == ElementKind.CLASS) {
				processType(typeElement, this.baseClasses, true, environment);
			} else if (type == ElementKind.INTERFACE) {
				processType(typeElement, this.baseInterfaces, false, environment);
			}
		}
	}

	@Override
	public SortedSet<TypeElement> getBaseClasses() {
		return Collections.unmodifiableSortedSet(this.baseClasses);
	}
	
	@Override
	public SortedSet<TypeElement> getBaseInterfaces() {
		return Collections.unmodifiableSortedSet(this.baseInterfaces);
	}

	/**
	 * Adjust the Class Tree. Add the class interface  in to it's super classes
	 * or super interface's sub-interface set.
	 *
	 * @param map the entire map.
	 * @param superclass java.lang.Object or the super-interface.
	 * @param typeElement sub-interface to be mapped.
	 * @returns boolean true if class added, false if class already processed.
	 */
	@SuppressWarnings("static-method")
	private boolean add(Map<TypeElement, SortedSet<TypeElement>> map, TypeElement superclass, TypeElement typeElement) {
		final var sset = map.computeIfAbsent(superclass, s ->  new TreeSet<>((a, b) -> a.getQualifiedName().toString().compareTo(b.getQualifiedName().toString())));
		if (sset.contains(typeElement)) {
			return false;
		}
		sset.add(typeElement);
		return true;
	}

	/**
	 * For the class passed map it to its own sub-class listing.
	 *
	 * @param typeElement for which sub class mapping is to be generated.
	 * @param bases the collection of base types to update.
	 * @param updateImplementingClasses indicates if the mapping of implementing classes must be updated.
	 * @param environment the current configuration of the doclet.
	 */
	protected void processType(TypeElement typeElement, Set<TypeElement> bases,
			boolean updateImplementingClasses, SarlDocletEnvironment environment) {
		final var superClass = getElementUtils().getFirstVisibleSuperType(typeElement, false, environment);
		if (superClass != null) {
			final var superElement = getElementUtils().asTypeElement(superClass, environment.getTypeUtils());
			if (!add(this.subTypes, superElement, typeElement)) {
				return;
			}
			processType(superElement, bases, updateImplementingClasses, environment);
		} else {
			bases.add(typeElement);
		}
		if (updateImplementingClasses) {
			final var interfaces = getElementUtils().getAllInterfaces(typeElement, environment);
			for (final var interf : interfaces) {
				final TypeElement interf0 = getElementUtils().asTypeElement(interf, environment.getTypeUtils());
				add(this.implementingClasses, interf0, typeElement);
			}
		}
	}

	@Override
	public SortedSet<? extends TypeElement> getDirectSubTypes(TypeElement typeElement) {
		final var subtypes0 = this.subTypes.computeIfAbsent(typeElement,
				t ->  new TreeSet<>((a, b) -> a.getQualifiedName().toString().compareTo(b.getQualifiedName().toString())));
		return Collections.unmodifiableSortedSet(subtypes0);
	}

	@Override
	public Set<TypeElement> getImplementingClasses(TypeElement typeElement) {
		final var subtypes0 = this.implementingClasses.computeIfAbsent(typeElement,
				t ->  new TreeSet<>((a, b) -> a.getQualifiedName().toString().compareTo(b.getQualifiedName().toString())));
		return Collections.unmodifiableSortedSet(subtypes0);
	}

	private void fillCandidates(SarlDocletEnvironment environment, boolean includeInterfaces, TypeElement type,
			Deque<TypeElement> candidates, Set<String> done) {
		final var superMirror = type.getSuperclass();
		if (superMirror != null) {
			final var superElement = environment.getTypeUtils().asElement(superMirror);
			if (superElement instanceof TypeElement te && !environment.getApidocExcluder().isExcluded(superElement)) {
				final var qn = getElementUtils().getLocalIdentifier(te);
				if (done.add(qn)) {
					candidates.add(te);
				}
			}
		}
		if (includeInterfaces) {
			for (final var interfaceMirror : type.getInterfaces()) {
				final var superElement = environment.getTypeUtils().asElement(interfaceMirror);
				if (superElement instanceof TypeElement te && environment.isIncluded(superElement)) {
					final var qn = getElementUtils().getLocalIdentifier(te);
					if (done.add(qn)) {
						candidates.add(te);
					}
				}
			}
		}
	}

	@Override
	public Collection<? extends Element> getInheritedElements(TypeElement typeElement, boolean includeInterfaces, 
			boolean includedElementsOnly, boolean includeDuplicates, boolean considerTypeElementMembers, 
			SarlDocletEnvironment environment, Predicate<Element> selector) {
		final var collection = new ArrayList<Element>();
		if (typeElement != null && selector != null) {
			final var candidates = new LinkedList<TypeElement>();
			final var memberDone = new TreeSet<String>();
			//
			if (!includeDuplicates && considerTypeElementMembers) {
				for (final var member : typeElement.getEnclosedElements()) {
					if (selector.test(member)) {
						final var qn = getElementUtils().getLocalIdentifier(member);
						memberDone.add(qn);
					}
				}
			}
			final var done = new TreeSet<String>();
			fillCandidates(environment, includeInterfaces, typeElement, candidates, done);
			while (!candidates.isEmpty()) {
				final var candidate = candidates.removeFirst();
				for (final var member : candidate.getEnclosedElements()) {
					if (!includedElementsOnly || environment.isIncluded(member)) {
						final var qn = getElementUtils().getLocalIdentifier(member);
						if (selector.test(member) && (includeDuplicates || memberDone.add(qn))) {
							collection.add(member);
						}
					}
				}
				fillCandidates(environment, includeInterfaces, candidate, candidates, done);
			}
		}
		return collection;
	}

	@Override
	public Collection<? extends Element> getDeclaredElements(TypeElement typeElement, boolean includeInterfaces, 
			boolean includedElementsOnly, boolean includeDuplicates, SarlDocletEnvironment environment, Predicate<Element> selector) {
		final var collection = new ArrayList<Element>();
		if (typeElement != null && selector != null) {
			final var candidates = new LinkedList<TypeElement>();
			final var memberDone = new TreeSet<String>();
			//
			for (final var member : typeElement.getEnclosedElements()) {
				final var qn = getElementUtils().getLocalIdentifier(member);
				if (selector.test(member) && (includeDuplicates || memberDone.add(qn))) {
					collection.add(member);
				}
			}
			final var done = new TreeSet<String>();
			fillCandidates(environment, includeInterfaces, typeElement, candidates, done);
			while (!candidates.isEmpty()) {
				final var candidate = candidates.removeFirst();
				for (final var member : candidate.getEnclosedElements()) {
					if (!includedElementsOnly || environment.isIncluded(member)) {
						final var qn = getElementUtils().getLocalIdentifier(member);
						if (selector.test(member) && (includeDuplicates || memberDone.add(qn))) {
							collection.add(member);
						}
					}
				}
				fillCandidates(environment, includeInterfaces, candidate, candidates, done);
			}
		}
		return collection;
	}

}
