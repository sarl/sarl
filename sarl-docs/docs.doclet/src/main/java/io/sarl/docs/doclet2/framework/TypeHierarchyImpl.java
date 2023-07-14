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

package io.sarl.docs.doclet2.framework;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Predicate;

import javax.inject.Inject;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;

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
		for (TypeElement typeElement : typeElements) {
			final ElementKind type = typeElement.getKind();
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
		final SortedSet<TypeElement> sset = map.computeIfAbsent(superclass, s ->  new TreeSet<>((a, b) -> a.getQualifiedName().toString().compareTo(b.getQualifiedName().toString())));
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
		final TypeMirror superClass = getElementUtils().getFirstVisibleSuperType(typeElement, false, environment);
		if (superClass != null) {
			final TypeElement superElement = getElementUtils().asTypeElement(superClass, environment.getTypeUtils());
			if (!add(this.subTypes, superElement, typeElement)) {
				return;
			}
			processType(superElement, bases, updateImplementingClasses, environment);
		} else {
			bases.add(typeElement);
		}
		if (updateImplementingClasses) {
			final SortedSet<? extends TypeMirror> interfaces = getElementUtils().getAllInterfaces(typeElement, environment);
			for (TypeMirror interf : interfaces) {
				final TypeElement interf0 = getElementUtils().asTypeElement(interf, environment.getTypeUtils());
				add(this.implementingClasses, interf0, typeElement);
			}
		}
	}

	@Override
	public SortedSet<? extends TypeElement> getDirectSubTypes(TypeElement typeElement) {
		final SortedSet<TypeElement> subtypes0 = this.subTypes.computeIfAbsent(typeElement,
				t ->  new TreeSet<>((a, b) -> a.getQualifiedName().toString().compareTo(b.getQualifiedName().toString())));
		return Collections.unmodifiableSortedSet(subtypes0);
	}

	@Override
	public Set<TypeElement> getImplementingClasses(TypeElement typeElement) {
		final SortedSet<TypeElement> subtypes0 = this.implementingClasses.computeIfAbsent(typeElement,
				t ->  new TreeSet<>((a, b) -> a.getQualifiedName().toString().compareTo(b.getQualifiedName().toString())));
		return Collections.unmodifiableSortedSet(subtypes0);
	}

	private void fillCandidates(SarlDocletEnvironment environment, boolean includeInterfaces, TypeElement type,
			Deque<TypeElement> candidates, Set<String> done) {
		final TypeMirror superMirror = type.getSuperclass();
		if (superMirror != null) {
			final Element superElement = environment.getTypeUtils().asElement(superMirror);
			if (superElement instanceof TypeElement && !environment.getApidocExcluder().isExcluded(superElement)) {
				final TypeElement te = (TypeElement) superElement;
				final String qn = getElementUtils().getLocalIdentifier(te);
				if (done.add(qn)) {
					candidates.add(te);
				}
			}
		}
		if (includeInterfaces) {
			for (final TypeMirror interfaceMirror : type.getInterfaces()) {
				final Element superElement = environment.getTypeUtils().asElement(interfaceMirror);
				if (superElement instanceof TypeElement && environment.isIncluded(superElement)) {
					final TypeElement te = (TypeElement) superElement;
					final String qn = getElementUtils().getLocalIdentifier(te);
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
		final List<Element> collection = new ArrayList<>();
		if (typeElement != null && selector != null) {
			final Deque<TypeElement> candidates = new LinkedList<>();
			final Set<String> memberDone = new TreeSet<>();
			//
			if (!includeDuplicates && considerTypeElementMembers) {
				for (final Element member : typeElement.getEnclosedElements()) {
					if (selector.test(member)) {
						final String qn = getElementUtils().getLocalIdentifier(member);
						memberDone.add(qn);
					}
				}
			}
			final Set<String> done = new TreeSet<>();
			fillCandidates(environment, includeInterfaces, typeElement, candidates, done);
			while (!candidates.isEmpty()) {
				final TypeElement candidate = candidates.removeFirst();
				for (final Element member : candidate.getEnclosedElements()) {
					if (!includedElementsOnly || environment.isIncluded(member)) {
						final String qn = getElementUtils().getLocalIdentifier(member);
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
		final List<Element> collection = new ArrayList<>();
		if (typeElement != null && selector != null) {
			final Deque<TypeElement> candidates = new LinkedList<>();
			final Set<String> memberDone = new TreeSet<>();
			//
			for (final Element member : typeElement.getEnclosedElements()) {
				final String qn = getElementUtils().getLocalIdentifier(member);
				if (selector.test(member) && (includeDuplicates || memberDone.add(qn))) {
					collection.add(member);
				}
			}
			final Set<String> done = new TreeSet<>();
			fillCandidates(environment, includeInterfaces, typeElement, candidates, done);
			while (!candidates.isEmpty()) {
				final TypeElement candidate = candidates.removeFirst();
				for (final Element member : candidate.getEnclosedElements()) {
					if (!includedElementsOnly || environment.isIncluded(member)) {
						final String qn = getElementUtils().getLocalIdentifier(member);
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
