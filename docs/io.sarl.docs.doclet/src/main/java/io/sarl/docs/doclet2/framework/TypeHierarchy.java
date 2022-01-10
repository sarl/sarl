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

package io.sarl.docs.doclet2.framework;

import java.util.Collection;
import java.util.Set;
import java.util.SortedSet;
import java.util.function.Predicate;

import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;

/** Store the type hierarchy for all the types.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public interface TypeHierarchy {

	/** Build the type hierarchy.
	 *
	 * @param typeElements the initial set of types for which documentation must be generated.
	 * @param environment the generation environment.
	 */
	void buildTree(Iterable<? extends TypeElement> typeElements, SarlDocletEnvironment environment);

	/** Replies the base classes.
	 *
	 * @return the base classes.
	 */
	SortedSet<TypeElement> getBaseClasses();

	/** Replies the base interfaces.
	 *
	 * @return the base interfaces.
	 */
	SortedSet<TypeElement> getBaseInterfaces();

	/** Replies the direct subtypes of the given type.
	 *
	 * @param typeElement the type to search for.
	 * @return the direct subtypes.
	 */
	SortedSet<? extends TypeElement> getDirectSubTypes(TypeElement typeElement);

	/** Replies the implemented classes for the given type if it is an interface.
	 *
	 * @param typeElement the interface type.
	 * @return the list of implementing interfaces.
	 */
	Set<TypeElement> getImplementingClasses(TypeElement typeElement);

	/** Replies the collection of elements that are validating the selector and inherited from the super-types.
	 *
	 * @param typeElement the type from which the type hierarchy is explored.
	 * @param includeInterfaces indicates if the interface hierarchy must be included, or not.
	 * @param includedElementsOnly indicates if only the {@link SarlDocletEnvironment#isIncluded(Element) "included" elements} are considered.
	 * @param includeDuplicates indicates if duplicate prototypes are included.
	 * @param considerTypeElementMembers indicates if the members of the {@code typeElement} are considered.
	 * @param environment the generation environment.
	 * @param selector the selector of the inherited members.
	 * @return the list of inherited members.
	 * @see #getDeclaredElements(TypeElement, boolean, boolean, SarlDocletEnvironment, Predicate)
	 */
	Collection<? extends Element> getInheritedElements(TypeElement typeElement, boolean includeInterfaces,
			boolean includedElementsOnly, boolean includeDuplicates, boolean considerTypeElementMembers,
			SarlDocletEnvironment environment, Predicate<Element> selector);

	/** Replies the collection of elements that are validating the selector and inherited from the super-types.
	 *
	 * @param typeElement the type from which the type hierarchy is explored.
	 * @param includeInterfaces indicates if the interface hierarchy must be included, or not.
	 * @param environment the generation environment.
	 * @param selector the selector of the inherited members.
	 * @return the list of inherited members.
	 * @see #getDeclaredElements(TypeElement, boolean, boolean, SarlDocletEnvironment, Predicate)
	 */
	default Collection<? extends Element> getInheritedElements(TypeElement typeElement, boolean includeInterfaces,
			SarlDocletEnvironment environment, Predicate<Element> selector) {
		return getInheritedElements(typeElement, includeInterfaces, true, false, true, environment, selector);
	}

	/** Replies the collection of elements that are validating the selector in the current type
	 * and inherited from the super-types.
	 *
	 * @param typeElement the type from which the type hierarchy is explored.
	 * @param includeInterfaces indicates if the interface hierarchy must be included, or not.
	 * @param includedElementsOnly indicates if only the {@link SarlDocletEnvironment#isIncluded(Element) "included" elements} are considered.
	 * @param includeDuplicates indicates if duplicate prototypes are included.
	 * @param environment the generation environment.
	 * @param selector the selector of the inherited members.
	 * @return the list of inherited members.
	 * @see #getInheritedElements(TypeElement, boolean, boolean, SarlDocletEnvironment, Predicate)
	 */
	Collection<? extends Element> getDeclaredElements(TypeElement typeElement, boolean includeInterfaces, 
			boolean includedElementsOnly, boolean includeDuplicates, SarlDocletEnvironment environment, Predicate<Element> selector);

	/** Replies the collection of elements that are validating the selector in the current type
	 * and inherited from the super-types.
	 *
	 * @param typeElement the type from which the type hierarchy is explored.
	 * @param includeInterfaces indicates if the interface hierarchy must be included, or not.
	 * @param environment the generation environment.
	 * @param selector the selector of the inherited members.
	 * @return the list of inherited members.
	 * @see #getInheritedElements(TypeElement, boolean, SarlDocletEnvironment, Predicate)
	 */
	default Collection<? extends Element> getDeclaredElements(TypeElement typeElement, boolean includeInterfaces, 
			SarlDocletEnvironment environment, Predicate<Element> selector) {
		return getDeclaredElements(typeElement, includeInterfaces, true, false, environment, selector);
	}

}
