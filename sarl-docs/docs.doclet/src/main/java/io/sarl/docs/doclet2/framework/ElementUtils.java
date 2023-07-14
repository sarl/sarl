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

import java.util.Comparator;
import java.util.Map;
import java.util.SortedSet;

import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.Elements;
import javax.lang.model.util.Types;

import com.sun.source.doctree.DocTree;

/** Utilities for object elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public interface ElementUtils {

	/** Change the utilities for Java elements.
	 *
	 * @param elementUtils the element utils.
	 */
	void setElements(Elements elementUtils);

	/** Change the utilities for Java types.
	 *
	 * @param typeUtils the type utils.
	 */
	void setTypes(Types typeUtils);

	/** Replies the type symbol with the given signature.
	 *
	 * @param signature the type signature.
	 * @return the type mirror.
	 */
	default TypeMirror getSymbol(String signature) {
		return getSymbol(signature, null);
	}

	/** Replies the type symbol with the given signature.
	 *
	 * @param signature the type signature.
	 * @param findQualifiedNames a function invokes to retrieve the candidates for qualified names. The argument of the function is
	 *      the signature of a type to be found (it is the {@code signature}. The function returns a collection of qualified name
	 *      candidates.
	 * @return the type mirror or {@code null} if it is not found.
	 */
	TypeMirror getSymbol(String signature, QualifiedNameSetBuilder findQualifiedNames);

	/** Replies the "Object" type.
	 *
	 * @return the Object type.
	 */
	TypeMirror getObjectType();

	/**
	 * Return this type as a {@code TypeElement} if it represents a class,
	 * interface or annotation.  Array dimensions are ignored.
	 * If this type {@code ParameterizedType} or {@code WildcardType}, return
	 * the {@code TypeElement} of the type's erasure.  If this is an
	 * annotation, return this as a {@code TypeElement}.
	 * If this is a primitive type, return {@code null}.
	 *
	 * @param type the type to transform.
	 * @param typeUtils type utilities.
	 * @return the {@code TypeElement} of this type,
	 *         or {@code null} if it is a primitive type.
	 */
	TypeElement asTypeElement(TypeMirror type, Types typeUtils);

	/** Replies the first visible super type.
	 *
	 * @param typeElement is the type element.
	 * @param assumeObject indicates if the {@code Object} type is assumed as root.
	 * @param environment the generation environment.
	 * @return the super type.
	 */
	TypeMirror getFirstVisibleSuperType(TypeElement typeElement, boolean assumeObject, SarlDocletEnvironment environment);

	/** Replies if the given element is public.
	 *
	 * @param element is the element to test.
	 * @return {@code true} if the element is public.
	 */
	boolean isPublic(Element element);

	/** Replies if the given element is protected.
	 *
	 * @param element is the element to test.
	 * @return {@code true} if the element is protected.
	 */
	boolean isProtected(Element element);

	/** Replies if the given element is external to the current project.
	 *
	 * @param element is the element to test.
	 * @param environment the generation environment.
	 * @return {@code true} if the element is external.
	 */
	boolean isExternal(Element element, SarlDocletEnvironment environment);

	/**
	 * Return true if this class is linkable and false if we can't link to the desired class.
	 *
	 * @param typeElement the element to test.
	 * @param environment the generation environment.
	 * @return {@code true} if this class is linkable and {@code false} if we can't link to the
	 *     desired class.
	 */
	boolean isLinkable(TypeElement typeElement, SarlDocletEnvironment environment);

	/**
	 * Replies if the given element is a static element.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the element is static.
	 */
	boolean isStatic(Element element);

	/**
	 * Replies if the given element is a final element.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the element is final.
	 */
	boolean isFinal(Element element);

	/**
	 * Replies if the given element is an abstract element.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the element is abstract.
	 */
	boolean isAbstract(Element element);

	/**
	 * Replies all the implemented interfaces, including those extended by implemented interfaces. 
	 *
	 * @param typeElement the element to test.
	 * @param environment the generation environment.
	 * @return the set of implemented interfaces.
	 */
	SortedSet<? extends TypeMirror> getAllInterfaces(TypeElement typeElement, SarlDocletEnvironment environment);

	/**
	 * Replies the name of the given element. This function supports unnnamed element. 
	 *
	 * @param element the element to use.
	 * @return the name of the element.
	 */
	String getElementName(ModuleElement element);

	/**
	 * Replies the name of the given element. This function supports unnnamed element. 
	 *
	 * @param element the element to use.
	 * @return the name of the element.
	 */
	String getElementName(PackageElement element);

	/** Replies the string representation of the type's modifiers.
	 * This function supports the modifiers that are specific to SARL (agent, event, etc.).
	 *
	 * @param element the element to analyze.
	 * @param trailingSpace indicates if a trailing space must be added.
	 * @param showFinalModifier indicates if the {@code final} modifier could be added in the returned string of characters.
	 * @param showVarValModifier indicates if the {@code var} or {@code val} modifier could be added in the returned string of characters.
	 * @return the string representation of the modifiers.
	 */
	String getModifiersString(Element element, boolean trailingSpace, boolean showFinalModifier, boolean showVarValModifier);

	/** Replies the string representation of the type's modifiers.
	 *
	 * @param element the element to analyze.
	 * @param trailingSpace indicates if a trailing space must be added.
	 * @return the string representation of the modifiers.
	 */
	String getVisibilityModifiersString(Element element, boolean trailingSpace);

	/**
	 * Given an annotation, return {@code true} if it should be documented and {@code false}
	 * otherwise.
	 *
	 * @param annotation the annotation to check.     *
	 * @return {@code true} if it should be documented and {@code false} otherwise.
	 */
	boolean isDocumentedAnnotation(TypeElement annotation);

	/** Replies the fully qualified name of the given element.
	 * This function is a generalization of the {@code getQualifiedName()} that is provided
	 * by several types of {@code Element}.
	 *
	 * @param element the element to analyze
	 * @param outer indicates if the name of the enclosing type must be the prefix of the qualified name (for elements that are not
	 *    providing a {@code getQualifiedName()} function).
	 * @return the fully qualified name.
	 */
	String getFullyQualifiedName(Element element, final boolean outer);

	/** Replies the internal identifier of the given element without the fully qualified name of the enclosing element.
	 *
	 * @param element the element to analyze
	 * @return the identifier.
	 * @see #getFullIdentifier(Element)
	 */
	String getLocalIdentifier(Element element);

	/** Replies the internal identifier of the given element with the fully qualified name of the enclosing element.
	 *
	 * @param element the element to analyze
	 * @return the identifier.
	 * @see #getLocalIdentifier(Element)
	 * @see #getFullIdentifier(TypeMirror)
	 */
	String getFullIdentifier(Element element);

	/** Replies the internal identifier of the given type with the fully qualified name of the enclosing element.
	 *
	 * @param type the type to analyze
	 * @return the identifier.
	 * @see #getFullIdentifier(Element)
	 */
	String getFullIdentifier(TypeMirror type);

	/** Replies the qualified name of the given element.
	 * This function replies the concatenation of the enclosing type name and the inner type name.
	 *
	 * @param element the element to analyze
	 * @return the inner type qualified name.
	 */
	String getInnerTypeQualifiedName(Element element);

	/** Replies if the given element is synthetized.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the element is synthetized.
	 */
	boolean isSynthetized(AnnotationMirror element);

	/** Replies if the given set of annotation value is an array.
	 *
	 * @param annotationValues the values to check.
	 * @return {@code true} if the annotation values is an array.
	 */
	boolean isAnnotationArray(Map<? extends ExecutableElement, ? extends AnnotationValue> annotationValues);

	/**
	 * Return the type's dimension information, as a string.
	 * For example, a two dimensional array of String returns "{@code [][]}".
	 *
	 * @param type the type.
	 * @return the type's dimension information as a string.
	 */
	String getDimension(TypeMirror type);

	/**
	 * Replies if the given element is deprecated with the removal tag.
	 *
	 * @param element the element to analyze.
	 * @return {@code true} if the element is deprecated for removal.
	 */
	boolean isDeprecatedForRemoval(Element element);

	/**
	 * Replies "since" message of the deprecated annotation.
	 *
	 * @param element the element to analyze.
	 * @return The message.
	 */
	String getDeprecatedSince(Element element);

	/** Replies if the given element is executable.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the given element is executable. 
	 */
	boolean isExecutableElement(Element element);

	/** Replies if the given element is variable.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the given element is a variable. 
	 */
	boolean isVariableElement(Element element);

	/** Replies if the given element is a package.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the given element is a package. 
	 */
	boolean isPackage(Element element);

	/** Replies if the given element is a constructor.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the given element is a constructor. 
	 */
	boolean isConstructor(Element element);

	/** Replies the enclosing type element.
	 *
	 * @param executable the executable.
	 * @return the type element.
	 */
	TypeElement getEnclosingTypeElement(Element executable);

	/** Replies if the given element is a type element.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the elemet is a type.
	 */
	boolean isTypeElement(Element element);

	/** Replies if the given element is marked as private API, or one of its container.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the element or one of its container is marked as part of the private API.
	 */
	boolean isPrivateAPI(Element element);

	/** Replies the element that is referenced in the given documentation.
	 *
	 * @param documentation the documentation.
	 * @param currentType is the type that could be used by default if no type is specified into the documentation.
	 * @param findQualifiedNames a function invokes to retrieve the candidates for qualified names. The argument of the function is
	 *      the signature of a type to be found (it is the {@code signature}. The function returns a collection of qualified name
	 *      candidates.
	 * @return the reference element or {@code null} if none.
	 */
	Element getReferencedElement(DocTree documentation, TypeMirror currentType, QualifiedNameSetBuilder findQualifiedNames);

	/** Replies the type that is referenced by the given documentation.
	 *
	 * @param documentation the documentation reference.
	 * @param currentType is the type that could be used by default if no type is specified into the documentation.
	 * @param findQualifiedNames a function invokes to retrieve the candidates for qualified names. The argument of the function is
	 *      the signature of a type to be found (it is the {@code signature}. The function returns a collection of qualified name
	 *      candidates.
	 * @return the type element or {@code null} if none.
	 */
	TypeElement getReferencedClass(DocTree documentation, TypeMirror currentType, QualifiedNameSetBuilder findQualifiedNames);

	/** Replies the member that is referenced by the given documentation.
	 *
	 * @param documentation the documentation reference.
	 * @param currentType is the type that could be used by default if no type is specified into the documentation.
	 * @param findQualifiedNames a function invokes to retrieve the candidates for qualified names. The argument of the function is
	 *      the signature of a type to be found (it is the {@code signature}. The function returns a collection of qualified name
	 *      candidates.
	 * @return the member element or {@code null} if none.
	 */
	Element getReferencedMember(DocTree documentation, TypeMirror currentType, QualifiedNameSetBuilder findQualifiedNames);

	/** Replies the comparator for executable elements.
	 *
	 * @param <T> the type of the executable element.
	 * @return the comparator.
	 */
	<T extends ExecutableElement> Comparator<? super T> getExecutableElementComparator();

	/** Replies the comparator for module elements.
	 *
	 * @param <T> the type of the module element.
	 * @return the comparator.
	 */
	<T extends ModuleElement> Comparator<? super T> getModuleElementComparator();

	/** Replies the comparator for package elements.
	 *
	 * @param <T> the type of the package element.
	 * @return the comparator.
	 */
	<T extends PackageElement> Comparator<? super T> getPackageElementComparator();

	/** Replies the comparator for type elements.
	 *
	 * @param <T> the type of the type element.
	 * @return the comparator.
	 */
	<T extends TypeElement> Comparator<? super T> getTypeElementComparator();

	/** Replies the comparator for type elements that is sorting on basename first.
	 *
	 * @param <T> the type of the type element.
	 * @return the comparator.
	 */
	<T extends TypeElement> Comparator<? super T> getTypeElementBasenameComparator();

	/** Replies the comparator for type mirrors.
	 *
	 * @param <T> the type of the type mirrors.
	 * @return the comparator.
	 */
	<T extends TypeMirror> Comparator<? super T> getTypeMirrorComparator();

	/** Replies the comparator for variable elements.
	 *
	 * @param <T> the type of the variable element.
	 * @return the comparator.
	 */
	<T extends VariableElement> Comparator<? super T> getVariableElementComparator();

	/** Replies if the given element could contain SARL event handlers.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the given element can contain event handlers. 
	 */
	boolean isEventHandlerContainer(TypeElement element);

	/** Replies if the given element could use SARL capacities.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the given element could use SARL capacities. 
	 */
	boolean isCapacityUser(TypeElement element);

	/** Replies if the given element is an agent type.
	 *
	 * @param type the type to test.
	 * @return {@code true} if the given type is agent.
	 */
	boolean isSarlAgent(TypeElement type);

	/** Replies if the given element is a behavior type.
	 *
	 * @param type the type to test.
	 * @return {@code true} if the given type is behavior.
	 */
	boolean isSarlBehavior(TypeElement type);

	/** Replies if the given element is a capacity type.
	 *
	 * @param type the type to test.
	 * @return {@code true} if the given type is capacity.
	 */
	boolean isSarlCapacity(TypeElement type);

	/** Replies if the given element is a skill type.
	 *
	 * @param type the type to test.
	 * @return {@code true} if the given type is skill.
	 */
	boolean isSarlSkill(TypeElement type);

	/** Replies if the given element is an event type.
	 *
	 * @param type the type to test.
	 * @return {@code true} if the given type is event.
	 */
	boolean isSarlEvent(TypeElement type);

	/** Replies if the given element is an agent-oriented type.
	 *
	 * @param type the type to test.
	 * @return {@code true} if the given type is agent-oriented.
	 * @see #isSarlAgent(TypeElement)
	 * @see #isSarlBehavior(TypeElement)
	 * @see #isSarlCapacity(TypeElement)
	 * @see #isSarlSkill(TypeElement)
	 * @see #isSarlEvent(TypeElement)
	 */
	default boolean isAopElement(TypeElement type) {
		return isSarlAgent(type) || isSarlBehavior(type) || isSarlCapacity(type) || isSarlSkill(type) || isSarlEvent(type);
	}

	/** Replies the first enclosing element of the given element that is marked as deprecated.
	 *
	 * @param element the element from which the exploration must be done.
	 * @return the first deprecated element, or {@code null} if no deprecated element was found.
	 */
	Element getFirstEnclosingDeprecatedElement(Element element);

}
