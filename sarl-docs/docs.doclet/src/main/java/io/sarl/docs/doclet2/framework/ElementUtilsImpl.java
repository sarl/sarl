/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

import static javax.lang.model.element.Modifier.ABSTRACT;
import static javax.lang.model.element.Modifier.FINAL;
import static javax.lang.model.element.Modifier.PRIVATE;
import static javax.lang.model.element.Modifier.PROTECTED;
import static javax.lang.model.element.Modifier.PUBLIC;
import static javax.lang.model.element.Modifier.STATIC;
import static javax.lang.model.type.TypeKind.NONE;

import java.lang.annotation.Documented;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.RecordComponentElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.ArrayType;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.ErrorType;
import javax.lang.model.type.ExecutableType;
import javax.lang.model.type.IntersectionType;
import javax.lang.model.type.NoType;
import javax.lang.model.type.NullType;
import javax.lang.model.type.PrimitiveType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.type.TypeVariable;
import javax.lang.model.type.UnionType;
import javax.lang.model.type.WildcardType;
import javax.lang.model.util.AbstractTypeVisitor9;
import javax.lang.model.util.ElementKindVisitor9;
import javax.lang.model.util.Elements;
import javax.lang.model.util.SimpleAnnotationValueVisitor9;
import javax.lang.model.util.SimpleElementVisitor9;
import javax.lang.model.util.SimpleTypeVisitor9;
import javax.lang.model.util.Types;

import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.util.Strings;

import com.google.inject.Inject;
import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.LinkTree;
import com.sun.source.doctree.ProvidesTree;
import com.sun.source.doctree.ReferenceTree;
import com.sun.source.doctree.SeeTree;
import com.sun.source.doctree.SerialFieldTree;
import com.sun.source.doctree.UsesTree;
import com.sun.source.doctree.ValueTree;
import com.sun.source.util.SimpleDocTreeVisitor;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.annotation.PrivateAPI;
import io.sarl.lang.jvmmodel.IDefaultVisibilityProvider;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Utilities for object elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class ElementUtilsImpl implements ElementUtils {

	/** Default name for an element.
	 */
	public static final String DEFAULT_ELEMENT_NAME = "<Unnamed>"; //$NON-NLS-1$

	private static final String SPACE = " "; //$NON-NLS-1$

	private static final Pattern MEMBER_NAME_PATTERN = Pattern.compile("(.+?)(?:\\((.*)\\))?"); //$NON-NLS-1$

	private Map<String, TypeMirror> typeCache = new HashMap<>();

	private Elements elementUtils;

	private Types typeUtils;

	private DocUtils docUtils;

	private SARLGrammarKeywordAccess keywords;

	private TypeMirror agentTypeCache;

	private TypeMirror behaviorTypeCache;

	private TypeMirror skillTypeCache;

	private TypeMirror eventTypeCache;

	private TypeMirror capacityTypeCache;

	private final Comparator<ModuleElement> moduleElementComparator = (a, b) -> {
		if (a == b) {
			return 0;
		}
		if (a == null) {
			return Integer.MIN_VALUE;
		}
		if (b == null) {
			return Integer.MAX_VALUE;
		}
		final var na = a.getQualifiedName().toString();
		final var nb = b.getQualifiedName().toString();
		final var cmp = na.compareToIgnoreCase(nb);
		if (cmp != 0) {
			return cmp;
		}
		return na.compareTo(nb);
	};

	private final Comparator<PackageElement> packageElementComparator = (a, b) -> {
		if (a == b) {
			return 0;
		}
		if (a == null) {
			return Integer.MIN_VALUE;
		}
		if (b == null) {
			return Integer.MAX_VALUE;
		}
		final var na = a.getQualifiedName().toString();
		final var nb = b.getQualifiedName().toString();
		final var cmp = na.compareToIgnoreCase(nb);
		if (cmp != 0) {
			return cmp;
		}
		return na.compareTo(nb);
	};

	private final Comparator<TypeElement> typeElementComparator = (a, b) -> {
		if (a == b) {
			return 0;
		}
		if (a == null) {
			return Integer.MIN_VALUE;
		}
		if (b == null) {
			return Integer.MAX_VALUE;
		}
		final var na = a.getQualifiedName().toString();
		final var nb = b.getQualifiedName().toString();
		final var cmp = na.compareToIgnoreCase(nb);
		if (cmp != 0) {
			return cmp;
		}
		return na.compareTo(nb);
	};

	private final Comparator<TypeElement> typeElementBasenameComparator = (a, b) -> {
		if (a == b) {
			return 0;
		}
		if (a == null) {
			return Integer.MIN_VALUE;
		}
		if (b == null) {
			return Integer.MAX_VALUE;
		}
		final var na = a.getSimpleName().toString();
		final var nb = b.getSimpleName().toString();
		final var cmp = na.compareToIgnoreCase(nb);
		if (cmp != 0) {
			return cmp;
		}
		final var na0 = a.getQualifiedName().toString();
		final var nb0 = b.getQualifiedName().toString();
		final var cmp0 = na0.compareToIgnoreCase(nb0);
		if (cmp0 != 0) {
			return cmp0;
		}
		return na0.compareTo(nb0);
	};

	private final Comparator<TypeMirror> typeMirrorComparator = (a, b) -> {
		if (a == b) {
			return 0;
		}
		if (a == null) {
			return Integer.MIN_VALUE;
		}
		if (b == null) {
			return Integer.MAX_VALUE;
		}
		return this.typeElementComparator.compare(asTypeElement(a, this.typeUtils), asTypeElement(b, this.typeUtils));
	};

	private final Comparator<ExecutableElement> executableComparator = (a, b) -> {
		if (a == b) {
			return 0;
		}
		if (a == null) {
			return Integer.MIN_VALUE;
		}
		if (b == null) {
			return Integer.MAX_VALUE;
		}
		final var na0 = a.getSimpleName().toString();
		final var nb0 = b.getSimpleName().toString();
		var nameCmp = na0.compareToIgnoreCase(nb0);
		if (nameCmp != 0) {
			return nameCmp;
		}
		nameCmp = na0.compareTo(nb0);
		if (nameCmp != 0) {
			return nameCmp;
		}
		final var nparamCmp = a.getParameters().size() - b.getParameters().size();
		if (nparamCmp != 0) {
			return nparamCmp;
		}
		assert a.getParameters().size() == b.getParameters().size();
		final var sz = a.getParameters().size();
		for (var i = 0; i < sz; ++i) {
			final var pa = a.getParameters().get(i);
			final var pb = b.getParameters().get(i);
			final var ta = pa.asType();
			final var tb = pb.asType();
			final var cmp = this.typeMirrorComparator.compare(ta, tb);
			if (cmp != 0) {
				return cmp;
			}
		}
		return 0;
	};

	private final Comparator<VariableElement> variableComparator = (a, b) -> {
		if (a == b) {
			return 0;
		}
		if (a == null) {
			return Integer.MIN_VALUE;
		}
		if (b == null) {
			return Integer.MAX_VALUE;
		}
		final var na0 = a.getSimpleName().toString();
		final var nb0 = b.getSimpleName().toString();
		final var cmp = na0.compareToIgnoreCase(nb0);
		if (cmp != 0) {
			return cmp;
		}
		return na0.compareTo(nb0);
	};

	/** Replies the root type for the SARL agents.
	 *
	 * @return the root agent type.
	 */
	protected TypeMirror getAgentType() {
		if (this.agentTypeCache == null) {
			this.agentTypeCache = this.elementUtils.getTypeElement(Agent.class.getName()).asType();
		}
		return this.agentTypeCache;
	}

	/** Replies the root type for the SARL behaviors.
	 *
	 * @return the root behavior type.
	 */
	protected TypeMirror getBehaviorType() {
		if (this.behaviorTypeCache == null) {
			this.behaviorTypeCache = this.elementUtils.getTypeElement(Behavior.class.getName()).asType();
		}
		return this.behaviorTypeCache;
	}

	/** Replies the root type for the SARL skills.
	 *
	 * @return the root skill type.
	 */
	protected TypeMirror getSkillType() {
		if (this.skillTypeCache == null) {
			this.skillTypeCache = this.elementUtils.getTypeElement(Skill.class.getName()).asType();
		}
		return this.skillTypeCache;
	}

	/** Replies the root type for the SARL events.
	 *
	 * @return the root event type.
	 */
	protected TypeMirror getEventType() {
		if (this.eventTypeCache == null) {
			this.eventTypeCache = this.elementUtils.getTypeElement(Event.class.getName()).asType();
		}
		return this.eventTypeCache;
	}

	/** Replies the root type for the SARL capacities.
	 *
	 * @return the root capacity type.
	 */
	protected TypeMirror getCapacityType() {
		if (this.capacityTypeCache == null) {
			this.capacityTypeCache = this.elementUtils.getTypeElement(Capacity.class.getName()).asType();
		}
		return this.capacityTypeCache;
	}

	/** Change the documentation utilities.
	 *
	 * @param utils the utilities.
	 */
	@Inject
	public void setDocUtils(DocUtils utils) {
		this.docUtils = utils;
	}

	/** Replies the documentation utilities.
	 *
	 * @return the utilities.
	 */
	public DocUtils getDocUtils() {
		return this.docUtils;
	}

	/** Change the accessor to the SARL keywords.
	 *
	 * @param accessor is the accessor.
	 */
	@Inject
	public void setSARLGrammarKeywordAccess(SARLGrammarKeywordAccess accessor) {
		this.keywords = accessor;
	}

	/** Replies the accessor to the SARL keywords.
	 *
	 * @return the accessor.
	 */
	public SARLGrammarKeywordAccess getSARLGrammarKeywordAccess() {
		return this.keywords;
	}

	@Override
	public void setElements(Elements elementUtils) {
		this.elementUtils = elementUtils;
	}

	@Override
	public void setTypes(Types typeUtils) {
		this.typeUtils = typeUtils;
	}

	private static boolean isNoType(TypeMirror t) {
		return t.getKind() == NONE;
	}

	@Override
	public TypeMirror getSymbol(String signature, QualifiedNameSetBuilder findQualifiedNames) {
		final var signatures = new LinkedList<String>();
		signatures.addLast(signature);
		var useQualifiedNames = findQualifiedNames != null;
		while (!signatures.isEmpty()) {
			final var sig = signatures.removeFirst();
			var type = this.typeCache.get(sig);
			if (type != null) {
				return type;
			}
			var typeElement = this.elementUtils.getTypeElement(sig);
			if (typeElement != null) {
				type = typeElement.asType();
				if (type != null) {
					this.typeCache.put(sig, type);
					return type;
				}
			}
			if (useQualifiedNames) {
				assert findQualifiedNames != null;
				useQualifiedNames = false;
				final var qualifiedNames = findQualifiedNames.buildCandidateList(sig);
				if (qualifiedNames != null) {
					for (final var qn : qualifiedNames) {
						signatures.addLast(qn);
					}
				}
			}
		}
		return null;
	}

	@Override
	public TypeMirror getObjectType() {
		return getSymbol(Object.class.getName());
	}

	/** Replies if the given type is annotated.
	 *
	 * @param e the mirror.
	 * @return {@code true} if the mirror is annotated.
	 */
	public static boolean isAnnotated(TypeMirror e) {
		return !e.getAnnotationMirrors().isEmpty();
	}

	@Override
	public TypeElement asTypeElement(TypeMirror type, Types typeUtils) {
		return new SimpleTypeVisitor9<TypeElement, Void>() {

			@Override
			public TypeElement visitDeclared(DeclaredType t, Void p) {
				return (TypeElement) t.asElement();
			}

			@Override
			public TypeElement visitArray(ArrayType t, Void p) {
				return visit(t.getComponentType());
			}

			@Override
			public TypeElement visitTypeVariable(TypeVariable t, Void p) {
				if (isAnnotated(t)) {
					return visit(t.asElement().asType());
				}
				return visit(typeUtils.erasure(t));
			}

			@Override
			public TypeElement visitWildcard(WildcardType t, Void p) {
				return visit(typeUtils.erasure(t));
			}

			@Override
			public TypeElement visitError(ErrorType t, Void p) {
				return (TypeElement) t.asElement();
			}

			@Override
			protected TypeElement defaultAction(TypeMirror e, Void p) {
				return super.defaultAction(e, p);
			}
		}.visit(type);
	}

	@Override
	public boolean isPublic(Element element) {
		return element.getModifiers().contains(Modifier.PUBLIC);
	}

	@Override
	public boolean isProtected(Element element) {
		return element.getModifiers().contains(Modifier.PROTECTED);
	}

	/** Replies the element that may be considered as includable from the given element.
	 *
	 * @param element the source element.
	 * @return the includable element from the inut.
	 */
	protected Element toIncludableElement(Element element) {
		switch (element.getKind()) {
		case ANNOTATION_TYPE:
		case CLASS:
		case ENUM:
		case INTERFACE:
		case MODULE:
		case PACKAGE:
			return element;
		case CONSTRUCTOR:
		case ENUM_CONSTANT:
		case EXCEPTION_PARAMETER:
		case FIELD:
		case INSTANCE_INIT:
		case LOCAL_VARIABLE:
		case METHOD:
		case PARAMETER:
		case RESOURCE_VARIABLE:
		case STATIC_INIT:
		case TYPE_PARAMETER:
			return getEnclosingTypeElement(element);
		case OTHER:
		default:
			break;
		}
		return null;
	}

	@Override
	public boolean isExternal(Element element, SarlDocletEnvironment environment) {
		final var packageElement = environment.getElementUtils().getPackageOf(element);
		if (packageElement == null || packageElement.isUnnamed()) {
			return false;
		}
		final var elt = toIncludableElement(element);
		return !environment.getIncludedElements().contains(elt);
	}

	@Override
	public boolean isLinkable(TypeElement typeElem, SarlDocletEnvironment environment) {
		return
				(typeElem != null && environment.isIncluded(typeElem)) ||
				(isExternal(typeElem, environment) && (isPublic(typeElem) || isProtected(typeElem)));
	}

	@Override
	public TypeMirror getFirstVisibleSuperType(TypeElement typeElement, boolean assumeObject, SarlDocletEnvironment environment) {
		var superType = typeElement.getSuperclass();
		if (isNoType(superType)) {
			if (assumeObject) {
				superType = getObjectType();
			} else {
				return null;
			}
		}
		var superClass = asTypeElement(superType, environment.getTypeUtils());
		// skip "hidden" classes
		while (superClass != null
				&& ((!environment.isIncluded(superClass))
				|| (!isPublic(superClass) && !isLinkable(superClass, environment)))) {
			var supersuperType = superClass.getSuperclass();
			var supersuperClass = asTypeElement(supersuperType, environment.getTypeUtils());
			if (supersuperClass == null
					|| supersuperClass.getQualifiedName().equals(superClass.getQualifiedName())) {
				break;
			}
			superType = supersuperType;
			superClass = supersuperClass;
		}
		if (typeElement.asType().equals(superType)) {
			return null;
		}
		return superType;
	}

	@Override
	public String getElementName(ModuleElement element) {
		if (element == null || element.isUnnamed()) {
			return DEFAULT_ELEMENT_NAME;
		}
		return element.getQualifiedName().toString();
	}

	@Override
	public String getElementName(PackageElement element) {
		if (element == null || element.isUnnamed()) {
			return DEFAULT_ELEMENT_NAME;
		}
		return element.getQualifiedName().toString();
	}

	@Override
	public SortedSet<? extends TypeMirror> getAllInterfaces(TypeElement typeElement, SarlDocletEnvironment environment) {
		final var candidates = new LinkedList<TypeMirror>();
		final var done = new TreeSet<TypeMirror>((a, b) -> a.toString().compareTo(b.toString()));
		final var interfaces = new TreeSet<TypeMirror>((a, b) -> a.toString().compareTo(b.toString()));
		final var types = environment.getTypeUtils();
		//
		var superType = typeElement.getSuperclass();
		if (superType.getKind() != TypeKind.NONE) {
			candidates.add(superType);
		}
		candidates.addAll(typeElement.getInterfaces());
		//
		while (!candidates.isEmpty()) {
			final var candidate = candidates.removeFirst();
			if (done.add(candidate)) {
				final var superElement = asTypeElement(candidate, types);
				if (superElement.getKind() == ElementKind.INTERFACE) {
					interfaces.add(candidate);
				} else {
					superType = superElement.getSuperclass();
					if (superType.getKind() != TypeKind.NONE) {
						candidates.add(superType);
					}
				}
				candidates.addAll(superElement.getInterfaces());
			}
		}
		return interfaces;
	}

	/** Replies the default visibility for the given element. This function uses the default visibility flags from
	 * the SARL compiler.
	 * 
	 * @param element the element to test.
	 * @return the visibility modifier.
	 */
	protected Modifier getDefaultVisibilityFor(Element element) {
		var jvmVisibility = JvmVisibility.PUBLIC;
		final var enclosing = element.getEnclosingElement();
		if (enclosing != null) {
			final var enclosingType = enclosing.asType();
			final var tester = new IDefaultVisibilityProvider.Tester() {
				@Override
				public boolean isInterface() {
					return enclosing.getKind() == ElementKind.INTERFACE;
				}

				@Override
				public boolean isEvent() {
					return enclosing.getKind() == ElementKind.CLASS
							&& ElementUtilsImpl.this.typeUtils.isAssignable(enclosingType, getEventType());
				}

				@Override
				public boolean isAnnotationType() {
					return enclosing.getKind() == ElementKind.ANNOTATION_TYPE;
				}

				@Override
				public boolean isAgent() {
					return enclosing.getKind() == ElementKind.CLASS
							&& ElementUtilsImpl.this.typeUtils.isAssignable(enclosingType, getAgentType());
				}
			};
			switch (element.getKind()) {
			case ANNOTATION_TYPE:
				jvmVisibility = IDefaultVisibilityProvider.getAnnotationTypeDefaultVisibilityIn(tester);
				break;
			case CLASS:
				jvmVisibility = IDefaultVisibilityProvider.getClassDefaultVisibilityIn(tester);
				break;
			case ENUM:
				jvmVisibility = IDefaultVisibilityProvider.getEnumerationDefaultVisibilityIn(tester);
				break;
			case FIELD:
				jvmVisibility = IDefaultVisibilityProvider.getFieldDefaultVisibilityIn(tester);
				break;
			case INTERFACE:
				jvmVisibility = IDefaultVisibilityProvider.getInterfaceDefaultVisibilityIn(tester);
				break;
			case METHOD:
				jvmVisibility = IDefaultVisibilityProvider.getActionDefaultVisibilityIn(tester);
				break;
			default:
			}
		}
		switch (jvmVisibility) {
		case PRIVATE:
			return Modifier.PRIVATE;
		case PROTECTED:
			return Modifier.PROTECTED;
		case PUBLIC:
			return Modifier.PUBLIC;
		case DEFAULT:
		default:
			return Modifier.DEFAULT;
		}
	}

	@Override
	public String getVisibilityModifiersString(Element element, boolean trailingSpace) {
		final var modifierList = new TreeSet<>(element.getModifiers());
		removeModifiersToIgnore(modifierList);
		final var defaultVisibility = getDefaultVisibilityFor(element);
		return new ElementKindVisitor9<String, SortedSet<Modifier>>() {
			final StringBuilder stringRepresentation = new StringBuilder();

			private void addVisibilityModifier(Set<Modifier> modifiers) {
				if (modifiers.contains(PUBLIC)) {
					if (defaultVisibility == null || defaultVisibility != PUBLIC) {
						this.stringRepresentation.append(getSARLGrammarKeywordAccess().getPublicKeyword()).append(SPACE);
					}
				} else if (modifiers.contains(PROTECTED)) {
					if (defaultVisibility == null || defaultVisibility != PROTECTED) {
						this.stringRepresentation.append(getSARLGrammarKeywordAccess().getProtectedKeyword()).append(SPACE);
					}
				} else if (modifiers.contains(PRIVATE)) {
					if (defaultVisibility == null || defaultVisibility != PRIVATE) {
						this.stringRepresentation.append(getSARLGrammarKeywordAccess().getPrivateKeyword()).append(SPACE);
					}
				} else if (defaultVisibility == null) {
					this.stringRepresentation.append(getSARLGrammarKeywordAccess().getPackageKeyword()).append(SPACE);
				}
			}

			private String finalString(String s) {
				this.stringRepresentation.append(s);
				if (trailingSpace) {
					if (this.stringRepresentation.lastIndexOf(SPACE) == this.stringRepresentation.length() - 1) {
						return this.stringRepresentation.toString();
					}
					return this.stringRepresentation.append(SPACE).toString();
				}
				return this.stringRepresentation.toString().trim();
			}

			@Override
			public String visitTypeAsInterface(TypeElement elt, SortedSet<Modifier> param) {
				addVisibilityModifier(param);
				return finalString(getSARLGrammarKeywordAccess().getInterfaceKeyword());
			}

			@Override
			public String visitTypeAsEnum(TypeElement elt, SortedSet<Modifier> param) {
				addVisibilityModifier(param);
				return finalString(getSARLGrammarKeywordAccess().getEnumKeyword());
			}

			@Override
			public String visitTypeAsAnnotationType(TypeElement elt, SortedSet<Modifier> param) {
				addVisibilityModifier(param);
				return finalString(getSARLGrammarKeywordAccess().getAnnotationKeyword());
			}

			@Override
			public String visitTypeAsClass(TypeElement elt, SortedSet<Modifier> param) {
				addVisibilityModifier(param);
				return finalString(getSARLGrammarKeywordAccess().getClassKeyword());
			}

			@Override
			public String visitTypeAsRecord(TypeElement elt, SortedSet<Modifier> param) {
				addVisibilityModifier(param);
				//TODO: Record is not part of the SARL syntax yet
				return finalString("record"); //$NON-NLS-1$
			}

			@Override
			protected String defaultAction(Element elt, SortedSet<Modifier> param) {
				addVisibilityModifier(param);
				return this.stringRepresentation.toString().trim();
			}

		}.visit(element, modifierList);
	}

	private static void removeModifiersToIgnore(SortedSet<Modifier> modifierList) {
		modifierList.remove(Modifier.NATIVE);
		modifierList.remove(Modifier.STRICTFP);
		modifierList.remove(Modifier.SYNCHRONIZED);
	}

	@Override
	public String getModifiersString(Element element, boolean trailingSpace, boolean showFinalModifier, boolean showVarValModifier) {
		final var modifierList = new TreeSet<>(element.getModifiers());
		removeModifiersToIgnore(modifierList);
		final var defaultVisibility = getDefaultVisibilityFor(element);
		return new ElementKindVisitor9<String, SortedSet<Modifier>>() {
			final StringBuilder stringRepresentation = new StringBuilder();

			private void addVisibilityModifier(Set<Modifier> modifiers) {
				if (modifiers.contains(PUBLIC)) {
					if (defaultVisibility == null || defaultVisibility != PUBLIC) {
						this.stringRepresentation.append(getSARLGrammarKeywordAccess().getPublicKeyword()).append(SPACE);
					}
				} else if (modifiers.contains(PROTECTED)) {
					if (defaultVisibility == null || defaultVisibility != PROTECTED) {
						this.stringRepresentation.append(getSARLGrammarKeywordAccess().getProtectedKeyword()).append(SPACE);
					}
				} else if (modifiers.contains(PRIVATE)) {
					if (defaultVisibility == null || defaultVisibility != PRIVATE) {
						this.stringRepresentation.append(getSARLGrammarKeywordAccess().getPrivateKeyword()).append(SPACE);
					}
				} else if (defaultVisibility == null) {
					this.stringRepresentation.append(getSARLGrammarKeywordAccess().getPackageKeyword()).append(SPACE);
				}
			}

			private void addStaticModifier(Set<Modifier> modifiers) {
				if (modifiers.contains(STATIC)) {
					this.stringRepresentation.append(getSARLGrammarKeywordAccess().getStaticStaticKeyword()).append(SPACE);
				}
			}

			private void addAbstractModifier(Set<Modifier> modifiers, ElementKind elementType) {
				if (modifiers.contains(ABSTRACT) && elementType != ElementKind.INTERFACE) {
					this.stringRepresentation.append(getSARLGrammarKeywordAccess().getAbstractKeyword()).append(SPACE);
				}
			}

			private void addFinalModifier(Set<Modifier> modifiers) {
				if (modifiers.contains(FINAL) && showFinalModifier) {
					this.stringRepresentation.append(getSARLGrammarKeywordAccess().getFinalKeyword()).append(SPACE);
				}
			}

			private void addVarValModifier(Set<Modifier> modifiers) {
				if (showVarValModifier) {
					final String kw;
					if (modifiers.contains(FINAL)) {
						kw = getSARLGrammarKeywordAccess().getValKeyword();
					} else {
						kw = getSARLGrammarKeywordAccess().getWriteableVarKeyword();
					}
					this.stringRepresentation.append(kw).append(SPACE);
				}
			}

			private void addOtherModifiers(Set<Modifier> modifiers) {
				final var str = modifierList.stream().filter(
						it -> it != null && it != PUBLIC && it != PROTECTED && it != PRIVATE && it != STATIC && it != FINAL && it != ABSTRACT)
						.map(Modifier::toString).collect(Collectors.joining(SPACE));
				this.stringRepresentation.append(str);
				if (!str.isEmpty()) {
					this.stringRepresentation.append(SPACE);
				}
			}

			private String addTypeModifier(String s) {
				this.stringRepresentation.append(s);
				if (trailingSpace) {
					if (this.stringRepresentation.lastIndexOf(SPACE) == this.stringRepresentation.length() - 1) {
						return this.stringRepresentation.toString();
					}
					return this.stringRepresentation.append(SPACE).toString();
				}
				return this.stringRepresentation.toString().trim();
			}

			private String getSarlSpecificInterfaceKeyword(TypeElement type) {
				final var tm = type.asType();
				if (ElementUtilsImpl.this.typeUtils.isAssignable(tm, getCapacityType())) {
					return getSARLGrammarKeywordAccess().getCapacityKeyword();
				}
				return null;
			}

			private String getSarlSpecificClassKeyword(TypeElement type) {
				final var tm = type.asType();
				if (ElementUtilsImpl.this.typeUtils.isAssignable(tm, getAgentType())) {
					return getSARLGrammarKeywordAccess().getAgentKeyword();
				}
				if (ElementUtilsImpl.this.typeUtils.isAssignable(tm, getEventType())) {
					return getSARLGrammarKeywordAccess().getEventKeyword();
				}
				if (ElementUtilsImpl.this.typeUtils.isAssignable(tm, getBehaviorType())) {
					return getSARLGrammarKeywordAccess().getBehaviorKeyword();
				}
				if (ElementUtilsImpl.this.typeUtils.isAssignable(tm, getSkillType())) {
					return getSARLGrammarKeywordAccess().getSkillKeyword();
				}
				return null;
			}

			private void addModifiersInStandardOrder(SortedSet<Modifier> param, ElementKind elementType) {
				addVisibilityModifier(param);
				addStaticModifier(param);
				addAbstractModifier(param, elementType);
				addFinalModifier(param);
				addVarValModifier(param);
				addOtherModifiers(param);
			}

			@Override
			public String visitTypeAsInterface(TypeElement elt, SortedSet<Modifier> param) {
				addModifiersInStandardOrder(param, elt.getKind());
				final var sarlKw = getSarlSpecificInterfaceKeyword(elt);
				if (!Strings.isEmpty(sarlKw)) {
					return addTypeModifier(sarlKw);
				}
				return addTypeModifier(getSARLGrammarKeywordAccess().getInterfaceKeyword());
			}

			@Override
			public String visitTypeAsEnum(TypeElement elt, SortedSet<Modifier> param) {
				addModifiersInStandardOrder(param, elt.getKind());
				return addTypeModifier(getSARLGrammarKeywordAccess().getEnumKeyword());
			}

			@Override
			public String visitTypeAsAnnotationType(TypeElement elt, SortedSet<Modifier> param) {
				addModifiersInStandardOrder(param, elt.getKind());
				return addTypeModifier(getSARLGrammarKeywordAccess().getAnnotationKeyword());
			}

			@Override
			public String visitTypeAsClass(TypeElement elt, SortedSet<Modifier> param) {
				addModifiersInStandardOrder(param, elt.getKind());
				final var sarlKw = getSarlSpecificClassKeyword(elt);
				if (!Strings.isEmpty(sarlKw)) {
					return addTypeModifier(sarlKw);
				}
				return addTypeModifier(getSARLGrammarKeywordAccess().getClassKeyword());
			}

			@Override
			public String visitTypeAsRecord(TypeElement elt, SortedSet<Modifier> param) {
				addModifiersInStandardOrder(param, elt.getKind());
				final var sarlKw = getSarlSpecificClassKeyword(elt);
				if (!Strings.isEmpty(sarlKw)) {
					return addTypeModifier(sarlKw);
				}
				//TODO: Record is not part of the SARL syntax yet
				return addTypeModifier("record"); //$NON-NLS-1$
			}

			@Override
			protected String defaultAction(Element elt, SortedSet<Modifier> param) {
				addModifiersInStandardOrder(param, elt.getKind());
				return this.stringRepresentation.toString().trim();
			}

		}.visit(element, modifierList);
	}

	@Override
	public String getFullyQualifiedName(Element e, final boolean outer) {
		return new SimpleElementVisitor9<String, Void>() {
			@Override
			public String visitModule(ModuleElement e, Void p) {
				return e.getQualifiedName().toString();
			}

			@Override
			public String visitPackage(PackageElement e, Void p) {
				return e.getQualifiedName().toString();
			}

			@Override
			public String visitType(TypeElement e, Void p) {
				return e.getQualifiedName().toString();
			}

			@Override
			protected String defaultAction(Element e, Void p) {
				if (outer) {
					final var outerName = visit(e.getEnclosingElement());
					if (!Strings.isEmpty(outerName)) {
						return outerName + "." + e.getSimpleName().toString(); //$NON-NLS-1$
					}
				}
				return e.getSimpleName().toString();
			}
		}.visit(e);
	}

	@Override
	public String getLocalIdentifier(Element e) {
		return new SimpleElementVisitor9<String, Void>() {
			@Override
			public String visitExecutable(ExecutableElement e, Void p) {
				final var basename = new StringBuilder();
				basename.append(e.getSimpleName().toString());
				basename.append("("); //$NON-NLS-1$
				var first = true;
				for (final var parameter : e.getParameters()) {
					if (first) {
						first = false;
					} else {
						basename.append(","); //$NON-NLS-1$
					}
					final var typeMirror = parameter.asType();
					final var type = asTypeElement(typeMirror, ElementUtilsImpl.this.typeUtils);
					if (type != null) {
						basename.append(type.getQualifiedName().toString());
					} else {
						basename.append(typeMirror.toString());
					}
				}
				basename.append(")"); //$NON-NLS-1$
				return basename.toString();
			}

			@Override
			public String visitRecordComponent(RecordComponentElement elt, Void param) {
				return e.getSimpleName().toString();
			}
			
			@Override
			protected String defaultAction(Element e, Void p) {
				return e.getSimpleName().toString();
			}
		}.visit(e);
	}

	@Override
	public String getFullIdentifier(TypeMirror type) {
		return new AbstractTypeVisitor9<String, Void>() {
			@Override
			public String visitPrimitive(PrimitiveType t, Void p) {
				return t.toString();
			}

			@Override
			public String visitNull(NullType t, Void p) {
				return ""; //$NON-NLS-1$
			}

			@Override
			public String visitArray(ArrayType t, Void p) {
				return visit(t.getComponentType()) + "[]"; //$NON-NLS-1$
			}

			@Override
			public String visitDeclared(DeclaredType t, Void p) {
				return getFullIdentifier(t.asElement());
			}

			@Override
			public String visitError(ErrorType t, Void p) {
				return ""; //$NON-NLS-1$
			}

			@Override
			public String visitTypeVariable(TypeVariable t, Void p) {
				var tp = t.getUpperBound();
				if (tp == null) {
					tp = t.getLowerBound();
				}
				if (tp == null || tp.getKind() == TypeKind.NULL || tp.getKind() == TypeKind.NONE) {
					// Assume Object type
					return Object.class.getName();
				}
				return visit(tp);
			}

			@Override
			public String visitWildcard(WildcardType t, Void p) {
				return ""; //$NON-NLS-1$
			}

			@Override
			public String visitExecutable(ExecutableType t, Void p) {
				return ""; //$NON-NLS-1$
			}

			@Override
			public String visitNoType(NoType t, Void p) {
				return ""; //$NON-NLS-1$
			}

			@Override
			public String visitIntersection(IntersectionType t, Void p) {
				return ""; //$NON-NLS-1$
			}

			@Override
			public String visitUnion(UnionType t, Void p) {
				return ""; //$NON-NLS-1$
			}
		}.visit(type);
	}

	@Override
	public String getFullIdentifier(Element e) {
		return new SimpleElementVisitor9<String, Void>() {
			@Override
			public String visitModule(ModuleElement e, Void p) {
				return e.getQualifiedName().toString();
			}

			@Override
			public String visitPackage(PackageElement e, Void p) {
				return e.getQualifiedName().toString();
			}

			@Override
			public String visitType(TypeElement e, Void p) {
				return e.getQualifiedName().toString();
			}

			@Override
			public String visitExecutable(ExecutableElement e, Void p) {
				final var basename = new StringBuilder();
				basename.append(e.getSimpleName().toString());
				basename.append("("); //$NON-NLS-1$
				var first = true;
				for (final var parameter : e.getParameters()) {
					if (first) {
						first = false;
					} else {
						basename.append(","); //$NON-NLS-1$
					}
					final var typeMirror = parameter.asType();
					final var type = asTypeElement(typeMirror, ElementUtilsImpl.this.typeUtils);
					if (type != null) {
						basename.append(type.getQualifiedName().toString());
					} else {
						basename.append(typeMirror.toString());
					}
				}
				basename.append(")"); //$NON-NLS-1$
				final var outerId = visit(e.getEnclosingElement());
				if (!Strings.isEmpty(outerId)) {
					return outerId + "." + basename.toString(); //$NON-NLS-1$
				}
				return basename.toString();
			}

			@Override
			protected String defaultAction(Element e, Void p) {
				final var basename = e.getSimpleName().toString();
				final var outerId = visit(e.getEnclosingElement());
				if (!Strings.isEmpty(outerId)) {
					return outerId + "." + basename; //$NON-NLS-1$
				}
				return basename;
			}
		}.visit(e);
	}

	@Override
	public String getInnerTypeQualifiedName(Element e) {
		return new SimpleElementVisitor9<String, Void>() {
			@Override
			public String visitModule(ModuleElement e, Void p) {
				return e.getSimpleName().toString();
			}

			@Override
			public String visitPackage(PackageElement e, Void p) {
				return e.getSimpleName().toString();
			}

			@Override
			public String visitType(TypeElement e, Void p) {
				return build(e);
			}

			@Override
			protected String defaultAction(Element e, Void p) {
				return build(e);
			}

			private String build(Element e) {
				final var enclosing = e.getEnclosingElement();
				if (enclosing != null) {
					return enclosing.getSimpleName().toString() + "." + e.getSimpleName().toString(); //$NON-NLS-1$
				}
				return e.getSimpleName().toString();
			}
		}.visit(e);
	}

	@Override
	public boolean isDocumentedAnnotation(TypeElement annotation) {
		for (final var anno : annotation.getAnnotationMirrors()) {
			if (Documented.class.getName().equals(getFullyQualifiedName(anno.getAnnotationType().asElement(), true))) {
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean isSynthetized(AnnotationMirror element) {
		try {
			final var method = getClass().getMethod("isSynthetized"); //$NON-NLS-1$
			final var value = method.invoke(element);
			if (value instanceof Boolean cvalue) {
				return cvalue.booleanValue();
			}
		} catch (Throwable ex) {
			//
		}
		return false;
	}

	@Override
	public boolean isAnnotationArray(Map<? extends ExecutableElement, ? extends AnnotationValue> annotationValues) {
		for (final AnnotationValue annotationValue : annotationValues.values()) {
			var rvalue = new SimpleAnnotationValueVisitor9<Boolean, Void>() {
				@Override
				public Boolean visitArray(List<? extends AnnotationValue> vals, Void p) {
					if (vals.size() > 1) {
						if (vals.get(0) instanceof AnnotationMirror) {
							return new SimpleAnnotationValueVisitor9<Boolean, Void>() {
								@Override
								public Boolean visitAnnotation(AnnotationMirror a, Void p) {
									return Boolean.TRUE;
								}
								@Override
								protected Boolean defaultAction(Object o, Void p) {
									return Boolean.FALSE;
								}
							}.visit(vals.get(0));
						}
					}
					return Boolean.FALSE;
				}

				@Override
				protected Boolean defaultAction(Object o, Void p) {
					return Boolean.FALSE;
				}
			}.visit(annotationValue).booleanValue();
			if (rvalue) {
				return true;
			}
		}
		return false;
	}

	@Override
	public String getDimension(TypeMirror type) {
		return new SimpleTypeVisitor9<String, Void>() {
			final StringBuilder dimension = new StringBuilder(""); //$NON-NLS-1$
			@Override
			public String visitArray(ArrayType t, Void p) {
				this.dimension.append("[]"); //$NON-NLS-1$
				return visit(t.getComponentType());
			}

			@Override
			protected String defaultAction(TypeMirror e, Void p) {
				return this.dimension.toString();
			}

		}.visit(type);
	}

	private AnnotationMirror getDeprecatedAnnotation(Element element) {
		final var annotationList = element.getAnnotationMirrors();
		for (final var annotation : annotationList) {
			final var annotationType = annotation.getAnnotationType();
			final var qn = getFullyQualifiedName(annotationType.asElement(), true);
			if (Strings.equal(qn, Deprecated.class.getName())) {
				return annotation;
			}
		}
		return null;
	}

	@Override
	public boolean isDeprecated(Element element) {
		final var annotation = getDeprecatedAnnotation(element);
		return annotation != null;
	}

	@Override
	public boolean isDeprecatedForRemoval(Element element) {
		final var annotation = getDeprecatedAnnotation(element);
		if (annotation != null) {
			final var pairs = annotation.getElementValues();
			if (pairs != null) {
				for (final var elementEntry : pairs.entrySet()) {
					if (elementEntry.getKey() != null && elementEntry.getValue() != null) {
						if (elementEntry.getKey().getSimpleName().contentEquals("forRemoval")) { //$NON-NLS-1$
							final var value = elementEntry.getValue();
							if (value != null) {
								return Boolean.parseBoolean(value.toString());
							}
						}
					}
				}
			}
		}
		return false;
	}

	@Override
	public String getDeprecatedSince(Element element) {
		final var annotation = getDeprecatedAnnotation(element);
		if (annotation != null) {
			final var pairs = annotation.getElementValues();
			if (pairs != null) {
				for (final var elementEntry : pairs.entrySet()) {
					if (elementEntry.getKey() != null && elementEntry.getValue() != null) {
						if (elementEntry.getKey().getSimpleName().contentEquals("since")) { //$NON-NLS-1$
							final var value = elementEntry.getValue();
							if (value != null) {
								final var rvalue = value.getValue();
								if (rvalue != null) {
									final var str = rvalue.toString();
									if (str.isEmpty()) {
										return null;
									}
									return str;
								}
							}
						}
					}
				}
			}
		}
		return null;
	}

	@Override
	public Element getFirstEnclosingDeprecatedElement(Element element) {
		var current = element;
		do {
			if (this.elementUtils.isDeprecated(current)) {
				return current;
			}
			current = current.getEnclosingElement();
		} while (current != null);
		return null;
	}

	@Override
	public boolean isExecutableElement(Element element) {
		final var kind = element.getKind();
		switch (kind) {
		case CONSTRUCTOR:
		case METHOD:
		case INSTANCE_INIT:
			return true;
		default:
			break;
		}
		return false;
	}

	@Override
	public boolean isVariableElement(Element element) {
		final var kind = element.getKind();
		switch (kind) {
		case ENUM_CONSTANT:
		case EXCEPTION_PARAMETER:
		case FIELD:
		case LOCAL_VARIABLE:
		case PARAMETER:
		case RESOURCE_VARIABLE:
			return true;
		default:
			//
		}
		return false;
	}

	@Override
	public boolean isPackage(Element element) {
		final var kind = element.getKind();
		return kind == ElementKind.PACKAGE;
	}

	@Override
	public boolean isConstructor(Element element) {
		final var kind = element.getKind();
		return kind == ElementKind.CONSTRUCTOR;
	}

	@Override
	public TypeElement getEnclosingTypeElement(Element executable) {
		var elt = executable.getEnclosingElement();
		while (elt != null && !(elt instanceof TypeElement)) {
			elt = elt.getEnclosingElement();
		}
		if (elt instanceof TypeElement cvalue) {
			return cvalue;
		}
		return null;
	}

	@Override
	public boolean isStatic(Element element) {
		return element.getModifiers().contains(Modifier.STATIC);
	}

	@Override
	public boolean isFinal(Element element) {
		return element.getModifiers().contains(Modifier.FINAL);
	}

	@Override
	public boolean isAbstract(Element element) {
		return element.getModifiers().contains(Modifier.ABSTRACT);
	}

	@Override
	public boolean isTypeElement(Element element) {
		switch (element.getKind()) {
		case CLASS:
		case ENUM:
		case INTERFACE:
		case ANNOTATION_TYPE:
			return true;
		default:
			//
		}
		return false;
	}

	@Override
	public boolean isPrivateAPI(Element element) {
		final var annotationList = element.getAnnotationMirrors();
		for (final var annotation : annotationList) {
			final var annotationType = annotation.getAnnotationType();
			final var qn = getFullyQualifiedName(annotationType.asElement(), true);
			if (Strings.equal(qn, PrivateAPI.class.getName())) {
				final var pairs = annotation.getElementValues();
				for (final var valueEntry : pairs.entrySet()) {
					if (valueEntry.getKey() != null && valueEntry.getKey().getSimpleName().contentEquals("isCallerOnly")) { //$NON-NLS-1$
						final var pvalue = valueEntry.getValue();
						if (pvalue != null) {
							return ! Boolean.parseBoolean(pvalue.toString());
						}
					}
				}
				return true;
			}
		}
		return false;
	}

	@Override
	public TypeElement getReferencedClass(DocTree dtree, TypeMirror currentType, QualifiedNameSetBuilder findQualifiedNames) {
		final var e = getReferencedElement(dtree, currentType, findQualifiedNames);
		if (e == null) {
			return null;
		} else if (isTypeElement(e)) {
			return (TypeElement) e;
		} else if (!isPackage(e)) {
			return getEnclosingTypeElement(e);
		}
		return null;
	}

	@Override
	public Element getReferencedMember(DocTree dtree, TypeMirror currentType, QualifiedNameSetBuilder findQualifiedNames) {
		final var e = getReferencedElement(dtree, currentType, findQualifiedNames);
		if (e == null) {
			return null;
		}
		return (isExecutableElement(e) || isVariableElement(e)) ? e : null;
	}

	@Override
	public Element getReferencedElement(DocTree documentation, TypeMirror currentType, QualifiedNameSetBuilder findQualifiedNames) {
		return new SimpleDocTreeVisitor<Element, Void>() {
			@Override
			public Element visitSee(SeeTree node, Void p) {
				for (final var dt : node.getReference()) {
					return visit(dt, null);
				}
				return null;
			}

			@Override
			public Element visitLink(LinkTree node, Void p) {
				return visit(node.getReference(), null);
			}

			@Override
			public Element visitProvides(ProvidesTree node, Void p) {
				return visit(node.getServiceType(), null);
			}

			@Override
			public Element visitValue(ValueTree node, Void p) {
				return visit(node.getReference(), null);
			}

			@Override
			public Element visitReference(ReferenceTree node, Void p) {
				return getElement(node, currentType, findQualifiedNames);
			}

			@Override
			public Element visitSerialField(SerialFieldTree node, Void p) {
				return visit(node.getType(), null);
			}

			@Override
			public Element visitUses(UsesTree node, Void p) {
				return visit(node.getServiceType(), null);
			}

			@Override
			protected Element defaultAction(DocTree node, Void p) {
				return null;
			}
		}.visit(documentation, null);
	}

	/** Replies the Java element that is referenced by the given tree.
	 *
	 * @param rtree the reference tree.
	 * @param currentType is the current type that is used by default.
	 * @param findQualifiedNames a function invokes to retrieve the candidates for qualified names. The argument of the function is
	 *      the signature of a type to be found (it is the {@code signature}. The function returns a collection of qualified name
	 *      candidates.
	 * @return the Java element, or {@code null} if none.
	 */
	protected Element getElement(ReferenceTree rtree, TypeMirror currentType, QualifiedNameSetBuilder findQualifiedNames) {
		final var signature = rtree.getSignature();
		final var componentIndex = signature.indexOf('#');
		final String typeName;
		final String componentName;
		if (componentIndex >= 0) {
			typeName = signature.substring(0, componentIndex);
			componentName = signature.substring(componentIndex + 1);
		} else {
			typeName = signature;
			componentName = null;
		}

		final TypeMirror typeMirror;
		if (!Strings.isEmpty(typeName)) {
			typeMirror = getSymbol(typeName, findQualifiedNames);
		} else {
			typeMirror = currentType;
		}

		if (typeMirror != null) {
			final var element = this.typeUtils.asElement(typeMirror);
			if (element instanceof TypeElement && !Strings.isEmpty(componentName)) {
				final var matcher = MEMBER_NAME_PATTERN.matcher(componentName);
				final String sname;
				final String[] params;
				if (matcher.matches()) {
					sname = matcher.group(1);
					final var p = matcher.group(2);
					if (!Strings.isEmpty(p)) {
						params = p.split("\\s*,\\s*"); //$NON-NLS-1$
					} else {
						params = null;
					}
				} else {
					sname = componentName;
					params = null;
				}
				assert sname != null;
				if (sname.equals(element.getSimpleName().toString())) {
					// It is a constructor reference.
					for (final var member : element.getEnclosedElements()) {
						if (isConstructor(member)) {
							final var executableElement = (ExecutableElement) member;
							if (params == null || params.length == 0) {
								if (executableElement.getParameters().size() == 0) {
									return executableElement;
								}
							} else if (areSameParams(params, executableElement.getParameters())) {
								return member;
							}
						}
					}
				} else {
					// Explore the methods in the local and super types.
					final var treated = new TreeSet<>(getTypeMirrorComparator());
					final var candidates = new LinkedList<TypeMirror>();
					candidates.addLast(element.asType());
					while (!candidates.isEmpty()) {
						final var m0 = candidates.removeFirst();
						if (treated.add(m0)) {
							final var m1 = asTypeElement(m0, this.typeUtils);
							if (m1 != null) {
								for (final var member : m1.getEnclosedElements()) {
									if (Strings.equal(member.getSimpleName().toString(), sname)) {
										if (isExecutableElement(member)) {
											final var executableElement = (ExecutableElement) member;
											if (params == null || params.length == 0) {
												if (executableElement.getParameters().size() == 0) {
													return executableElement;
												}
											} else if (areSameParams(params, executableElement.getParameters())) {
												return member;
											}
										} else if (isVariableElement(member)) {
											return member;
										}
									}
								}
								candidates.add(m1.getSuperclass());
								candidates.addAll(m1.getInterfaces());
							}
						}
					}
				}
				return null;
			}
			return element;
		}

		return null;
	}

	private static boolean isSameTypename(String qualifiedName, String name) {
		final String n0;
		if (qualifiedName.endsWith("...")) { //$NON-NLS-1$
			n0 = qualifiedName.replaceFirst("\\.\\.\\.$", "[]"); //$NON-NLS-1$ //$NON-NLS-2$
		} else {
			n0 = qualifiedName;
		}
		final String n1;
		if (name.endsWith("...")) { //$NON-NLS-1$
			n1 = name.replaceFirst("\\.\\.\\.$", "[]"); //$NON-NLS-1$ //$NON-NLS-2$
		} else {
			n1 = name;
		}
		if (Strings.equal(n0, n1) || n0.endsWith("." + n1)) { //$NON-NLS-1$
			return true;
		}
		return false;
	}

	private boolean areSameParams(String[] expected, List<? extends VariableElement> actual) {
		if (expected.length == actual.size()) {
			for (var i = 0; i < expected.length; ++i) {
				final var paramType = actual.get(i).asType();
				final var atype = getFullIdentifier(paramType);
				final var etype = expected[i];
				if (!isSameTypename(atype, etype)) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	@Override
	public <T extends ExecutableElement> Comparator<? super T> getExecutableElementComparator() {
		return this.executableComparator;
	}

	@Override
	public <T extends ModuleElement> Comparator<? super T> getModuleElementComparator() {
		return this.moduleElementComparator;
	}

	@Override
	public <T extends PackageElement> Comparator<? super T> getPackageElementComparator() {
		return this.packageElementComparator;
	}

	@Override
	public <T extends TypeElement> Comparator<? super T> getTypeElementComparator() {
		return this.typeElementComparator;
	}

	@Override
	public <T extends TypeElement> Comparator<? super T> getTypeElementBasenameComparator() {
		return this.typeElementBasenameComparator;
	}

	@Override
	public <T extends TypeMirror> Comparator<? super T> getTypeMirrorComparator() {
		return this.typeMirrorComparator;
	}

	@Override
	public <T extends VariableElement> Comparator<? super T> getVariableElementComparator() {
		return this.variableComparator;
	}

	@Override
	public boolean isEventHandlerContainer(TypeElement element) {
		if (element != null && element.getKind() == ElementKind.CLASS) {
			final var tm = element.asType();
			if (tm != null) {
				return this.typeUtils.isAssignable(tm, getAgentType())
						|| this.typeUtils.isAssignable(tm, getSkillType())
						|| this.typeUtils.isAssignable(tm, getBehaviorType());
			}
		}
		return false;
	}

	@Override
	public boolean isCapacityUser(TypeElement element) {
		if (element != null && element.getKind() == ElementKind.CLASS) {
			final var tm = element.asType();
			if (tm != null) {
				return this.typeUtils.isAssignable(tm, getAgentType())
						|| this.typeUtils.isAssignable(tm, getSkillType())
						|| this.typeUtils.isAssignable(tm, getBehaviorType());
			}
		}
		return false;
	}

	@Override
	public boolean isSarlAgent(TypeElement type) {
		if (type != null && type.getKind() == ElementKind.CLASS) {
			final var tm = type.asType();
			if (tm != null) {
				return this.typeUtils.isAssignable(tm, getAgentType());
			}
		}
		return false;
	}

	@Override
	public boolean isSarlBehavior(TypeElement type) {
		if (type != null && type.getKind() == ElementKind.CLASS) {
			final var tm = type.asType();
			if (tm != null) {
				return this.typeUtils.isAssignable(tm, getBehaviorType());
			}
		}
		return false;
	}

	@Override
	public boolean isSarlCapacity(TypeElement type) {
		if (type != null && type.getKind() == ElementKind.INTERFACE) {
			final var tm = type.asType();
			if (tm != null) {
				return this.typeUtils.isAssignable(tm, getCapacityType());
			}
		}
		return false;
	}

	@Override
	public boolean isSarlSkill(TypeElement type) {
		if (type != null && type.getKind() == ElementKind.CLASS) {
			final var tm = type.asType();
			if (tm != null) {
				return this.typeUtils.isAssignable(tm, getSkillType());
			}
		}
		return false;
	}

	@Override
	public boolean isSarlEvent(TypeElement type) {
		if (type != null && type.getKind() == ElementKind.CLASS) {
			final var tm = type.asType();
			if (tm != null) {
				return this.typeUtils.isAssignable(tm, getEventType());
			}
		}
		return false;
	}

}
