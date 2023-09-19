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

package io.sarl.docs.doclet2.html.types;

import javax.inject.Inject;
import javax.inject.Provider;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.Types;

import io.sarl.docs.doclet2.framework.ElementUtils;
import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.types.aop.AgentDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.aop.BehaviorDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.aop.CapacityDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.aop.EventDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.aop.SkillDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.oop.AnnotationDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.oop.ClassDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.oop.EnumerationDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.oop.InterfaceDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.oop.RecordDocumentationGenerator;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;

/** Tool that replies the best generator for type element.
 *
 * @author $Author: sgalland$
 * @version docs.doclet 0.13.0 20230919-093059
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public class SarlTypeDocumentationGeneratorSelector implements TypeDocumentationGeneratorSelector {

	private Provider<ClassDocumentationGenerator> classGeneratorProvider;

	private Provider<RecordDocumentationGenerator> recordGeneratorProvider;

	private Provider<InterfaceDocumentationGenerator> interfaceGeneratorProvider;

	private Provider<EnumerationDocumentationGenerator> enumerationGeneratorProvider;

	private Provider<AnnotationDocumentationGenerator> annotationGeneratorProvider;

	private Provider<AgentDocumentationGeneratorImpl> agentGeneratorProvider;

	private Provider<BehaviorDocumentationGeneratorImpl> behaviorGeneratorProvider;

	private Provider<SkillDocumentationGeneratorImpl> skillGeneratorProvider;

	private Provider<EventDocumentationGeneratorImpl> eventGeneratorProvider;

	private Provider<CapacityDocumentationGeneratorImpl> capacityGeneratorProvider;

	private ElementUtils elementUtils;

	/** Change the element utilities.
	 * 
	 * @param utils the element utilities.
	 */
	@Inject
	public void setElementUtils(ElementUtils utils) {
		this.elementUtils = utils;
	}

	/** Replies the element utilities.
	 * 
	 * @return the utilities.
	 */
	public ElementUtils getElementUtils() {
		return this.elementUtils;
	}

	/** Change the provider for a class generator.
	 * 
	 * @param provider is the provider.
	 */
	@Inject
	public void setClassGeneratorProvider(Provider<ClassDocumentationGenerator> provider) {
		this.classGeneratorProvider = provider;
	}

	/** Replies the provider for a class generator.
	 * 
	 * @return the provider.
	 */
	public Provider<ClassDocumentationGenerator> getClassGeneratorProvider() {
		return this.classGeneratorProvider;
	}

	/** Change the provider for a record generator.
	 * 
	 * @param provider is the provider.
	 */
	@Inject
	public void setRecordGeneratorProvider(Provider<RecordDocumentationGenerator> provider) {
		this.recordGeneratorProvider = provider;
	}

	/** Replies the provider for a record generator.
	 * 
	 * @return the provider.
	 */
	public Provider<RecordDocumentationGenerator> getRecordGeneratorProvider() {
		return this.recordGeneratorProvider;
	}

	/** Change the provider for an interface generator.
	 * 
	 * @param provider is the provider.
	 */
	@Inject
	public void setInterfaceGeneratorProvider(Provider<InterfaceDocumentationGenerator> provider) {
		this.interfaceGeneratorProvider = provider;
	}

	/** Replies the provider for an interface generator.
	 * 
	 * @return the provider.
	 */
	public Provider<InterfaceDocumentationGenerator> getInterfaceGeneratorProvider() {
		return this.interfaceGeneratorProvider;
	}

	/** Change the provider for an annotation generator.
	 * 
	 * @param provider is the provider.
	 */
	@Inject
	public void setAnnotationGeneratorProvider(Provider<AnnotationDocumentationGenerator> provider) {
		this.annotationGeneratorProvider = provider;
	}

	/** Replies the provider for an annotation generator.
	 * 
	 * @return the provider.
	 */
	public Provider<AnnotationDocumentationGenerator> getAnnotationGeneratorProvider() {
		return this.annotationGeneratorProvider;
	}

	/** Change the provider for an enumeration generator.
	 * 
	 * @param provider is the provider.
	 */
	@Inject
	public void setEnumerationGeneratorProvider(Provider<EnumerationDocumentationGenerator> provider) {
		this.enumerationGeneratorProvider = provider;
	}

	/** Replies the provider for an enumeration generator.
	 * 
	 * @return the provider.
	 */
	public Provider<EnumerationDocumentationGenerator> getEnumerationGeneratorProvider() {
		return this.enumerationGeneratorProvider;
	}

	/** Change the provider for an agent generator.
	 * 
	 * @param provider is the provider.
	 */
	@Inject
	public void setAgentGeneratorProvider(Provider<AgentDocumentationGeneratorImpl> provider) {
		this.agentGeneratorProvider = provider;
	}

	/** Replies the provider for an agent generator.
	 * 
	 * @return the provider.
	 */
	public Provider<AgentDocumentationGeneratorImpl> getAgentGeneratorProvider() {
		return this.agentGeneratorProvider;
	}

	/** Change the provider for a behavior generator.
	 * 
	 * @param provider is the provider.
	 */
	@Inject
	public void setBehaviorGeneratorProvider(Provider<BehaviorDocumentationGeneratorImpl> provider) {
		this.behaviorGeneratorProvider = provider;
	}

	/** Replies the provider for a behavior generator.
	 * 
	 * @return the provider.
	 */
	public Provider<BehaviorDocumentationGeneratorImpl> getBehaviorGeneratorProvider() {
		return this.behaviorGeneratorProvider;
	}

	/** Change the provider for a skill generator.
	 * 
	 * @param provider is the provider.
	 */
	@Inject
	public void setSkillGeneratorProvider(Provider<SkillDocumentationGeneratorImpl> provider) {
		this.skillGeneratorProvider = provider;
	}

	/** Replies the provider for a skill generator.
	 * 
	 * @return the provider.
	 */
	public Provider<SkillDocumentationGeneratorImpl> getSkillGeneratorProvider() {
		return this.skillGeneratorProvider;
	}

	/** Change the provider for an event generator.
	 * 
	 * @param provider is the provider.
	 */
	@Inject
	public void setEventGeneratorProvider(Provider<EventDocumentationGeneratorImpl> provider) {
		this.eventGeneratorProvider = provider;
	}

	/** Replies the provider for an event generator.
	 * 
	 * @return the provider.
	 */
	public Provider<EventDocumentationGeneratorImpl> getEventGeneratorProvider() {
		return this.eventGeneratorProvider;
	}

	/** Change the provider for a capacity generator.
	 * 
	 * @param provider is the provider.
	 */
	@Inject
	public void setCapacityGeneratorProvider(Provider<CapacityDocumentationGeneratorImpl> provider) {
		this.capacityGeneratorProvider = provider;
	}

	/** Replies the provider for a capacity generator.
	 * 
	 * @return the provider.
	 */
	public Provider<CapacityDocumentationGeneratorImpl> getCapacityGeneratorProvider() {
		return this.capacityGeneratorProvider;
	}

	/** Replies if the first type is a subtype of the second type but not same.
	 *
	 * @param environment the generation environment.
	 * @param t1 first type.
	 * @param t2 second type.
	 * @return {@code true} if {@code t1} is a subtype of {@code t2} and {@code t1} is not the same
	 *     as {@code t2}.
	 */
	@SuppressWarnings("static-method")
	protected boolean isSubtypeStrict(SarlDocletEnvironment environment, TypeMirror t1, TypeMirror t2) {
		final Types types = environment.getTypeUtils();
		return types.isSubtype(t1, t2) && !types.isSameType(t1, t2);
	}

	@Override
	public TypeDocumentationGenerator getTypeGeneratorFor(final TypeElement element, final SarlDocletEnvironment environment) {
		if (element != null) {
			final ElementKind kind = element.getKind();
			if (kind != null) {
				final TypeMirror elementType = element.asType();
				switch (kind) {
				case CLASS:
					final TypeMirror agentType = getElementUtils().getSymbol(Agent.class.getName());
					if (isSubtypeStrict(environment, elementType, agentType)) {
						return this.agentGeneratorProvider.get();
					}
					final TypeMirror eventType = getElementUtils().getSymbol(Event.class.getName());
					if (isSubtypeStrict(environment, elementType, eventType)) {
						return this.eventGeneratorProvider.get();
					}
					final TypeMirror skillType = getElementUtils().getSymbol(Skill.class.getName());
					if (isSubtypeStrict(environment, elementType, skillType)) {
						return this.skillGeneratorProvider.get();
					}
					final TypeMirror behaviorType = getElementUtils().getSymbol(Behavior.class.getName());
					if (isSubtypeStrict(environment, elementType, behaviorType)) {
						return this.behaviorGeneratorProvider.get();
					}
					return this.classGeneratorProvider.get();
				case INTERFACE:
					final TypeMirror capacityType = getElementUtils().getSymbol(Capacity.class.getName());
					if (isSubtypeStrict(environment, elementType, capacityType)) {
						return this.capacityGeneratorProvider.get();
					}
					return this.interfaceGeneratorProvider.get();
				case ENUM:
					return this.enumerationGeneratorProvider.get();
				case ANNOTATION_TYPE:
					return this.annotationGeneratorProvider.get();
				case RECORD:
					return this.recordGeneratorProvider.get();
				case LOCAL_VARIABLE:
				case METHOD:
				case MODULE:
				case OTHER:
				case PACKAGE:
				case PARAMETER:
				case RESOURCE_VARIABLE:
				case STATIC_INIT:
				case TYPE_PARAMETER:
				case ENUM_CONSTANT:
				case CONSTRUCTOR:
				case EXCEPTION_PARAMETER:
				case FIELD:
				case INSTANCE_INIT:
				case BINDING_VARIABLE:
				case RECORD_COMPONENT:
					break;
				default:
					break;
				}
			}
		}
		return null;
	}
	
}
