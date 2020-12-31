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

package io.sarl.lang.typesystem;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.util.Utils;

/**
 * Utilities for managing inheritance of JVM and SARL elements.
 *
 * <p>This class was introduced for computing the inheritance with JVM elements (with the
 * {@link LightweightTypeReference} API) and with SARL elements, when the inferrer has not
 * yet generated the JVM elements for the SARL elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class InheritanceHelper {

	@Inject
	private CommonTypeComputationServices services;

	@Inject
	private SarlJvmModelAssociations sarlAssociations;

	@Inject
	private SARLAnnotationUtil annotationUtil;

	/** Replies if the type candidate is a subtype of the given super type.
	 *
	 * @param candidate the type to test.
	 * @param jvmSuperType the expected JVM super-type.
	 * @param sarlSuperType the expected SARL super-type.
	 * @return <code>true</code> if the candidate is a sub-type of the super-type.
	 */
	public boolean isSubTypeOf(JvmTypeReference candidate, Class<?> jvmSuperType,
			Class<? extends XtendTypeDeclaration> sarlSuperType) {
		final LightweightTypeReference reference = Utils.toLightweightTypeReference(candidate, this.services);
		return isSubTypeOf(reference, jvmSuperType, sarlSuperType);
	}

	/** Replies if the type candidate is a subtype of the given super type.
	 *
	 * @param candidate the type to test.
	 * @param jvmSuperType the expected JVM super-type.
	 * @param sarlSuperType the expected SARL super-type.
	 * @param onlyInterface <code>true</code> if only interface types are matching; <code>false</code> if
	 *     not-interface types are matching.
	 * @return <code>true</code> if the candidate is a sub-type of the super-type.
	 */
	public boolean isSubTypeOf(JvmTypeReference candidate, Class<?> jvmSuperType,
			Class<? extends XtendTypeDeclaration> sarlSuperType, boolean onlyInterface) {
		// Test the subtyping between the JVM elements.
		final LightweightTypeReference reference = Utils.toLightweightTypeReference(candidate, this.services);
		if (reference.isInterfaceType() != onlyInterface) {
			return false;
		}
		return isSubTypeOf(reference, jvmSuperType, sarlSuperType);
	}

	/** Replies if the type candidate is a subtype of the given super type.
	 *
	 * @param candidate the type to test.
	 * @param jvmSuperType the expected JVM super-type.
	 * @param sarlSuperType the expected SARL super-type.
	 * @return <code>true</code> if the candidate is a sub-type of the super-type.
	 */
	public boolean isSubTypeOf(JvmType candidate, Class<?> jvmSuperType,
			Class<? extends XtendTypeDeclaration> sarlSuperType) {
		final LightweightTypeReference reference = Utils.toLightweightTypeReference(candidate, this.services);
		return isSubTypeOf(reference, jvmSuperType, sarlSuperType);
	}

	/** Replies if the type candidate is a subtype of the given super type.
	 *
	 * @param candidate the type to test.
	 * @param jvmSuperType the expected JVM super-type.
	 * @param sarlSuperType the expected SARL super-type.
	 * @param onlyInterface <code>true</code> if only interface types are matching; <code>false</code> if
	 *     not-interface types are matching.
	 * @return <code>true</code> if the candidate is a sub-type of the super-type.
	 */
	public boolean isSubTypeOf(JvmType candidate, Class<?> jvmSuperType,
			Class<? extends XtendTypeDeclaration> sarlSuperType, boolean onlyInterface) {
		// Test the subtyping between the JVM elements.
		final LightweightTypeReference reference = Utils.toLightweightTypeReference(candidate, this.services);
		if (reference.isInterfaceType() != onlyInterface) {
			return false;
		}
		return isSubTypeOf(reference, jvmSuperType, sarlSuperType);
	}

	/** Replies if the type candidate is a subtype of the given super type.
	 *
	 * @param candidate the type to test.
	 * @param jvmSuperType the expected JVM super-type.
	 * @param sarlSuperType the expected SARL super-type.
	 * @return <code>true</code> if the candidate is a sub-type of the super-type.
	 */
	public boolean isSubTypeOf(LightweightTypeReference candidate, Class<?> jvmSuperType,
			Class<? extends XtendTypeDeclaration> sarlSuperType) {
		if (candidate.isSubtypeOf(jvmSuperType)) {
			return true;
		}
		if (sarlSuperType != null) {
			final JvmType type = candidate.getType();
			if (type instanceof JvmGenericType) {
				final JvmGenericType genType = (JvmGenericType) type;
				if (genType.getSuperTypes().isEmpty()) {
					for (final EObject sarlObject : this.sarlAssociations.getSourceElements(type)) {
						if (sarlSuperType.isInstance(sarlObject)) {
							return true;
						}
					}
				}
			}
		}
		return false;
	}

	/** Replies if the type candidate is a subtype of the given super type.
	 *
	 * @param candidate the type to test.
	 * @param jvmSuperType the expected JVM super-type.
	 * @param sarlSuperType the expected SARL super-type.
	 * @return <code>true</code> if the candidate is a sub-type of the super-type.
	 */
	public boolean isProxyOrSubTypeOf(JvmTypeReference candidate, Class<?> jvmSuperType,
			Class<? extends XtendTypeDeclaration> sarlSuperType) {
		final LightweightTypeReference reference = Utils.toLightweightTypeReference(candidate, this.services);
		return isSubTypeOf(reference, jvmSuperType, sarlSuperType);
	}

	/** Replies if the type candidate is a proxy (unresolved type) or a subtype of the given super type.
	 *
	 * @param candidate the type to test.
	 * @param jvmSuperType the expected JVM super-type.
	 * @param sarlSuperType the expected SARL super-type.
	 * @param onlyInterface <code>true</code> if only interface types are matching; <code>false</code> if
	 *     not-interface types are matching.
	 * @return <code>true</code> if the candidate is a sub-type of the super-type.
	 */
	public boolean isProxyOrSubTypeOf(JvmTypeReference candidate, Class<?> jvmSuperType,
			Class<? extends XtendTypeDeclaration> sarlSuperType, boolean onlyInterface) {
		if (candidate.eIsProxy()) {
			return true;
		}
		return isSubTypeOf(candidate, jvmSuperType, sarlSuperType, onlyInterface);
	}

	/** Replies if the type candidate is a proxy (unresolved type) or a subtype of the given super type.
	 *
	 * @param candidate the type to test.
	 * @param jvmSuperType the expected JVM super-type.
	 * @param sarlSuperType the expected SARL super-type.
	 * @return <code>true</code> if the candidate is a sub-type of the super-type.
	 */
	public boolean isProxyOrSubTypeOf(LightweightTypeReference candidate, Class<?> jvmSuperType,
			Class<? extends XtendTypeDeclaration> sarlSuperType) {
		if (!candidate.isResolved()) {
			return true;
		}
		return isSubTypeOf(candidate, jvmSuperType, sarlSuperType);
	}

	/** Extract and replies the Ecore type, provided by {@link SarlPackage} for the given JvmElement.
	 *
	 * @param type the JVM type to test.
	 * @return the code of the type, see {@link SarlPackage}; or {@code 0} if the code is unknown.
	 * @since 0.6
	 */
	public int getSarlElementEcoreType(JvmGenericType type) {
		final JvmAnnotationReference annotationRef = this.annotationUtil.findAnnotation(type, SarlElementType.class.getName());
		if (annotationRef != null) {
			final Integer intValue = this.annotationUtil.findIntValue(annotationRef);
			if (intValue != null) {
				return intValue.intValue();
			}
		}
		return 0;
	}

	/** Extract and replies the Ecore type, provided by {@link SarlPackage} for the given JvmElement.
	 *
	 * @param type the JVM type to test.
	 * @return the code of the type, see {@link SarlPackage}; or {@code 0} if the code is unknown.
	 * @since 0.8
	 */
	public int getSarlElementEcoreType(LightweightTypeReference type) {
		final JvmType jvmType = type.getType();
		if (jvmType instanceof JvmGenericType) {
			return getSarlElementEcoreType((JvmGenericType) jvmType);
		}
		return 0;
	}

	/** Replies if the given JVM element is a SARL agent.
	 *
	 * @param type the JVM type to test.
	 * @return {@code true} if the given type is a SARL agent, or not.
	 * @since 0.6
	 */
	public boolean isSarlAgent(JvmGenericType type) {
		return !type.isInterface() && getSarlElementEcoreType(type) == SarlPackage.SARL_AGENT;
	}

	/** Replies if the given JVM element is a SARL agent.
	 *
	 * @param type the JVM type to test.
	 * @return {@code true} if the given type is a SARL agent, or not.
	 * @since 0.8
	 */
	public boolean isSarlAgent(LightweightTypeReference type) {
		return !type.isInterfaceType() && (getSarlElementEcoreType(type) == SarlPackage.SARL_AGENT
				|| type.isSubtypeOf(Agent.class));
	}

	/** Replies if the given JVM element is a SARL behavior.
	 *
	 * @param type the JVM type to test.
	 * @return {@code true} if the given type is a SARL behavior, or not.
	 * @since 0.6
	 */
	public boolean isSarlBehavior(JvmGenericType type) {
		return !type.isInterface() && getSarlElementEcoreType(type) == SarlPackage.SARL_BEHAVIOR;
	}

	/** Replies if the given JVM element is a SARL behavior.
	 *
	 * @param type the JVM type to test.
	 * @return {@code true} if the given type is a SARL behavior, or not.
	 * @since 0.8
	 */
	public boolean isSarlBehavior(LightweightTypeReference type) {
		return !type.isInterfaceType() && (getSarlElementEcoreType(type) == SarlPackage.SARL_BEHAVIOR
				|| type.isSubtypeOf(Behavior.class));
	}

	/** Replies if the given JVM element is a SARL capacity.
	 *
	 * @param type the JVM type to test.
	 * @return {@code true} if the given type is a SARL capacity, or not.
	 * @since 0.6
	 */
	public boolean isSarlCapacity(JvmGenericType type) {
		return type.isInterface() && getSarlElementEcoreType(type) == SarlPackage.SARL_CAPACITY;
	}

	/** Replies if the given JVM element is a SARL capacity.
	 *
	 * @param type the JVM type to test.
	 * @return {@code true} if the given type is a SARL capacity, or not.
	 * @since 0.8
	 */
	public boolean isSarlCapacity(LightweightTypeReference type) {
		return type.isInterfaceType() && (getSarlElementEcoreType(type) == SarlPackage.SARL_CAPACITY
				|| type.isSubtypeOf(Capacity.class));
	}

	/** Replies if the given JVM element is a SARL event.
	 *
	 * @param type the JVM type to test.
	 * @return {@code true} if the given type is a SARL event, or not.
	 * @since 0.6
	 */
	public boolean isSarlEvent(JvmGenericType type) {
		return !type.isInterface() && getSarlElementEcoreType(type) == SarlPackage.SARL_EVENT;
	}

	/** Replies if the given JVM element is a SARL event.
	 *
	 * @param type the JVM type to test.
	 * @return {@code true} if the given type is a SARL event, or not.
	 * @since 0.8
	 */
	public boolean isSarlEvent(LightweightTypeReference type) {
		return !type.isInterfaceType() && (getSarlElementEcoreType(type) == SarlPackage.SARL_EVENT
				|| type.isSubtypeOf(Event.class));
	}

	/** Replies if the given JVM element is a SARL skill.
	 *
	 * @param type the JVM type to test.
	 * @return {@code true} if the given type is a SARL skill, or not.
	 * @since 0.6
	 */
	public boolean isSarlSkill(JvmGenericType type) {
		return !type.isInterface() && getSarlElementEcoreType(type) == SarlPackage.SARL_SKILL;
	}

	/** Replies if the given JVM element is a SARL skill.
	 *
	 * @param type the JVM type to test.
	 * @return {@code true} if the given type is a SARL skill, or not.
	 * @since 0.8
	 */
	public boolean isSarlSkill(LightweightTypeReference type) {
		return !type.isInterfaceType() && (getSarlElementEcoreType(type) == SarlPackage.SARL_SKILL
				|| type.isSubtypeOf(Skill.class));
	}

}
