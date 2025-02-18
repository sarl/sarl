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
 */

package io.sarl.lang.validation;

import java.util.List;
import java.util.Map;

import com.google.inject.ImplementedBy;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.validation.IssueSeverities;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import io.sarl.lang.sarl.SarlEvent;

/**
 * Tools for implementing the validators.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
@ImplementedBy(SARLValidator.class)
public interface ISARLValidator {

	/** Replies the type computation services.
	 *
	 * @return the services.
	 */
	CommonTypeComputationServices getServices();

	/** Create a lightweight type reference from the given type.
	 *
	 * @param type the type to point to.
	 * @param context the context in which the reference is located.
	 * @return the reference.
	 */
	LightweightTypeReference toLightweightTypeReference(JvmType type, EObject context);

	/** Create a lightweight type reference from the given type.
	 *
	 * @param typeRef the type to point to.
	 * @return the reference.
	 */
	LightweightTypeReference toLightweightTypeReference(JvmTypeReference typeRef);

	/** Create a lightweight type reference from the given type.
	 *
	 * @param typeRef the type to point to.
	 * @param keepUnboundWildcardInformation indicates if the wildcard information is kept.
	 * @return the reference.
	 */
	LightweightTypeReference toLightweightTypeReference(JvmTypeReference typeRef, boolean keepUnboundWildcardInformation);

	/** Replies the reference to the inferred type of the given expression.
	 *
	 * @param expression the expression to evaluate
	 * @return the type of the given expression.
	 */
	LightweightTypeReference getActualType(XExpression expression);

	/** Replies if the issues with the given identifier should be ignored.
	 *
	 * @param issueCode the code of the issue.
	 * @return {@code true} if the issue should be ignored.
	 * @see #isIgnored(String, EObject)
	 */
	boolean isIgnored(String issueCode);

	/** Replies if the given issue is ignored for the given object.
	 *
	 * @param issueCode the code if the issue.
	 * @param currentObject the current object.
	 * @return {@code true} if the issue is ignored.
	 * @see #isIgnored(String)
	 */
	boolean isIgnored(String issueCode, EObject currentObject);

	/** Replies if the given type is declared as final (cannot be overridden).
	 *
	 * @param type the type to test.
	 * @return {@code true} if the type is declared as final.
	 */
	boolean isFinal(LightweightTypeReference type);
	
	/** Replies if the given target is marked as locally used in the given container.
	 *
	 * @param target the object to check for its usage.
	 * @param containerToFindUsage the context of the use.
	 * @return {@code true} if the {@code target} is marked as used in the container.
	 */
	boolean isLocallyUsed(EObject target, EObject containerToFindUsage);

	/** Replies if the given type parameter is locally used in the given event.
	 *
	 * @param parameter the type parameter to search for a local usage.
	 * @param event the container in which the local uses should be checked.
	 * @return {@code true} if the type parameter is locally used.
	 */
	boolean isTypeParameterLocallyUsedInEvent(JvmTypeParameter parameter, SarlEvent event);

	/** Add an issue.
	 *
	 * @param message the explanation message.
	 * @param source the source of the issue.
	 * @param issueCode the code of the issue.
	 */
	void addIssue(String message, EObject source, String issueCode);

	/** Check of all the fields are properly initialized for the given type.
	 *
	 * @param type the type to check.
	 * @param acceptor the message acceptor, may be {@code null}.
	 */
	void doCheckFinalFieldInitialization(JvmGenericType type, ValidationMessageAcceptor acceptor);

	/** Replies the generator configuration that is used for creating the JVM objects.
	 * 
	 * @param element
	 * @return the general configuration.
	 */
	GeneratorConfig getGeneratorConfig(EObject element);
	
	/** Replies if the given member is member of the type hierarchy of the given type.
	 *
	 * @param type the type for which the type hierarchy is computed.
	 * @param potentialMember the member to search for in the type hierarchy.
	 * @return {@code true} if {@code potentialMember} is defined inside the type hierarchy of the {@code type}.
	 */
	boolean memberOfTypeHierarchy(LightweightTypeReference type, LightweightTypeReference potentialMember);

	/** Replies if the given type corresponds to a primitive void.
	 *
	 * @param typeReference the type reference.
	 * @return {@code true} if the given type reference corresponds to a primitive void.
	 */
	boolean isPrimitiveVoid(JvmTypeReference typeReference);

	/** Check the type parameters for the given list.
	 * 
	 * @param sourceTypeParameters the list of type parameters to check.
	 */
	void doCheckTypeParameterForwardReference(List<JvmTypeParameter> sourceTypeParameters);

	/** Replies the issue severity provider.
	 *
	 * @param context the current context
	 * @param eObject the object.
	 * @return the severities.
	 */
	IssueSeverities getIssueSeverities(Map<Object, Object> context, EObject eObject);

	/** Replies the receiver of the issue messages.
	 *
	 * @return the receiver of the issue messages.
	 */
	ValidationMessageAcceptor getMessageAcceptor();
	
	/** Check if the given type has valid type arguments according to the type parameters of the referenced type.
	 * 
	 * @param typeRef the type reference to check.
	 * @param context the context in which the reference is defined, for finding the other types.
	 * @param feature the grammar feature that contains the {@code typeRef}.
	 * @param superTypeIndex the index of the feature in the grammar.
	 * @param allowWildcardAsAny indicates if wildcards are allowed in type conformance checking for representing "any" type that is conformant.
	 *     Usually, this argument is {@code false} when type checking a construct declaration, e.g., {@code extends A<B>}. It is
	 *     {@code true} when type checking specific usage of the type, e.g., {@code on A<?>}.
	 * @param rawTypeWarningIfNoArgument indicates if a warning "Raw Type" is generated when there is no type argument provided.
	 *     If it is {@code true}, and there is no type argument, only the warning is generated and the type conformance is not checked.
	 *     If it is {code false}, the type conformance is always checked.
	 * @param messageAcceptor the acceptor of error and warning messages to be used.
	 * @return {@code true} of the reference is valid; {@code false} if the reference is invalid.
	 */
	boolean doCheckValidSuperTypeArgumentDefinition(LightweightTypeReference typeRef, EObject context,
			EStructuralFeature feature, int superTypeIndex, boolean allowWildcardAsAny, boolean rawTypeWarningIfNoArgument,
			ValidationMessageAcceptor messageAcceptor);

}
