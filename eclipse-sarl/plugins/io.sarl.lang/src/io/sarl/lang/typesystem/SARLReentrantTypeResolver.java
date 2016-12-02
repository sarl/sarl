/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import java.util.Map;
import java.util.Set;

import com.google.inject.Inject;
import org.eclipse.xtend.core.typesystem.XtendReentrantTypeResolver;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.eclipse.xtext.xbase.scoping.batch.IFeatureScopeSession;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.internal.ResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.Maps2;

import io.sarl.lang.annotation.ImportedCapacityFeature;
import io.sarl.lang.util.Utils;

/**
 * The customized reentrant type resolver is responsible for proper typing function calls for
 * extension fields that are associated to used capacities in order to redirect the capacity's function
 * calls to the internal hidden functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLReentrantTypeResolver extends XtendReentrantTypeResolver {

	@Inject
	private AnnotationLookup annotationLookup;

	@Override
	protected IFeatureScopeSession addExtensionFieldsToMemberSession(
			ResolvedTypes resolvedTypes,
			IFeatureScopeSession featureScopeSession,
			JvmDeclaredType type,
			JvmIdentifiableElement thisFeature,
			Set<String> seenNames,
			Set<JvmType> seenTypes) {
		if (seenTypes.add(type)) {
			final Iterable<JvmField> fields = type.getDeclaredFields();
			Map<XExpression, LightweightTypeReference> extensionProviders = null;
			for (final JvmField field : fields) {
				if (featureScopeSession.isVisible(field) && seenNames.add(field.getSimpleName()) && isExtensionProvider(field)) {
					if (extensionProviders == null) {
						extensionProviders = Maps2.newLinkedHashMapWithExpectedSize(3);
					}
					// Sarl specific block of code
					XAbstractFeatureCall extensionProvider = createSarlCapacityExtensionProvider(thisFeature, field);
					final LightweightTypeReference fieldType;
					if (extensionProvider == null) {
						extensionProvider = createExtensionProvider(thisFeature, field);
						fieldType = resolvedTypes.getActualType(field);
					} else {
						fieldType = getSarlCapacityFieldType(resolvedTypes, field);
					}
					// End of Sarl specific
					extensionProviders.put(extensionProvider, fieldType);
				}
			}
			// traverse the type hierarchy to create the feature scope sessions
			final JvmTypeReference superType = getExtendedClass(type);
			IFeatureScopeSession result = featureScopeSession;
			if (superType != null) {
				result = addExtensionFieldsToMemberSession(resolvedTypes, featureScopeSession, (JvmDeclaredType) superType.getType(),
						thisFeature, seenNames, seenTypes);
			}
			if (extensionProviders != null) {
				result = result.addToExtensionScope(extensionProviders);
			}
			return result;
		}
		return featureScopeSession;
	}

	/** Replies the type of the field that represents a SARL capacity buffer.
	 *
	 * @param resolvedTypes the resolver of types.
	 * @param field the field.
	 * @return the type, never {@code null}.
	 */
	protected LightweightTypeReference getSarlCapacityFieldType(IResolvedTypes resolvedTypes, JvmField field) {
		LightweightTypeReference fieldType = resolvedTypes.getActualType(field);
		final JvmAnnotationReference capacityAnnotation = this.annotationLookup.findAnnotation(field,
				ImportedCapacityFeature.class);
		if (capacityAnnotation != null) {
			final JvmTypeReference ref = ((JvmTypeAnnotationValue) capacityAnnotation.getValues().get(0)).getValues().get(0);
			fieldType = resolvedTypes.getActualType(ref.getType());
		}
		return fieldType;
	}

	/** Create the extension provider dedicated to the access to the used capacity functions.
	 *
	 * @param thisFeature the current object.
	 * @param field the extension field.
	 * @return the SARL capacity extension provider, or {@code null}.
	 */
	protected XAbstractFeatureCall createSarlCapacityExtensionProvider(JvmIdentifiableElement thisFeature, JvmField field) {
		if (thisFeature instanceof JvmDeclaredType) {
			final JvmAnnotationReference capacityAnnotation = this.annotationLookup.findAnnotation(field,
					ImportedCapacityFeature.class);
			if (capacityAnnotation != null) {
				final String methodName = Utils.createNameForHiddenCapacityImplementationCallingMethodFromFieldName(
						field.getSimpleName());
				final JvmOperation callerOperation = findOperation((JvmDeclaredType) thisFeature, methodName);
				if (callerOperation != null) {
					final XbaseFactory baseFactory = getXbaseFactory();
					final XMemberFeatureCall extensionProvider = baseFactory.createXMemberFeatureCall();
					extensionProvider.setFeature(callerOperation);
					final XFeatureCall thisAccess = baseFactory.createXFeatureCall();
					thisAccess.setFeature(thisFeature);
					extensionProvider.setMemberCallTarget(thisAccess);
					return extensionProvider;
				}
			}
		}
		return null;
	}

	private static JvmOperation findOperation(JvmDeclaredType type, String operationName) {
		for (final JvmOperation declaredOperation : type.getDeclaredOperations()) {
			if (declaredOperation.getSimpleName().equals(operationName)) {
				return declaredOperation;
			}
		}
		return null;
	}

}
