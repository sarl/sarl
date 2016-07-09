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

import com.google.inject.Inject;
import org.eclipse.xtend.core.typesystem.XtendReentrantTypeResolver;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XbaseFactory;

import io.sarl.lang.annotation.ImportedCapacityFeature;
import io.sarl.lang.util.Utils;

/**
 * The customized reentrant type resolver is responsible for proper typing function calls for
 * extension fields that are associated to used capacities.
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
	protected XAbstractFeatureCall createExtensionProvider(JvmIdentifiableElement thisFeature, JvmField field) {
		if (thisFeature instanceof JvmDeclaredType) {
			final JvmAnnotationReference capacityAnnotation = this.annotationLookup.findAnnotation(field,
					ImportedCapacityFeature.class);
			if (capacityAnnotation != null) {
				// Get caller operation
				final String methodName = Utils.createNameForHiddenCapacityImplementationCallingMethodFromFieldName(
						field.getSimpleName());
				final JvmOperation callerOperation = findOperation((JvmDeclaredType) thisFeature, methodName);
				if (callerOperation != null) {
					final XbaseFactory baseFactory = getXbaseFactory();

					// this
					final XFeatureCall thisAccess = baseFactory.createXFeatureCall();
					thisAccess.setFeature(thisFeature);

					// Call the operation
					final XMemberFeatureCall getterCall = baseFactory.createXMemberFeatureCall();
					getterCall.setMemberCallTarget(thisAccess);
					getterCall.setFeature(callerOperation);

					return getterCall;
				}
			}
		}
		return super.createExtensionProvider(thisFeature, field);
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
