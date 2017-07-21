/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import javax.inject.Inject;
import javax.inject.Singleton;

import org.eclipse.xtend.core.typing.XtendExpressionHelper;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import io.sarl.lang.util.Utils;

/**
 * Helper on expressions.
 *
 * <p>This implementation extends the Xtend expression helper by assuming that any function
 * with a name starting with "get", "is", "has" is a pure function.
 * It also assumes that "equals", "hashCode", "clone" and "toString" are also pure functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#validation"
 * @see IOperationHelper
 * @see SARLOperationHelper
 */
@Singleton
public class SARLExpressionHelper extends XtendExpressionHelper {

	@Inject
	private IPureOperationNameValidator nameValidator;

	@Inject
	private CommonTypeComputationServices services;

	@Override
	public boolean hasSideEffects(XAbstractFeatureCall featureCall, boolean inspectContents) {
		if (super.hasSideEffects(featureCall, inspectContents)) {
			final JvmIdentifiableElement feature = featureCall.getFeature();
			// Several operations are not marked with @Pure but they have a clear semantic without border effects,
			// e.g. the "is", "get" functions.
			if (feature != null && !feature.eIsProxy() && feature instanceof JvmOperation) {
				final JvmOperation operation = (JvmOperation) feature;
				return this.nameValidator.isNamePatternForNotPureOperation(operation)
						|| !this.nameValidator.isNamePatternForPureOperation(operation)
						|| !hasPrimitiveParameters(operation);
			}
			return true;
		}
		return false;
	}

	private boolean hasPrimitiveParameters(JvmOperation op) {
		for (final JvmFormalParameter parameter : op.getParameters()) {
			final JvmTypeReference type = parameter.getParameterType();
			if (type == null || !Utils.toLightweightTypeReference(type, this.services).isPrimitive()) {
				return false;
			}
		}
		return true;
	}

}
