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

package io.sarl.lang.ui.hover;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.ide.hover.XtendHoverSerializer;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;

import io.sarl.lang.util.Utils;

/**
 * Serialize SARL elements for hovers.
 *
 * <p>This class extends the standard Xtend serializer by replacing the example of code, written
 * in Java or Xtend, by the same example with the SARL syntax.
 *
 * @author $Author: sgalland$
 * @version io.sarl.lang.ui 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 * @since 0.5
 */
@SuppressWarnings("restriction")
public class SARLHoverSerializer extends XtendHoverSerializer {

	@Override
	public String computeUnsugaredExpression(EObject object) {
		if (object instanceof XAbstractFeatureCall featureCall) {
			final var feature = featureCall.getFeature();
			if (feature instanceof JvmExecutable && !feature.eIsProxy()
					&& (featureCall.getImplicitReceiver() != null || featureCall.getImplicitFirstArgument() != null)
					&& !featureCall.isStatic()) {
				final var receiver = featureCall.getActualReceiver();
				if (receiver instanceof XMemberFeatureCall cvalue) {
					final var memberFeature = cvalue.getFeature();
					final var name = memberFeature.getSimpleName();
					if (Utils.isNameForHiddenCapacityImplementationCallingMethod(name)) {
						final var op = (JvmOperation) memberFeature;
						final var result = new StringBuilder();
						result.append("getSkill(typeof("); //$NON-NLS-1$
						result.append(op.getReturnType().getSimpleName());
						result.append("))."); //$NON-NLS-1$
						result.append(feature.getSimpleName());
						result.append(computeArguments(featureCall));
						return result.toString();
					}
				}
			}
		}
		return super.computeUnsugaredExpression(object);
	}

}
