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

package io.sarl.lang.jvmmodel;

import java.util.List;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider;

/** Detector of the type of access to the default value of a formal parameter.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class DefaultValueAccessDetector implements IDefaultValueAccessDetector {

	@Inject
	private ILogicalContainerProvider logicalContainerProvider;

	@Override
	public boolean isStaticFieldStorage(EObject sourceObject, XExpression defaultValue, JvmExecutable parameterContainer, JvmGenericType executableContainer) {
		if (executableContainer.isInterface()) {
			return true;
		}
		if (parameterContainer instanceof JvmConstructor) {
			return true;
		}
		if (defaultValue instanceof XFeatureCall || defaultValue instanceof XMemberFeatureCall) {
			return false;
		}
		final List<XAbstractFeatureCall> calls = EcoreUtil2.getAllContentsOfType(defaultValue, XAbstractFeatureCall.class);
		for (final XAbstractFeatureCall call : calls) {
			if (call instanceof XFeatureCall || call instanceof XMemberFeatureCall) {
				return false;
			}
		}
		return true;
	}

	@Override
	@SuppressWarnings("checkstyle:nestedifdepth")
	public boolean isStaticAccess(JvmOperation operation) {
		if (!operation.isStatic() && operation.getDeclaringType() instanceof JvmGenericType) {
			final JvmGenericType type = (JvmGenericType) operation.getDeclaringType();
			if (!type.isInterface()) {
				final XExpression body = this.logicalContainerProvider.getAssociatedExpression(operation);
				if (body != null) {
					final List<XAbstractFeatureCall> calls = EcoreUtil2.getAllContentsOfType(body, XAbstractFeatureCall.class);
					if (body instanceof XAbstractFeatureCall) {
						calls.add(0, (XAbstractFeatureCall) body);
					}
					for (final XAbstractFeatureCall call : calls) {
						if (call instanceof XMemberFeatureCall || call instanceof XFeatureCall) {
							final JvmIdentifiableElement feature = call.getFeature();
							if (feature instanceof JvmField && !((JvmField) feature).isStatic()) {
								return false;
							}
							if (feature instanceof JvmOperation && !((JvmOperation) feature).isStatic()) {
								return false;
							}
						}
					}
				}
			}
		}
		return true;
	}

}
