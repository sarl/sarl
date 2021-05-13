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

package io.sarl.lang.validation;

import javax.inject.Inject;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendEnum;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XClosure;
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider;
import org.eclipse.xtext.xbase.lib.InputOutput;

import io.sarl.lang.annotation.PrivateAPI;
import io.sarl.lang.typesystem.SARLAnnotationUtil;
import io.sarl.lang.util.SarlUtils;
import io.sarl.lang.util.Utils;

/** Validator of the feature calls.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DefaultFeatureCallValidator implements IFeatureCallValidator {

	@Inject
	private SARLAnnotationUtil annotations;

	@Inject
	private ILogicalContainerProvider containerProvider;

	/** Construct the validator.
	 */
	public DefaultFeatureCallValidator() {
		//
	}

	private static boolean isInsideOOTypeDeclaration(XAbstractFeatureCall call) {
		final XtendTypeDeclaration declaration = EcoreUtil2.getContainerOfType(call, XtendTypeDeclaration.class);
		return declaration != null
			&& (declaration instanceof XtendClass
				|| declaration instanceof XtendEnum
				|| declaration instanceof XtendInterface
				|| declaration instanceof XtendAnnotationType);
	}

	@Override
	public boolean isDisallowedCall(XAbstractFeatureCall call) {
		if (call != null && call.getFeature() != null) {
			final JvmIdentifiableElement feature = call.getFeature();
			final String id = feature.getQualifiedName();
			// Exit is forbidden on a agent-based system
			if ("java.lang.System.exit".equals(id)) { //$NON-NLS-1$
				return !isInsideOOTypeDeclaration(call);
			}
			// Avoid any call to the hidden functions (function name contains "$" character).
			final String simpleName = feature.getSimpleName();
			if (SarlUtils.isHiddenMember(simpleName)
				&& !Utils.isNameForHiddenCapacityImplementationCallingMethod(simpleName)
				&& (!Utils.isImplicitLambdaParameterName(simpleName) || !isInsideClosure(call))) {
				return true;
			}
			// Avoid any reference to private API.
			if (isPrivateAPI(feature) && !isPrivateAPI(call)) {
				return true;
			}
		}
		return false;
	}

	private static boolean isInsideClosure(XAbstractFeatureCall call) {
		final EObject container = Utils.getFirstContainerForPredicate(call, it -> {
			return it instanceof XClosure || it instanceof XtendMember;
		});
		return container instanceof XClosure;
	}

	private boolean isPrivateAPI(JvmIdentifiableElement element) {
		JvmMember featureContainer = EcoreUtil2.getContainerOfType(element, JvmMember.class);
		while (featureContainer != null) {
			final Boolean value = this.annotations.findBooleanValue(featureContainer, PrivateAPI.class);
			if (value != null && !value.booleanValue()) {
				return true;
			}
			featureContainer = EcoreUtil2.getContainerOfType(featureContainer.eContainer(), JvmMember.class);
		}
		return false;
	}

	private boolean isPrivateAPI(XAbstractFeatureCall element) {
		final JvmIdentifiableElement jvmElement = this.containerProvider.getNearestLogicalContainer(element);
		return jvmElement != null && isPrivateAPICaller(jvmElement);
	}

	private boolean isPrivateAPICaller(JvmIdentifiableElement element) {
		JvmMember featureContainer = EcoreUtil2.getContainerOfType(element, JvmMember.class);
		while (featureContainer != null) {
			if (this.annotations.findAnnotation(featureContainer, PrivateAPI.class.getName()) != null) {
				return true;
			}
			featureContainer = EcoreUtil2.getContainerOfType(featureContainer.eContainer(), JvmMember.class);
		}
		return false;
	}

	@Override
	public boolean isDiscouragedCall(XAbstractFeatureCall call) {
		if (call != null && call.getFeature() != null) {
			final JvmIdentifiableElement feature = call.getFeature();
			final String id = feature.getQualifiedName();
			if (id != null) {
				switch (id) {
				case "java.lang.System.err": //$NON-NLS-1$
				case "java.lang.System.out": //$NON-NLS-1$
				case "java.lang.System.setErr": //$NON-NLS-1$
				case "java.lang.System.setOut": //$NON-NLS-1$
				case "java.lang.System.console": //$NON-NLS-1$
				case "java.lang.System.inheritedChannel": //$NON-NLS-1$
					return true;
				case "java.lang.System.exit": //$NON-NLS-1$
					return isInsideOOTypeDeclaration(call);
				default:
					if (id.startsWith(InputOutput.class.getName())) {
						return true;
					}
					if (id.startsWith(Thread.class.getName())) {
						return true;
					}
				}
			}
		}
		return false;
	}

}
