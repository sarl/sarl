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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import javax.inject.Inject;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.impl.XtendMemberImplCustom;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmVisibility;

/** Provide the default visibility modifier for elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DefaultVisibilityProvider implements IDefaultVisibilityProvider {

	private final Method visiblityMethod;

	@Inject
	private SarlJvmModelAssociations associations;

	/** Constructor.
	 * @throws RuntimeException a runtime exception
	 */
	public DefaultVisibilityProvider() {
		try {
			this.visiblityMethod = XtendMemberImplCustom.class.getDeclaredMethod("getDefaultVisibility"); //$NON-NLS-1$
			this.visiblityMethod.setAccessible(true);
		} catch (NoSuchMethodException | SecurityException exception) {
			throw new RuntimeException(exception);
		}
	}

	@Override
	public JvmVisibility getDefaultJvmVisibility(EObject element) {
		EObject realObject = element;
		if (realObject instanceof JvmIdentifiableElement) {
			final EObject obj = this.associations.getPrimarySourceElement(realObject);
			if (obj != null) {
				realObject = obj;
			}
		}
		if (realObject instanceof XtendMemberImplCustom) {
			try {
				return (JvmVisibility) this.visiblityMethod.invoke(realObject);
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException exception) {
				throw new RuntimeException(exception);
			}
		}
		return JvmVisibility.DEFAULT;
	}

	@Override
	@SuppressWarnings("checkstyle:npathcomplexity")
	public JvmVisibility getDefaultJvmVisibility(EObject container, EClass element) {
		EObject realObject = container;
		if (realObject instanceof JvmIdentifiableElement) {
			final EObject obj = this.associations.getPrimarySourceElement(realObject);
			if (obj != null) {
				realObject = obj;
			}
		}
		if (realObject != null) {
			if (XtendPackage.eINSTANCE.getXtendFunction().isSuperTypeOf(element)) {
				return IDefaultVisibilityProvider.getActionDefaultVisibilityIn(realObject);
			}
			if (XtendPackage.eINSTANCE.getXtendField().isSuperTypeOf(element)) {
				return IDefaultVisibilityProvider.getFieldDefaultVisibilityIn(realObject);
			}
			if (XtendPackage.eINSTANCE.getXtendClass().isSuperTypeOf(element)) {
				return IDefaultVisibilityProvider.getClassDefaultVisibilityIn(realObject);
			}
			if (XtendPackage.eINSTANCE.getXtendInterface().isSuperTypeOf(element)) {
				return IDefaultVisibilityProvider.getInterfaceDefaultVisibilityIn(realObject);
			}
			if (XtendPackage.eINSTANCE.getXtendEnum().isSuperTypeOf(element)) {
				return IDefaultVisibilityProvider.getEnumerationDefaultVisibilityIn(realObject);
			}
			if (XtendPackage.eINSTANCE.getXtendAnnotationType().isSuperTypeOf(element)) {
				return IDefaultVisibilityProvider.getAnnotationTypeDefaultVisibilityIn(realObject);
			}
			if (XtendPackage.eINSTANCE.getXtendMember().isSuperTypeOf(element)) {
				return JvmVisibility.PUBLIC;
			}
		}
		return JvmVisibility.DEFAULT;
	}

}
