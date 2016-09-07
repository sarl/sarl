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

package io.sarl.lang.jvmmodel;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.impl.XtendMemberImplCustom;
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

	/** Constructor.
	 */
	public DefaultVisibilityProvider() {
		try {
			this.visiblityMethod = XtendMemberImplCustom.class.getDeclaredMethod("getDefaultVisibility"); //$NON-NLS-1$
		} catch (NoSuchMethodException | SecurityException exception) {
			throw new RuntimeException(exception);
		}
	}

	@Override
	public JvmVisibility getDefaultJvmVisibility(EObject element) {
		if (element instanceof XtendMemberImplCustom) {
			this.visiblityMethod.setAccessible(true);
			try {
				return (JvmVisibility) this.visiblityMethod.invoke(element);
			} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException exception) {
				throw new RuntimeException(exception);
			}
		}
		return JvmVisibility.DEFAULT;
	}

}
