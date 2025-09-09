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

package io.sarl.eclipse.launching.shortcuts;

import com.google.inject.Inject;
import com.google.inject.Provider;
import org.eclipse.core.resources.IResource;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.xtext.xbase.ui.launching.JavaElementDelegate;
import org.eclipse.xtext.xbase.ui.launching.JavaElementDelegateAdapterFactory;

/** Factory for Java element delegates that should be used to map SARL elements to JDT elements.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.6
 */
@SuppressWarnings("restriction")
public class SarlJavaElementDelegateAdapterFactory extends JavaElementDelegateAdapterFactory {

	@Inject
	private Provider<SarlJavaElementDelegateMainLaunch> mainDelegateProvider;

	@SuppressWarnings("unchecked")
	@Override
	public Object getAdapter(Object adaptableObject, @SuppressWarnings("rawtypes")  Class adapterType) {
		JavaElementDelegate result = null;
		if (SarlJavaElementDelegateMainLaunch.class.equals(adapterType)) {
			result = this.mainDelegateProvider.get();
		}
		if (result != null) {
			if (adaptableObject instanceof IFileEditorInput cvalue) {
				result.initializeWith(cvalue);
				return result;
			}
			if (adaptableObject instanceof IResource cvalue) {
				result.initializeWith(cvalue);
				return result;
			}
			if (adaptableObject instanceof IEditorPart cvalue) {
				result.initializeWith(cvalue);
				return result;
			}
		}
		return super.getAdapter(adaptableObject, adapterType);
	}

}
