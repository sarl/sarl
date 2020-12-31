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
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
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
			if (adaptableObject instanceof IFileEditorInput) {
				result.initializeWith((IFileEditorInput) adaptableObject);
				return result;
			}
			if (adaptableObject instanceof IResource) {
				result.initializeWith((IResource) adaptableObject);
				return result;
			}
			if (adaptableObject instanceof IEditorPart) {
				result.initializeWith((IEditorPart) adaptableObject);
				return result;
			}
		}
		return super.getAdapter(adaptableObject, adapterType);
	}

}
