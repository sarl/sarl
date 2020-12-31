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

package io.sarl.eclipse.wizards.elements;

import javax.inject.Inject;

import org.eclipse.jdt.ui.dialogs.ITypeInfoImageProvider;
import org.eclipse.jdt.ui.dialogs.ITypeInfoRequestor;
import org.eclipse.jdt.ui.dialogs.TypeSelectionExtension;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.ui.labeling.IQualifiedNameImageProvider;

/** Extension for the type selector.
 *
 * <p>This specific implementation uses the SARL IQualifiedNameImageProvider.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SarlSpecificTypeSelectionExtension extends TypeSelectionExtension
		implements ITypeInfoImageProvider {

	private final IJvmTypeProvider typeProvider;

	@Inject
	private IQualifiedNameImageProvider imageProvider;

	@Inject
	private IQualifiedNameConverter converter;

	/** Constructor.
	 *
	 * @param typeProvider the provider of JVM types.
	 */
	public SarlSpecificTypeSelectionExtension(IJvmTypeProvider typeProvider) {
		this.typeProvider = typeProvider;
	}

	@Override
	public ITypeInfoImageProvider getImageProvider() {
		return this;
	}

	@Override
	public ImageDescriptor getImageDescriptor(ITypeInfoRequestor typeInfoRequestor) {
		QualifiedName qualifiedName;
		final String enclosing = typeInfoRequestor.getEnclosingName();
		if (Strings.isEmpty(enclosing)) {
			final String packageName = typeInfoRequestor.getPackageName();
			if (Strings.isEmpty(packageName)) {
				qualifiedName = null;
			} else {
				qualifiedName = this.converter.toQualifiedName(packageName);
			}
		} else {
			qualifiedName = this.converter.toQualifiedName(enclosing);
		}
		final QualifiedName qn = this.converter.toQualifiedName(typeInfoRequestor.getTypeName());
		if (qualifiedName == null) {
			qualifiedName = qn;
		} else {
			qualifiedName = qualifiedName.append(qn);
		}
		return this.imageProvider.getImageDescriptorForQualifiedName(qualifiedName, this.typeProvider.getResourceSet(),
				this.typeProvider);
	}

}
