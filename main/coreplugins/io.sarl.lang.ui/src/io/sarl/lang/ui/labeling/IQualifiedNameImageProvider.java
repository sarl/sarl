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

package io.sarl.lang.ui.labeling;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.xbase.lib.Inline;

/**
 * An image provider that is seaching from a qualified name.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface IQualifiedNameImageProvider {

	/** Replies the image associated to the element with the qualified name.
	 *
	 * @param qualifiedName the qualified name.
	 * @param context the context of the reference to the qualified name of the element.
	 * @param jvmTypeProvider the provider of types that should be used for retreiving the type description.
	 * @return the image.
	 */
	Image getImageForQualifiedName(String qualifiedName, Notifier context, IJvmTypeProvider jvmTypeProvider);

	/** Replies the image associated to the element with the qualified name.
	 *
	 * @param qualifiedName the qualified name.
	 * @param context the context of the reference to the qualified name of the element.
	 * @param jvmTypeProvider the provider of types that should be used for retreiving the type description.
	 * @return the image.
	 */
	@Inline("getImageForQualifiedName(($1).toString(), $2)")
	default Image getImageForQualifiedName(QualifiedName qualifiedName, Notifier context,
			IJvmTypeProvider jvmTypeProvider) {
		return getImageForQualifiedName(qualifiedName.toString(), context, jvmTypeProvider);
	}

	/** Replies the image descriptor associated to the element with the qualified name.
	 *
	 * @param qualifiedName the qualified name.
	 * @param context the context of the reference to the qualified name of the element.
	 * @param jvmTypeProvider the provider of types that should be used for retreiving the type description.
	 * @return the image descriptor.
	 */
	ImageDescriptor getImageDescriptorForQualifiedName(String qualifiedName, Notifier context,
			IJvmTypeProvider jvmTypeProvider);

	/** Replies the image descriptor associated to the element with the qualified name.
	 *
	 * @param qualifiedName the qualified name.
	 * @param context the context of the reference to the qualified name of the element.
	 * @param jvmTypeProvider the provider of types that should be used for retreiving the type description.
	 * @return the image descriptor.
	 */
	@Inline("getImageDescriptorForQualifiedName(($1).toString(), $2)")
	default ImageDescriptor getImageDescriptorForQualifiedName(QualifiedName qualifiedName, Notifier context,
			IJvmTypeProvider jvmTypeProvider) {
		return getImageDescriptorForQualifiedName(qualifiedName.toString(), context, jvmTypeProvider);
	}

}
