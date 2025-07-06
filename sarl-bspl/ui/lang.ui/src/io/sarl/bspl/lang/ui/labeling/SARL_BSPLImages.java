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

package io.sarl.bspl.lang.ui.labeling;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.ui.IImageHelper.IImageDescriptorHelper;
import org.eclipse.xtext.xbase.ui.labeling.XbaseImages2;
import org.eclipse.jdt.core.Flags;

/**
 * Providers of images for the SARL IDE.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
@SuppressWarnings("restriction")
public class SARL_BSPLImages extends XbaseImages2 {

	@Inject
	private IImageDescriptorHelper imageHelper;

	/** Replies the image descriptor for the "SARL script".
	 *
	 * @return the image descriptor for the SARL script.
	 */
	public ImageDescriptor forFile() {
		return this.imageHelper.getImageDescriptor("bspl-file.png"); //$NON-NLS-1$
	}

	/** Replies the image descriptor for the "protocols".
	 *
	 * @param visibility the visibility of the protocols.
	 * @param flags the mark flags. See {@link JavaElementImageDescriptor#setAdornments(int)} for
	 *                a description of the available flags.
	 * @return the image descriptor for the protocols.
	 */
	public ImageDescriptor forProtocol(JvmVisibility visibility, int flags) {
		final ImageDescriptor desc;
		final var iconName = new StringBuilder("bspl_protocol");
		if (Flags.isPackageDefault(flags)) {
			iconName.append("_package"); //$NON-NLS-1$
		} else if (Flags.isProtected(flags)) {
			iconName.append("_protected"); //$NON-NLS-1$
		} else if (Flags.isPrivate(flags)) {
			iconName.append("_private"); //$NON-NLS-1$
		}
		iconName.append(".png"); //$NON-NLS-1$
		desc = this.imageHelper.getImageDescriptor(iconName.toString());
		return getDecorated(desc, flags);
	}

}
