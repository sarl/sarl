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

import io.sarl.bspl.lang.bspl.BsplProtocol;
import io.sarl.bspl.lang.bspl.BsplProtocolMember;

import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementImageProvider;

/**
 * Providers of images for the BSPL plugins.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
@SuppressWarnings("restriction")
public class BSPLImages extends XbaseImages2 {

	private static final boolean USE_LIGHT_ICONS = false;

	@Inject
	private IImageDescriptorHelper imageHelper;

	/** Replies the JVM visibility that corresponds to the definition of the given protocol.
	 *
	 * @param protocol the protocol to analyze.
	 * @return the JVM visibility that is associated to the given protocol.
	 */
	public static JvmVisibility toJvmVisibility(BsplProtocol protocol) {
		final JvmVisibility visibility;
		if (protocol.isPrivateVisibility()) {
			visibility = JvmVisibility.PRIVATE;
		} else if (protocol.isPackageVisibility()) {
			visibility = JvmVisibility.DEFAULT;
		} else if (protocol.isProtectedVisibility()) {
			visibility = JvmVisibility.PROTECTED;
		} else if (protocol.isPublicVisibility()) {
			visibility = JvmVisibility.PUBLIC;
		} else {
			visibility = protocol.getDefaultVisibility();
		}
		assert visibility != null : "visibility cannot be null"; //$NON-NLS-1$
		return visibility;
	}
	
	/** Replies the JVM visibility that corresponds to the definition of the given protocol member.
	 *
	 * @param member the protocol member to analyze.
	 * @return the JVM visibility that is associated to the given protocol member.
	 */
	public static JvmVisibility toJvmVisibility(BsplProtocolMember member) {
		final JvmVisibility visibility;
		if (member.isPrivateVisibility()) {
			visibility = JvmVisibility.PRIVATE;
		} else if (member.isPackageVisibility()) {
			visibility = JvmVisibility.DEFAULT;
		} else if (member.isProtectedVisibility()) {
			visibility = JvmVisibility.PROTECTED;
		} else if (member.isPublicVisibility()) {
			visibility = JvmVisibility.PUBLIC;
		} else {
			visibility = member.getDefaultVisibility();
		}
		assert visibility != null : "visibility cannot be null"; //$NON-NLS-1$
		return visibility;
	}

	/** Replies the image descriptor for the given element.
	 *
	 * @param type the type of the BSPL element, or {@code null} if unknown.
	 * @param flags the adornments.
	 * @param useLightIcons indicates of light icons should be used.
	 * @return the image descriptor.
	 */
	public ImageDescriptor getTypeImageDescriptor(
			BsplElementType type, int flags, boolean useLightIcons) {
		final ImageDescriptor desc;
		if (type != null) {
			final var iconName = new StringBuilder(type.getSimpleBasename());
			if (flags != 0) {
				if (Flags.isPackageDefault(flags)) {
					iconName.append("_package"); //$NON-NLS-1$
				} else if (Flags.isProtected(flags)) {
					iconName.append("_protected"); //$NON-NLS-1$
				} else if (Flags.isPrivate(flags)) {
					iconName.append("_private"); //$NON-NLS-1$
				}
			}
			iconName.append(".png"); //$NON-NLS-1$
			desc = this.imageHelper.getImageDescriptor(iconName.toString());
		} else {
			desc = JavaElementImageProvider.getTypeImageDescriptor(false, false, flags, useLightIcons);
		}
		return desc;
	}

	/** Replies the image descriptor for the given element.
	 *
	 * @param type the type of the BSPL element, or {@code null} if unknown.
	 * @return the image descriptor.
	 */
	public ImageDescriptor getTypeImageDescriptor(BsplElementType type) {
		return getTypeImageDescriptor(type, 0, USE_LIGHT_ICONS);
	}

	/** Replies the image descriptor for the given element.
	 *
	 * @param type the type of the BSPL element, or {@code null} if unknown.
	 * @param flags the adornments.
	 * @return the image descriptor.
	 */
	public ImageDescriptor getTypeImageDescriptor(
			BsplElementType type,
			int flags) {
		return getTypeImageDescriptor(type, flags, USE_LIGHT_ICONS);
	}

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
		assert visibility != null : "visibility cannot be null"; //$NON-NLS-1$
		return getDecorated(getTypeImageDescriptor(BsplElementType.PROTOCOL, toFlags(visibility)), flags);
	}

	/** Replies the image descriptor for the "roles".
	 *
	 * @param flags the mark flags. See {@link JavaElementImageDescriptor#setAdornments(int)} for
	 *                a description of the available flags.
	 * @return the image descriptor for the roles.
	 */
	public ImageDescriptor forRole(int flags) {
		return getDecorated(getTypeImageDescriptor(BsplElementType.ROLE), flags);
	}

	/** Replies the image descriptor for the "parameters" that are not "key".
	 *
	 * @param visibility the visibility of the parameters.
	 * @param flags the mark flags. See {@link JavaElementImageDescriptor#setAdornments(int)} for
	 *                a description of the available flags.
	 * @return the image descriptor for the parameters.
	 */
	public ImageDescriptor forParameter(JvmVisibility visibility, int flags) {
		assert visibility != null : "visibility cannot be null"; //$NON-NLS-1$
		return getDecorated(getTypeImageDescriptor(BsplElementType.PARAMETER, toFlags(visibility)), flags);
	}

	/** Replies the image descriptor for the "key parameters".
	 *
	 * @param visibility the visibility of the key parameters.
	 * @param flags the mark flags. See {@link JavaElementImageDescriptor#setAdornments(int)} for
	 *                a description of the available flags.
	 * @return the image descriptor for the key parameters.
	 */
	public ImageDescriptor forKeyParameter(JvmVisibility visibility, int flags) {
		assert visibility != null : "visibility cannot be null"; //$NON-NLS-1$
		return getDecorated(getTypeImageDescriptor(BsplElementType.KEY, toFlags(visibility)), flags);
	}

	/** Replies the image descriptor for the "messages".
	 *
	 * @param flags the mark flags. See {@link JavaElementImageDescriptor#setAdornments(int)} for
	 *                a description of the available flags.
	 * @return the image descriptor for the messages.
	 */
	public ImageDescriptor forMessage(int flags) {
		return getDecorated(getTypeImageDescriptor(BsplElementType.MESSAGE), flags);
	}

	/**
	 * Type of the BSPL element.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	public enum BsplElementType {
		/** Protocol.
		 */
		PROTOCOL {
			@Override
			public String getSimpleBasename() {
				return "bspl-protocol"; //$NON-NLS-1$
			}
		},

		/** Role.
		 */
		ROLE {
			@Override
			public String getSimpleBasename() {
				return "bspl-role"; //$NON-NLS-1$
			}
		},

		/** Parameter that is not a key.
		 */
		PARAMETER {
			@Override
			public String getSimpleBasename() {
				return "bspl-parameter"; //$NON-NLS-1$
			}
		},

		/** Parameter that is a key.
		 */
		KEY {
			@Override
			public String getSimpleBasename() {
				return "bspl-key"; //$NON-NLS-1$
			}
		},

		/** Parameter that is a parameter.
		 */
		MESSAGE {
			@Override
			public String getSimpleBasename() {
				return "bspl-message"; //$NON-NLS-1$
			}
		};

		/** Replies the simple basename of the file representing the icon of the element.
		 *
		 * @return the simple basename.
		 */
		public abstract String getSimpleBasename();

	}

}
