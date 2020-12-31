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

package io.sarl.eclipse.explorer;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import javax.inject.Inject;
import javax.inject.Named;

import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementImageProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.xtext.Constants;

/**
 * Provides the images for the Package Explorer.
 *
 * <p>This provider inherits its behavior from the JDT package explorer image provider,
 * and add the following changes:
 * <ul>
 * <li>the icon of the package fragments are computed in order to consider the SARL scripts inside.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SARLElementImageProvider extends JavaElementImageProvider {

	private final Set<String> fileExtensions = new HashSet<>();

	/** Set the file extensions.
	 *
	 * @param fileExtensions the file extensions.
	 */
	@Inject
	public void setFileExtensions(@Named(Constants.FILE_EXTENSIONS) String fileExtensions) {
		this.fileExtensions.clear();
		this.fileExtensions.addAll(Arrays.asList(fileExtensions.split("[,;: ]+"))); //$NON-NLS-1$
	}

	/** Replies the file extensions.
	 *
	 * @return the file extensions.
	 */
	public Set<String> getFileExtensions() {
		return this.fileExtensions;
	}

	@Override
	public ImageDescriptor getBaseImageDescriptor(IJavaElement element, int renderFlags) {
		if (element.getElementType() == IJavaElement.PACKAGE_FRAGMENT) {
			final ImageDescriptor desc = getPackageFragmentIcon((IPackageFragment) element);
			if (desc != null) {
				return desc;
			}
		}
		return super.getBaseImageDescriptor(element, renderFlags);
	}

	/** Replies the image description of the package fragment.
	 *
	 * @param fragment the element.
	 * @return the descriptor.
	 */
	@SuppressWarnings("checkstyle:all")
	private ImageDescriptor getPackageFragmentIcon(IPackageFragment fragment) {
		boolean containsJavaElements = false;
		try {
			containsJavaElements = fragment.hasChildren();
		} catch (JavaModelException e) {
			// assuming no children;
		}
		try {
			if (!containsJavaElements) {
				final Object[] resources = fragment.getNonJavaResources();
				if (resources.length > 0) {
					for (final Object child : resources) {
						if (isSarlResource(child)) {
							return JavaPluginImages.DESC_OBJS_PACKAGE;
						}
					}
					return JavaPluginImages.DESC_OBJS_EMPTY_PACKAGE_RESOURCES;
				}
			}
		} catch (JavaModelException exception) {
			//
		}
		if (!containsJavaElements) {
			return JavaPluginImages.DESC_OBJS_EMPTY_PACKAGE;
		}
		return JavaPluginImages.DESC_OBJS_PACKAGE;
	}

	/** Replies if the given resource is a SARL resource.
	 *
	 * @param resource the resource.
	 * @return {@code true} if the given resource is a SARL resource.
	 */
	protected boolean isSarlResource(Object resource) {
		if (resource instanceof IFile) {
			final IFile file = (IFile) resource;
			return getFileExtensions().contains(file.getFileExtension());
		}
		return false;
	}

}
