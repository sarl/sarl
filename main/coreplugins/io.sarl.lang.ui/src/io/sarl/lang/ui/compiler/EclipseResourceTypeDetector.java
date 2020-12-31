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

package io.sarl.lang.ui.compiler;

import com.google.inject.Singleton;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;

import io.sarl.lang.SARLConfig;
import io.sarl.lang.compiler.AbstractResourceTypeDetector;


/** Detect the type of a folder.
 *
 * <p>A resource may be inside a standard source folder or a folder that is dedicated
 * to test code. Detecting in which case a resource is is the purpose of this interface.
 *
 * <p>This detector tests if one source folder has a name prefixed with {@code src/test}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 * @see SARLConfig#FOLDER_TEST_SOURCE_SARL
 * @see SARLConfig#FOLDER_TEST_SOURCE_GENERATED
 */
@Singleton
public class EclipseResourceTypeDetector extends AbstractResourceTypeDetector {

	@Override
	public Boolean isTestResource(Resource resource) {
		return isJavaOrMavenTestResource(resource);
	}

	/** Replies the given resource is a test resource according to the Java or Maven standard.
	 *
	 * @param resource the resource to test.
	 * @return {@link Boolean#TRUE} if the resource is a test resource. {@link Boolean#FALSE}
	 *     if the resource is not a test resource. {@code null} if the detector cannot determine
	 *     the type of the resource.
	 */
	@SuppressWarnings("checkstyle:nestedifdepth")
	protected static Boolean isJavaOrMavenTestResource(Resource resource) {
		final URI uri = resource.getURI();
		if (uri.isPlatformResource()) {
			final String platformString = uri.toPlatformString(true);
			final IResource iresource = ResourcesPlugin.getWorkspace().getRoot().findMember(platformString);
			if (iresource != null) {
				final IProject project = iresource.getProject();
				final IJavaProject javaProject = JavaCore.create(project);
				try {
					final IPackageFragment packageFragment = javaProject.findPackageFragment(iresource.getParent().getFullPath());
					final IPackageFragmentRoot root = (IPackageFragmentRoot) packageFragment.getAncestor(IJavaElement.PACKAGE_FRAGMENT_ROOT);
					if (root != null) {
						final IPath rootPath = root.getPath();
						String name = null;
						if (root.isExternal()) {
							name = rootPath.toOSString();
						} else if (javaProject.getElementName().equals(rootPath.segment(0))) {
						    if (rootPath.segmentCount() != 1) {
								name = rootPath.removeFirstSegments(1).makeRelative().toString();
						    }
						} else {
							name = rootPath.toString();
						}
						if (name != null && name.startsWith(SARLConfig.FOLDER_MAVEN_TEST_PREFIX)) {
							return Boolean.TRUE;
						}
					}
				} catch (JavaModelException e) {
					//
				}
			}
		}
		return null;
	}

}
