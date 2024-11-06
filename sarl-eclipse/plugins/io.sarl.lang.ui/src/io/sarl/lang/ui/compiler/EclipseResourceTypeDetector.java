/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.core.IJavaElement;
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
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 * @since 0.8
 * @see SARLConfig#FOLDER_TEST_SOURCE_SARL
 * @see SARLConfig#FOLDER_INTEGRATION_TEST_SOURCE_SARL
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
	protected static Boolean isJavaOrMavenTestResource(Resource resource) {
		final var uri = resource.getURI();
		if (uri.isPlatformResource()) {
			final var platformString = uri.toPlatformString(true);
			final var iresource = ResourcesPlugin.getWorkspace().getRoot().findMember(platformString);
			if (iresource != null) {
				final var project = iresource.getProject();
				final var javaProject = JavaCore.create(project);
				try {
					final var packageFragment = javaProject.findPackageFragment(iresource.getParent().getFullPath());
					final var root = (IPackageFragmentRoot) packageFragment.getAncestor(IJavaElement.PACKAGE_FRAGMENT_ROOT);
					if (root != null) {
						final var rootPath = root.getPath();
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
						if (name != null
								&& (name.startsWith(SARLConfig.FOLDER_MAVEN_TEST_PREFIX)
									|| name.startsWith(SARLConfig.FOLDER_MAVEN_INTEGRATION_TEST_PREFIX))) {
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
