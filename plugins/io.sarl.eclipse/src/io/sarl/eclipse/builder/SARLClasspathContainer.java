/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.eclipse.builder;

import io.sarl.eclipse.util.PluginUtil;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.naming.NameNotFoundException;

import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.osgi.framework.Bundle;

/** Classpath container dedicated to the SARL environment.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLClasspathContainer implements IClasspathContainer {

	/** Identifier of the container.
	 */
	public static final IPath CONTAINER_ID = new Path(PluginUtil.PLUGIN_ID + ".launching.SARL_SUPPORT"); //$NON-NLS-1$

	/**
	 * Text that is describing the SARL libraries.
	 */
	public static final String DESCRIPTION = "SARL Libraries"; //$NON-NLS-1$

	/** Names of the reference libraries that are required to compile the SARL
	 * code and the generated Java code.
	 */
	public static final String[] SARL_REFERENCE_LIBRARIES = {
		"org.eclipse.xtext.xbase.lib", //$NON-NLS-1$
		"com.google.guava", //$NON-NLS-1$
		"io.sarl.lang.core", //$NON-NLS-1$
		"io.sarl.util", //$NON-NLS-1$
		"io.sarl.core", //$NON-NLS-1$
	};

	private IClasspathEntry[] entries;

	private IProject project;

	/**
	 * @param project - the project that container the SARL container.
	 */
	public SARLClasspathContainer(IProject project) {
		this.project = project;
	}

	private static IPath computeBundlePath(Bundle bundle, IProject project) {
		IPath bundlePath;
		try {
			URL bundleLocation = new URL(bundle.getLocation());
			URI bundleFile = new URI(bundleLocation.getFile());
			bundlePath = URIUtil.toPath(bundleFile);
		} catch (URISyntaxException | IOException e1) {
			throw new RuntimeException(e1);
		}

		// Ensure that the bundle path is absolute (mandatory for beeing a classpath entry)
		if (!bundlePath.isAbsolute()) {
			bundlePath = project.getLocation().append(bundlePath);
		}
		assert (bundlePath.isAbsolute()) : "The bundle path is not absolute: " + bundlePath; //$NON-NLS-1$
		return bundlePath;
	}

	private static IPath computeWorkspaceProjectPathForBundle(IPath bundlePath, IPath workspaceRoot) {
		// Determine the path from the output folders of the
		// Java projects in the current workspace.
		IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(bundlePath.lastSegment());
		IPath newBundlePath = null;
		try {
			if (project != null && project.hasNature(JavaCore.NATURE_ID)) {
				IJavaProject javaProject = JavaCore.create(project);
				newBundlePath = javaProject.getOutputLocation();
				if (newBundlePath != null) {
					newBundlePath = workspaceRoot.append(newBundlePath);
					// Test if the bundle path exists
					if (newBundlePath != null && !newBundlePath.toFile().exists()) {
						newBundlePath = null;
					}
				}
			}
		} catch (Exception e) {
			// Ignore the exceptions since they are not useful (hopefully)
		}

		if (newBundlePath != null) {
			assert (newBundlePath.isAbsolute()) : "The bundle path is not absolute: " + newBundlePath; //$NON-NLS-1$
			return newBundlePath;
		}

		// Detect the binary folder in the bundle.
		//
		// TODO: Replace by a dynamic detection based on Jdt API.
		File localFile = bundlePath.toFile();
		File binFolder = new File(new File(localFile, "target"), "classes"); //$NON-NLS-1$//$NON-NLS-2$
		if (binFolder.exists()) {
			newBundlePath = bundlePath.append("target").append("classes"); //$NON-NLS-1$//$NON-NLS-2$
		} else {
			binFolder = new File(localFile, "bin"); //$NON-NLS-1$
			if (binFolder.exists()) {
				newBundlePath = bundlePath.append("bin"); //$NON-NLS-1$
			} else {
				newBundlePath = bundlePath;
			}
		}

		assert (newBundlePath.isAbsolute()) : "The bundle path is not absolute: " + bundlePath; //$NON-NLS-1$
		return newBundlePath;
	}

	@Override
	public synchronized IClasspathEntry[] getClasspathEntries() {
		if (this.entries == null) {
			try {
				updateEntries();
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
		return this.entries;
	}

	/** Reset the container.
	 */
	synchronized void reset() {
		this.entries = null;
	}

	@Override
	public String getDescription() {
		//TODO: Use NLS
		return DESCRIPTION;
	}

	@Override
	public int getKind() {
		return K_SYSTEM;
	}

	@Override
	public IPath getPath() {
		return CONTAINER_ID;
	}

	private void updateEntries() throws Exception {
		List<IClasspathEntry> newEntries = new ArrayList<>();
		for (String referenceLibrary : SARL_REFERENCE_LIBRARIES) {
			// Retreive the bundle
			Bundle bundle = Platform.getBundle(referenceLibrary);
			if (bundle == null) {
				throw new NameNotFoundException("No bundle found for: " + referenceLibrary); //$NON-NLS-1$
			}

			IPath bundlePath = computeBundlePath(bundle, this.project);
			IPath binPath = computeWorkspaceProjectPathForBundle(
					bundlePath,
					this.project.getWorkspace().getRoot().getLocation());

			// Create the classpath entry
			IClasspathEntry classPathEntry = JavaCore.newLibraryEntry(
					binPath,
					null,
					null,
					true);
			newEntries.add(classPathEntry);
		}
		this.entries = newEntries.toArray(new IClasspathEntry[newEntries.size()]);
	}

}
