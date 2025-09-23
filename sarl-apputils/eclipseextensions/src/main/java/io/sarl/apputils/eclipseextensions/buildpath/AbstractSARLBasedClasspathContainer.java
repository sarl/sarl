/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.apputils.eclipseextensions.buildpath;

import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.launching.JavaRuntime;
import org.osgi.framework.BundleException;

/** Classpath container dedicated to the SARL environment.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public abstract class AbstractSARLBasedClasspathContainer implements IClasspathContainer {

	private IClasspathEntry[] entries;

	private final IPath containerPath;

	private IJavaProject project;

	private final Comparator<IClasspathEntry> entryComparator;

	/** Constructor.
	 * @param containerPath the path of the container, e.g. the project.
	 * @param javaProject the reference to the containing Java project
	 * @param entryComparator the comparator of classpath entries to use.
	 * @since 0.15
	 */
	public AbstractSARLBasedClasspathContainer(IPath containerPath, IJavaProject javaProject, Comparator<IClasspathEntry> entryComparator) {
		this.containerPath = containerPath;
		this.project = javaProject;
		this.entryComparator = entryComparator == null ? NameSegmentClasspathEntryComparator.getSingleton() : entryComparator;
	}

	/** Replies the classpath entry that must be used by this object.
	 *
	 * @return the comparator, never {@code null}.
	 * @since 0.15
	 */
	protected Comparator<IClasspathEntry> getClasspathEntryComparator() {
		return this.entryComparator;
	}

	/** Replies if the associated project is module or not.
	 *
	 * @return {@code true} if the project is module; otherwise {@code false}.
	 */
	protected boolean isModular() {
		try {
			if (JavaRuntime.isModularProject(this.project)) {
				return true;
			}
			var vm = JavaRuntime.getVMInstall(this.project);
			if (vm == null) {
				vm = JavaRuntime.getDefaultVMInstall();
			}
			return JavaRuntime.isModularJava(vm);
		} catch (Throwable exception) {
			//
		}
		return false;
	}

	/** Replies the list of the symbolic names of the bundle dependencies.
	 *
	 * @return the bundle symbolic names of the dependencies.
	 * @throws BundleException if a bundle cannot be resolved.
	 */
	public final Set<String> getBundleDependencies() throws BundleException {
		final var bundles = new TreeSet<String>();
		updateBundleList(bundles);
		return bundles;
	}

	/** Compute the entries of the container.
	 *
	 * <p>This is called from {@link #getBundleDependencies()}. It is defined for enabling subclasses
	 * to update the list of the bundle entries.
	 *
	 * @param entries the list of entries to update.
	 * @throws BundleException if a bundle cannot be resolved.
	 */
	protected abstract void updateBundleList(Set<String> entries) throws BundleException;

	@Override
	public final synchronized IClasspathEntry[] getClasspathEntries() {
		if (this.entries == null) {
			final var newEntries = new TreeSet<>(getClasspathEntryComparator());
			try {
				updateClasspathEntries(newEntries);
			} catch (BundleException ex) {
				throw new RuntimeException(ex);
			}
			this.entries = newEntries.toArray(new IClasspathEntry[newEntries.size()]);
		}
		return this.entries;
	}

	/** Compute the list of classpath entries for the current container.
	 *
	 * <p>This is called from {@link #getClasspathEntries()}. It is defined for enabling subclasses
	 * to update the list of the classpath entries.
	 *
	 * @param entries the list of entries to update.
	 * @throws BundleException if a bundle cannot be resolved.
	 */
	protected abstract void updateClasspathEntries(Set<IClasspathEntry> entries) throws BundleException;

	/** Reset the container.
	 */
	public synchronized void reset() {
		this.entries = null;
	}

	@Override
	public IPath getPath() {
		return this.containerPath;
	}

}
