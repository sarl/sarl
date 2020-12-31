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

package io.sarl.eclipse.buildpath;

import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.JavaRuntime;

/** Classpath container dedicated to the SARL environment.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSARLBasedClasspathContainer implements IClasspathContainer {

	/** Comparator of classpath entries.
	 */
	protected static final Comparator<IClasspathEntry> CLASSPATH_ENTRY_COMPARATOR = (entry1, entry2) -> {
		if (entry1 == entry2) {
			return 0;
		}
		if (entry1 == null) {
			return 1;
		}
		if (entry2 == null) {
			return -1;
		}
		return entry1.getPath().toPortableString().compareTo(entry2.getPath().toPortableString());
	};

	private IClasspathEntry[] entries;

	private final IPath containerPath;

	private IJavaProject project;

	/** Constructor.
	 * @param containerPath the path of the container, e.g. the project.
	 * @param javaProject the reference to the containing Java project
	 * @since 0.12
	 */
	public AbstractSARLBasedClasspathContainer(IPath containerPath, IJavaProject javaProject) {
		this.containerPath = containerPath;
		this.project = javaProject;
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
			IVMInstall vm = JavaRuntime.getVMInstall(this.project);
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
	 */
	public final Set<String> getBundleDependencies() {
		final Set<String> bundles = new TreeSet<>();
		updateBundleList(bundles);
		return bundles;
	}

	/** Compute the entries of the container.
	 *
	 * <p>This is called from {@link #getBundleDependencies()}. It is defined for enabling subclasses
	 * to update the list of the bundle entries.
	 *
	 * @param entries the list of entries to update.
	 */
	protected abstract void updateBundleList(Set<String> entries);

	@Override
	public final synchronized IClasspathEntry[] getClasspathEntries() {
		if (this.entries == null) {
			final Set<IClasspathEntry> newEntries = new TreeSet<>(CLASSPATH_ENTRY_COMPARATOR);
			updateClasspathEntries(newEntries);
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
	 */
	protected abstract void updateClasspathEntries(Set<IClasspathEntry> entries);

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
