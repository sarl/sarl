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
package io.sarl.eclipse.launch.sre;

import io.sarl.eclipse.util.PluginUtil;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.launching.LibraryLocation;
import org.eclipse.jdt.launching.PropertyChangeEvent;
import org.osgi.framework.Version;

/**
 * Abstract implementation of a SRE install.
 * <p>
 * Clients implementing SRE installs must subclass this class.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSREInstall implements ISREInstall {

	/** Empty string.
	 */
	protected static final String EMPTY_STRING = ""; //$NON-NLS-1$

	private String id;
	private String name;
	private String minimalSarlVersion;
	private String maximalSarlVersion;
	private String mainClass;
	private LibraryLocation[] libraryLocations;
	private Map<String, String> attributeMap;
	private boolean dirty = true;
	// whether change events should be fired
	private boolean notify = true;

	/** Construct a SRE installation.
	 *
	 * @param id - the identifier of this SRE installation.
	 */
	public AbstractSREInstall(String id) {
		this.id = id;
	}

	/** Ensure that the given value is not <code>null</code>
	 * but empty instead.
	 *
	 * @param value - the value to check.
	 * @return the correct value.
	 */
	protected static String unnullify(String value) {
		if (value == null) {
			return EMPTY_STRING;
		}
		return value;
	}

	@Override
	public AbstractSREInstall clone() {
		try {
			AbstractSREInstall clone = (AbstractSREInstall) super.clone();
			clone.attributeMap = this.attributeMap == null ? null
					: new HashMap<>(this.attributeMap);
			if (this.libraryLocations != null) {
				clone.libraryLocations = this.libraryLocations.clone();
			}
			return clone;
		} catch (CloneNotSupportedException e) {
			throw new Error(e);
		}
	}

	@Override
	public ISREInstall copy(String id) {
		AbstractSREInstall copy = clone();
		copy.id = id;
		return copy;
	}

	@Override
	public String toString() {
		return getName();
	}

	@Override
	public void setNotify(boolean notify) {
		this.notify = notify;
	}

	@Override
	public boolean getNotify() {
		return this.notify;
	}

	/** Replies if this installation has one of its field unresolved.
	 *
	 * @return <code>true</code> if one field has a too old value.
	 */
	protected boolean isDirty() {
		return this.dirty;
	}

	/** Set if this installation has one of its field unresolved.
	 *
	 * @param dirty - <code>true</code> if one field has a too old value.
	 */
	protected void setDirty(boolean dirty) {
		this.dirty = dirty;
	}

	@Override
	public IStatus validate() {
		try {
			setDirty(false);
			resolveDirtyFields(false);
			return PluginUtil.createOkStatus();
		} catch (Throwable e) {
			return PluginUtil.createStatus(IStatus.ERROR, e);
		}
	}

	/** Invoked when the JAR file has changed for updating the other
	 * fields.
	 *
	 * @param forceSettings - indicates if the fields of this SRE must be set.
	 */
	protected abstract void resolveDirtyFields(boolean forceSettings);

	@Override
	public String getId() {
		return this.id;
	}

	@Override
	public String getName() {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		return this.name;
	}

	@Override
	public String getMinimalSARLVersion() {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		return this.minimalSarlVersion;
	}

	@Override
	public String getMaximalSARLVersion() {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		return this.maximalSarlVersion;
	}

	@Override
	public String getMainClass() {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		return this.mainClass;
	}

	@Override
	public LibraryLocation[] getLibraryLocations() {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		if (this.libraryLocations == null) {
			return new LibraryLocation[0];
		}
		return this.libraryLocations;
	}

	@Override
	public Map<String, String> getVMSpecificAttributesMap() {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		if (this.attributeMap == null) {
			return Collections.emptyMap();
		}
		return Collections.unmodifiableMap(this.attributeMap);
	}

	@Override
	public void setName(String name) {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		if (!PluginUtil.equalsString(name, this.name)) {
			PropertyChangeEvent event = new PropertyChangeEvent(
					this, ISREInstallChangedListener.PROPERTY_NAME, this.name, name);
			this.name = unnullify(name);
			if (this.notify) {
				SARLRuntime.fireSREChanged(event);
			}
		}
	}

	/**
	 * Change the library locations of this ISREInstall.
	 *
	 * @param libraries - The library locations of this ISREInstall.
	 * Must not be <code>null</code>.
	 */
	@Override
	public void setLibraryLocations(LibraryLocation... libraries) {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		if ((libraries == null && this.libraryLocations != null)
				|| (libraries != null
				&& (this.libraryLocations == null
				|| Arrays.equals(libraries, this.libraryLocations)))) {
			PropertyChangeEvent event = new PropertyChangeEvent(
					this, ISREInstallChangedListener.PROPERTY_LIBRARY_LOCATIONS,
					this.libraryLocations, libraries);
			this.libraryLocations = libraries;
			if (this.notify) {
				SARLRuntime.fireSREChanged(event);
			}
		}
	}

	@Override
	public void setMinimalSARLVersion(String version) {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		String newVersion = version;
		if (newVersion != null) {
			// Check version number
			try {
				Version.parseVersion(newVersion);
			} catch (Throwable _) {
				return;
			}
		}
		if (!PluginUtil.equalsString(newVersion, this.minimalSarlVersion)) {
			PropertyChangeEvent event = new PropertyChangeEvent(
					this, ISREInstallChangedListener.PROPERTY_MINIMAL_SARL_VERSION,
					this.minimalSarlVersion, newVersion);
			this.minimalSarlVersion = unnullify(newVersion);
			if (this.notify) {
				SARLRuntime.fireSREChanged(event);
			}
		}
	}

	@Override
	public void setMaximalSARLVersion(String version) {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		String newVersion = version;
		if (newVersion != null) {
			// Check version number
			try {
				Version.parseVersion(newVersion);
			} catch (Throwable _) {
				return;
			}
		}
		if (!PluginUtil.equalsString(newVersion, this.maximalSarlVersion)) {
			PropertyChangeEvent event = new PropertyChangeEvent(
					this, ISREInstallChangedListener.PROPERTY_MAXIMAL_SARL_VERSION,
					this.maximalSarlVersion, newVersion);
			this.maximalSarlVersion = unnullify(newVersion);
			if (this.notify) {
				SARLRuntime.fireSREChanged(event);
			}
		}
	}

	@Override
	public void setVMSpecificAttributesMap(Map<String, String> attributes) {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		if (!PluginUtil.equals(attributes, this.attributeMap)) {
			Map<String , String> o;
			Map<String , String> n;
			if (attributes == null) {
				n = Collections.emptyMap();
			} else {
				n = attributes;
			}
			if (this.attributeMap == null) {
				o = Collections.emptyMap();
			} else {
				o = this.attributeMap;
			}
			PropertyChangeEvent event = new PropertyChangeEvent(
					this, ISREInstallChangedListener.PROPERTY_VM_ATTRIBUTES,
					o, n);
			this.attributeMap = attributes;
			if (this.notify) {
				SARLRuntime.fireSREChanged(event);
			}
		}
	}

	@Override
	public void setMainClass(String mainClass) {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		if (!PluginUtil.equalsString(mainClass, this.mainClass)) {
			PropertyChangeEvent event = new PropertyChangeEvent(
					this, ISREInstallChangedListener.PROPERTY_NAME, this.mainClass, mainClass);
			this.mainClass = unnullify(mainClass);
			if (this.notify) {
				SARLRuntime.fireSREChanged(event);
			}
		}
	}

	@Override
	public boolean isValidInstallation() {
		try {
			String mainClass = getMainClass();
			if (mainClass == null || EMPTY_STRING.equals(mainClass)) {
				return false;
			}
			String name = getName();
			if (name == null || EMPTY_STRING.equals(name)) {
				return false;
			}
			LibraryLocation[] locations = getLibraryLocations();
			return locations != null && locations.length > 0;
		} catch (Throwable _) {
			return false;
		}
	}

}
