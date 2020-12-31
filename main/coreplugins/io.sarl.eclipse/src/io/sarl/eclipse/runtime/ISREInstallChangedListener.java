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

package io.sarl.eclipse.runtime;

import java.util.EventListener;

import org.eclipse.jdt.launching.PropertyChangeEvent;

import io.sarl.eclipse.SARLEclipsePlugin;

/**
 * A SRE install changed listener is notified when
 * the workspace default SRE install changes, or when an attribute of
 * a specific SRE install changes.
 * Listeners register with <code>SARLRuntime</code>.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface ISREInstallChangedListener extends EventListener {

	/**
	 * Property constant indicating the name associated
	 * with a SRE install has changed.
	 */
	String PROPERTY_NAME = SARLEclipsePlugin.PLUGIN_ID + ".PROPERTY_NAME"; //$NON-NLS-1$

	/**
	 * Property constant indicating the minimal SARL version associated
	 * with a SRE install has changed.
	 */
	String PROPERTY_MINIMAL_SARL_VERSION = SARLEclipsePlugin.PLUGIN_ID
			+ ".PROPERTY_MINIMAL_SARL_VERSION"; //$NON-NLS-1$

	/**
	 * Property constant indicating the maximal SARL version associated
	 * with a SRE install has changed.
	 */
	String PROPERTY_MAXIMAL_SARL_VERSION = SARLEclipsePlugin.PLUGIN_ID
			+ ".PROPERTY_MAXIMAL_SARL_VERSION"; //$NON-NLS-1$

	/**
	 * Property constant indicating the libraries in the SRE.
	 */
	String PROPERTY_LIBRARY_LOCATIONS = SARLEclipsePlugin.PLUGIN_ID
			+ ".PROPERTY_LIBRARY_LOCATIONS"; //$NON-NLS-1$

	/**
	 * Property constant indicating the VM-specific attributes.
	 */
	String PROPERTY_VM_ATTRIBUTES = SARLEclipsePlugin.PLUGIN_ID
			+ ".PROPERTY_VM_ATTRIBUTES"; //$NON-NLS-1$

	/**
	 * Property constant indicating the associated JAR file.
	 */
	String PROPERTY_JAR_FILE = SARLEclipsePlugin.PLUGIN_ID
			+ ".PROPERTY_JAR_FILE"; //$NON-NLS-1$

	/**
	 * Property constant indicating the arguments for the VM.
	 */
	String PROPERTY_VM_ARGUMENTS = SARLEclipsePlugin.PLUGIN_ID
			+ ".PROPERTY_VM_ARGUMENTS"; //$NON-NLS-1$

	/**
	 * Property constant indicating the arguments for the program.
	 */
	String PROPERTY_PROGRAM_ARGUMENTS = SARLEclipsePlugin.PLUGIN_ID
			+ ".PROPERTY_PROGRAM_ARGUMENTS"; //$NON-NLS-1$

	/**
	 * Property constant indicating if a SRE is standalone.
	 */
	String PROPERTY_STANDALONE_SRE = SARLEclipsePlugin.PLUGIN_ID
			+ ".STANDALONE_SRE"; //$NON-NLS-1$

	/**
	 * Property constant indicating the main class associated
	 * with a SRE install has changed.
	 */
	String PROPERTY_MAINCLASS = SARLEclipsePlugin.PLUGIN_ID + ".PROPERTY_MAINCLASS"; //$NON-NLS-1$

	/**
	 * Property constant indicating the bootstrap associated
	 * with a SRE install has changed.
	 */
	String PROPERTY_BOOTSTRAP = SARLEclipsePlugin.PLUGIN_ID + ".PROPERTY_BOOTSTRAP"; //$NON-NLS-1$

	/**
	 * Notification that the workspace default SRE install
	 * has changed.
	 *
	 * @param previous the SRE install that was previously assigned
	 * 	   to the workspace, possibly {@code null}
	 * @param current the SRE install that is currently assigned to the
	 * 	   workspace, possibly {@code null}
	 */
	void defaultSREInstallChanged(ISREInstall previous, ISREInstall current);

	/**
	 * Notification that a property of a SRE install has changed.
	 *
	 * @param event event describing the change. The SRE that has changed
	 * 	   is the source object associated with the event.
	 */
	void sreChanged(PropertyChangeEvent event);

	/**
	 * Notification that a SRE has been created.
	 *
	 * @param sre the SRE that has been created.
	 */
	void sreAdded(ISREInstall sre);

	/**
	 * Notification that a SRE has been disposed.
	 *
	 * @param sre the SRE that has been disposed.
	 */
	void sreRemoved(ISREInstall sre);

}
