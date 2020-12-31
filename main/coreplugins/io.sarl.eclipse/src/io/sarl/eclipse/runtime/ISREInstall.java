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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Represents a particular installation of a SARL runtime environment (SRE).
 * A SRE instance holds all parameters specific to a SRE installation.
 * SRE instances can be created and configured dynamically at run-time.
 * This is typically done by the user interactively in the UI.
 *
 * <p>Rather than implementing this interface directly, it is strongly recommended that
 * clients subclass {@link AbstractSREInstall} to be insulated
 * from potential API additions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface ISREInstall extends Cloneable {

	/** Error code for the status that corresponds to an invalid state of the ISREInstall.
	 */
	int CODE_GENERAL = 1;

	/** Error code related to the library locations.
	 */
	int CODE_LIBRARY_LOCATION = 2;

	/** Error code related to the required SARL version.
	 */
	int CODE_SARL_VERSION = 4;

	/** Error code related to the main class name.
	 */
	int CODE_MAIN_CLASS = 8;

	/** Error code related to the name.
	 */
	int CODE_NAME = 16;

	/** Error code related to the source file.
	 */
	int CODE_SOURCE = 32;

	/** Clone this SRE.
	 * The clone has the same Id as the cloned object.
	 *
	 * @return the clone.
	 * @see #copy(String)
	 */
	ISREInstall clone();

	/** Copy this SRE.
	 * The copy has not the same Id as the copied object.
	 *
	 * @param id the identifier for the copy.
	 * @return the copy.
	 * @see #clone()
	 */
	ISREInstall copy(String id);

	/** Returns the id for this SRE.
	 * The SRE id is not intended to be presented to users.
	 *
	 * @return the SRE identifier. Must not return {@code null}.
	 */
	String getId();

	/**
	 * Returns the display name of this SRE.
	 * The SRE name is intended to be presented to users.
	 *
	 * <p>This function replies the name of the SRE, or
	 * a default value if there is no name replied by {@link #getNameNoDefault()}.
	 * Consequently, this function never replies {@code null}.
	 *
	 * @return the display name of this SRE. May not return {@code null}.
	 * @see #getNameNoDefault()
	 */
	String getName();

	/**
	 * Returns the display name of this SRE without considering to reply a default value for the name.
	 *
	 * <p>This function replies the name of the SRE but never
	 * any default value replied by {@link #getName()}.
	 * Consequently, this function could reply {@code null}.
	 *
	 * @return the display name of this SRE. May return {@code null}.
	 * @see #getName()
	 */
	String getNameNoDefault();

	/**
	 * Returns the fully qualified name of the main class for launching
	 * the runtime environment.
	 *
	 * @return the fully qualified name of the main class of the SRE.
	 *      Must not return {@code null}.
	 */
	String getMainClass();

	/**
	 * Returns the fully qualified name of the SRE bootstrap associated to this SRE.
	 *
	 * @return the fully qualified name of the bootstrap, or {@code null} if none.
	 */
	String getBootstrap();

	/**
	 * Returns the location of this runtime environment.
	 * The semantic of the location depends on the type of the ISREInstall.
	 *
	 * @return the location of the runtime environment. Must be never {@code null}.
	 */
	String getLocation();

	/**
	 * Returns the minimal version number of the SARL specification
	 * that is supported by the SRE.
	 *
	 * @return the minimal version number. Must not return {@code null}.
	 */
	String getMinimalSARLVersion();

	/**
	 * Returns the maximal version number of the SARL specification
	 * that is supported by the SRE.
	 *
	 * @return the maximal version number. Must not return {@code null}.
	 */
	String getMaximalSARLVersion();

	/**
	 * Returns the library locations of this ISREInstall.
	 *
	 * @return 	The library locations of this ISREInstall.
	 *     Must not return {@code null}.
	 */
	List<IRuntimeClasspathEntry> getClassPathEntries();

	/** Replies the path o the preferred container.
	 *
	 * @return the preferred container's path, or {@code null}.
	 * @since 0.7
	 */
	IPath getPreferredClassPathContainerPath();

	/** Replies the available command line options for this SRE.
	 *
	 * <p>The replied map describes the CLI options to use in specific use cases.
	 * The keys are defined in the {@link SREConstants SRE constants}, where
	 * the key names are started by <code>MANIFEST_CLI_</code>.
	 *
	 * @return the program arguments to pass to the SRE (not the Java virtual machine).
	 * @see #getJVMArguments()
	 */
	Map<String, String> getAvailableCommandLineOptions();

	/** Replies the arguments to pass to the SRE instance.
	 *
	 * @return the arguments for the SRE.
	 */
	String getSREArguments();

	/** Replies the arguments to pass to the virtual machine
	 * as VM arguments for this SRE.
	 * The replied arguments could contain the main class
	 * replied by {@link #getMainClass()}.
	 *
	 * @return the VM arguments to pass to the virtual machine for this SRE.
	 * @see #getSREArguments()
	 */
	String getJVMArguments();

	/** Replies the <code>Map</code> that contains String name/value pairs that
	 * represent VM-specific attributes for this SRE.
	 *
	 * @return the VM-specific attributes.
	 */
	Map<String, String> getVMSpecificAttributesMap();

	/**
	 * Change the display name of this SRE.
	 * The SRE name is intended to be presented to users.
	 *
	 * @param name the display name of this SRE. May be {@code null}.
	 */
	void setName(String name);

	/**
	 * Change the library locations of this ISREInstall.
	 *
	 * @param libraries The library locations of this ISREInstall.
	 *     Must not be {@code null}.
	 */
	void setClassPathEntries(List<IRuntimeClasspathEntry> libraries);

	/**
	 * Change the library locations of this ISREInstall.
	 *
	 * @param libraries The library locations of this ISREInstall.
	 *     Must not be {@code null}.
	 */
	default void setClassPathEntries(Iterable<IRuntimeClasspathEntry> libraries) {
		final List<IRuntimeClasspathEntry> list;
		if (libraries == null) {
			list = null;
		} else if (libraries instanceof List<?>) {
			list = (List<IRuntimeClasspathEntry>) libraries;
		} else {
			final Set<String> added = new TreeSet<>();
			list = new ArrayList<>();
			for (final IRuntimeClasspathEntry cpe : libraries) {
				final String location = cpe.getLocation();
				assert location != null;
				if (added.add(location)) {
					list.add(cpe);
				}
			}
		}
		setClassPathEntries(list);
	}

	/**
	 * Change the minimal version number of the SARL specification
	 * that is supported by the SRE.
	 *
	 * @param version the minimal version number. Must not be {@code null}.
	 */
	void setMinimalSARLVersion(String version);

	/**
	 * Change the maximal version number of the SARL specification
	 * that is supported by the SRE.
	 *
	 * @param version the maximal version number. Must not be {@code null}.
	 */
	void setMaximalSARLVersion(String version);

	/**
	 * Change the fully qualified name of the main class for launching
	 * the runtime environment.
	 *
	 * @param mainClass the fully qualified name of the main class of the SRE.
	 *      Must not be {@code null}.
	 */
	void setMainClass(String mainClass);

	/**
	 * Change the fully qualified name of the bootstrap.
	 *
	 * @param bootstrap the fully qualified name of the bootstrap, or {@code null}.
	 */
	void setBootstrap(String bootstrap);

	/** Change the <code>Map</code> that contains String name/value pairs that
	 * represent VM-specific attributes for this SRE.
	 *
	 * @param attributes the VM-specific attributes.
	 */
	void setVMSpecificAttributesMap(Map<String, String> attributes);

	/** Replies the XML representation of this SRE installation.
	 * The XML string could be used to save this SRE installation in
	 * the preferences.
	 *
	 * @param document the XML document is which the XML element is located.
	 * @param element the XML node that must be the representation of this SRE installation.
	 * @throws IOException if cannot create the XML representation.
	 */
	void getAsXML(Document document, Element element) throws IOException;

	/** Set this SRE installation from the given XML representation.
	 *
	 * @param element the XML node that must be the representation of this SRE installation.
	 * @throws IOException if cannot read the XML representation.
	 */
	void setFromXML(Element element) throws IOException;

	/** Validate the SRE.
	 * The validation does not ignore any invalidity cause.
	 *
	 * @return the validation status.
	 * @see #getValidity(int)
	 */
	default IStatus getValidity() {
		return getValidity(0);
	}

	/** Validate the SRE.
	 * The validation does not ignore any invalidity cause.
	 *
	 * @param ignoreCauses a set of bits that indicates the invalidity causes to ignore.
	 * @return the validation status.
	 * @see #getValidity()
	 */
	IStatus getValidity(int ignoreCauses);

	/** Force the computation of the installation validity.
	 *
	 * @return the validity status.
	 */
	IStatus revalidate();

	/**
	 * Whether this SRE should fire property change notifications.
	 *
	 * @param notify if this SRE should fire property change notifications.
	 */
	void setNotify(boolean notify);

	/**
	 * Replies if this SRE should fire property change notifications.
	 *
	 * @return if this SRE should fire property change notifications.
	 */
	boolean getNotify();

}
