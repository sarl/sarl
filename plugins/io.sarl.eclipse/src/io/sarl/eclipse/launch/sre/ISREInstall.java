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

import java.io.IOException;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.launching.LibraryLocation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Represents a particular installation of a SARL runtime environment (SRE).
 * A SRE instance holds all parameters specific to a SRE installation.
 * SRE instances can be created and configured dynamically at run-time.
 * This is typically done by the user interactively in the UI.
 *
 * Rather than implementing this interface directly, it is strongly recommended that
 * clients subclass {@link AbstractSREInstall} to be insulated
 * from potential API additions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface ISREInstall extends Cloneable {

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
	 * @param id - the identifier for the copy.
	 * @return the copy.
	 * @see #clone()
	 */
	ISREInstall copy(String id);

	/** Returns the id for this SRE.
	 * The SRE id is not intended to be presented to users.
	 *
	 * @return the SRE identifier. Must not return <code>null</code>.
	 */
	String getId();

	/**
	 * Returns the display name of this SRE.
	 * The SRE name is intended to be presented to users.
	 * <p>
	 * This function replies the name of the SRE, or
	 * a default value if there is no name replied by {@link #getNameNoDefault()}.
	 * Consequently, this function never replies <code>null</code>.
	 *
	 * @return the display name of this SRE. May not return <code>null</code>.
	 * @see #getNameNoDefault()
	 */
	String getName();

	/**
	 * Returns the display name of this SRE by not
	 * any default value.
	 * <p>
	 * This function replies the name of the SRE but never
	 * any default value replied by {@link #getName()}.
	 * Consequently, this function could reply <code>null</code>.
	 *
	 * @return the display name of this SRE. May return <code>null</code>.
	 * @see #getNameNoDefault()
	 */
	String getNameNoDefault();

	/**
	 * Returns the fully qualified name of the main class for launching
	 * the runtime environment.
	 *
	 * @return the fully qualified name of the main class of the SRE.
	 *  Must not return <code>null</code>.
	 */
	String getMainClass();

	/**
	 * Returns the location of this runtime environment.
	 * The semantic of the location depends on the type of the ISREInstall.
	 *
	 * @return the location of the runtime environment. Must be never <code>null</code>.
	 */
	String getLocation();

	/**
	 * Returns the minimal version number of the SARL specification
	 * that is supported by the SRE.
	 *
	 * @return the minimal version number. Must not return <code>null</code>.
	 */
	String getMinimalSARLVersion();

	/**
	 * Returns the maximal version number of the SARL specification
	 * that is supported by the SRE.
	 *
	 * @return the maximal version number. Must not return <code>null</code>.
	 */
	String getMaximalSARLVersion();

	/**
	 * Returns the library locations of this ISREInstall.
	 *
	 * @return 	The library locations of this ISREInstall.
	 * Must not return <code>null</code>.
	 */
	LibraryLocation[] getLibraryLocations();

	/** Replies the arguments to pass to the virtual machine
	 * as program arguments for this SRE.
	 * The replied arguments may contain the main class
	 * replied by {@link #getMainClass()}.
	 *
	 * @return the program arguments to pass to the virtual machine for this SRE.
	 */
	String getProgramArguments();

	/** Replies the arguments to pass to the virtual machine
	 * as VM arguments for this SRE.
	 * The replied arguments may contain the main class
	 * replied by {@link #getMainClass()}.
	 *
	 * @return the VM arguments to pass to the virtual machine for this SRE.
	 */
	String getVMArguments();

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
	 * @param name - the display name of this SRE. May be <code>null</code>.
	 */
	void setName(String name);

	/**
	 * Change the library locations of this ISREInstall.
	 *
	 * @param libraries - The library locations of this ISREInstall.
	 * Must not be <code>null</code>.
	 */
	void setLibraryLocations(LibraryLocation... libraries);

	/**
	 * Change the minimal version number of the SARL specification
	 * that is supported by the SRE.
	 *
	 * @param version - the minimal version number. Must not be <code>null</code>.
	 */
	void setMinimalSARLVersion(String version);

	/**
	 * Change the maximal version number of the SARL specification
	 * that is supported by the SRE.
	 *
	 * @param version - the maximal version number. Must not be <code>null</code>.
	 */
	void setMaximalSARLVersion(String version);

	/**
	 * Change the fully qualified name of the main class for launching
	 * the runtime environment.
	 *
	 * @param mainClass - the fully qualified name of the main class of the SRE.
	 *  Must not be <code>null</code>.
	 */
	void setMainClass(String mainClass);

	/** Change the <code>Map</code> that contains String name/value pairs that
	 * represent VM-specific attributes for this SRE.
	 *
	 * @param attributes - the VM-specific attributes.
	 */
	void setVMSpecificAttributesMap(Map<String, String> attributes);

	/** Replies if the SRE is correctly installed.
	 *
	 * @return <code>true</code> if the SRE is correctly installed.
	 */
	boolean isValidInstallation();

	/** Replies the XML representation of this SRE installation.
	 * The XML string could be used to save this SRE installation in
	 * the preferences.
	 *
	 * @param document - the XML document is which the XML element is located.
	 * @param element - the XML node that must be the representation of this SRE installation.
	 * @throws IOException if cannot create the XML representation.
	 */
	void getAsXML(Document document, Element element) throws IOException;

	/** Set this SRE installation from the given XML representation.
	 *
	 * @param element - the XML node that must be the representation of this SRE installation.
	 * @throws IOException if cannot read the XML representation.
	 */
	void setFromXML(Element element) throws IOException;

	/** Validate the SRE.
	 *
	 * @return the validation status.
	 */
	IStatus validate();

	/**
	 * Whether this SRE should fire property change notifications.
	 *
	 * @param notify - if this SRE should fire property change notifications.
	 */
	void setNotify(boolean notify);

	/**
	 * Replies if this SRE should fire property change notifications.
	 *
	 * @return if this SRE should fire property change notifications.
	 */
	boolean getNotify();

}
