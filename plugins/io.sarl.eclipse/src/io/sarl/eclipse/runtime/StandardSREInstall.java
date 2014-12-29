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
package io.sarl.eclipse.runtime;

import io.sarl.eclipse.SARLEclipsePlugin;

import java.io.File;
import java.io.IOException;
import java.lang.ref.SoftReference;
import java.net.URL;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.jar.Attributes;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.launching.LibraryLocation;
import org.eclipse.jdt.launching.PropertyChangeEvent;
import org.osgi.framework.Version;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Maps;


/**
 * Standard SRE install.
 * <p>
 * The standard SRE install assumes: <ul>
 * <li>The SRE is based on a single JAR file.</li>
 * <li>The main class of the SRE is defined in the Manifest field <code>"Main-Class"</code>.</li>
 * <li>The Manifest contains a section named <code>"SARL-Runtime-Environment"</code>. This
 *     section contains the following entries: <ul>
 *     <li>The version number of the SARL sepcifications that are supported by the SRE is
 *     defined in the Manifest field <code>"SARL-Spec-Version"</code>.</li>
 *     <li>The name of the SRE may be given by the field <code>"Name"</code>.</li>
 *     <li>The VM arguments of the SRE may be given by the field <code>"VM-Arguments"</code>.</li>
 *     <li>The program arguments of the SRE may be given by the field <code>"Program-Arguments"</code>.</li>
 *     <li>The command line option for avoiding the logo is given by the field <code>"CLI-Hide-Logo"</code>.</li>
 *     <li>The command line option for displaying the logo is given by the field <code>"CLI-Show-Logo"</code>.</li>
 *     <li>The command line option for displaying the information messages is given by the
 *     		field<code>"CLI-Show-Info"</code>.</li>
 *     <li>The command line option for hiding the information messages is given by the
 *     		field<code>"CLI-Hide-Info"</code>.</li>
 *     <li>The command line option for using the default root context id is given by the
 *     		field<code>"CLI-Default-Context-ID"</code>.</li>
 *     <li>The command line option for using the random root context id is given by the
 *     		field<code>"CLI-Random-Context-ID"</code>.</li>
 *     <li>The command line option for using the agent-type-based root context id is given by the
 *     		field<code>"CLI-BootAgent-Context-ID"</code>.</li>
 *     </ul></li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class StandardSREInstall extends AbstractSREInstall {

	private IPath jarFile;
	private String vmArguments = SARLEclipsePlugin.EMPTY_STRING;
	private String programArguments = SARLEclipsePlugin.EMPTY_STRING;

	private String cliLogoOff;
	private String cliLogoOn;
	private String cliShowInfo;
	private String cliHideInfo;
	private String cliDefaultContextID;
	private String cliRandomContextID;
	private String cliBootAgentContextID;
	private String cliSreOffline;

	private String manifestMainClass;
	private String manifestName;

	private transient SoftReference<Map<String, String>> optionBuffer;

	/** Construct a SRE installation.
	 *
	 * @param id - the identifier of this SRE installation.
	 */
	public StandardSREInstall(String id) {
		super(id);
	}

	@Override
	public StandardSREInstall clone() {
		StandardSREInstall clone = (StandardSREInstall) super.clone();
		clone.jarFile = this.jarFile == null ? null
				: Path.fromPortableString(clone.jarFile.toPortableString());
		return clone;
	}

	@Override
	public StandardSREInstall copy(String id) {
		return (StandardSREInstall) super.copy(id);
	}

	/** Replies the path to the JAR file that is supporting this SRE installation.
	 *
	 * @return the path to the JAR file. Must not be <code>null</code>.
	 */
	public IPath getJarFile() {
		return this.jarFile;
	}

	/** Change the path to the JAR file that is supporting this SRE installation.
	 *
	 * @param jarFile - the path to the JAR file. Must not be <code>null</code>.
	 */
	public void setJarFile(IPath jarFile) {
		if (!Objects.equal(jarFile, this.jarFile)) {
			PropertyChangeEvent event = new PropertyChangeEvent(
					this, ISREInstallChangedListener.PROPERTY_JAR_FILE,
					this.jarFile, jarFile);
			this.jarFile = jarFile;
			setDirty(true);
			if (getNotify()) {
				SARLRuntime.fireSREChanged(event);
			}
		}
	}

	@Override
	public String getLocation() {
		IPath jarFile = getJarFile();
		if (jarFile == null) {
			return getName();
		}
		return jarFile.toOSString();
	}

	@Override
	public String getName() {
		String n = super.getName();
		if (Strings.isNullOrEmpty(n)) {
			IPath p = getJarFile();
			if (p != null) {
				n = p.removeFileExtension().lastSegment();
			} else {
				n = getId();
			}
		}
		return n;
	}

	@Override
	public String getNameNoDefault() {
		return super.getName();
	}

	@Override
	protected void resolveDirtyFields(boolean forceSettings) {
		if (this.jarFile != null) {
			try (JarFile jFile = new JarFile(this.jarFile.toFile())) {
				Manifest manifest = jFile.getManifest();
				//
				// Main class
				this.manifestMainClass = manifest.getMainAttributes().getValue(SREConstants.MANIFEST_MAIN_CLASS);
				if (Strings.isNullOrEmpty(this.manifestMainClass)) {
					throw new SREException(Messages.StandardSREInstall_0 + getId());
				}
				if (forceSettings || Strings.isNullOrEmpty(getMainClass())) {
					setMainClass(this.manifestMainClass);
				}
				// Get SARL section:
				Attributes sarlSection = manifest.getAttributes(SREConstants.MANIFEST_SECTION_SRE);
				if (sarlSection == null) {
					throw new SREException(Messages.StandardSREInstall_1);
				}
				//
				// Stand-alone SRE
				String strStandalone = sarlSection.getValue(SREConstants.MANIFEST_STANDALONE_SRE);
				boolean isStandalone = false;
				if (strStandalone != null && !strStandalone.isEmpty()) {
					isStandalone = Boolean.parseBoolean(strStandalone);
				}
				setStandalone(isStandalone);
				//
				// SARL version
				String sarlVersion = sarlSection.getValue(SREConstants.MANIFEST_SARL_SPEC_VERSION);
				String minVersion = null;
				String maxVersion = null;
				if (!Strings.isNullOrEmpty(sarlVersion)) {
					try {
						Version sarlVer = Version.parseVersion(sarlVersion);
						if (sarlVer != null) {
							minVersion = new Version(sarlVer.getMajor(), sarlVer.getMinor(), 0).toString();
							maxVersion = new Version(sarlVer.getMajor(), sarlVer.getMinor() + 1, 0).toString();
						}
					} catch (Throwable _) {
						//
					}
				}
				if (forceSettings || Strings.isNullOrEmpty(getMinimalSARLVersion())) {
					setMinimalSARLVersion(minVersion);
				}
				if (forceSettings || Strings.isNullOrEmpty(getMaximalSARLVersion())) {
					setMaximalSARLVersion(maxVersion);
				}
				//
				// SRE Name
				this.manifestName = Strings.nullToEmpty(sarlSection.getValue(SREConstants.MANIFEST_SRE_NAME));
				if (forceSettings || Strings.isNullOrEmpty(getNameNoDefault())) {
					setName(this.manifestName);
				}
				//
				// VM arguments
				String vmArgs = Strings.nullToEmpty(sarlSection.getValue(SREConstants.MANIFEST_VM_ARGUMENTS));
				if (!this.vmArguments.equals(vmArgs)) {
					PropertyChangeEvent event = new PropertyChangeEvent(
							this, ISREInstallChangedListener.PROPERTY_VM_ARGUMENTS,
							this.vmArguments, Strings.nullToEmpty(vmArgs));
					this.vmArguments = vmArgs;
					if (getNotify()) {
						SARLRuntime.fireSREChanged(event);
					}
				}
				//
				// VM-specific attributes
				setVMSpecificAttributesMap(null);
				//
				// Specific CLI Options
				this.optionBuffer = null;
				this.cliLogoOn = sarlSection.getValue(SREConstants.MANIFEST_CLI_SHOW_LOGO);
				this.cliLogoOff = sarlSection.getValue(SREConstants.MANIFEST_CLI_HIDE_LOGO);
				this.cliShowInfo = sarlSection.getValue(SREConstants.MANIFEST_CLI_SHOW_INFO);
				this.cliHideInfo = sarlSection.getValue(SREConstants.MANIFEST_CLI_HIDE_INFO);
				this.cliDefaultContextID = sarlSection.getValue(SREConstants.MANIFEST_CLI_DEFAULT_CONTEXT_ID);
				this.cliRandomContextID = sarlSection.getValue(SREConstants.MANIFEST_CLI_RANDOM_CONTEXT_ID);
				this.cliBootAgentContextID = sarlSection.getValue(SREConstants.MANIFEST_CLI_BOOT_AGENT_CONTEXT_ID);
				this.cliSreOffline = sarlSection.getValue(SREConstants.MANIFEST_CLI_SRE_OFFLINE);
				//
				// Program arguments
				String programArgs = Strings.nullToEmpty(sarlSection.getValue(SREConstants.MANIFEST_PROGRAM_ARGUMENTS));
				if (!this.programArguments.equals(programArgs)) {
					PropertyChangeEvent event = new PropertyChangeEvent(
							this, ISREInstallChangedListener.PROPERTY_PROGRAM_ARGUMENTS,
							this.programArguments, programArgs);
					this.programArguments = programArgs;
					if (getNotify()) {
						SARLRuntime.fireSREChanged(event);
					}
				}
				//
				// Library Location
				if (forceSettings || getLibraryLocations().length == 0) {
					List<LibraryLocation> classPath = new ArrayList<>();
					classPath.add(new LibraryLocation(this.jarFile, Path.EMPTY, Path.EMPTY));
					String classPathStr = manifest.getMainAttributes().getValue(SREConstants.MANIFEST_CLASS_PATH);
					if (!Strings.isNullOrEmpty(classPathStr)) {
						for (String cpElement : classPathStr.split(Pattern.quote(":"))) { //$NON-NLS-1$
							IPath p = Path.fromPortableString(cpElement);
							classPath.add(new LibraryLocation(p, Path.EMPTY, Path.EMPTY));
						}
					}
					LibraryLocation[] locations = classPath.toArray(new LibraryLocation[classPath.size()]);
					setLibraryLocations(locations);
				}
			} catch (SREException e) {
				throw e;
			} catch (Throwable e) {
				throw new SREException(e);
			}
		} else {
			throw new SREException(Messages.StandardSREInstall_2);
		}
	}

	@Override
	public String getProgramArguments() {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}

		StringBuilder cliArguments = new StringBuilder();
		cliArguments.append(getMainClass());

		if (!this.programArguments.isEmpty()) {
			cliArguments.append(" "); //$NON-NLS-1$
			cliArguments.append(this.programArguments);
		}

		return cliArguments.toString();
	}

	@Override
	public String getVMArguments() {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		return this.vmArguments;
	}

	@Override
	public IStatus getValidity(int ignoreCauses) {
		if (isDirty()) {
			return revalidate(ignoreCauses);
		}
		try {
			IPath path = getJarFile();
			if (path == null && (ignoreCauses & CODE_SOURCE) == 0) {
				return SARLEclipsePlugin.createStatus(IStatus.ERROR, CODE_SOURCE, Messages.StandardSREInstall_2);
			}
			File file = (path == null) ? null : path.toFile();
			if ((file == null || !file.canRead()) && (ignoreCauses & CODE_SOURCE) == 0) {
				return SARLEclipsePlugin.createStatus(IStatus.ERROR, CODE_SOURCE, Messages.StandardSREInstall_3);
			}
		} catch (Throwable e) {
			if ((ignoreCauses & CODE_GENERAL) == 0) {
				return SARLEclipsePlugin.createStatus(IStatus.ERROR, CODE_GENERAL, e);
			}
		}
		return super.getValidity(ignoreCauses);
	}

	/** {@inheritDoc}
	 */
	@Override
	public void getAsXML(Document document, Element element) throws IOException {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		IPath path = getJarFile();
		element.setAttribute(SREConstants.XML_STANDALONE_SRE, Boolean.toString(isStandalone()));
		element.setAttribute(SREConstants.XML_LIBRARY_PATH, path.toPortableString());
		String name = Strings.nullToEmpty(getName());
		if (!name.equals(this.manifestName)) {
			element.setAttribute(SREConstants.XML_SRE_NAME, name);
		}
		String mainClass = Strings.nullToEmpty(getMainClass());
		if (!mainClass.equals(this.manifestMainClass)) {
			element.setAttribute(SREConstants.XML_MAIN_CLASS, mainClass);
		}
		LibraryLocation[] libraries = getLibraryLocations();
		if (libraries.length != 1 || !libraries[0].getSystemLibraryPath().equals(this.jarFile)) {
			for (LibraryLocation location : libraries) {
				Element libraryNode = document.createElement(SREConstants.XML_LIBRARY_LOCATION);
				libraryNode.setAttribute(SREConstants.XML_SYSTEM_LIBRARY_PATH,
						location.getSystemLibraryPath().toPortableString());
				libraryNode.setAttribute(SREConstants.XML_PACKAGE_ROOT_PATH, location.getPackageRootPath().toPortableString());
				libraryNode.setAttribute(SREConstants.XML_SOURCE_PATH, location.getSystemLibrarySourcePath().toPortableString());
				URL javadoc = location.getJavadocLocation();
				if (javadoc != null) {
					libraryNode.setAttribute(SREConstants.XML_JAVADOC_PATH, javadoc.toString());
				}
				element.appendChild(libraryNode);
			}
		}
	}

	/** {@inheritDoc}
	 */
	@Override
	public void setFromXML(Element element) throws IOException {
		IPath path = parsePath(element.getAttribute(SREConstants.XML_LIBRARY_PATH), null);
		try {
			if (path != null) {
				setJarFile(path);

				String strStandalone = element.getAttribute(SREConstants.XML_STANDALONE_SRE);
				boolean isStandalone = false;
				if (strStandalone != null && !strStandalone.isEmpty()) {
					isStandalone = Boolean.parseBoolean(strStandalone);
				}
				setStandalone(isStandalone);

				String name = element.getAttribute(SREConstants.XML_SRE_NAME);
				if (!Strings.isNullOrEmpty(name)) {
					setName(name);
				}
				String mainClass = element.getAttribute(SREConstants.XML_MAIN_CLASS);
				if (!Strings.isNullOrEmpty(mainClass)) {
					setMainClass(mainClass);
				}

				List<LibraryLocation> locations = new ArrayList<>();
				NodeList children = element.getChildNodes();
				for (int i = 0; i < children.getLength(); ++i) {
					Node node = children.item(i);
					if (node instanceof Element && SREConstants.XML_LIBRARY_LOCATION.equalsIgnoreCase(node.getNodeName())) {
						Element libraryNode = (Element) node;
						IPath systemLibraryPath = parsePath(
								libraryNode.getAttribute(SREConstants.XML_SYSTEM_LIBRARY_PATH), null);
						if (systemLibraryPath != null) {
							IPath packageRootPath = parsePath(
									libraryNode.getAttribute(SREConstants.XML_PACKAGE_ROOT_PATH), Path.EMPTY);
							IPath sourcePath = parsePath(
									libraryNode.getAttribute(SREConstants.XML_SOURCE_PATH), Path.EMPTY);
							URL javadoc = null;
							try {
								String urlTxt = libraryNode.getAttribute(SREConstants.XML_JAVADOC_PATH);
								javadoc = new URL(urlTxt);
							} catch (Throwable _) {
								//
							}
							LibraryLocation location = new LibraryLocation(
									systemLibraryPath, sourcePath, packageRootPath,
									javadoc);
							locations.add(location);
						} else {
							SARLEclipsePlugin.logErrorMessage(
									MessageFormat.format(Messages.StandardSREInstall_4, getId()));
						}
					}
				}

				if (!locations.isEmpty()) {
					LibraryLocation[] tab = locations.toArray(new LibraryLocation[locations.size()]);
					setLibraryLocations(tab);
				}

				return;
			}
		} catch (Throwable _) {
			//
		}
		throw new IOException(MessageFormat.format(Messages.StandardSREInstall_5, getId()));
	}

	private static IPath parsePath(String path, IPath defaultPath) {
		if (!Strings.isNullOrEmpty(path)) {
			try {
				IPath p = Path.fromPortableString(path);
				if (p != null) {
					return p;
				}
			} catch (Throwable _) {
				//
			}
		}
		return defaultPath;
	}

	@Override
	public Map<String, String> getCommandLineOptions() {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}

		Map<String, String> options = (this.optionBuffer == null) ? null : this.optionBuffer.get();
		if (options == null) {
			options = Maps.newHashMap();
			putIfNotempty(options, SREConstants.MANIFEST_CLI_SHOW_LOGO, this.cliLogoOn);
			putIfNotempty(options, SREConstants.MANIFEST_CLI_HIDE_LOGO, this.cliLogoOff);
			putIfNotempty(options, SREConstants.MANIFEST_CLI_SHOW_INFO, this.cliShowInfo);
			putIfNotempty(options, SREConstants.MANIFEST_CLI_HIDE_INFO, this.cliHideInfo);
			putIfNotempty(options, SREConstants.MANIFEST_CLI_DEFAULT_CONTEXT_ID, this.cliDefaultContextID);
			putIfNotempty(options, SREConstants.MANIFEST_CLI_RANDOM_CONTEXT_ID, this.cliRandomContextID);
			putIfNotempty(options, SREConstants.MANIFEST_CLI_BOOT_AGENT_CONTEXT_ID, this.cliBootAgentContextID);
			putIfNotempty(options, SREConstants.MANIFEST_CLI_SRE_OFFLINE, this.cliSreOffline);
			this.optionBuffer = new SoftReference<>(options);
		}
		return Collections.unmodifiableMap(options);
	}

	private static void putIfNotempty(Map<String, String> map, String key, String value) {
		if (!Strings.isNullOrEmpty(value)) {
			map.put(key, value);
		}
	}

}
