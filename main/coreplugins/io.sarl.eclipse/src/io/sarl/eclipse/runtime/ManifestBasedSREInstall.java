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

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
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
import java.util.zip.ZipEntry;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Maps;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.launching.RuntimeClasspathEntry;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.LibraryLocation;
import org.eclipse.jdt.launching.PropertyChangeEvent;
import org.osgi.framework.Version;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.Utilities;

/**
 * SRE install that is based on the information within the MANIFEST file.
 *
 * <p>The standard SRE install assumes:
 * <ul>
 * <li>The SRE is based on a single JAR file.</li>
 * <li>The main class of the SRE is defined in the Manifest field <code>"Main-Class"</code>.</li>
 * <li>The Manifest contains a section named <code>"SARL-Runtime-Environment"</code>. This section contains the following entries:
 * <ul>
 * <li>The version number of the SARL specifications that are supported by the SRE is defined in the Manifest field <code>"SARL-Spec-Version"</code>.
 * </li>
 * <li>The name of the SRE may be given by the field <code>"Name"</code>.</li>
 * <li>The VM arguments of the SRE may be given by the field <code>"VM-Arguments"</code>.</li>
 * <li>The program arguments of the SRE may be given by the field <code>"Program-Arguments"</code>.</li>
 * <li>The command line option for avoiding the logo is given by the field <code>"CLI-Hide-Logo"</code>.</li>
 * <li>The command line option for displaying the logo is given by the field <code>"CLI-Show-Logo"</code>.</li>
 * <li>The command line option for displaying the information messages is given by the field<code>"CLI-Show-Info"</code>.</li>
 * <li>The command line option for hiding the information messages is given by the field<code>"CLI-Hide-Info"</code>.</li>
 * <li>The command line option for using the default root context id is given by the field<code>"CLI-Default-Context-ID"</code>.</li>
 * <li>The command line option for using the random root context id is given by the field<code>"CLI-Random-Context-ID"</code>.</li>
 * <li>The command line option for using the agent-type-based root context id is given by the field<code>"CLI-BootAgent-Context-ID"</code>.</li>
 * </ul>
 * </li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ManifestBasedSREInstall extends AbstractSREInstall {

	private IPath jarFile;

	private String vmArguments = Utilities.EMPTY_STRING;

	private String programArguments = Utilities.EMPTY_STRING;

	private String cliDefaultContextID;

	private String cliRandomContextID;

	private String cliBootAgentContextID;

	private String cliNoMoreOption;

	private String cliEmbedded;

	private String cliLogOption;

	private String cliLogValues;

	private String cliLogDefaultValue;

	private String manifestMainClass;

	private String manifestName;

	private transient SoftReference<Map<String, String>> optionBuffer;

	/**
	 * Construct a SRE installation.
	 *
	 * @param id the identifier of this SRE installation.
	 */
	public ManifestBasedSREInstall(String id) {
		super(id);
	}

	@Override
	public IPath getPreferredClassPathContainerPath() {
		return null;
	}

	@Override
	public ManifestBasedSREInstall clone() {
		final ManifestBasedSREInstall clone = (ManifestBasedSREInstall) super.clone();
		clone.jarFile = this.jarFile == null ? null : Path.fromPortableString(clone.jarFile.toPortableString());
		return clone;
	}

	@Override
	public ManifestBasedSREInstall copy(String id) {
		return (ManifestBasedSREInstall) super.copy(id);
	}

	/**
	 * Replies the path to the JAR file that is supporting this SRE installation.
	 *
	 * @return the path to the JAR file. Must not be {@code null}.
	 */
	public IPath getJarFile() {
		return this.jarFile;
	}

	/**
	 * Change the path to the JAR file that is supporting this SRE installation.
	 *
	 * @param jarFile the path to the JAR file. Must not be {@code null}.
	 */
	public void setJarFile(IPath jarFile) {
		if (!Objects.equal(jarFile, this.jarFile)) {
			final PropertyChangeEvent event = new PropertyChangeEvent(this, ISREInstallChangedListener.PROPERTY_JAR_FILE,
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
		final IPath iJarFile = getJarFile();
		if (iJarFile == null) {
			return getName();
		}
		return iJarFile.toOSString();
	}

	@Override
	public String getName() {
		String nam = getNameNoDefault();
		if (Strings.isNullOrEmpty(nam)) {
			final IPath path = getJarFile();
			if (path != null) {
				nam = path.removeFileExtension().lastSegment();
			} else {
				nam = getId();
			}
		}
		return nam;
	}

	@SuppressWarnings({ "checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity" })
	@Override
	protected void resolveDirtyFields(boolean forceSettings) {
		if (this.jarFile != null) {
			try (JarFile jFile = new JarFile(this.jarFile.toFile())) {
				final Manifest manifest = jFile.getManifest();
				//
				// Main class
				this.manifestMainClass = manifest.getMainAttributes().getValue(SREManifestPreferenceConstants.MANIFEST_MAIN_CLASS);
				if (Strings.isNullOrEmpty(this.manifestMainClass)) {
					throw new SREException(MessageFormat.format(Messages.StandardSREInstall_0, getId()));
				}
				if (forceSettings || Strings.isNullOrEmpty(getMainClass())) {
					setMainClass(this.manifestMainClass);
				}
				// Get SARL section:
				final Attributes sarlSection = manifest.getAttributes(SREManifestPreferenceConstants.MANIFEST_SECTION_SRE);
				if (sarlSection == null) {
					throw new SREException(Messages.StandardSREInstall_1);
				}
				//
				// SARL version
				final String sarlVersion = sarlSection.getValue(SREManifestPreferenceConstants.MANIFEST_SARL_SPEC_VERSION);
				String minVersion = null;
				String maxVersion = null;
				if (!Strings.isNullOrEmpty(sarlVersion)) {
					try {
						final Version sarlVer = Version.parseVersion(sarlVersion);
						if (sarlVer != null) {
							minVersion = new Version(sarlVer.getMajor(), sarlVer.getMinor(), 0).toString();
							maxVersion = new Version(sarlVer.getMajor(), sarlVer.getMinor() + 1, 0).toString();
						}
					} catch (Throwable exception) {
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
				this.manifestName = Strings.nullToEmpty(sarlSection.getValue(SREManifestPreferenceConstants.MANIFEST_SRE_NAME));
				if (forceSettings || Strings.isNullOrEmpty(getNameNoDefault())) {
					setName(this.manifestName);
				}
				//
				// VM arguments
				final String vmArgs = Strings.nullToEmpty(sarlSection.getValue(SREManifestPreferenceConstants.MANIFEST_VM_ARGUMENTS));
				if (!this.vmArguments.equals(vmArgs)) {
					final PropertyChangeEvent event = new PropertyChangeEvent(this,
							ISREInstallChangedListener.PROPERTY_VM_ARGUMENTS, this.vmArguments, Strings.nullToEmpty(vmArgs));
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
				this.cliDefaultContextID = sarlSection.getValue(SRECommandLineOptions.CLI_DEFAULT_CONTEXT_ID);
				this.cliRandomContextID = sarlSection.getValue(SRECommandLineOptions.CLI_RANDOM_CONTEXT_ID);
				this.cliBootAgentContextID = sarlSection.getValue(SRECommandLineOptions.CLI_BOOT_AGENT_CONTEXT_ID);
				this.cliNoMoreOption = sarlSection.getValue(SRECommandLineOptions.CLI_NO_MORE_OPTION);
				this.cliEmbedded = sarlSection.getValue(SRECommandLineOptions.CLI_EMBEDDED);
				this.cliLogOption = sarlSection.getValue(SRECommandLineOptions.CLI_LOG);
				this.cliLogValues = sarlSection.getValue(SRECommandLineOptions.CLI_LOG_VALUES);
				this.cliLogDefaultValue = sarlSection.getValue(SRECommandLineOptions.CLI_LOG_DEFAULT_VALUE);
				//
				// Program arguments
				final String programArgs = Strings.nullToEmpty(sarlSection.getValue(SREManifestPreferenceConstants.MANIFEST_PROGRAM_ARGUMENTS));
				if (!this.programArguments.equals(programArgs)) {
					final PropertyChangeEvent event = new PropertyChangeEvent(this,
							ISREInstallChangedListener.PROPERTY_PROGRAM_ARGUMENTS, this.programArguments, programArgs);
					this.programArguments = programArgs;
					if (getNotify()) {
						SARLRuntime.fireSREChanged(event);
					}
				}
				//
				// Library Location
				if (forceSettings || getClassPathEntries().isEmpty()) {
					final List<IRuntimeClasspathEntry> classPath = new ArrayList<>();
					LibraryLocation location = new LibraryLocation(this.jarFile, Path.EMPTY, Path.EMPTY);
					IClasspathEntry cpEntry = JavaCore.newLibraryEntry(location.getSystemLibraryPath(),
							location.getSystemLibrarySourcePath(), location.getPackageRootPath());

					IRuntimeClasspathEntry rtcpEntry = new RuntimeClasspathEntry(cpEntry);
					// No more a bootstrap library for enabling it to be in the classpath (not the JVM bootstrap).
					// In fact you can have a single bootstrap library if you add a new one you remove the default one and it bugs
					rtcpEntry.setClasspathProperty(IRuntimeClasspathEntry.USER_CLASSES);
					classPath.add(rtcpEntry);
					//
					final String classPathStr = manifest.getMainAttributes().getValue(SREManifestPreferenceConstants.MANIFEST_CLASS_PATH);
					final IPath rootPath = this.jarFile.removeLastSegments(1);
					if (!Strings.isNullOrEmpty(classPathStr)) {
						for (final String cpElement : classPathStr.split(Pattern.quote(":"))) { //$NON-NLS-1$
							final IPath path = parsePath(cpElement, Path.EMPTY, rootPath);
							location = new LibraryLocation(path, Path.EMPTY, Path.EMPTY);
							cpEntry = JavaCore.newLibraryEntry(location.getSystemLibraryPath(),
									location.getSystemLibrarySourcePath(), location.getPackageRootPath());
							rtcpEntry = new RuntimeClasspathEntry(cpEntry);
							// No more a bootstrap library for enabling it to be in the classpath (not the JVM bootstrap).
							// In fact you can have a single bootstrap library if you add a new one you remove the default one and it bugs
							rtcpEntry.setClasspathProperty(IRuntimeClasspathEntry.USER_CLASSES);
							classPath.add(rtcpEntry);
						}
					}
					setClassPathEntries(classPath);
				}
				//
				// Bootstrap
				final ZipEntry jEntry = jFile.getEntry(SREManifestPreferenceConstants.SERVICE_SRE_BOOTSTRAP);
				String bootstrap = null;
				if (jEntry != null) {
					try (InputStream is = jFile.getInputStream(jEntry)) {
						try (BufferedReader reader = new BufferedReader(new InputStreamReader(is))) {
							String line = reader.readLine();
							if (line != null) {
								line = line.trim();
								if (!line.isEmpty()) {
									bootstrap = line;
								}
							}
						}
					}
				}
				if (forceSettings || Strings.isNullOrEmpty(getBootstrap())) {
					setBootstrap(bootstrap);
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
	public String getSREArguments() {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		return this.programArguments;
	}

	@Override
	public String getJVMArguments() {
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
			final IPath path = getJarFile();
			if (path == null && (ignoreCauses & CODE_SOURCE) == 0) {
				return SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, CODE_SOURCE, Messages.StandardSREInstall_2);
			}
			final File file = (path == null) ? null : path.toFile();
			if ((file == null || !file.canRead()) && (ignoreCauses & CODE_SOURCE) == 0) {
				return SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, CODE_SOURCE, Messages.StandardSREInstall_3);
			}
		} catch (Throwable e) {
			if ((ignoreCauses & CODE_GENERAL) == 0) {
				return SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, CODE_GENERAL, e);
			}
		}
		return super.getValidity(ignoreCauses);
	}

	@Override
	public void getAsXML(Document document, Element element) throws IOException {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}
		final IPath path = getJarFile();
		element.setAttribute(SREXmlPreferenceConstants.XML_LIBRARY_PATH, path.toPortableString());
		final String name = Strings.nullToEmpty(getName());
		if (!name.equals(this.manifestName)) {
			element.setAttribute(SREXmlPreferenceConstants.XML_SRE_NAME, name);
		}
		final String mainClass = Strings.nullToEmpty(getMainClass());
		if (!mainClass.equals(this.manifestMainClass)) {
			element.setAttribute(SREXmlPreferenceConstants.XML_MAIN_CLASS, mainClass);
		}
		final String bootstrap = Strings.nullToEmpty(getBootstrap());
		if (!Strings.isNullOrEmpty(bootstrap)) {
			element.setAttribute(SREXmlPreferenceConstants.XML_BOOTSTRAP, bootstrap);
		} else {
			element.removeAttribute(SREXmlPreferenceConstants.XML_BOOTSTRAP);
		}
		final List<IRuntimeClasspathEntry> libraries = getClassPathEntries();
		if (libraries.size() != 1 || !libraries.get(0).getClasspathEntry().getPath().equals(this.jarFile)) {
			final IPath rootPath = path.removeLastSegments(1);
			for (final IRuntimeClasspathEntry location : libraries) {
				final Element libraryNode = document.createElement(SREXmlPreferenceConstants.XML_LIBRARY_LOCATION);
				libraryNode.setAttribute(SREXmlPreferenceConstants.XML_SYSTEM_LIBRARY_PATH,
						makeRelativePath(location.getPath(), path, rootPath));
				libraryNode.setAttribute(SREXmlPreferenceConstants.XML_PACKAGE_ROOT_PATH,
						makeRelativePath(location.getSourceAttachmentRootPath(), path, rootPath));
				libraryNode.setAttribute(SREXmlPreferenceConstants.XML_SOURCE_PATH,
						makeRelativePath(location.getSourceAttachmentPath(), path, rootPath));
				/* No javadoc path accessible from ClasspathEntry
				final URL javadoc = location.getJavadocLocation();
				if (javadoc != null) {
					libraryNode.setAttribute(SREConstants.XML_JAVADOC_PATH, javadoc.toString());
				}
				*/
				element.appendChild(libraryNode);
			}
		}
	}

	@Override
	@SuppressWarnings("checkstyle:npathcomplexity")
	public void setFromXML(Element element) throws IOException {
		final IPath path = parsePath(
				element.getAttribute(SREXmlPreferenceConstants.XML_LIBRARY_PATH), null, null);
		try {
			if (path != null) {
				setJarFile(path);

				final String name = element.getAttribute(SREXmlPreferenceConstants.XML_SRE_NAME);
				if (!Strings.isNullOrEmpty(name)) {
					setName(name);
				}
				final String mainClass = element.getAttribute(SREXmlPreferenceConstants.XML_MAIN_CLASS);
				if (!Strings.isNullOrEmpty(mainClass)) {
					setMainClass(mainClass);
				}

				final String bootstrap = element.getAttribute(SREXmlPreferenceConstants.XML_BOOTSTRAP);
				if (!Strings.isNullOrEmpty(bootstrap)) {
					setBootstrap(bootstrap);
				}

				final List<IRuntimeClasspathEntry> locations = new ArrayList<>();
				final NodeList children = element.getChildNodes();
				final IPath rootPath = path.removeLastSegments(1);
				for (int i = 0; i < children.getLength(); ++i) {
					final Node node = children.item(i);
					if (node instanceof Element && SREXmlPreferenceConstants.XML_LIBRARY_LOCATION.equalsIgnoreCase(node.getNodeName())) {
						final Element libraryNode = (Element) node;
						final IPath systemLibraryPath = parsePath(
								libraryNode.getAttribute(SREXmlPreferenceConstants.XML_SYSTEM_LIBRARY_PATH), null, rootPath);
						if (systemLibraryPath != null) {
							final IPath packageRootPath = parsePath(
									libraryNode.getAttribute(SREXmlPreferenceConstants.XML_PACKAGE_ROOT_PATH),
									Path.EMPTY, rootPath);
							final IPath sourcePath = parsePath(
									libraryNode.getAttribute(SREXmlPreferenceConstants.XML_SOURCE_PATH), Path.EMPTY, rootPath);
							URL javadoc = null;
							try {
								final String urlTxt = libraryNode.getAttribute(SREXmlPreferenceConstants.XML_JAVADOC_PATH);
								javadoc = new URL(urlTxt);
							} catch (Throwable exception) {
								//
							}
							final LibraryLocation location = new LibraryLocation(systemLibraryPath, sourcePath, packageRootPath,
									javadoc);
							final IClasspathEntry cpEntry = JavaCore.newLibraryEntry(
									location.getSystemLibraryPath(),
									location.getSystemLibrarySourcePath(),
									location.getPackageRootPath());
							final IRuntimeClasspathEntry rtcpEntry = new RuntimeClasspathEntry(cpEntry);
							// No more a bootstrap library for enabling it to be in the classpath (not the JVM bootstrap).
							// In fact you can have a single bootstrap library if you add a new one you remove the default one and it bugs
							rtcpEntry.setClasspathProperty(IRuntimeClasspathEntry.USER_CLASSES);
							locations.add(rtcpEntry);
						} else {
							SARLEclipsePlugin.getDefault()
									.logErrorMessage(MessageFormat.format(Messages.StandardSREInstall_4, getId()));
						}
					}
				}

				if (!locations.isEmpty()) {
					setClassPathEntries(locations);
				}

				return;
			}
		} catch (Throwable exception) {
			throw new IOException(MessageFormat.format(Messages.StandardSREInstall_5, getId()), exception);
		}
		throw new IOException(MessageFormat.format(Messages.StandardSREInstall_5, getId()));
	}

	/** Path the given string for extracting a path.
	 *
	 * @param path the string representation of the path to parse.
	 * @param defaultPath the default path.
	 * @param rootPath the root path to use is the given path is not absolute.
	 * @return the absolute path.
	 */
	private static IPath parsePath(String path, IPath defaultPath, IPath rootPath) {
		if (!Strings.isNullOrEmpty(path)) {
			try {
				final IPath pathObject = Path.fromPortableString(path);
				if (pathObject != null) {
					if (rootPath != null && !pathObject.isAbsolute()) {
						return rootPath.append(pathObject);
					}
					return pathObject;
				}
			} catch (Throwable exception) {
				//
			}
		}
		return defaultPath;
	}

	private static String makeRelativePath(IPath pathToConvert, IPath jarPath, IPath rootPath) {
		if (pathToConvert == null) {
			return null;
		}
		if (!jarPath.equals(pathToConvert) && rootPath.isPrefixOf(pathToConvert)) {
			return pathToConvert.makeRelativeTo(rootPath).toPortableString();
		}
		return pathToConvert.toPortableString();
	}

	@Override
	public Map<String, String> getAvailableCommandLineOptions() {
		if (isDirty()) {
			setDirty(false);
			resolveDirtyFields(true);
		}

		Map<String, String> options = (this.optionBuffer == null) ? null : this.optionBuffer.get();
		if (options == null) {
			options = Maps.newHashMap();
			putIfNotempty(options, SRECommandLineOptions.CLI_DEFAULT_CONTEXT_ID, this.cliDefaultContextID);
			putIfNotempty(options, SRECommandLineOptions.CLI_RANDOM_CONTEXT_ID, this.cliRandomContextID);
			putIfNotempty(options, SRECommandLineOptions.CLI_BOOT_AGENT_CONTEXT_ID, this.cliBootAgentContextID);
			putIfNotempty(options, SRECommandLineOptions.CLI_NO_MORE_OPTION, this.cliNoMoreOption);
			putIfNotempty(options, SRECommandLineOptions.CLI_EMBEDDED, this.cliEmbedded);
			putIfNotempty(options, SRECommandLineOptions.CLI_LOG, this.cliLogOption);
			putIfNotempty(options, SRECommandLineOptions.CLI_LOG_VALUES, this.cliLogValues);
			putIfNotempty(options, SRECommandLineOptions.CLI_LOG_DEFAULT_VALUE, this.cliLogDefaultValue);
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
