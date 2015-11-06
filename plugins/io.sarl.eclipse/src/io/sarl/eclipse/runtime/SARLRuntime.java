/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;
import java.util.concurrent.locks.ReentrantLock;
import java.util.jar.Attributes;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.internal.launching.LaunchingMessages;
import org.eclipse.jdt.launching.PropertyChangeEvent;
import org.osgi.framework.Version;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;

/**
 * The central access point for launching support. This class manages
 * the registered SRE types contributed through the
 * extension point with the name {@link SARLEclipseConfig#EXTENSION_POINT_SARL_RUNTIME_ENVIRONMENT}.
 * As well, this class provides SRE install change notification.
 *
 * <p>This class was inspired from <code>JavaRuntime</code>.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @noinstantiate This class is not intended to be instantiated by clients.
 */
@SuppressWarnings({"checkstyle:classfanoutcomplexity", "checkstyle:classdataabstractioncoupling"})
public final class SARLRuntime {

	/**
	 * Preference key for the String of XML that defines all installed SREs.
	 */
	public static final String DEFAULT_PREFERENCE_KEY = SARLEclipsePlugin.PLUGIN_ID + ".runtime.PREF_SRE_XML"; //$NON-NLS-1$

	private static String currentPreferenceKey = DEFAULT_PREFERENCE_KEY;

	/**
	 * SRE change listeners.
	 */
	private static final ListenerList SRE_LISTENERS = new ListenerList();

	private static final Map<String, ISREInstall> ALL_SRE_INSTALLS = new HashMap<>();

	private static Set<String> platformSREInstalls;

	private static String defaultSREId;

	private static final ReentrantLock LOCK = new ReentrantLock();

	private SARLRuntime() {
		//
	}

	/** Replies the key used for storing the SARL runtime configuration
	 * into the preferences.
	 *
	 * @return the current preference key.
	 */
	public static String getCurrentPreferenceKey() {
		LOCK.lock();
		try {
			return currentPreferenceKey;
		} finally {
			LOCK.unlock();
		}
	}

	/** Change the key used for storing the SARL runtime configuration
	 * into the preferences.
	 *
	 * <p>If the given key is <code>null</code> or empty, the preference key
	 * is reset to the {@link #DEFAULT_PREFERENCE_KEY}.
	 *
	 * @param key - the new key or <code>null</code>.
	 */
	public static void setCurrentPreferenceKey(String key) {
		LOCK.lock();
		try {
			currentPreferenceKey = (Strings.isNullOrEmpty(key)) ? DEFAULT_PREFERENCE_KEY : key;
		} finally {
			LOCK.unlock();
		}
	}

	/**
	 * Adds the given listener to the list of registered SRE install changed
	 * listeners. Has no effect if an identical listener is already registered.
	 *
	 * @param listener - the listener to add
	 */
	public static void addSREInstallChangedListener(ISREInstallChangedListener listener) {
		SRE_LISTENERS.add(listener);
	}

	/**
	 * Removes the given listener from the list of registered SRE install changed
	 * listeners. Has no effect if an identical listener is not already registered.
	 *
	 * @param listener - the listener to remove
	 */
	public static void removeSREInstallChangedListener(ISREInstallChangedListener listener) {
		SRE_LISTENERS.remove(listener);
	}

	/**
	 * Notifies all SRE install changed listeners of the given property change.
	 *
	 * @param event - event describing the change.
	 */
	public static void fireSREChanged(PropertyChangeEvent event) {
		for (Object listener : SRE_LISTENERS.getListeners()) {
			((ISREInstallChangedListener) listener).sreChanged(event);
		}
	}

	/**
	 * Notifies all SRE install changed listeners of the addition of a SRE.
	 *
	 * @param installation - the installed SRE.
	 */
	public static void fireSREAdded(ISREInstall installation) {
		for (Object listener : SRE_LISTENERS.getListeners()) {
			((ISREInstallChangedListener) listener).sreAdded(installation);
		}
	}

	/**
	 * Notifies all SRE install changed listeners of the removed of a SRE.
	 *
	 * @param installation - the removed SRE.
	 */
	public static void fireSRERemoved(ISREInstall installation) {
		for (Object listener : SRE_LISTENERS.getListeners()) {
			((ISREInstallChangedListener) listener).sreRemoved(installation);
		}
	}

	/**
	 * Return the SRE corresponding to the specified Id.
	 *
	 * @param id - the id that specifies an instance of ISREInstall
	 * @return the SRE corresponding to the specified Id, or <code>null</code>.
	 */
	public static ISREInstall getSREFromId(String id) {
		if (Strings.isNullOrEmpty(id)) {
			return null;
		}
		initializeSREs();
		LOCK.lock();
		try {
			return ALL_SRE_INSTALLS.get(id);
		} finally {
			LOCK.unlock();
		}
	}

	/**
	 * Returns the default SRE id determined during the initialization of the SRE types.
	 *
	 * @return the id of the default SRE, or <code>null</code> if none.
	 */
	private static String getDefaultSREId() {
		LOCK.lock();
		try {
			initializeSREs();
			return defaultSREId;
		} finally {
			LOCK.unlock();
		}
	}

	/**
	 * Return the default SRE set with <code>setDefaultSRE()</code>.
	 *
	 * @return	Returns the default SRE. May return <code>null</code> when no default
	 *     SRE was set or when the default SRE has been disposed.
	 */
	public static ISREInstall getDefaultSREInstall() {
		ISREInstall install = getSREFromId(getDefaultSREId());
		if (install != null && install.getValidity().isOK()) {
			return install;
		}
		// if the default SRE goes missing, re-detect
		LOCK.lock();
		try {
			//defaultSREId = null;
			initializeSREs();
		} finally {
			LOCK.unlock();
		}
		return getSREFromId(getDefaultSREId());
	}

	/**
	 * Sets the installed SREs.
	 *
	 * @param sres - The installed SREs.
	 * @param monitor the progress monitor to use for reporting progress to the user. It is the caller's responsibility
	 *        to call done() on the given monitor. Accepts <code>null</code>, indicating that no progress should be
	 *        reported and that the operation cannot be cancelled.
	 * @throws CoreException if trying to set the default SRE install encounters problems
	 */
	@SuppressWarnings("checkstyle:npathcomplexity")
	public static void setSREInstalls(ISREInstall[] sres, IProgressMonitor monitor) throws CoreException {
		SubMonitor mon = SubMonitor.convert(monitor,
				io.sarl.eclipse.runtime.Messages.SARLRuntime_0,
				sres.length * 2 + ALL_SRE_INSTALLS.size());
		initializeSREs();
		String oldDefaultId;
		String newDefaultId;
		List<ISREInstall> newElements = new ArrayList<>();
		Map<String, ISREInstall> allKeys;
		LOCK.lock();
		try {
			oldDefaultId = getDefaultSREId();
			newDefaultId = oldDefaultId;
			allKeys = new TreeMap<>(ALL_SRE_INSTALLS);
			for (ISREInstall sre : sres) {
				if (allKeys.remove(sre.getId()) == null) {
					newElements.add(sre);
					ALL_SRE_INSTALLS.put(sre.getId(), sre);
				}
				mon.worked(1);
			}
			for (ISREInstall sre : allKeys.values()) {
				ALL_SRE_INSTALLS.remove(sre.getId());
				platformSREInstalls.remove(sre.getId());
				mon.worked(1);
			}
			if (oldDefaultId != null && !ALL_SRE_INSTALLS.containsKey(oldDefaultId)) {
				newDefaultId = null;
			}
		} finally {
			LOCK.unlock();
		}
		boolean changed = false;
		mon.subTask(io.sarl.eclipse.runtime.Messages.SARLRuntime_1);
		if (oldDefaultId != null && newDefaultId == null) {
			changed = true;
			setDefaultSREInstall(null, monitor);
		}
		mon.worked(1);
		mon.subTask(io.sarl.eclipse.runtime.Messages.SARLRuntime_2);
		for (ISREInstall sre : allKeys.values()) {
			changed = true;
			fireSRERemoved(sre);
		}
		for (ISREInstall sre : newElements) {
			changed = true;
			fireSREAdded(sre);
		}
		mon.worked(1);
		if (changed) {
			saveSREConfiguration(mon.newChild(sres.length - 2));
		}
	}

	/**
	 * Sets a SRE as the system-wide default SRE, and notifies registered SRE install
	 * change listeners of the change.
	 *
	 * @param sre - The SRE to make the default. May be <code>null</code> to clear
	 * 				the default.
	 * @param monitor - progress monitor or <code>null</code>
	 * @throws CoreException if trying to set the default SRE install encounters problems
	 */
	public static void setDefaultSREInstall(ISREInstall sre, IProgressMonitor monitor) throws CoreException {
		setDefaultSREInstall(sre, monitor, true);
	}

	/**
	 * Sets a SRE as the system-wide default SRE, and notifies registered SRE install
	 * change listeners of the change.
	 *
	 * @param sre - The SRE to make the default. May be <code>null</code> to clear
	 * 				the default.
	 * @param monitor - progress monitor or <code>null</code>
	 * @param savePreference - If <code>true</code>, update workbench preferences to reflect
	 * 		   				  the new default SRE.
	 * @throws CoreException if trying to set the default SRE install encounters problems
	 */
	public static void setDefaultSREInstall(ISREInstall sre, IProgressMonitor monitor,
			boolean savePreference) throws CoreException {
		initializeSREs();
		ISREInstall previous = null;
		ISREInstall current = null;
		LOCK.lock();
		try {
			if (defaultSREId != null) {
				previous = getSREFromId(defaultSREId);
			}
			defaultSREId = ((sre == null) ? null : sre.getId());
			if (savePreference) {
				saveSREConfiguration(monitor);
			}
			if (defaultSREId != null) {
				current = getSREFromId(defaultSREId);
			}
		} finally {
			LOCK.unlock();
		}
		if (previous != current) {
			fireDefaultSREChanged(previous, current);
		}
	}

	/**
	 * Notifies registered listeners that the default SRE has changed.
	 *
	 * @param previous - the previous SRE
	 * @param current - the new current default SRE
	 */
	private static void fireDefaultSREChanged(ISREInstall previous, ISREInstall current) {
		for (Object listener : SRE_LISTENERS.getListeners()) {
			((ISREInstallChangedListener) listener).defaultSREInstallChanged(previous, current);
		}
	}

	/**
	 * Returns the list of registered SREs. SRE types are registered via
	 * extension point with the name {@link SARLEclipseConfig#EXTENSION_POINT_SARL_RUNTIME_ENVIRONMENT}.
	 * Returns an empty list if there are no registered SREs.
	 *
	 * @return the list of registered SREs.
	 */
	public static ISREInstall[] getSREInstalls() {
		initializeSREs();
		LOCK.lock();
		try {
			return ALL_SRE_INSTALLS.values().toArray(new ISREInstall[ALL_SRE_INSTALLS.size()]);
		} finally {
			LOCK.unlock();
		}
	}

	/** Replies if the given SRE is provided by the Eclipse platform
	 * through an extension point.
	 *
	 * @param sre - the sre.
	 * @return <code>true</code> if the SRE was provided through an extension
	 *     point.
	 */
	public static boolean isPlatformSRE(ISREInstall sre) {
		if (sre != null) {
			LOCK.lock();
			try {
				return platformSREInstalls.contains(sre.getId());
			} finally {
				LOCK.unlock();
			}
		}
		return false;
	}

	/**
	 * Saves the SRE configuration information to the preferences. This includes
	 * the following information:
	 * <ul>
	 * <li>The list of all defined ISREInstall instances.</li>
	 * <li>The default SRE.</li>
	 * </ul>
	 * This state will be read again upon first access to SRE
	 * configuration information.
	 *
	 * @param monitor - the progression monitor, or <code>null</code>.
	 * @throws CoreException if trying to save the current state of SREs encounters a problem
	 */
	public static void saveSREConfiguration(IProgressMonitor monitor) throws CoreException {
		SARLEclipsePlugin plugin = SARLEclipsePlugin.getDefault();
		plugin.getPreferences().put(getCurrentPreferenceKey(), getSREsAsXML(monitor));
		plugin.savePreferences();
	}

	/**
	 * Remove the SRE configuration information from the preferences.
	 *
	 * @throws CoreException if trying to save the current state of SREs encounters a problem
	 */
	public static void clearSREConfiguration() throws CoreException {
		SARLEclipsePlugin plugin = SARLEclipsePlugin.getDefault();
		plugin.getPreferences().remove(getCurrentPreferenceKey());
		plugin.savePreferences();
	}

	/**
	 * Initializes SRE extensions.
	 */
	private static void initializeSREExtensions() {
		MultiStatus status = new MultiStatus(SARLEclipsePlugin.PLUGIN_ID,
				IStatus.OK, "Exceptions occurred", null);  //$NON-NLS-1$
		IExtensionPoint extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(
				SARLEclipsePlugin.PLUGIN_ID, SARLEclipseConfig.EXTENSION_POINT_SARL_RUNTIME_ENVIRONMENT);
		if (extensionPoint != null) {
			Object obj;
			for (IConfigurationElement element : extensionPoint.getConfigurationElements()) {
				try {
					obj = element.createExecutableExtension("class"); //$NON-NLS-1$
					if (obj instanceof ISREInstall) {
						ISREInstall sre = (ISREInstall) obj;
						platformSREInstalls.add(sre.getId());
						ALL_SRE_INSTALLS.put(sre.getId(), sre);
					} else {
						SARLEclipsePlugin.getDefault().logErrorMessage(
								"Cannot instance extension point: " + element.getName()); //$NON-NLS-1$
					}
				} catch (CoreException e) {
					status.add(e.getStatus());
				}
			}
			if (!status.isOK()) {
				//only happens on a CoreException
				SARLEclipsePlugin.getDefault().getLog().log(status);
			}
		}
	}

	/**
	 * Returns the XML representation of the given SRE.
	 *
	 * @param sre - the SRE to serialize.
	 * @return an XML representation of the given SRE.
	 * @throws CoreException if trying to compute the XML for the SRE state encounters a problem.
	 */
	public static String getSREAsXML(ISREInstall sre) throws CoreException {
		try {
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			Document xmldocument = builder.newDocument();

			Element sreNode = xmldocument.createElement("SRE"); //$NON-NLS-1$
			sreNode.setAttribute("platform", Boolean.toString(isPlatformSRE(sre))); //$NON-NLS-1$
			sreNode.setAttribute("id", sre.getId()); //$NON-NLS-1$
			sreNode.setAttribute("class", sre.getClass().getName()); //$NON-NLS-1$
			sre.getAsXML(xmldocument, sreNode);
			xmldocument.appendChild(sreNode);

			TransformerFactory transFactory = TransformerFactory.newInstance();
			Transformer trans = transFactory.newTransformer();
			try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
				DOMSource source = new DOMSource(xmldocument);
				PrintWriter flot = new PrintWriter(baos);
				StreamResult xmlStream = new StreamResult(flot);
				trans.transform(source, xmlStream);
				return new String(baos.toByteArray());
			}
		} catch (Throwable e) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, e));
		}
	}

	/**
	 * Returns the XML representation of the given SRE.
	 *
	 * @param sre - the SRE to serialize.
	 * @param xml - the XML representation of the given SRE.
	 * @throws CoreException if trying to compute the XML for the SRE state encounters a problem.
	 */
	public static void setSREFromXML(ISREInstall sre, String xml) throws CoreException {
		try {
			Element root = parseXML(xml, false);
			sre.setFromXML(root);
		} catch (Throwable e) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, e));
		}
	}

	/**
	 * Returns the listing of currently installed SREs as a single XML file.
	 *
	 * @param monitor - monitor on the XML building.
	 * @return an XML representation of all of the currently installed SREs.
	 * @throws CoreException if trying to compute the XML for the SRE state encounters a problem.
	 */
	public static String getSREsAsXML(IProgressMonitor monitor) throws CoreException {
		initializeSREs();
		try {
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			Document xmldocument = builder.newDocument();
			Element rootElement = getXml(xmldocument);
			xmldocument.appendChild(rootElement);
			TransformerFactory transFactory = TransformerFactory.newInstance();
			Transformer trans = transFactory.newTransformer();
			try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
				DOMSource source = new DOMSource(xmldocument);
				PrintWriter flot = new PrintWriter(baos);
				StreamResult xmlStream = new StreamResult(flot);
				trans.transform(source, xmlStream);
				return new String(baos.toByteArray());
			}
		} catch (Throwable e) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, e));
		}
	}

	private static Element getXml(Document xmlDocument) throws IOException {
		Element sresNode = xmlDocument.createElement("SREs"); //$NON-NLS-1$
		LOCK.lock();
		try {
			for (ISREInstall sre : ALL_SRE_INSTALLS.values()) {
				Element sreNode = xmlDocument.createElement("SRE"); //$NON-NLS-1$
				sreNode.setAttribute("platform", Boolean.toString(isPlatformSRE(sre))); //$NON-NLS-1$
				sreNode.setAttribute("id", sre.getId()); //$NON-NLS-1$
				sreNode.setAttribute("class", sre.getClass().getName()); //$NON-NLS-1$
				sre.getAsXML(xmlDocument, sreNode);
				sresNode.appendChild(sreNode);
			}
			if (!Strings.isNullOrEmpty(defaultSREId)) {
				sresNode.setAttribute("defaultSRE", defaultSREId); //$NON-NLS-1$
			}
		} finally {
			LOCK.unlock();
		}
		return sresNode;
	}

	private static Element parseXML(String rawXml, boolean isMultiple) throws IOException {
		try (InputStream stream = new BufferedInputStream(new ByteArrayInputStream(rawXml.getBytes()))) {
			return parseXML(stream, isMultiple);
		}
	}

	private static Element parseXML(InputStream stream, boolean isMultiple) throws IOException {
		Element config = null;
		try {
			DocumentBuilder parser = DocumentBuilderFactory.newInstance().newDocumentBuilder();
			parser.setErrorHandler(new DefaultHandler());
			config = parser.parse(new InputSource(stream)).getDocumentElement();
		} catch (SAXException e) {
			throw new IOException(e);
		} catch (ParserConfigurationException e) {
			throw new IOException(LaunchingMessages.JavaRuntime_badFormat);
		}

		// If the top-level node wasn't what we expected, bail out
		if (config == null
				|| (isMultiple && !config.getNodeName().equalsIgnoreCase("SREs")) //$NON-NLS-1$
				|| (!isMultiple && !config.getNodeName().equalsIgnoreCase("SRE"))) { //$NON-NLS-1$
			throw new IOException("Invalid XML format of the SRE preferences");  //$NON-NLS-1$
		}

		return config;
	}

	private static ISREInstall createSRE(String classname, String id) {
		if (!Strings.isNullOrEmpty(id)) {
			try {
				Class<? extends ISREInstall> type = Class.forName(classname).asSubclass(ISREInstall.class);
				Constructor<? extends ISREInstall> cons = type.getConstructor(String.class);
				return cons.newInstance(id);
			} catch (Throwable exception) {
				//
			}
		}
		return null;
	}

	/**
	 * This method loads installed SREs based an existing user preference
	 * or old SRE configurations file.
	 */
	@SuppressWarnings("checkstyle:cyclomaticcomplexity")
	private static String initializePersistedSREs() {
		//		// FOR DEBUG
		//		try {
		//			clearSREConfiguration();
		//		} catch (CoreException e1) {
		//			e1.printStackTrace();
		//		}
		String rawXml = SARLEclipsePlugin.getDefault().getPreferences().get(getCurrentPreferenceKey(), ""); //$NON-NLS-1$

		try {
			Element config = null;
			// If the preference was found, load SREs from it into memory
			if (!Strings.isNullOrEmpty(rawXml)) {
				config = parseXML(rawXml, true);
			} else {
				// Otherwise, look for the old file that previously held the SRE definitions
				SARLEclipsePlugin plugin = SARLEclipsePlugin.getDefault();
				if (plugin.getBundle() != null) {
					IPath stateLocation = plugin.getStateLocation();
					IPath stateFile = stateLocation.append("sreConfiguration.xml"); //$NON-NLS-1$
					File file = stateFile.toFile();
					if (file.exists()) {
						// If file exists, load SRE definitions from it into memory and
						// write the definitions to the preference store WITHOUT triggering
						// any processing of the new value
						try (InputStream fileInputStream = new BufferedInputStream(new FileInputStream(file))) {
							config = parseXML(fileInputStream, true);
						} catch (IOException e) {
							SARLEclipsePlugin.getDefault().log(e);
						}
					}
				}
			}
			if (config != null) {
				String defaultId = config.getAttribute("defaultSRE"); //$NON-NLS-1$
				NodeList children = config.getChildNodes();
				for (int i = 0; i < children.getLength(); ++i) {
					try {
						Node child = children.item(i);
						if ("SRE".equalsIgnoreCase(child.getNodeName()) //$NON-NLS-1$
								&& child instanceof Element) {
							Element element = (Element) child;
							boolean isPlatform = Boolean.parseBoolean(element.getAttribute("platform")); //$NON-NLS-1$
							String id = element.getAttribute("id"); //$NON-NLS-1$
							if (!isPlatform || !(ALL_SRE_INSTALLS.containsKey(id))) {
								ISREInstall sre = createSRE(
										element.getAttribute("class"), //$NON-NLS-1$
										id);
								if (sre == null) {
									throw new IOException("Invalid XML format of the SRE preferences of " + id); //$NON-NLS-1$
								}
								try {
									sre.setFromXML(element);
								} catch (IOException e) {
									SARLEclipsePlugin.getDefault().log(e);
								}
								ALL_SRE_INSTALLS.put(id, sre);
								if (isPlatform) {
									platformSREInstalls.add(id);
								}
							} else {
								ISREInstall sre = ALL_SRE_INSTALLS.get(id);
								if (sre != null) {
									try {
										sre.setFromXML(element);
									} catch (IOException e) {
										SARLEclipsePlugin.getDefault().log(e);
									}
								}
							}
						}
					} catch (IOException e) {
						SARLEclipsePlugin.getDefault().log(e);
					}
				}
				return defaultId;
			}
		} catch (IOException e) {
			SARLEclipsePlugin.getDefault().log(e);
		}
		return null;
	}

	/**
	 * Perform SRE install initialization. Does not hold locks
	 * while performing change notification.
	 *
	 * @since 3.2
	 */
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:variabledeclarationusagedistance",
			"checkstyle:npathcomplexity"})
	private static void initializeSREs() {
		ISREInstall[] newSREs = new ISREInstall[0];
		boolean savePrefs = false;
		LOCK.lock();
		String previousDefault = defaultSREId;
		try {
			if (platformSREInstalls == null) {
				platformSREInstalls = new HashSet<>();
				ALL_SRE_INSTALLS.clear();

				// Install the SREs from the Eclipse extension points
				initializeSREExtensions();
				// install the SREs from the user-defined preferences.
				String predefinedDefaultId = Strings.nullToEmpty(initializePersistedSREs());

				newSREs = new ISREInstall[ALL_SRE_INSTALLS.size()];

				// Verify default SRE is valid
				ISREInstall initDefaultSRE = null;
				Iterator<ISREInstall> iterator = ALL_SRE_INSTALLS.values().iterator();
				for (int i = 0; iterator.hasNext(); ++i) {
					ISREInstall sre = iterator.next();
					newSREs[i] = sre;
					if (sre.getValidity().isOK()) {
						if (initDefaultSRE == null
								&& sre.getId().equals(predefinedDefaultId)) {
							initDefaultSRE = sre;
						}
					}
				}

				String oldDefaultId = initDefaultSRE == null ? null : initDefaultSRE.getId();
				defaultSREId = oldDefaultId;
				savePrefs = true;
			}

			if (Strings.isNullOrEmpty(defaultSREId)) {
				ISREInstall firstSRE = null;
				ISREInstall firstValidSRE = null;
				Iterator<ISREInstall> iterator = ALL_SRE_INSTALLS.values().iterator();
				while (firstValidSRE == null && iterator.hasNext()) {
					ISREInstall sre = iterator.next();
					if (firstSRE == null) {
						firstSRE = sre;
					}
					if (sre.getValidity().isOK()) {
						firstValidSRE = sre;
					}
				}
				if (firstValidSRE == null) {
					firstValidSRE = firstSRE;
				}
				if (firstValidSRE != null) {
					savePrefs = true;
					defaultSREId = firstValidSRE.getId();
				}
			}
		} finally {
			LOCK.unlock();
		}

		// Save the preferences.
		if (savePrefs) {
			safeSaveSREConfiguration();
		}

		if (newSREs.length > 0) {
			for (ISREInstall sre : newSREs) {
				fireSREAdded(sre);
			}
		}

		if (!Objects.equal(previousDefault, defaultSREId)) {
			fireDefaultSREChanged(
					getSREFromId(previousDefault),
					getSREFromId(defaultSREId));
		}
	}

	private static void safeSaveSREConfiguration() {
		try {
			saveSREConfiguration(null);
		} catch (CoreException e) {
			SARLEclipsePlugin.getDefault().log(e);
		}
	}

	/** Replies an unique identifier.
	 *
	 * @return a unique identifier.
	 */
	public static String createUniqueIdentifier() {
		String id = null;
		do {
			id = UUID.randomUUID().toString();
		} while (getSREFromId(id) != null);
		return id;
	}

	/** Reset the list of the SREs to the default ones (the platform SREs).
	 *
	 * @throws CoreException if a problem occurs during the reset.
	 */
	@SuppressWarnings("checkstyle:variabledeclarationusagedistance")
	public static void reset() throws CoreException {
		LOCK.lock();
		try {
			// Clear the SRE configuration stored into the preferences.
			clearSREConfiguration();
			// Reset the internal data structures.
			ISREInstall previous = getDefaultSREInstall();
			Map<String, ISREInstall> oldSREs = new HashMap<>(ALL_SRE_INSTALLS);
			ALL_SRE_INSTALLS.clear();
			platformSREInstalls = null;
			defaultSREId = null;
			// Notify about the removals
			for (ISREInstall sre : oldSREs.values()) {
				fireSRERemoved(sre);
			}
			if (previous != null) {
				fireDefaultSREChanged(previous, null);
			}
			// Re-read the data
			initializeSREs();
		} finally {
			LOCK.unlock();
		}
	}

	/** Replies if the given directory contains a SRE.
	 *
	 * @param directory - the directory.
	 * @return <code>true</code> if the given directory contains a SRE. Otherwise <code>false</code>.
	 * @see #isPackedSRE(File)
	 */
	public static boolean isUnpackedSRE(File directory) {
		File manifestFile = new File(directory, "META-INF"); //$NON-NLS-1$
		manifestFile = new File(manifestFile, "MANIFEST.MF"); //$NON-NLS-1$
		if (manifestFile.canRead()) {
			try (InputStream manifestStream = new FileInputStream(manifestFile)) {
				Manifest manifest = new Manifest(manifestStream);
				Attributes sarlSection = manifest.getAttributes(SREConstants.MANIFEST_SECTION_SRE);
				if (sarlSection == null) {
					return false;
				}
				String sarlVersion = sarlSection.getValue(SREConstants.MANIFEST_SARL_SPEC_VERSION);
				if (sarlVersion == null || sarlVersion.isEmpty()) {
					return false;
				}
				Version sarlVer = Version.parseVersion(sarlVersion);
				return sarlVer != null;
			} catch (IOException exception) {
				return false;
			}
		}
		return false;
	}

	/** Replies if the given directory contains a SRE.
	 *
	 * @param directory - the directory.
	 * @return <code>true</code> if the given directory contains a SRE. Otherwise <code>false</code>.
	 * @see #isPackedSRE(File)
	 */
	public static boolean isUnpackedSRE(IPath directory) {
		IFile location = ResourcesPlugin.getWorkspace().getRoot().getFile(directory);
		if (location != null) {
			IPath path = location.getLocation();
			if (path != null) {
				File file = path.toFile();
				if (file.exists()) {
					if (file.isDirectory()) {
						return isUnpackedSRE(file);
					}
					return false;
				}
			}
		}
		return isUnpackedSRE(directory.makeAbsolute().toFile());
	}

	/** Replies if the given JAR file contains a SRE.
	 *
	 * @param jarFile - the JAR file to test.
	 * @return <code>true</code> if the given directory contains a SRE. Otherwise <code>false</code>.
	 * @see #isUnpackedSRE(File)
	 */
	public static boolean isPackedSRE(File jarFile) {
		try (JarFile jFile = new JarFile(jarFile)) {
			Manifest manifest = jFile.getManifest();
			if (manifest == null) {
				return false;
			}
			Attributes sarlSection = manifest.getAttributes(SREConstants.MANIFEST_SECTION_SRE);
			if (sarlSection == null) {
				return false;
			}
			String sarlVersion = sarlSection.getValue(SREConstants.MANIFEST_SARL_SPEC_VERSION);
			if (sarlVersion == null || sarlVersion.isEmpty()) {
				return false;
			}
			Version sarlVer = Version.parseVersion(sarlVersion);
			return sarlVer != null;
		} catch (IOException exception) {
			return false;
		}
	}

	/** Replies if the given JAR file contains a SRE.
	 *
	 * @param jarFile - the JAR file to test.
	 * @return <code>true</code> if the given directory contains a SRE. Otherwise <code>false</code>.
	 * @see #isUnpackedSRE(File)
	 */
	public static boolean isPackedSRE(IPath jarFile) {
		IFile location = ResourcesPlugin.getWorkspace().getRoot().getFile(jarFile);
		if (location != null) {
			IPath path = location.getLocation();
			if (path != null) {
				File file = path.toFile();
				if (file.exists()) {
					if (file.isFile()) {
						return isPackedSRE(file);
					}
					return false;
				}
			}
		}
		return isPackedSRE(jarFile.makeAbsolute().toFile());
	}

}
