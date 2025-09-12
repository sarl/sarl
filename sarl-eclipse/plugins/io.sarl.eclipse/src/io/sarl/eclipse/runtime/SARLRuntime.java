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

package io.sarl.eclipse.runtime;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;
import java.util.concurrent.locks.ReentrantLock;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.internal.launching.LaunchingMessages;
import org.eclipse.jdt.launching.PropertyChangeEvent;
import org.osgi.framework.Version;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import io.sarl.apputils.eclipseextensions.sreprovider.ISREInstall;
import io.sarl.apputils.eclipseextensions.sreprovider.ISREInstalls;
import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;

/**
 * The central access point for launching support. This class manages
 * the registered SRE types contributed through the
 * extension point with the name {@link SARLEclipseConfig#EXTENSION_POINT_SARL_RUNTIME_ENVIRONMENT}.
 * As well, this class provides SRE install change notification.
 *
 * <p>This class was inspired from {@code JavaRuntime}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @noinstantiate This class is not intended to be instantiated by clients.
 */
@SuppressWarnings("restriction")
public final class SARLRuntime {

	/**
	 * Preference key for the String of XML that defines all installed SREs.
	 */
	public static final String DEFAULT_PREFERENCE_KEY = SARLEclipsePlugin.PLUGIN_ID + ".runtime.PREF_SRE_XML"; //$NON-NLS-1$

	/** Flag that enables to turn on/off the SRE extensions points.
	 * This flag is mainly used for unit tests. It is not expected to be changed outside unit tests.
	 */
	private static boolean enableSreExtensionPoints = true;

	private static String currentPreferenceKey = DEFAULT_PREFERENCE_KEY;

	/**
	 * SRE change listeners.
	 */
	private static final ListenerList<ISREInstallChangedListener> SRE_LISTENERS = new ListenerList<>();

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
	 * <p>If the given key is {@code null} or empty, the preference key
	 * is reset to the {@link #DEFAULT_PREFERENCE_KEY}.
	 *
	 * @param key the new key or {@code null}.
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
	 * @param listener the listener to add
	 */
	public static void addSREInstallChangedListener(ISREInstallChangedListener listener) {
		SRE_LISTENERS.add(listener);
	}

	/**
	 * Removes the given listener from the list of registered SRE install changed
	 * listeners. Has no effect if an identical listener is not already registered.
	 *
	 * @param listener the listener to remove
	 */
	public static void removeSREInstallChangedListener(ISREInstallChangedListener listener) {
		SRE_LISTENERS.remove(listener);
	}

	/**
	 * Notifies all SRE install changed listeners of the given property change.
	 *
	 * @param event event describing the change.
	 */
	public static void fireSREChanged(PropertyChangeEvent event) {
		for (final var listener : SRE_LISTENERS.getListeners()) {
			((ISREInstallChangedListener) listener).sreChanged(event);
		}
	}

	/**
	 * Notifies all SRE install changed listeners of the addition of a SRE.
	 *
	 * @param installation the installed SRE.
	 */
	public static void fireSREAdded(ISREInstall installation) {
		for (final var listener : SRE_LISTENERS.getListeners()) {
			((ISREInstallChangedListener) listener).sreAdded(installation);
		}
	}

	/**
	 * Notifies all SRE install changed listeners of the removed of a SRE.
	 *
	 * @param installation the removed SRE.
	 */
	public static void fireSRERemoved(ISREInstall installation) {
		for (final var listener : SRE_LISTENERS.getListeners()) {
			((ISREInstallChangedListener) listener).sreRemoved(installation);
		}
	}

	/**
	 * Return the SRE corresponding to the specified Id.
	 *
	 * @param id the id that specifies an instance of ISREInstall
	 * @return the SRE corresponding to the specified Id, or {@code null}.
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
	 * @return the id of the default SRE, or {@code null} if none.
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
	 * Return the default SRE set with {@code setDefaultSRE()}.
	 *
	 * @return	Returns the default SRE. May return {@code null} when no default
	 *     SRE was set or when the default SRE has been disposed.
	 */
	public static ISREInstall getDefaultSREInstall() {
		final var install = getSREFromId(getDefaultSREId());
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
	 * @param sres The installed SREs.
	 * @param monitor the progress monitor to use for reporting progress to the user. It is the caller's responsibility
	 *        to call done() on the given monitor. Accepts {@code null}, indicating that no progress should be
	 *        reported and that the operation cannot be canceled.
	 * @throws CoreException if trying to set the default SRE install encounters problems
	 */
	public static void setSREInstalls(ISREInstall[] sres, IProgressMonitor monitor) throws CoreException {
		final var mon = SubMonitor.convert(monitor,
				io.sarl.eclipse.runtime.Messages.SARLRuntime_0,
				sres.length * 2 + ALL_SRE_INSTALLS.size());
		initializeSREs();
		final String oldDefaultId;
		String newDefaultId;
		final var newElements = new ArrayList<ISREInstall>();
		final Map<String, ISREInstall> allKeys;
		LOCK.lock();
		try {
			oldDefaultId = getDefaultSREId();
			newDefaultId = oldDefaultId;
			allKeys = new TreeMap<>(ALL_SRE_INSTALLS);
			for (final var sre : sres) {
				if (allKeys.remove(sre.getId()) == null) {
					newElements.add(sre);
					ALL_SRE_INSTALLS.put(sre.getId(), sre);
				}
				mon.worked(1);
			}
			for (final var sre : allKeys.values()) {
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
		var changed = false;
		mon.subTask(io.sarl.eclipse.runtime.Messages.SARLRuntime_1);
		if (oldDefaultId != null && newDefaultId == null) {
			changed = true;
			setDefaultSREInstall(null, monitor);
		}
		mon.worked(1);
		mon.subTask(io.sarl.eclipse.runtime.Messages.SARLRuntime_2);
		for (final var sre : allKeys.values()) {
			changed = true;
			fireSRERemoved(sre);
		}
		for (final var sre : newElements) {
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
	 * @param sre The SRE to make the default. May be {@code null} to clear
	 * 				the default.
	 * @param monitor progress monitor or {@code null}
	 * @throws CoreException if trying to set the default SRE install encounters problems
	 */
	public static void setDefaultSREInstall(ISREInstall sre, IProgressMonitor monitor) throws CoreException {
		setDefaultSREInstall(sre, monitor, true);
	}

	/**
	 * Sets a SRE as the system-wide default SRE, and notifies registered SRE install
	 * change listeners of the change.
	 *
	 * @param sre The SRE to make the default. May be {@code null} to clear
	 * 				the default.
	 * @param monitor progress monitor or {@code null}
	 * @param savePreference If {@code true}, update workbench preferences to reflect
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
			defaultSREId = sre == null ? null : sre.getId();
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
	 * @param previous the previous SRE
	 * @param current the new current default SRE
	 */
	private static void fireDefaultSREChanged(ISREInstall previous, ISREInstall current) {
		for (final var listener : SRE_LISTENERS.getListeners()) {
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
	 * @param sre the sre.
	 * @return {@code true} if the SRE was provided through an extension
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
	 * @param monitor the progression monitor, or {@code null}.
	 * @throws CoreException if trying to save the current state of SREs encounters a problem
	 */
	public static void saveSREConfiguration(IProgressMonitor monitor) throws CoreException {
		final var plugin = SARLEclipsePlugin.getDefault();
		plugin.getPreferences().put(getCurrentPreferenceKey(), getSREsAsXML(monitor));
		plugin.savePreferences();
	}

	/**
	 * Remove the SRE configuration information from the preferences.
	 *
	 * @throws CoreException if trying to save the current state of SREs encounters a problem
	 */
	public static void clearSREConfiguration() throws CoreException {
		final var plugin = SARLEclipsePlugin.getDefault();
		plugin.getPreferences().remove(getCurrentPreferenceKey());
		plugin.savePreferences();
	}

	/**
	 * Initializes SRE extensions.
	 */
	private static void initializeSREExtensions() {
		ISREInstalls.getSREInstallStreamFromExtension()
			.forEach(sre -> {
				platformSREInstalls.add(sre.getId());
				ALL_SRE_INSTALLS.put(sre.getId(), sre);
			});
	}

	/**
	 * Returns the XML representation of the given SRE.
	 *
	 * @param sre the SRE to serialize.
	 * @return an XML representation of the given SRE.
	 * @throws CoreException if trying to compute the XML for the SRE state encounters a problem.
	 */
	public static String getSREAsXML(ISREInstall sre) throws CoreException {
		try {
			final var factory = DocumentBuilderFactory.newInstance();
			final var builder = factory.newDocumentBuilder();
			final var xmldocument = builder.newDocument();

			final var sreNode = xmldocument.createElement("SRE"); //$NON-NLS-1$
			sreNode.setAttribute("platform", Boolean.toString(isPlatformSRE(sre))); //$NON-NLS-1$
			sreNode.setAttribute("id", sre.getId()); //$NON-NLS-1$
			sreNode.setAttribute("class", sre.getClass().getName()); //$NON-NLS-1$
			sre.getAsXML(xmldocument, sreNode);
			xmldocument.appendChild(sreNode);

			final var transFactory = TransformerFactory.newInstance();
			final var trans = transFactory.newTransformer();
			try (var baos = new ByteArrayOutputStream()) {
				final var source = new DOMSource(xmldocument);
				final var flot = new PrintWriter(baos);
				final var xmlStream = new StreamResult(flot);
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
	 * @param sre the SRE to serialize.
	 * @param xml the XML representation of the given SRE.
	 * @throws CoreException if trying to compute the XML for the SRE state encounters a problem.
	 */
	public static void setSREFromXML(ISREInstall sre, String xml) throws CoreException {
		try {
			final var root = parseXML(xml, false);
			sre.setFromXML(root);
		} catch (Throwable e) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, e));
		}
	}

	/**
	 * Returns the listing of currently installed SREs as a single XML file.
	 *
	 * @param monitor monitor on the XML building.
	 * @return an XML representation of all of the currently installed SREs.
	 * @throws CoreException if trying to compute the XML for the SRE state encounters a problem.
	 */
	public static String getSREsAsXML(IProgressMonitor monitor) throws CoreException {
		initializeSREs();
		try {
			final var factory = DocumentBuilderFactory.newInstance();
			final var builder = factory.newDocumentBuilder();
			final var xmldocument = builder.newDocument();
			final var rootElement = getXml(xmldocument);
			xmldocument.appendChild(rootElement);
			final var transFactory = TransformerFactory.newInstance();
			final var trans = transFactory.newTransformer();
			try (var baos = new ByteArrayOutputStream()) {
				final var source = new DOMSource(xmldocument);
				final var flot = new PrintWriter(baos);
				final var xmlStream = new StreamResult(flot);
				trans.transform(source, xmlStream);
				return new String(baos.toByteArray());
			}
		} catch (Throwable e) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, e));
		}
	}

	private static Element getXml(Document xmlDocument) throws IOException {
		final var sresNode = xmlDocument.createElement("SREs"); //$NON-NLS-1$
		LOCK.lock();
		try {
			for (final var sre : ALL_SRE_INSTALLS.values()) {
				final var sreNode = xmlDocument.createElement("SRE"); //$NON-NLS-1$
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
		try (var stream = new BufferedInputStream(new ByteArrayInputStream(rawXml.getBytes()))) {
			return parseXML(stream, isMultiple);
		}
	}

	private static Element parseXML(InputStream stream, boolean isMultiple) throws IOException {
		Element config = null;
		try {
			final var parser = DocumentBuilderFactory.newInstance().newDocumentBuilder();
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
				final var type = Class.forName(classname).asSubclass(ISREInstall.class);
				final var cons = type.getConstructor(String.class);
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
	private static String initializePersistedSREs() {
		//		// FOR DEBUG
		//		try {
		//			clearSREConfiguration();
		//		} catch (CoreException e1) {
		//			e1.printStackTrace();
		//		}
		final var rawXml = SARLEclipsePlugin.getDefault().getPreferences().get(
				getCurrentPreferenceKey(), ""); //$NON-NLS-1$

		try {
			Element config = null;
			// If the preference was found, load SREs from it into memory
			if (!Strings.isNullOrEmpty(rawXml)) {
				config = parseXML(rawXml, true);
			} else {
				// Otherwise, look for the old file that previously held the SRE definitions
				final var plugin = SARLEclipsePlugin.getDefault();
				if (plugin.getBundle() != null) {
					final var stateLocation = plugin.getStateLocation();
					final var stateFile = stateLocation.append("sreConfiguration.xml"); //$NON-NLS-1$
					final var file = stateFile.toFile();
					if (file.exists()) {
						// If file exists, load SRE definitions from it into memory and
						// write the definitions to the preference store WITHOUT triggering
						// any processing of the new value
						try (var fileInputStream = new BufferedInputStream(new FileInputStream(file))) {
							config = parseXML(fileInputStream, true);
						} catch (IOException e) {
							SARLEclipsePlugin.getDefault().log(e);
						}
					}
				}
			}
			if (config != null) {
				final var defaultId = config.getAttribute("defaultSRE"); //$NON-NLS-1$
				final var children = config.getChildNodes();
				for (var i = 0; i < children.getLength(); ++i) {
					try {
						final var child = children.item(i);
						if ("SRE".equalsIgnoreCase(child.getNodeName()) //$NON-NLS-1$
								&& child instanceof Element element) {
							final var isPlatform = Boolean.parseBoolean(element.getAttribute("platform")); //$NON-NLS-1$
							final var id = element.getAttribute("id"); //$NON-NLS-1$
							if (!isPlatform || !(ALL_SRE_INSTALLS.containsKey(id))) {
								final var sre = createSRE(
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
								final var sre = ALL_SRE_INSTALLS.get(id);
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
	private static void initializeSREs() {
		var newSREs = new ISREInstall[0];
		var savePrefs = false;
		LOCK.lock();
		final var previousDefault = defaultSREId;
		try {
			if (platformSREInstalls == null) {
				platformSREInstalls = new HashSet<>();
				ALL_SRE_INSTALLS.clear();

				// Install the SREs from the Eclipse extension points
				if (enableSreExtensionPoints) {
					initializeSREExtensions();
				}
				// install the SREs from the user-defined preferences.
				final var predefinedDefaultId = Strings.nullToEmpty(initializePersistedSREs());

				newSREs = new ISREInstall[ALL_SRE_INSTALLS.size()];

				// Verify default SRE is valid
				ISREInstall initDefaultSRE = null;
				final var iterator = ALL_SRE_INSTALLS.values().iterator();
				for (var i = 0; iterator.hasNext(); ++i) {
					final var sre = iterator.next();
					newSREs[i] = sre;
					if (sre.getValidity().isOK()) {
						if (initDefaultSRE == null
								&& sre.getId().equals(predefinedDefaultId)) {
							initDefaultSRE = sre;
						}
					}
				}

				final var oldDefaultId = initDefaultSRE == null ? null : initDefaultSRE.getId();
				defaultSREId = oldDefaultId;
				savePrefs = true;
			}

			if (Strings.isNullOrEmpty(defaultSREId)) {
				ISREInstall firstSRE = null;
				ISREInstall firstValidSRE = null;
				final var iterator = ALL_SRE_INSTALLS.values().iterator();
				while (firstValidSRE == null && iterator.hasNext()) {
					final var sre = iterator.next();
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
			for (final ISREInstall sre : newSREs) {
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
		String id;
		do {
			id = UUID.randomUUID().toString();
		} while (getSREFromId(id) != null);
		return id;
	}

	/** Reset the list of the SREs to the default ones (the platform SREs).
	 *
	 * @throws CoreException if a problem occurs during the reset.
	 */
	public static void reset() throws CoreException {
		LOCK.lock();
		try {
			// Clear the SRE configuration stored into the preferences.
			clearSREConfiguration();
			// Reset the internal data structures.
			final var previous = getDefaultSREInstall();
			final var oldSREs = new HashMap<>(ALL_SRE_INSTALLS);
			ALL_SRE_INSTALLS.clear();
			platformSREInstalls = null;
			defaultSREId = null;
			// Notify about the removals
			for (final var sre : oldSREs.values()) {
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
	 * @param directory the directory.
	 * @return {@code true} if the given directory contains a SRE. Otherwise {@code false}.
	 * @see #isPackedSRE(File)
	 */
	public static boolean isUnpackedSRE(File directory) {
		var manifestFile = new File(directory, "META-INF"); //$NON-NLS-1$
		manifestFile = new File(manifestFile, "MANIFEST.MF"); //$NON-NLS-1$
		if (manifestFile.canRead()) {
			try (var manifestStream = new FileInputStream(manifestFile)) {
				final var manifest = new Manifest(manifestStream);
				final var sarlSection = manifest.getAttributes(SREManifestPreferenceConstants.MANIFEST_SECTION_SRE);
				if (sarlSection == null) {
					return false;
				}
				final var sarlVersion = sarlSection.getValue(SREManifestPreferenceConstants.MANIFEST_SARL_SPEC_VERSION);
				if (sarlVersion == null || sarlVersion.isEmpty()) {
					return false;
				}
				final var sarlVer = Version.parseVersion(sarlVersion);
				return sarlVer != null;
			} catch (IOException exception) {
				return false;
			}
		}
		return false;
	}

	/** Replies if the given directory contains a SRE.
	 *
	 * @param directory the directory.
	 * @return {@code true} if the given directory contains a SRE. Otherwise {@code false}.
	 * @see #isPackedSRE(File)
	 */
	public static boolean isUnpackedSRE(IPath directory) {
		try {
			final var location = ResourcesPlugin.getWorkspace().getRoot().getFile(directory);
			if (location != null) {
				final var path = location.getLocation();
				if (path != null) {
					final var file = path.toFile();
					if (file.exists()) {
						if (file.isDirectory()) {
							return isUnpackedSRE(file);
						}
						return false;
					}
				}
			}
			return isUnpackedSRE(directory.makeAbsolute().toFile());
		} catch (Exception exception) {
			return false;
		}
	}

	/** Replies if the given JAR file contains a SRE.
	 *
	 * <p>The SRE detection is based on the content of the manifest.
	 *
	 * @param jarFile the JAR file to test.
	 * @return {@code true} if the given directory contains a SRE. Otherwise {@code false}.
	 * @see #isUnpackedSRE(File)
	 */
	public static boolean isPackedSRE(File jarFile) {
		try (var jFile = new JarFile(jarFile)) {
			final var manifest = jFile.getManifest();
			if (manifest == null) {
				return false;
			}
			final var sarlSection = manifest.getAttributes(SREManifestPreferenceConstants.MANIFEST_SECTION_SRE);
			if (sarlSection == null) {
				return false;
			}
			final var sarlVersion = sarlSection.getValue(SREManifestPreferenceConstants.MANIFEST_SARL_SPEC_VERSION);
			if (sarlVersion == null || sarlVersion.isEmpty()) {
				return false;
			}
			final var sarlVer = Version.parseVersion(sarlVersion);
			return sarlVer != null;
		} catch (IOException exception) {
			return false;
		}
	}

	/** Replies if the given JAR file contains a SRE.
	 *
	 * <p>The SRE detection is based on the content of the manifest.
	 *
	 * @param jarFile the JAR file to test.
	 * @return {@code true} if the given directory contains a SRE. Otherwise {@code false}.
	 * @see #isUnpackedSRE(File)
	 */
	public static boolean isPackedSRE(IPath jarFile) {
		try {
			final var location = ResourcesPlugin.getWorkspace().getRoot().getFile(jarFile);
			if (location != null) {
				final var path = location.getLocation();
				if (path != null) {
					final var file = path.toFile();
					if (file.exists()) {
						if (file.isFile()) {
							return isPackedSRE(file);
						}
						return false;
					}
				}
			}
			return isPackedSRE(jarFile.makeAbsolute().toFile());
		} catch (Exception exception) {
			return false;
		}
	}

	/** Replies if the given directory contains a SRE bootstrap.
	 *
	 * <p>The SRE bootstrap detection is based on the service definition within META-INF folder.
	 *
	 * @param directory the directory.
	 * @return {@code true} if the given directory contains a SRE. Otherwise {@code false}.=
	 * @since 0.7
	 * @see #containsPackedBootstrap(File)
	 */
	public static boolean containsUnpackedBootstrap(File directory) {
		final var elements = SREManifestPreferenceConstants.SERVICE_SRE_BOOTSTRAP.split("/"); //$NON-NLS-1$
		var serviceFile = directory;
		for (final var element : elements) {
			serviceFile = new File(serviceFile, element);
		}
		if (serviceFile.isFile() && serviceFile.canRead()) {
			try (var is = new FileInputStream(serviceFile)) {
				try (var reader = new BufferedReader(new InputStreamReader(is))) {
					var line = reader.readLine();
					if (line != null) {
						line = line.trim();
						if (!line.isEmpty()) {
							return true;
						}
					}
				}
			} catch (Throwable exception) {
				//
			}
		}
		return false;
	}

	/** Replies if the given directory contains a SRE bootstrap.
	 *
	 * <p>The SRE bootstrap detection is based on the service definition within META-INF folder.
	 *
	 * @param directory the directory.
	 * @return {@code true} if the given directory contains a SRE. Otherwise {@code false}.
	 * @since 0.7
	 * @see #containsPackedBootstrap(File)
	 * @see #getDeclaredBootstrap(IPath)
	 */
	public static boolean containsUnpackedBootstrap(IPath directory) {
		final var location = ResourcesPlugin.getWorkspace().getRoot().getFile(directory);
		if (location != null) {
			final var path = location.getLocation();
			if (path != null) {
				final var file = path.toFile();
				if (file.exists()) {
					if (file.isDirectory()) {
						return containsUnpackedBootstrap(file);
					}
					return false;
				}
			}
		}
		return containsUnpackedBootstrap(directory.makeAbsolute().toFile());
	}

	/** Replies if the given JAR file contains a SRE bootstrap.
	 *
	 * <p>The SRE bootstrap detection is based on the service definition within META-INF folder.
	 *
	 * @param jarFile the JAR file to test.
	 * @return {@code true} if the given directory contains a SRE. Otherwise {@code false}.
	 * @since 0.7
	 * @see #containsUnpackedBootstrap(File)
	 */
	public static boolean containsPackedBootstrap(File jarFile) {
		try (var jFile = new JarFile(jarFile)) {
			final var jEntry = jFile.getEntry(SREManifestPreferenceConstants.SERVICE_SRE_BOOTSTRAP);
			if (jEntry != null) {
				try (var is = jFile.getInputStream(jEntry)) {
					try (var reader = new BufferedReader(new InputStreamReader(is))) {
						var line = reader.readLine();
						if (line != null) {
							line = line.trim();
							if (!line.isEmpty()) {
								return true;
							}
						}
					}
				}
			}
		} catch (IOException exception) {
			//
		}
		return false;
	}

	/** Replies if the given JAR file contains a SRE bootstrap.
	 *
	 * <p>The SRE bootstrap detection is based on the service definition within META-INF folder.
	 *
	 * @param jarFile the JAR file to test.
	 * @return {@code true} if the given directory contains a SRE. Otherwise {@code false}.
	 * @since 0.7
	 * @see #containsUnpackedBootstrap(File)
	 * @see #getDeclaredBootstrap(IPath)
	 */
	public static boolean containsPackedBootstrap(IPath jarFile) {
		try {
			final var location = ResourcesPlugin.getWorkspace().getRoot().getFile(jarFile);
			if (location != null) {
				final var path = location.getLocation();
				if (path != null) {
					final var file = path.toFile();
					if (file.exists()) {
						if (file.isFile()) {
							return containsPackedBootstrap(file);
						}
						return false;
					}
				}
			}
			return containsPackedBootstrap(jarFile.makeAbsolute().toFile());
		} catch (Exception exception) {
			return false;
		}
	}

	/** Replies the bootstrap name declared within the given path, corresponding to a JAR file or a folder.
	 *
	 * <p>The SRE bootstrap detection is based on the service definition within META-INF folder.
	 *
	 * @param path the path to test.
	 * @return the bootstrap or {@code null} if none.
	 * @since 0.7
	 * @see #containsPackedBootstrap(IPath)
	 * @see #containsUnpackedBootstrap(IPath)
	 */
	public static String getDeclaredBootstrap(IPath path) {
		try {
			final var location = ResourcesPlugin.getWorkspace().getRoot().getFile(path);
			if (location != null) {
				final var pathLocation = location.getLocation();
				if (pathLocation != null) {
					final var file = pathLocation.toFile();
					if (file.exists()) {
						if (file.isDirectory()) {
							return getDeclaredBootstrapInFolder(file);
						}
						if (file.isFile()) {
							return getDeclaredBootstrapInJar(file);
						}
						return null;
					}
				}
			}
			final var file = path.makeAbsolute().toFile();
			if (file.exists()) {
				if (file.isDirectory()) {
					return getDeclaredBootstrapInJar(file);
				}
				if (file.isFile()) {
					return getDeclaredBootstrapInFolder(file);
				}
			}
		} catch (Exception exception) {
			//
		}
		return null;
	}

	private static String getDeclaredBootstrapInJar(File jarFile) {
		try (var jFile = new JarFile(jarFile)) {
			final var jEntry = jFile.getEntry(SREManifestPreferenceConstants.SERVICE_SRE_BOOTSTRAP);
			if (jEntry != null) {
				try (var is = jFile.getInputStream(jEntry)) {
					try (var reader = new BufferedReader(new InputStreamReader(is))) {
						var line = reader.readLine();
						if (line != null) {
							line = line.trim();
							if (!line.isEmpty()) {
								return line;
							}
						}
					}
				}
			}
		} catch (IOException exception) {
			//
		}
		return null;
	}

	private static String getDeclaredBootstrapInFolder(File directory) {
		final var elements = SREManifestPreferenceConstants.SERVICE_SRE_BOOTSTRAP.split("/"); //$NON-NLS-1$
		var serviceFile = directory;
		for (final var element : elements) {
			serviceFile = new File(serviceFile, element);
		}
		if (serviceFile.isFile() && serviceFile.canRead()) {
			try (var is = new FileInputStream(serviceFile)) {
				try (var reader = new BufferedReader(new InputStreamReader(is))) {
					var line = reader.readLine();
					if (line != null) {
						line = line.trim();
						if (!line.isEmpty()) {
							return line;
						}
					}
				}
			} catch (Throwable exception) {
				//
			}
		}
		return null;
	}

}
