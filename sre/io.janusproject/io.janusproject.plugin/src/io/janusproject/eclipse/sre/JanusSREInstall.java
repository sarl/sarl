/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.janusproject.eclipse.sre;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.google.common.collect.Maps;
import io.janusproject.Boot;
import io.janusproject.JanusConfig;
import io.janusproject.eclipse.JanusEclipsePlugin;
import io.janusproject.eclipse.buildpath.JanusClasspathContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.core.ClasspathEntry;
import org.eclipse.jdt.internal.core.JavaProject;
import org.eclipse.jdt.internal.core.util.Util;
import org.eclipse.jdt.internal.launching.RuntimeClasspathEntry;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.xtext.xbase.lib.Pair;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;
import org.osgi.framework.wiring.BundleWire;
import org.osgi.framework.wiring.BundleWiring;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.runtime.AbstractSREInstall;
import io.sarl.eclipse.runtime.SREConstants;
import io.sarl.eclipse.util.BundleUtil;

/**
 * Provide Janus as a SRE install.
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JanusSREInstall extends AbstractSREInstall {

	/**
	 * The unique identifier of this SRE.
	 */
	public static final String JANUS_SRE_ID = "io.janusproject.plugin.sre"; //$NON-NLS-1$

	private static final String ROOT_NAME = "/"; //$NON-NLS-1$

	private static final String JAR_EXTENSION = "jar"; //$NON-NLS-1$

	private static final String DOT_JAR_EXTENSION = "." + JAR_EXTENSION; //$NON-NLS-1$

	private static final String DEFAULT_PATH_TO_CLASSES_IN_MAVEN_PROJECT = "target/classes"; //$NON-NLS-1$

	private static Set<String> janusBundleDependencies;

	/**
	 * The path where this SRE plugin jar is effectively installed.
	 */
	private IPath janusSREInstallPath;

	/**
	 * The map associating a given bundle (the Janus plugins and its transitive dependencies) to its corresponding ClassPath Entry.
	 * The version is used when we have multiple times the same bundle with different version, in this case, we only kept the latest version.
	 */
	private Map<Bundle, Pair<Version, List<IRuntimeClasspathEntry>>> dependencies = new TreeMap<>(new Comparator<Bundle>() {
		@Override
		public int compare(Bundle o1, Bundle o2) {
			return o1.getSymbolicName().compareTo(o2.getSymbolicName());
		}
	});

	/**
	 * The path of this installation of the Janus plugin.
	 */
	private String location;

	/**
	 * Creates the a JANUS SRE install.
	 */
	public JanusSREInstall() {
		super(JANUS_SRE_ID);
		final Bundle bundle = Platform.getBundle(JanusEclipsePlugin.PLUGIN_ID);
		final IPath bundlePath = BundleUtil.getBundlePath(bundle);
		if (bundlePath.toFile().isDirectory()) {
			// we have a directory, we assume we are in debug mode of the
			// product
			final IPath bundleSourcePath = BundleUtil.getSourceBundlePath(bundle, bundlePath);

			// Default value of the output folder for our project but will be
			// overload later in we find a .classpath precising the output
			// folder
			this.janusSREInstallPath = Path.fromPortableString(bundleSourcePath.toPortableString().concat(DEFAULT_PATH_TO_CLASSES_IN_MAVEN_PROJECT));

			URL janusBundleURL = null;
			try {
				janusBundleURL = new URL("file://" + bundleSourcePath.toPortableString()); //$NON-NLS-1$
			} catch (MalformedURLException e) {
				SARLEclipsePlugin.getDefault().log(e);
				return;
			}

			this.janusSREInstallPath = this.readDotClasspathAndReferencestoClasspath(bundle, janusBundleURL);

		} else {
			this.janusSREInstallPath = bundlePath;
			final IClasspathEntry cpEntry = newLibraryEntry(bundle, bundlePath);
			final List<IRuntimeClasspathEntry> cpEntries = new ArrayList<>();
			addToRuntimeClassPath(bundle, cpEntry, cpEntries);
			this.dependencies.put(bundle, new Pair<>(bundle.getVersion(), cpEntries));
		}
		this.location = this.janusSREInstallPath.toPortableString();
		this.setName(JanusConfig.JANUS_DEFAULT_PLATFORM_NAME);

		// Parsing all bundle dependencies
		getAllBundleDependencies(bundle, true);

		final List<IRuntimeClasspathEntry> classpathEntries = new ArrayList<>();
		for (final Pair<Version, List<IRuntimeClasspathEntry>> cpe : this.dependencies.values()) {
			classpathEntries.addAll(cpe.getValue());
		}

		this.setMainClass(Boot.class.getName());

		this.setClassPathEntries(new ArrayList<>(classpathEntries));
	}

	/** Create a library entry for the given bundle.
	 *
	 * @param bundle the bundle to point to. Never <code>null</code>.
	 * @param precomputedBundlePath the path to the bundle that is already available. If <code>null</code>,
	 *      the path is computed from the bundle with {@link BundleUtil}.
	 * @return the class path entry that corresponds to the bundle.
	 */
	private static IClasspathEntry newLibraryEntry(Bundle bundle, IPath precomputedBundlePath) {
		assert bundle != null;
		final IPath path = precomputedBundlePath == null ? BundleUtil.getBundlePath(bundle) : precomputedBundlePath;
		final IPath sourcePath = BundleUtil.getSourceBundlePath(bundle, path);
		return JavaCore.newLibraryEntry(path, sourcePath, null);
	}

	/** Add the giiven bundle to the entries.
	 *
	 * <p>This function add the classpath entry for the bundle, and the related fragments.
	 *
	 * @param bundle the bundle to point to. Never <code>null</code>.
	 * @param entry the classpath entry to add to. Never <code>null</code>.
	 * @param entries the list of entries to add to.
	 */
	private static void addToRuntimeClassPath(Bundle bundle, IClasspathEntry entry, Collection<IRuntimeClasspathEntry> entries) {
		assert bundle != null;
		assert entry != null;
		entries.add(new RuntimeClasspathEntry(entry));
		final Bundle[] fragments = Platform.getFragments(bundle);
		if (fragments != null && fragments.length > 0) {
			for (final Bundle fragment : fragments) {
				final IClasspathEntry fragmentEntry = newLibraryEntry(fragment, null);
				entries.add(new RuntimeClasspathEntry(fragmentEntry));
			}
		}
	}

	private static Set<String> getJanusBundleDependencies() {
		if (janusBundleDependencies == null) {
			final JanusClasspathContainer container = new JanusClasspathContainer(null);
			janusBundleDependencies = container.getBundleDependencies();
		}
		return janusBundleDependencies;
	}

	/**
	 * Recrusive function to get all the required runtime dependencies of this SRE plugin and adding the corresponding elements to the
	 * {@code classpathEntries} collection.
	 *
	 * @param bundle
	 *            - the bundle used as root to start the dynamic search of dependencies
	 * @param firstcall
	 *            - boolean specifying if we are at the first recursive call, in this case we use the {@code runtimeNecessaryDependencies} collections
	 *            to filter the dependencies that are really useful at runtime.
	 */
	@SuppressWarnings("checkstyle:nestedifdepth")
	private void getAllBundleDependencies(Bundle bundle, boolean firstcall) {

		final BundleWiring bundleWiring = bundle.adapt(BundleWiring.class);
		final List<BundleWire> bundleWires = bundleWiring.getRequiredWires(null);

		if (bundleWires != null) {
			Bundle dependency = null;
			for (final BundleWire wire : bundleWires) {

				dependency = wire.getProviderWiring().getBundle();
				final String dependencyInstallationPath = dependency.getLocation();

				final Pair<Version, List<IRuntimeClasspathEntry>> existingDependencyCPE = this.dependencies.get(dependency);
				if (existingDependencyCPE == null
						|| dependency.getVersion().compareTo(existingDependencyCPE.getKey()) > 0) {
					if (firstcall) {
						// First level of dependencies that are filtered according to runtimeNecessaryDependencies
						if (getJanusBundleDependencies().contains(dependency.getSymbolicName())) {
							URL u = null;
							try {
								u = FileLocator.resolve(dependency.getEntry(ROOT_NAME));
							} catch (IOException e) {
								SARLEclipsePlugin.getDefault().log(e);
								return;
							}

							if (dependencyInstallationPath.contains(JAR_EXTENSION) || u.getProtocol().equals(JAR_EXTENSION)) {

								final IClasspathEntry cpEntry = newLibraryEntry(dependency, null);
								final List<IRuntimeClasspathEntry> cpEntries = new ArrayList<>();
								addToRuntimeClassPath(bundle, cpEntry, cpEntries);
								this.dependencies.put(dependency, new Pair<>(dependency.getVersion(), cpEntries));

							} else {
								// Management of a project having a .classpath to get the classapth
								readDotClasspathAndReferencestoClasspath(dependency, u);
							}

							getAllBundleDependencies(dependency, false);

						} else {
							//DO NOTHING this is a dependency that is required at runtime by the launch configuration
						}

					} else {
						URL u = null;
						try {
							u = FileLocator.resolve(dependency.getEntry(ROOT_NAME));
						} catch (IOException e) {

							SARLEclipsePlugin.getDefault().log(e);
							return;
						}

						if (dependencyInstallationPath.contains(JAR_EXTENSION) || u.getProtocol().equals(JAR_EXTENSION)) {

							final IClasspathEntry cpEntry = newLibraryEntry(dependency, null);
							final List<IRuntimeClasspathEntry> cpEntries = new ArrayList<>();
							addToRuntimeClassPath(bundle, cpEntry, cpEntries);
							this.dependencies.put(dependency, new Pair<>(dependency.getVersion(), cpEntries));

						} else {
							// Management of a project having a .classpath to get
							// the classapth
							readDotClasspathAndReferencestoClasspath(dependency, u);
						}

						getAllBundleDependencies(dependency, false);
					}
				}
			}
		}
	}

	/**
	 * Detects if the specified bundle is a jar or a directory and react accordingly to parse its content.
	 *
	 * @param bundle
	 *            - the bundle to check
	 */
	/*private void addBundleClasspathAndReferencetoClasspath(Bundle bundle) {

        final IPath bundlePath = BundleUtil.getBundlePath(bundle);

        if (bundlePath.toFile().isDirectory()) {
            // we have a directory, we assume we are in debug mode of a product
            final IPath bundleSourcePath = BundleUtil.getSourceBundlePath(bundle, bundlePath);
            if (bundleSourcePath == null) {
                // Directory without source directory, could be just a directory
                // containing a collection of jar
                // FIXME when it is the case usually we have a problem with the
                // path used to refer the these jar files
                URL bundleURL = null;
                try {
                    bundleURL = new URL("file://" + bundlePath.toPortableString());
                } catch (MalformedURLException e) {
                    SARLEclipsePlugin.getDefault().log(e);
                    return;
                }
                readDotClasspathAndReferencestoClasspath(bundle, bundleURL);

            } else {
                // Directory with a source directory
                URL bundleURL = null;
                try {
                    bundleURL = new URL("file://" + bundleSourcePath.toPortableString());
                } catch (MalformedURLException e) {
                    SARLEclipsePlugin.getDefault().log(e);
                    return;
                }
                readDotClasspathAndReferencestoClasspath(bundle, bundleURL);
            }

        } else {
            if (bundlePath.toFile().isFile() && bundlePath.getFileExtension().equals("jar")) {
                // this is a common jar file, just add it to the classpath
                final IClasspathEntry cpEntry = newLibraryEntry(bundle, bundlePath);
                final List<IRuntimeClasspathEntry> cpEntries = new ArrayList<>();
                cpEntries.add(new RuntimeClasspathEntry(cpEntry));
                System.out.println("Adding bundle itself " + bundle);
                this.dependencies.put(bundle, new Pair<>(bundle.getVersion(), cpEntries));
            }

        }

    }*/

	/**
	 * Explore the various entries of a bundle to find its .classpath file, parse it and update accordingly the {@code classpathEntries}
	 * collection of this bundle.
	 *
	 * @param bundle
	 *            - the bundle to explore
	 * @param bundleInstallURL
	 *            - the URL where the specified bundle is stored
	 * @return the Path to the output folder used to store .class file if any (if we are in an eclipse project (debug mode))
	 */
	private IPath readDotClasspathAndReferencestoClasspath(Bundle bundle, URL bundleInstallURL) {
		IPath outputLocation = null;
		final List<IRuntimeClasspathEntry> cpEntries = new ArrayList<>();
		final Enumeration<String> entries = bundle.getEntryPaths("/"); //$NON-NLS-1$
		String entry = null;
		while (entries.hasMoreElements()) {
			entry = entries.nextElement();
			if (entry.contains(JavaProject.CLASSPATH_FILENAME)) { // $NON-NLS-1$
				try {
					// copied from See {@link JavaProject#decodeClasspath}
					final IClasspathEntry[][] classpath = JavaClasspathParser.readFileEntriesWithException(bundle.getSymbolicName(),
							bundleInstallURL);

					// extract the output location
					if (classpath[0].length > 0) {
						final IClasspathEntry outputLocationEntry = classpath[0][classpath[0].length - 1];
						if (outputLocationEntry.getContentKind() == ClasspathEntry.K_OUTPUT) {
							outputLocation = outputLocationEntry.getPath();
							cpEntries.add(new RuntimeClasspathEntry(outputLocationEntry));
						}
					}

					// discard the output location and add others real entries
					// to the classpath
					IClasspathEntry[] rawClassapth = null;
					if (classpath[0].length > 0) {
						final IClasspathEntry otherEntries = classpath[0][classpath[0].length - 1];
						if (otherEntries.getContentKind() == ClasspathEntry.K_OUTPUT) {
							final IClasspathEntry[] copy = new IClasspathEntry[classpath[0].length - 1];
							System.arraycopy(classpath[0], 0, copy, 0, copy.length);
							rawClassapth = copy;

							IClasspathEntry cpentry = null;
							for (int i = 0; i < rawClassapth.length; i++) {
								cpentry = rawClassapth[i];
								if (cpentry.getEntryKind() == IClasspathEntry.CPE_CONTAINER) {
									// FIXME do something if we have a
									// container, usually this is the JRE
									// container
									// already managed by the launch
									// configuration, that's why we do nothing
								} else if (cpentry.getEntryKind() == IClasspathEntry.CPE_SOURCE) {
									// FIXME do something with package fragments
								} else {
									// Common entry type:
									// CPE_PROJECT|CPE_LIBRARY|CPE_VARIABLE,
									// directly managed by RuntimeClasspathEntry
									cpEntries.add(new RuntimeClasspathEntry(cpentry));
								}
							}
						}

					}


				} catch (IOException | CoreException | URISyntaxException e) {

					SARLEclipsePlugin.getDefault().log(e);
					return null;
				}

			} else if (entry.contains(DOT_JAR_EXTENSION)) {
				// A jar inside the bundle
				// FIXME we have an error at runtime to these referenced jars
				try {
					final URL bundleJARfileFullURL = new URL(bundleInstallURL.toExternalForm().concat(File.separator).concat(entry));
					final File jarFile = Util.toLocalFile(bundleJARfileFullURL.toURI(), null);
					final IPath jarFilePath = new Path(jarFile.getAbsolutePath());
					final IClasspathEntry cpEntry = newLibraryEntry(bundle, jarFilePath);
					addToRuntimeClassPath(bundle, cpEntry, cpEntries);
				} catch (CoreException | URISyntaxException | MalformedURLException e) {

					SARLEclipsePlugin.getDefault().log(e);
					return null;
				}
			}
		}


		if (cpEntries.size() > 0) {
			this.dependencies.put(bundle, new Pair<>(bundle.getVersion(), cpEntries));
		}

		return outputLocation;
	}

	@Override
	public String getNameNoDefault() {
		return Messages.JanusSREInstall_0;
	}

	@Override
	public String getLocation() {
		return this.location;
	}

	@Override
	public Map<String, String> getAvailableCommandLineOptions() {
		final Map<String, String> options = Maps.newHashMap();
		options.put(SREConstants.MANIFEST_CLI_SHOW_LOGO, ""); //$NON-NLS-1$
		options.put(SREConstants.MANIFEST_CLI_HIDE_LOGO, formatCommandLineOption(Boot.CLI_OPTION_NOLOGO_LONG, null));
		options.put(SREConstants.MANIFEST_CLI_SHOW_INFO, formatCommandLineOption(Boot.CLI_OPTION_LOG_LONG, "info")); //$NON-NLS-1$
		options.put(SREConstants.MANIFEST_CLI_HIDE_INFO, formatCommandLineOption(Boot.CLI_OPTION_LOG_LONG, "warning")); //$NON-NLS-1$
		options.put(SREConstants.MANIFEST_CLI_DEFAULT_CONTEXT_ID, formatCommandLineOption(Boot.CLI_OPTION_WORLDID_LONG, null));
		options.put(SREConstants.MANIFEST_CLI_RANDOM_CONTEXT_ID, formatCommandLineOption(Boot.CLI_OPTION_RANDOMID_LONG, null));
		options.put(SREConstants.MANIFEST_CLI_BOOT_AGENT_CONTEXT_ID, formatCommandLineOption(Boot.CLI_OPTION_BOOTID_LONG, null));
		options.put(SREConstants.MANIFEST_CLI_SRE_OFFLINE, formatCommandLineOption(Boot.CLI_OPTION_OFFLINE_LONG, null));
		options.put(SREConstants.MANIFEST_CLI_EMBEDDED, formatCommandLineOption(Boot.CLI_OPTION_EMBEDDED_LONG, null));
		options.put(SREConstants.MANIFEST_CLI_NO_MORE_OPTION, formatCommandLineOption(null, null));
		return Collections.unmodifiableMap(options);
	}

	@Override
	public boolean isStandalone() {
		// Must return true to pass the test done by the SRE
		// AbstractSREInstall#getValidity
		// But the jar of this plugin is not standalone that why in the manifest
		// standalone=false
		return true;
	}

	@Override
	public String getSREArguments() {
		return ""; //$NON-NLS-1$
	}

	@Override
	public String getJVMArguments() {
		return ""; //$NON-NLS-1$
	}

	@Override
	public void getAsXML(Document document, Element element) throws IOException {
		// Ignore this function since the Janus SRE is embedded in the product.
		// There is no need to store the preferences and configuration into the SRE preferences.
	}

	@Override
	public void setFromXML(Element element) throws IOException {
		// Ignore this function since the Janus SRE is embedded in the product.
		// There is no need to read the preferences and configuration into the SRE preferences.
	}

	@Override
	protected void resolveDirtyFields(boolean forceSettings) {
		// Assuming that all the fields have a valid value.
	}

}
