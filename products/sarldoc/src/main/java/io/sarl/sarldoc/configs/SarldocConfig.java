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

package io.sarl.sarldoc.configs;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.Charset;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import com.google.common.base.Strings;
import io.bootique.annotation.BQConfig;
import io.bootique.annotation.BQConfigProperty;
import io.bootique.config.ConfigurationFactory;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.OperatingSystem;

/**
 * Configuration for the SARL API documentation generator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
@BQConfig("Configuration of sarldoc")
public class SarldocConfig {

	/**
	 * Prefix for the configuration entries of the path modules.
	 */
	public static final String PREFIX = "sarldoc"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the path to the javadoc executable.
	 */
	public static final String JAVADOC_EXECUTABLE_NAME = PREFIX + ".javadocExecutable"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the proxy definitions.
	 */
	public static final String PROXY_NAME = PREFIX + ".proxy"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the class name of the doclet.
	 */
	public static final String DOCLET_NAME = PREFIX + ".doclet"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the class path for the doclet.
	 */
	public static final String DOCLET_PATH_NAME = PREFIX + ".docletPath"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the destination folder for the generated HTML documentation.
	 */
	public static final String DOC_OUTPUT_DIRECTORY_NAME = PREFIX + ".documentationOutputDirectory"; //$NON-NLS-1$

	/**
	 * File value of the property that contains the destination folder for the generated HTML documentation.
	 */
	public static final File DOC_OUTPUT_DIRECTORY_FILE = FileSystem.join(
			new File("target"), //$NON-NLS-1$
			"site", //$NON-NLS-1$
			"apidocs"); //$NON-NLS-1$

	/**
	 * String value of the property that contains the destination folder for the generated HTML documentation.
	 */
	public static final String DOC_OUTPUT_DIRECTORY_VALUE = DOC_OUTPUT_DIRECTORY_FILE.getPath();

	/**
	 * Name of the property that contains options to javadoc.
	 */
	public static final String JAVADOC_OPTION_NAME = PREFIX + ".javadocOption"; //$NON-NLS-1$

	/**
	 * Name of the property that indicates if {@code @version} should be included into the documentation.
	 */
	public static final String ENABLE_VERSION_TAG_NAME = PREFIX + ".enableVesionTag"; //$NON-NLS-1$

	/**
	 * Name of the property that indicates if {@code @author} should be included into the documentation.
	 */
	public static final String ENABLE_AUTHOR_TAG_NAME = PREFIX + ".enableAuthorTag"; //$NON-NLS-1$

	/**
	 * Name of the property that indicates if {@code @deprecated} should be included into the documentation.
	 */
	public static final String ENABLE_DEPRECATED_TAG_NAME = PREFIX + ".enableDeprecatedTag"; //$NON-NLS-1$

	/**
	 * Name of the property that indicates if {@code @since} should be included into the documentation.
	 */
	public static final String ENABLE_SINCE_TAG_NAME = PREFIX + ".enableSinceTag"; //$NON-NLS-1$

	/**
	 * Name of the property that is the title of the documentation.
	 */
	public static final String TITLE_NAME = PREFIX + ".title"; //$NON-NLS-1$

	/**
	 * Name of the property that indicates the visiblity of the elements to put into the documentation.
	 */
	public static final String VISIBILITY_NAME = PREFIX + ".visibility"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the encoding of the documentation.
	 */
	public static final String ENCODING_NAME = PREFIX + ".encoding"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the locale of the documentation.
	 */
	public static final String LOCALE_NAME = PREFIX + ".locale"; //$NON-NLS-1$

	/**
	 * Default locale for Sarldoc.
	 */
	public static final Locale LOCALE_DEFAULT = Locale.US;

	/**
	 * Name of the property that contains the names of the excluded packages.
	 */
	public static final String EXCLUDED_PACKAGES_NAME = PREFIX + ".excludedPackages"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the custom tags.
	 */
	public static final String TAGS_NAME = PREFIX + ".tags"; //$NON-NLS-1$

	private static final String BIN_FOLDER = "bin"; //$NON-NLS-1$

	private static final String JAVADOC_BIN_WIN = "javadoc.exe"; //$NON-NLS-1$

	private static final String JAVADOC_BIN_UNIX = "javadoc"; //$NON-NLS-1$

	private static final String NOTMAC_LIB_FOLDER = "lib"; //$NON-NLS-1$

	private static final String MAC_LIB_FOLDER = "Classes"; //$NON-NLS-1$

	private static final String NOTMAC_TOOLS_JAR = "tools.jar"; //$NON-NLS-1$

	private static final String MAC_TOOLS_JAR = "Classes.jar"; //$NON-NLS-1$

	private static final String JAVA_HOME_PROPERTY_NAME = "JAVA_HOME"; //$NON-NLS-1$

	private static final String DEFAULT_NON_PROXY_HOSTS = "localhost|127.*.*.*|10.*.*.*"; //$NON-NLS-1$

	private static final char NO_PROXY_HOST_SEPARATOR = '|';

	private static final String NO_PROXY_HOST_SEPARATOR_STRING = "|"; //$NON-NLS-1$

	private static final String JAVA_HOME_KEY = "java.home"; //$NON-NLS-1$

	private String javadocExecutable;

	private List<String> proxy;

	private List<String> httpNoProxyHosts;

	private List<String> httpsNoProxyHosts;

	private List<String> javadocOption;

	private String doclet;

	private String docletPath;

	private String locale;

	private File documentationOutputDirectory;

	private boolean enableVesionTag = true;

	private boolean enableAuthorTag = true;

	private boolean enableDeprecatedTag = true;

	private boolean enableSinceTag = true;

	private String title;

	private String encoding;

	private Visibility visibility;

	private Set<String> excludedPackages;

	private List<Tag> customTags;

	/** Replies the configuration for SARLC.
	 *
	 * @param configFactory the general configuration factory.
	 * @return the SARLC configuration.
	 */
	public static SarldocConfig getConfiguration(ConfigurationFactory configFactory) {
		assert configFactory != null;
		return configFactory.config(SarldocConfig.class, PREFIX);
	}

	/** Replies the path to the executable of javadoc.
	 *
	 * @return the path to javadoc.
	 */
	public String getJavadocExecutable() {
		if (this.javadocExecutable == null) {
			this.javadocExecutable = findJavadocExecutable();
		}
		return this.javadocExecutable;
	}

	/** Change the path to the javadoc executable.
	 *
	 * @param javadocExecutable the path.
	 */
	@BQConfigProperty("Specify the path to the executable of Javadoc. If it is not specified, the value will "
			+ "be inferred from the current installation of the Java environment.")
	public void setJavadocExecutable(String javadocExecutable) {
		this.javadocExecutable = javadocExecutable;
	}

	/**
	 * Get the path of the Javadoc tool executable depending the user entry or try to find it depending the OS
	 * or the <code>java.home</code> system property or the <code>JAVA_HOME</code> environment variable.
	 *
	 * <p>This function is copied from the {@code maven-javadoc-plugin}.
	 *
	 * @return the path of the Javadoc tool.
	 */
	@SuppressWarnings("checkstyle:npathcomplexity")
	public static String findJavadocExecutable() {
		final String javadocCommand;
		if (OperatingSystem.getCurrentOS() == OperatingSystem.WIN) {
			javadocCommand = JAVADOC_BIN_WIN;
		} else {
			javadocCommand = JAVADOC_BIN_UNIX;
		}

		// ----------------------------------------------------------------------
		// Try to find javadocExe from System.getProperty( "java.home" )
		// By default, System.getProperty( "java.home" ) = JRE_HOME and JRE_HOME
		// should be in the JDK_HOME
		// ----------------------------------------------------------------------
		File javadocExe = FileSystem.join(new File(System.getProperty(JAVA_HOME_KEY)), FileSystem.PARENT_DIRECTORY, BIN_FOLDER, javadocCommand);

		// ----------------------------------------------------------------------
		// Try to find javadocExe from JAVA_HOME environment variable
		// ----------------------------------------------------------------------
		if (!javadocExe.exists() || !javadocExe.isFile()) {
			final String javaHome;
	        try {
	        	javaHome = System.getenv(JAVA_HOME_PROPERTY_NAME);
	    		if (Strings.isNullOrEmpty(javaHome)) {
					throw new RuntimeException(Messages.SarldocConfig_0);
	    		}
	        } catch (final SecurityException ex) {
				throw new RuntimeException(Messages.SarldocConfig_0);
	        }
			if (Strings.isNullOrEmpty(javaHome)) {
				throw new RuntimeException(Messages.SarldocConfig_0);
			}
			try {
				final File javaHomeDirectory = FileSystem.convertStringToFile(javaHome).getCanonicalFile();
				if (!javaHomeDirectory.exists() || javaHomeDirectory.isFile()) {
					throw new RuntimeException(MessageFormat.format(Messages.SarldocConfig_1, javaHome));
				}

				javadocExe = FileSystem.join(javaHomeDirectory, BIN_FOLDER, javadocCommand);
			} catch (IOException exception) {
				throw new RuntimeException(exception);
			}
		}

		try {
			final File javadocExeCanon = javadocExe.getCanonicalFile();
			if (!javadocExeCanon.exists() || !javadocExeCanon.isFile()) {
				throw new RuntimeException(MessageFormat.format(Messages.SarldocConfig_2, javadocExe));
			}
			return javadocExe.getAbsolutePath();
		} catch (IOException exception) {
			throw new RuntimeException(exception);
		}
	}

	/**
	 * Get the path of the "tools.jar" file depending the user entry or try to find it depending the OS
	 * or the <code>java.home</code> system property or the <code>JAVA_HOME</code> environment variable.
	 *
	 * @return the path of the "tools.jar" file, never {@code null}.
	 * @throws FileNotFoundException if the "tools.jar" was not found.
	 */
	@SuppressWarnings("checkstyle:npathcomplexity")
	public static File findToolsJar() throws FileNotFoundException {
		final String libFolder;
		final String toolsJar;
		if (OperatingSystem.MACOSX.isCurrentOS()) {
			libFolder = MAC_LIB_FOLDER;
			toolsJar = MAC_TOOLS_JAR;
		} else {
			libFolder = NOTMAC_LIB_FOLDER;
			toolsJar = NOTMAC_TOOLS_JAR;
		}

		// ----------------------------------------------------------------------
		// Try to find javadocExe from System.getProperty( "java.home" )
		// By default, System.getProperty( "java.home" ) = JRE_HOME and JRE_HOME
		// should be in the JDK_HOME
		// ----------------------------------------------------------------------
		File file = FileSystem.join(new File(System.getProperty(JAVA_HOME_KEY)),
				FileSystem.PARENT_DIRECTORY, libFolder, toolsJar);

		// ----------------------------------------------------------------------
		// Try to find javadocExe from JAVA_HOME environment variable
		// ----------------------------------------------------------------------
		if (!file.exists() || !file.isFile()) {
			final String javaHome;
	        try {
	        	javaHome = System.getenv(JAVA_HOME_PROPERTY_NAME);
	    		if (Strings.isNullOrEmpty(javaHome)) {
					throw new FileNotFoundException(Messages.SarldocConfig_3);
	    		}
	        } catch (final SecurityException ex) {
				throw new FileNotFoundException(Messages.SarldocConfig_3);
	        }
			if (Strings.isNullOrEmpty(javaHome)) {
				throw new FileNotFoundException(Messages.SarldocConfig_3);
			}
			try {
				final File javaHomeDirectory = FileSystem.convertStringToFile(javaHome).getCanonicalFile();
				if (!javaHomeDirectory.exists() || javaHomeDirectory.isFile()) {
					throw new FileNotFoundException(Messages.SarldocConfig_3);
				}

				file = FileSystem.join(javaHomeDirectory, libFolder, toolsJar);
			} catch (IOException exception) {
				throw new FileNotFoundException(Messages.SarldocConfig_3);
			}
		}

		try {
			final File fileCanon = file.getCanonicalFile();
			if (!fileCanon.exists() || !fileCanon.isFile()) {
				throw new FileNotFoundException(Messages.SarldocConfig_3);
			}
			return fileCanon;
		} catch (IOException exception) {
			throw new FileNotFoundException(Messages.SarldocConfig_3);
		}
	}

	/** Replies the proxy definitions.
	 *
	 * @return the proxy definitions.
	 */
	public List<String> getProxy() {
		if (this.proxy == null) {
			this.proxy = new ArrayList<>();
		}
		return this.proxy;
	}

	/** Change the proxy definitions.
	 *
	 * @param proxy the proxy defintiions.
	 */
	@BQConfigProperty("Specify the network proxies.")
	public void setProxy(List<String> proxy) {
		this.proxy = proxy;
	}

	/** Replies the hosts to which an HTTP proxy should not be used.
	 *
	 * @return the no proxy definitions for HTTP.
	 */
	public List<String> getHttpNoProxyHosts() {
		if (this.httpNoProxyHosts == null) {
			this.httpNoProxyHosts = new ArrayList<>();
			for (final String host : org.eclipse.xtext.util.Strings.split(DEFAULT_NON_PROXY_HOSTS, NO_PROXY_HOST_SEPARATOR)) {
				if (!Strings.isNullOrEmpty(host)) {
					this.httpNoProxyHosts.add(host);
				}
			}
		}
		return this.httpNoProxyHosts;
	}

	/** Replies the hosts to which an HTTP proxy should not be used.
	 *
	 * @return the no proxy definitions for HTTP, or {@code null} if none.
	 */
	public String getHttpNoProxyHostsString() {
		final List<String> hosts = getHttpNoProxyHosts();
		if (hosts.isEmpty()) {
			return null;
		}
		return org.eclipse.xtext.util.Strings.concat(NO_PROXY_HOST_SEPARATOR_STRING, hosts);
	}

	/** Change the hosts to which an HTTP proxy should not be used.
	 *
	 * @param noProxyHosts the no proxy definitions for HTTP.
	 */
	@BQConfigProperty("Specify the host names for which the HTTP proxy should not be used. The character '*' may be used as a wildcard.")
	public void setHttpNoProxyHostsProxy(List<String> noProxyHosts) {
		this.httpNoProxyHosts = noProxyHosts;
	}

	/** Replies the hosts to which an HTTPS proxy should not be used.
	 *
	 * @return the no proxy definitions for HTTPS.
	 */
	public List<String> getHttpsNoProxyHosts() {
		if (this.httpsNoProxyHosts == null) {
			this.httpsNoProxyHosts = new ArrayList<>();
			for (final String host : org.eclipse.xtext.util.Strings.split(DEFAULT_NON_PROXY_HOSTS, NO_PROXY_HOST_SEPARATOR)) {
				if (!Strings.isNullOrEmpty(host)) {
					this.httpsNoProxyHosts.add(host);
				}
			}
		}
		return this.httpsNoProxyHosts;
	}

	/** Replies the hosts to which an HTTPS proxy should not be used.
	 *
	 * @return the no proxy definitions for HTTPS, or {@code null} if none.
	 */
	public String getHttpsNoProxyHostsString() {
		final List<String> hosts = getHttpsNoProxyHosts();
		if (hosts.isEmpty()) {
			return null;
		}
		return org.eclipse.xtext.util.Strings.concat(NO_PROXY_HOST_SEPARATOR_STRING, hosts);
	}

	/** Change the hosts to which an HTTPS proxy should not be used.
	 *
	 * @param noProxyHosts the no proxy definitions for HTTPS.
	 */
	@BQConfigProperty("Specify the host names for which the HTTPS proxy should not be used. The character '*' may be used as a wildcard.")
	public void setHttpsNoProxyHostsProxy(List<String> noProxyHosts) {
		this.httpsNoProxyHosts = noProxyHosts;
	}

	/** Replies the options to pass to Javadoc.
	 *
	 * @return the options to pass to Javadoc.
	 */
	public List<String> getJavadocOption() {
		if (this.javadocOption == null) {
			this.javadocOption = new ArrayList<>();
		}
		return this.javadocOption;
	}

	/** Change the options to pass to Javadoc.
	 *
	 * @param options the options to pass to Javadoc.
	 */
	@BQConfigProperty("Specify the command-line options to pass directly to Javadoc.")
	public void setJavadocOptions(List<String> options) {
		this.javadocOption = options;
	}

	/** Replies the doclet.
	 *
	 * @return the classname of the doclet, or {@code null} if not specified.
	 */
	public String getDoclet() {
		return this.doclet;
	}

	/** Change the doclet.
	 *
	 * @param doclet the classname of the doclet.
	 */
	@BQConfigProperty("Specify the class name of the doclet to use. If it is not specified, the default SARL doclet is used.")
	public void setDoclet(String doclet) {
		this.doclet = doclet;
	}

	/** Replies the class path for the doclet.
	 *
	 * @return the class path for the doclet.
	 */
	public String getDocletPath() {
		return this.docletPath;
	}

	/** Change the class path for the doclet.
	 *
	 * @param path the class path for the doclet.
	 */
	@BQConfigProperty("Specify the class path for the doclet. If it is not specified, the class path of this command is used.")
	public void setDocletPath(String path) {
		this.docletPath = path;
	}

	/** Replies the output directory for the generated HTML documentation.
	 *
	 * @return the output directory
	 */
	public File getDocumentationOutputDirectory() {
		if (this.documentationOutputDirectory == null) {
			this.documentationOutputDirectory = DOC_OUTPUT_DIRECTORY_FILE;
		}
		return this.documentationOutputDirectory;
	}

	/** Change the output directory for the generated HTML documentation.
	 *
	 * @param outputDirectory the output directory.
	 */
	@BQConfigProperty("Specify the output folder into which the generated HTML documentation will be copied. "
			+ "If it is not specified, the default sarldoc folder is used.")
	public void setDocumentationOutputDirectory(File outputDirectory) {
		this.documentationOutputDirectory = outputDirectory;
	}

	/** Replies if {@code @version} should be included into the documentation.
	 *
	 * @return {@code true} if {@code @version} are included.
	 */
	public boolean getEnableVersionTag() {
		return this.enableVesionTag;
	}

	/** Change the flag that indicates if {@code @version} should be included into the documentation.
	 *
	 * @param enable is {@code true} for enabling the {@code @version} tags.
	 */
	@BQConfigProperty("Specify if the @version tags are enabled into the documentation. "
			+ "If it is not specified, the default is true.")
	public void setEnableVersionTag(boolean enable) {
		this.enableVesionTag = enable;
	}

	/** Replies if {@code @author} should be included into the documentation.
	 *
	 * @return {@code true} if {@code @author} are included.
	 */
	public boolean getEnableAuthorTag() {
		return this.enableAuthorTag;
	}

	/** Change the flag that indicates if {@code @author} should be included into the documentation.
	 *
	 * @param enable is {@code true} for enabling the {@code @author} tags.
	 */
	@BQConfigProperty("Specify if the @author tags are enabled into the documentation. "
			+ "If it is not specified, the default is true.")
	public void setEnableAuthorTag(boolean enable) {
		this.enableAuthorTag = enable;
	}

	/** Replies if {@code @deprecated} should be included into the documentation.
	 *
	 * @return {@code true} if {@code @deprecated} are included.
	 */
	public boolean getEnableDeprecatedTag() {
		return this.enableDeprecatedTag;
	}

	/** Change the flag that indicates if {@code @deprecated} should be included into the documentation.
	 *
	 * @param enable is {@code true} for enabling the {@code @deprecated} tags.
	 */
	@BQConfigProperty("Specify if the @deprecated tags are enabled into the documentation. "
			+ "If it is not specified, the default is true.")
	public void setEnableDeprecatedTag(boolean enable) {
		this.enableDeprecatedTag = enable;
	}

	/** Replies if {@code @since} should be included into the documentation.
	 *
	 * @return {@code true} if {@code @since} are included.
	 */
	public boolean getEnableSinceTag() {
		return this.enableSinceTag;
	}

	/** Change the flag that indicates if {@code @since} should be included into the documentation.
	 *
	 * @param enable is {@code true} for enabling the {@code @since} tags.
	 */
	@BQConfigProperty("Specify if the @since tags are enabled into the documentation. "
			+ "If it is not specified, the default is true.")
	public void setEnableSinceTag(boolean enable) {
		this.enableSinceTag = enable;
	}

	/** Replies the title of the documentation.
	 *
	 * @return the title.
	 */
	public String getTitle() {
		return this.title;
	}

	/** Change the title of the documentation.
	 *
	 * @param title the title.
	 */
	@BQConfigProperty("Specify the title of the documentation.")
	public void setTitle(String title) {
		this.title = title;
	}

	/** Replies the documentation encoding.
	 *
	 * @return the documentation encoding
	 */
	public String getEncoding() {
		if (this.encoding == null) {
			this.encoding = Charset.defaultCharset().displayName();
		}
		return this.encoding;
	}

	/** Change the documentation encoding.
	 *
	 * @param encoding the documentation encoding.
	 */
	@BQConfigProperty("Specify the character encoding of the documentation. If it is not specified, the value of "
			+ "the system property \"file.encoding\" is used. If this system property was not set, the default "
			+ "encoding is used (usually UTF-8).")
	public void setFileEncoding(String encoding) {
		this.encoding = encoding;
	}

	/** Replies the visibility of the elements to put into the documentation.
	 *
	 * @return the visibility.
	 */
	public Visibility getVisibility() {
		if (this.visibility == null) {
			this.visibility = Visibility.getDefault();
		}
		return this.visibility;
	}

	/** Change the visibility of the elements to appear into the documentation.
	 *
	 * @param visibility the visibility.
	 */
	@BQConfigProperty("Specify the visibiltiy of the elements that should appear into the documentation. If it is "
			+ "not specified, the protected visibility is assumed.")
	public void setVisibility(Visibility visibility) {
		this.visibility = visibility;
	}

	/** Replies the list of the excluded packages from the documentation.
	 *
	 * @return the excluded packages
	 */
	public Set<String> getExcludedPackages() {
		if (this.excludedPackages == null) {
			this.excludedPackages = new HashSet<>();
		}
		return this.excludedPackages;
	}

	/** Change the excluded packages.
	 *
	 * @param excludedPackages the excluded packages.
	 */
	@BQConfigProperty("Specify the list of the excluded packages.")
	public void setExcludedPackages(Set<String> excludedPackages) {
		this.excludedPackages = excludedPackages;
	}

	/** Replies the list of the custom tags.
	 *
	 * @return the custom tags.
	 */
	public List<Tag> getCustomTags() {
		if (this.customTags == null) {
			return Collections.emptyList();
		}
		return Collections.unmodifiableList(this.customTags);
	}

	/** Change the custom tags from a list of tags.
	 *
	 * @param customTags the definitions of the custom tags.
	 */
	@BQConfigProperty("Specify the custom tags to be recognized by the documentation generator.")
	public void setCustomTags(List<Tag> customTags) {
		if (customTags == null || customTags.isEmpty()) {
			this.customTags = null;
		} else {
			this.customTags = new ArrayList<>(customTags);
		}
	}

	/** Change the custom tags from a string specification.
	 *
	 * @param customTags the definitions of the custom tags.
	 * @see #setCustomTags(List)
	 * @see #getCustomTags()
	 */
	@BQConfigProperty("Specify the custom tags to be recognized by the documentation generator.")
	public void setTags(String customTags) {
		if (Strings.isNullOrEmpty(customTags)) {
			this.customTags = null;
		} else {
			this.customTags = Tag.valuesOf(customTags);
		}
	}

	/** Replies the locale of the documentation.
	 *
	 * @return the locale, or {@code null}.
	 */
	public String getLocale() {
		if (this.locale == null) {
			this.locale = LOCALE_DEFAULT.toString();
		}
		return this.locale;
	}

	/** Change the locale of the documentation.
	 *
	 * @param locale the locale.
	 */
	@BQConfigProperty("Specify the locale of the documentation.")
	public void setLocale(String locale) {
		this.locale = locale;
	}

}
