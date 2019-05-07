/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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
import java.io.IOException;
import java.net.URL;
import java.nio.charset.Charset;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import com.google.common.base.Strings;
import io.bootique.annotation.BQConfig;
import io.bootique.annotation.BQConfigProperty;
import io.bootique.config.ConfigurationFactory;
import org.apache.commons.lang3.SystemUtils;
import org.arakhne.afc.vmutil.ClasspathUtil;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.OperatingSystem;

import io.sarl.docs.doclet.SarlDoclet;
import io.sarl.maven.bootiqueapp.utils.SystemPath;

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
	 * Name of the property that contains the minimum amount of memory to allocate to the JVM.
	 */
	public static final String MINIMUM_MEMORY_NAME = PREFIX + ".minMemory"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the maximum amount of memory to allocate to the JVM.
	 */
	public static final String MAXIMUM_MEMORY_NAME = PREFIX + ".maxMemory"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the proxy definitions.
	 */
	public static final String PROXY_NAME = PREFIX + ".proxy"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the class name of the doclet.
	 */
	public static final String DOCLET_NAME = PREFIX + ".doclet"; //$NON-NLS-1$

	/**
	 * Default doclet.
	 */
	public static final String DOCLET_VALUE = SarlDoclet.class.getName();

	/**
	 * Name of the property that contains the class path for the doclet.
	 */
	public static final String DOCLET_PATH_NAME = PREFIX + ".docletPath"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the destination folder for the generated HTML documentation.
	 */
	public static final String DOC_OUTPUT_DIRECTORY_NAME = PREFIX + ".outputDirectory"; //$NON-NLS-1$

	/**
	 * Name of the property that contains additional JOptions.
	 */
	public static final String JOPTIONS_NAME = PREFIX + ".joption"; //$NON-NLS-1$

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

	private static final String JAVA_HOME_PROPERTY_NAME = "JAVA_HOME"; //$NON-NLS-1$

	private String javadocExecutable;

	private String minMemory;

	private String maxMemory;

	private List<String> proxy;

	private List<String> joption;

	private List<String> javadocOption;

	private String doclet;

	private String docletPath;

	private String locale;

	private File outputDirectory;

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
		File javadocExe = FileSystem.join(SystemUtils.getJavaHome(), FileSystem.PARENT_DIRECTORY, BIN_FOLDER, javadocCommand);

		// ----------------------------------------------------------------------
		// Try to find javadocExe from JAVA_HOME environment variable
		// ----------------------------------------------------------------------
		if (!javadocExe.exists() || !javadocExe.isFile()) {
			final String javaHome = SystemUtils.getEnvironmentVariable(JAVA_HOME_PROPERTY_NAME, null);
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
		} catch (IOException exception) {
			throw new RuntimeException(exception);
		}
		try {
			return javadocExe.getCanonicalPath();
		} catch (IOException exception) {
			throw new RuntimeException(exception);
		}
	}

	/** Replies the minimum amount of memory to allocate to the JVM.
	 *
	 * @return the minimum amount of memory.
	 */
	public String getMinMemory() {
		return this.minMemory;
	}

	/** Change the minimum amount of memory to allocate to the JVM.
	 *
	 * @param memory the minimum amount of memory to allocate to the JVM.
	 */
	@BQConfigProperty("Specify the minimum amount of memory to allocate to the JVM. If it is not specified, "
			+ "the default JVM value is used.")
	public void setMinMemory(String memory) {
		this.minMemory = memory;
	}

	/** Replies the maximum amount of memory to allocate to the JVM.
	 *
	 * @return the maximum amount of memory.
	 */
	public String getMaxMemory() {
		return this.maxMemory;
	}

	/** Change the maximum amount of memory to allocate to the JVM.
	 *
	 * @param memory the maximum amount of memory to allocate to the JVM.
	 */
	@BQConfigProperty("Specify the maximum amount of memory to allocate to the JVM. If it is not specified, "
			+ "the default JVM value is used.")
	public void setMaxMemory(String memory) {
		this.maxMemory = memory;
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

	/** Replies the additional JOptions.
	 *
	 * @return the additional JOptions.
	 */
	public List<String> getJOption() {
		if (this.joption == null) {
			this.joption = new ArrayList<>();
		}
		return this.joption;
	}

	/** Change the additional JOptions.
	 *
	 * @param options the additional JOptions.
	 */
	@BQConfigProperty("Specify the additional JOptions.")
	public void setJOptions(List<String> options) {
		this.joption = options;
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
	 * @return the classname of the doclet.
	 */
	public String getDoclet() {
		if (Strings.isNullOrEmpty(this.doclet)) {
			this.doclet = DOCLET_VALUE;
		}
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
		if (Strings.isNullOrEmpty(this.docletPath)) {
			final SystemPath path = new SystemPath();
			final Iterator<URL> iterator = ClasspathUtil.getClasspath();
			while (iterator.hasNext()) {
				final URL classpathEntry = iterator.next();
				final File entry = FileSystem.convertURLToFile(classpathEntry);
				path.add(entry);
			}
			this.docletPath = path.toString();
		}
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
	public File getOutputDirectory() {
		if (this.outputDirectory == null) {
			this.outputDirectory = FileSystem.join(new File("target"), "site", "apidocs"); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
		}
		return this.outputDirectory;
	}

	/** Change the output directory for the generated HTML documentation.
	 *
	 * @param outputDirectory the output directory.
	 */
	@BQConfigProperty("Specify the output folder into which the generated HTML documentation will be copied. "
			+ "If it is not specified, the default sarldoc folder is used.")
	public void setOutputDirectory(File outputDirectory) {
		this.outputDirectory = outputDirectory;
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
