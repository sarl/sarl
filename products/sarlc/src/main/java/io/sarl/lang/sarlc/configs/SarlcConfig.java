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

package io.sarl.lang.sarlc.configs;

import java.io.File;

import io.bootique.annotation.BQConfig;
import io.bootique.annotation.BQConfigProperty;
import io.bootique.config.ConfigurationFactory;

import io.sarl.lang.sarlc.configs.subconfigs.CompilerConfig;
import io.sarl.lang.sarlc.configs.subconfigs.ValidatorConfig;

/**
 * Configuration for the sarl tool.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@BQConfig("Configuration of the SARLC tool")
public class SarlcConfig {

	/**
	 * Prefix for the configuration entries of the modules.
	 */
	public static final String PREFIX = "sarlc"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the output path for the SARL code.
	 */
	public static final String OUTPUT_PATH_NAME = PREFIX + ".outputPath"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the temp directory used by the SARL compiler.
	 */
	public static final String TEMP_DIRECTORY_NAME = PREFIX + ".tempDirectory"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the output path for the Java byte code.
	 */
	public static final String CLASS_OUTPUT_PATH_NAME = PREFIX + ".classOutputPath"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the classpath.
	 */
	public static final String CLASSPATH_NAME = PREFIX + ".classpath"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the module-path.
	 * @since 0.12
	 */
	public static final String MODULEPATH_NAME = PREFIX + ".modulepath"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the Java boot classpath.
	 *
	 * @deprecated Since 0.12 not replacement because of Java 11.
	 */
	@Deprecated
	public static final String JAVA_BOOT_CLASSPATH_NAME = PREFIX + ".javaBootClasspath"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the SARL boot classpath.
	 */
	public static final String BOOT_CLASSPATH_NAME = PREFIX + ".bootClasspath"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the list of the extra-language generators.
	 */
	public static final String EXTRA_GENERATOR_NAME = PREFIX + ".extraGenerators"; //$NON-NLS-1$

	private String classPath;

	private String modulePath;

	private String bootClasspath;

	@Deprecated
	private String javaBootClasspath;

	private File outputPath;

	private File classOutputPath;

	private File tempDirectory;

	private CompilerConfig compilerConfig;

	private ValidatorConfig validatorConfig;

	private String extraGenerators;

	/** Replies the configuration for SARLC.
	 *
	 * @param configFactory the general configuration factory.
	 * @return the SARLC configuration.
	 */
	public static SarlcConfig getConfiguration(ConfigurationFactory configFactory) {
		assert configFactory != null;
		return configFactory.config(SarlcConfig.class, PREFIX);
	}

	/** Replies the classpath.
	 *
	 * @return the classpath
	 */
	public String getClasspath() {
		return this.classPath;
	}

	/** Change the class path.
	 *
	 * @param path the class path.
	 */
	@BQConfigProperty("Class path for the SARL compiler.")
	public void setClasspath(String  path) {
		this.classPath = path;
	}

	/** Replies the module-path.
	 *
	 * @return the module-path
	 * @since 0.12
	 */
	public String getModulePath() {
		return this.modulePath;
	}

	/** Change the module-path.
	 *
	 * @param path the module-path.
	 */
	@BQConfigProperty("Module path for the SARL compiler.")
	public void setModulePath(String  path) {
		this.modulePath = path;
	}

	/** Replies the boot classpath.
	 *
	 * @return the boot classpath
	 */
	public String  getBootClasspath() {
		return this.bootClasspath;
	}

	/** Change the boot class path.
	 *
	 * @param path the boot class path.
	 */
	@BQConfigProperty("SARL boot class path for the SARL compiler.")
	public void setBootClasspath(String  path) {
		this.bootClasspath = path;
	}

	/** Replies the Java boot classpath.
	 *
	 * @return the Java boot classpath
	 * @deprecated Since 0.12 not replacement because of Java 11.
	 */
	@Deprecated
	public String  getJavaBootClasspath() {
		return this.javaBootClasspath;
	}

	/** Change the Java boot class path.
	 *
	 * @param path the Java boot class path.
	 * @deprecated Since 0.12 not replacement because of Java 11.
	 */
	@Deprecated
	@BQConfigProperty("Java boot class path for the SARL compiler.")
	public void setJavaBootClasspath(String  path) {
		this.javaBootClasspath = path;
	}

	/** Replies the class output path.
	 *
	 * @return the class output path
	 */
	public File getClassOutputPath() {
		return this.classOutputPath;
	}

	/** Change the class output path.
	 *
	 * @param path the class output path.
	 */
	@BQConfigProperty("Output path for the Java compiler in which the byte-code is written.")
	public void setClassOutputPath(File path) {
		this.classOutputPath = path;
	}

	/** Replies the output path.
	 *
	 * @return the output path
	 */
	public File getOutputPath() {
		return this.outputPath;
	}

	/** Change the output path.
	 *
	 * @param path the output path.
	 */
	@BQConfigProperty("Output path for the SARL compiler in which the Java code is written.")
	public void setOutputPath(File path) {
		this.outputPath = path;
	}

	/** Replies the path in which the SARL compiler will write temp files.
	 *
	 * @return the working path
	 */
	public File getTempDirectory() {
		return this.tempDirectory;
	}

	/** Change the path in which the SARL compiler will write temp files.
	 *
	 * @param path the working path.
	 */
	@BQConfigProperty("Working/temporary path for the SARL compiler.")
	public void setTempDirectory(File path) {
		this.tempDirectory = path;
	}

	/** Replies the compiler configuration.
	 *
	 * @return the compiler configuration.
	 */
	public CompilerConfig getCompiler() {
		if (this.compilerConfig == null) {
			this.compilerConfig = new CompilerConfig();
		}
		return this.compilerConfig;
	}

	/** Change the compiler configuration.
	 *
	 * @param config the compiler configuration.
	 */
	@BQConfigProperty("Configuration of the SARL compiler.")
	public void setCompiler(CompilerConfig config) {
		this.compilerConfig = config;
	}

	/** Replies the validator configuration.
	 *
	 * @return the validator configuration.
	 */
	public ValidatorConfig getValidator() {
		if (this.validatorConfig == null) {
			this.validatorConfig = new ValidatorConfig();
		}
		return this.validatorConfig;
	}

	/** Change the validator configuration.
	 *
	 * @param config the validator configuration.
	 */
	@BQConfigProperty("Configuration of the SARL validator.")
	public void setValidator(ValidatorConfig config) {
		this.validatorConfig = config;
	}

	/** Replies the enabled extra generators.
	 *
	 * @return the identifiers of the extra generators, separator by {@link File#pathSeparator}.
	 */
	public String getExtraGenerators() {
		return this.extraGenerators;
	}

	/** Change the list of the enabled extra generators.
	 *
	 * @param identifiers the extra generators, separated by {@link File#pathSeparator}.
	 */
	@BQConfigProperty("List of the enable extra-language generators.")
	public void setExtraGenerators(String identifiers) {
		this.extraGenerators = identifiers;
	}

}
