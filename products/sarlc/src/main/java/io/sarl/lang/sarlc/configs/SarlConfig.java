/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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
public class SarlConfig {

	/**
	 * Prefix for the configuration entries of the path modules.
	 */
	public static final String PREFIX = "sarl"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the output path for the SARL code.
	 */
	public static final String OUTPUT_PATH_NAME = PREFIX + ".outputPath"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the working path.
	 */
	public static final String WORKING_PATH_NAME = PREFIX + ".workingPath"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the output path for the Java byte code.
	 */
	public static final String CLASS_OUTPUT_PATH_NAME = PREFIX + ".classOutputPath"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the classpath.
	 */
	public static final String CLASSPATH_NAME = PREFIX + ".classpath"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the boot classpath.
	 */
	public static final String BOOT_CLASSPATH_NAME = PREFIX + ".bootclasspath"; //$NON-NLS-1$

	private String classpath;

	private String bootClasspath;

	private File outputPath;

	private File classOutputPath;

	private File workingPath;

	private CompilerConfig compilerConfig;

	private ValidatorConfig validatorConfig;

	/** Replies the configuration factory for the logging.
	 *
	 * @param configFactory the general configuration factory.
	 * @return the logging configuration factory.
	 */
	public static SarlConfig getConfiguration(ConfigurationFactory configFactory) {
		assert configFactory != null;
		return configFactory.config(SarlConfig.class, PREFIX);
	}

	/** Replies the classpath.
	 *
	 * @return the classpath
	 */
	public String getClasspath() {
		return this.classpath;
	}

	/** Change the class path.
	 *
	 * @param path the class path.
	 */
	@BQConfigProperty("Class path for the SARL compiler")
	public void setClasspath(String  path) {
		this.classpath = path;
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
	@BQConfigProperty("Boot class path for the SARL compiler")
	public void setBootClasspath(String  path) {
		this.bootClasspath = path;
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
	@BQConfigProperty("Byte code's output path for the SARL compiler")
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
	@BQConfigProperty("Output path for the SARL compiler")
	public void setOutputPath(File path) {
		this.outputPath = path;
	}

	/** Replies the working path.
	 *
	 * @return the working path
	 */
	public File getWorkingPath() {
		return this.workingPath;
	}

	/** Change the working path.
	 *
	 * @param path the working path.
	 */
	@BQConfigProperty("Working/temporary path for the SARL compiler")
	public void setWorkingPath(File path) {
		this.workingPath = path;
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
	@BQConfigProperty("Configuration of the SARL compiler")
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
	@BQConfigProperty("Configuration of the SARL validator")
	public void setValidator(ValidatorConfig config) {
		this.validatorConfig = config;
	}

}
