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

package io.sarl.lang.sarlc.configs.subconfigs;

import java.util.HashMap;
import java.util.Map;

import io.bootique.annotation.BQConfig;
import io.bootique.annotation.BQConfigProperty;
import org.eclipse.xtext.diagnostics.Severity;

import io.sarl.lang.sarlc.configs.SarlcConfig;

/**
 * Configuration for the validator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@BQConfig("Configuration of the SARL validator")
public class ValidatorConfig {

	/**
	 * Prefix for the configuration entries of the path modules.
	 */
	public static final String PREFIX = SarlcConfig.PREFIX + ".validator"; //$NON-NLS-1$

	/**
	 * Name of the property that indicates if warnings are ignored.
	 */
	public static final String IGNORE_WARNINGS_NAME = PREFIX + ".ignoreWarnings"; //$NON-NLS-1$

	/**
	 * Name of the property that indicates if all warnings are displayed.
	 */
	public static final String ALL_WARNINGS_NAME = PREFIX + ".allWarnings"; //$NON-NLS-1$

	/**
	 * Name of the property that indicates if warnings are errors.
	 */
	public static final String ALL_ERRORS_NAME = PREFIX + ".allErrors"; //$NON-NLS-1$

	/**
	 * Name of the property that indicates the levels of specific warnings.
	 */
	public static final String WARNING_LEVELS_NAME = PREFIX + ".warningLevels"; //$NON-NLS-1$

	private boolean ignoreWarnings;

	private boolean allErrors;

	private boolean allWarnings;

	private Map<String, Severity> warningLevels = new HashMap<>();

	/** Replies if the specific warnings levels.
	 *
	 * @return the specific warning levels.
	 */
	public Map<String, Severity> getWarningLevels() {
		return this.warningLevels;
	}

	/** Change the specific warning levels.
	 *
	 * @param levels the warnings levels.
	 */
	@BQConfigProperty("Specify the levels of specific warnings")
	public void setWarningLevels(Map<String, Severity> levels) {
		if (levels == null) {
			this.warningLevels = new HashMap<>();
		} else {
			this.warningLevels = levels;
		}
	}

	/** Replies if the all the warnings are output.
	 *
	 * @return {@code true} if the warnings are output.
	 */
	public boolean getAllWarnings() {
		return this.allWarnings;
	}

	/** Change the flag for showing all the warnings.
	 *
	 * @param allWarnings {@code true} if warnings are output.
	 */
	@BQConfigProperty("All warnings are printed out")
	public void setAllWarnings(boolean allWarnings) {
		this.allWarnings = allWarnings;
	}

	/** Replies if the warnings are errors.
	 *
	 * @return {@code true} if the warnings are errors.
	 */
	public boolean getAllErrors() {
		return this.allErrors;
	}

	/** Change the flag for transforming all the warnings to errors.
	 *
	 * @param allErrors {@code true} if warnings are errors.
	 */
	@BQConfigProperty("A warning is assimilated to an error")
	public void setAllErrors(boolean allErrors) {
		this.allErrors = allErrors;
	}

	/** Replies if the warnings are ignored.
	 *
	 * @return {@code true} if the warnings are ignored.
	 */
	public boolean getIgnoreWarnings() {
		return this.ignoreWarnings;
	}

	/** Change the flag for ignoring warnings.
	 *
	 * @param ignore {@code true} if warnings are ignored.
	 */
	@BQConfigProperty("Ignore warnings")
	public void setIgnoreWarnings(boolean ignore) {
		this.ignoreWarnings = ignore;
	}

}
