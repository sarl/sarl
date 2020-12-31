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

import java.util.logging.Level;

import io.bootique.annotation.BQConfig;
import io.bootique.annotation.BQConfigProperty;
import io.bootique.config.ConfigurationFactory;

/**
 * Configuration for the compiler command.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@BQConfig("Configuration of the SARLC tool")
public class ProgressBarConfig {

	/**
	 * Prefix for the configuration entries of the compiler command.
	 */
	public static final String PREFIX = "progressBar"; //$NON-NLS-1$

	/**
	 * Name of the property that indicates if the progress bar is active or not.
	 */
	public static final String ENABLE = PREFIX + ".enable"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the logger level when the progress bar is active.
	 */
	public static final String LEVEL = PREFIX + ".level"; //$NON-NLS-1$

	/**
	 * Default logger level when the progress bar is active.
	 */
	public static final Level DEFAULT_LEVEL = Level.SEVERE;

	/**
	 * Name of the property that contains the output path for the Java byte code.
	 */
	public static final String STYLE = PREFIX + ".style"; //$NON-NLS-1$

	/**
	 * Default progress bar style.
	 */
	public static final ProgressBarStyle DEFAULT_STYLE = ProgressBarStyle.COLORED_UNICODE;

	private boolean enable;

	private Level level = DEFAULT_LEVEL;

	private ProgressBarStyle style = DEFAULT_STYLE;

	/** Replies the configuration factory for the logging.
	 *
	 * @param configFactory the general configuration factory.
	 * @return the logging configuration factory.
	 */
	public static ProgressBarConfig getConfiguration(ConfigurationFactory configFactory) {
		assert configFactory != null;
		return configFactory.config(ProgressBarConfig.class, PREFIX);
	}

	/** Replies if the progress bar is active or not..
	 *
	 * @return {@code true} if the progress bar is active.
	 */
	public boolean getEnable() {
		return this.enable;
	}

	/** Set if the progress bar is active or not..
	 *
	 * @param enable {@code true} if the progress bar is active.
	 */
	@BQConfigProperty("Enable or disable the progress bar.")
	public void setEnable(boolean enable) {
		this.enable = enable;
	}

	/** Replies the logger level that should be used when the progress bar is active.
	 *
	 * @return the logger level.
	 */
	public Level getLevel() {
		return this.level;
	}

	/** Set the logger level that should be used when the progress bar is active.
	 *
	 * @param level the progress bar level. If {@code null}, the {@link #DEFAULT_LEVEL default level} is assumed.
	 */
	@BQConfigProperty("Specify the logger level that should be used when the progress bar is active. Default is ERROR.")
	public void setLevel(Level level) {
		this.level = level == null ? DEFAULT_LEVEL : level;
	}

	/** Replies the style of the progress bar.
	 *
	 * @return the style.
	 */
	public ProgressBarStyle getStyle() {
		return this.style;
	}

	/** Set the style of the progress bar.
	 *
	 * @param style the style. If {@code null}, the {@link #DEFAULT_STYLE default style} is assumed.
	 */
	@BQConfigProperty("Specify the style of the progress bar. Default is COLORED_UNICODE.")
	public void setStyle(ProgressBarStyle style) {
		this.style = style == null ? DEFAULT_STYLE : style;
	}

}
