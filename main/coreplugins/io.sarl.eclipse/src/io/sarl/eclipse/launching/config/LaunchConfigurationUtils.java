/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2020 the original authors or authors.
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

package io.sarl.eclipse.launching.config;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import org.arakhne.afc.bootique.variables.VariableNames;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;

/** Utility functions for the launch configurations.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class LaunchConfigurationUtils {

	private static final String DECL_PREFIX = "-D"; //$NON-NLS-1$

	private static final String DECL_INFIX = "="; //$NON-NLS-1$

	private LaunchConfigurationUtils() {
		//
	}

	/** Replies a string that is the concatenation of the given values.
	 *
	 * @param values the values to merge.
	 * @return the concatenation result.
	 * @since 0.12
	 */
	public static String join(String... values) {
		final StringBuilder buffer = new StringBuilder();
		for (final String value : values) {
			if (!Strings.isNullOrEmpty(value)) {
				if (buffer.length() > 0) {
					buffer.append(" "); //$NON-NLS-1$
				}
				buffer.append(value);
			}
		}
		return buffer.toString();
	}

	/** Create a contributor-specific list  to pass SRE arguments to a launch configuration.
	 *
	 * @param contributorId the identifier of the contributor.
	 * @return the argument list
	 */
	public static OutputExtraSreArguments createOutputExtraSreArguments(String contributorId) {
		return new OutputExtraSreArguments(contributorId);
	}

	/** Create a contributor-specific to read SRE arguments from a launch configuration.
	 *
	 * @param contributorId the identifier of the contributor.
	 * @return the argument list
	 */
	public static InputExtraSreArguments createInputExtraSreArguments(String contributorId) {
		return new InputExtraSreArguments(contributorId);
	}

	/** Create a contributor-specific list  to pass JRE arguments to a launch configuration.
	 *
	 * @param contributorId the identifier of the contributor.
	 * @return the argument list
	 */
	public static OutputExtraJreArguments createOutputExtraJreArguments(String contributorId) {
		return new OutputExtraJreArguments(contributorId);
	}

	/** Create a contributor-specific to read JRE arguments from a launch configuration.
	 *
	 * @param contributorId the identifier of the contributor.
	 * @return the argument list
	 */
	public static InputExtraJreArguments createInputExtraJreArguments(String contributorId) {
		return new InputExtraJreArguments(contributorId);
	}

	/**
	 * Arguments to a launch configuration.
	 *
	 * @param <T> the type of the arguments list
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.12
	 */
	public abstract static class OutputExtraArguments<T extends OutputExtraArguments<?>> {

		/** The arguments.
		 */
		protected final StringBuilder arguments = new StringBuilder();

		private final String contributorId;

		OutputExtraArguments(String contributorId) {
			this.contributorId = contributorId;
		}

		/** Replies the identifier of the contributor.
		 *
		 * @return the contributor id.
		 */
		public String getContributorId() {
			return this.contributorId;
		}

		/** Create an argument with the given variable name and its associated value.
		 *
		 * @param name the name of the variable.
		 * @param value the value.
		 * @return {@code this}.
		 */
		@SuppressWarnings("unchecked")
		public T arg(String name, String value) {
			if (!Strings.isNullOrEmpty(value)) {
				final String varName = VariableNames.toPropertyName(name);
				final String arg = DECL_PREFIX + varName + DECL_INFIX + value;
				if (this.arguments.length() > 0) {
					this.arguments.append(" "); //$NON-NLS-1$
				}
				this.arguments.append(arg);
			}
			return (T) this;
		}

		/** Create an argument with the given variable name and its associated value.
		 *
		 * @param name the name of the variable.
		 * @param value the value.
		 * @return {@code this}.
		 */
		public T arg(String name, boolean value) {
			return arg(name, Boolean.toString(value));
		}

		/** Create an argument with the given variable name and its associated value.
		 *
		 * @param name the name of the variable.
		 * @param value the value.
		 * @return {@code this}.
		 */
		public T arg(String name, int value) {
			return arg(name, Integer.toString(value));
		}

		/** Create an argument with the given variable name and its associated value.
		 *
		 * @param name the name of the variable.
		 * @param value the value.
		 * @return {@code this}.
		 */
		public T arg(String name, float value) {
			return arg(name, Float.toString(value));
		}

		/** Create an argument with the given variable name and its associated value.
		 *
		 * @param name the name of the variable.
		 * @param value the value.
		 * @return {@code this}.
		 */
		public T arg(String name, double value) {
			return arg(name, Double.toString(value));
		}

		/** Create an argument with the given variable name and its associated value.
		 *
		 * @param name the name of the variable.
		 * @param value the value.
		 * @return {@code this}.
		 */
		public T arg(String name, long value) {
			return arg(name, Long.toString(value));
		}

		/** Create an argument with the given variable name and its associated value.
		 *
		 * @param name the name of the variable.
		 * @param value the value.
		 * @return {@code this}.
		 */
		public T arg(String name, byte value) {
			return arg(name, Byte.toString(value));
		}

		/** Create an argument with the given variable name and its associated value.
		 *
		 * @param name the name of the variable.
		 * @param value the value.
		 * @return {@code this}.
		 */
		public T arg(String name, short value) {
			return arg(name, Short.toString(value));
		}

		/** Create an argument with the given variable name and its associated value.
		 *
		 * @param name the name of the variable.
		 * @param value the value.
		 * @return {@code this}.
		 */
		public T arg(String name, char value) {
			return arg(name, Character.toString(value));
		}

		/** Add the arguments into the given configuration.
		 *
		 * <p>This function clears the list of arguments.
		 *
		 * @param configuration the configuration to update.
		 * @param configurator the access object to the configuration.
		 * @return {@code this}.
		 */
		@SuppressWarnings("unchecked")
		public final T apply(ILaunchConfigurationWorkingCopy configuration, ILaunchConfigurationConfigurator configurator) {
			writeArguments(configuration, configurator);
			this.arguments.setLength(0);
			return (T) this;
		}

		protected abstract void writeArguments(ILaunchConfigurationWorkingCopy configuration, ILaunchConfigurationConfigurator configurator);

	}

	/**
	 * Arguments to a launch configuration.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.12
	 */
	public static class OutputExtraSreArguments extends OutputExtraArguments<OutputExtraSreArguments> {

		OutputExtraSreArguments(String contributorId) {
			super(contributorId);
		}

		@Override
		protected void writeArguments(ILaunchConfigurationWorkingCopy configuration,
				ILaunchConfigurationConfigurator configurator) {
			configurator.setExtraSRELaunchingArguments(configuration, getContributorId(), this.arguments.toString());
		}

	}

	/**
	 * Arguments to a launch configuration.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.12
	 */
	public static class OutputExtraJreArguments extends OutputExtraArguments<OutputExtraJreArguments> {

		OutputExtraJreArguments(String contributorId) {
			super(contributorId);
		}

		@Override
		protected void writeArguments(ILaunchConfigurationWorkingCopy configuration,
				ILaunchConfigurationConfigurator configurator) {
			configurator.setExtraJRELaunchingArguments(configuration, getContributorId(), this.arguments.toString());
		}

	}

	/**
	 * Arguments from a launch configuration.
	 *
	 * @param <T> the type of the arguments list
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.12
	 */
	public abstract static class InputExtraArguments<T extends InputExtraArguments<?>> {

		/** List of arguments.
		 */
		protected final Map<String, String> arguments = new HashMap<>();

		private final String contributorId;

		InputExtraArguments(String contributorId) {
			this.contributorId = contributorId;
		}

		/** Replies the identifier of the contributor.
		 *
		 * @return the contributor id.
		 */
		public String getContributorId() {
			return this.contributorId;
		}

		/** Fill out this extra argument object with the values inside the given launch configurations.
		 *
		 * @param configuration the configuration to read.
		 * @param accessor the access object to the configuration.
		 * @return {@code this}.
		 */
		@SuppressWarnings("unchecked")
		public final T read(ILaunchConfiguration configuration, ILaunchConfigurationAccessor accessor) {
			this.arguments.clear();
			final String args = readArguments(configuration, accessor);
			if (!Strings.isNullOrEmpty(args)) {
				final Pattern pattern = Pattern.compile(Pattern.quote(DECL_PREFIX) + "(.+?)" + Pattern.quote(DECL_INFIX) + "([^ \n\r]*)"); //$NON_NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				final Matcher matcher = pattern.matcher(args);
				while (matcher.find()) {
					this.arguments.put(matcher.group(1), Strings.nullToEmpty(matcher.group(2)));
				}
			}
			return (T) this;
		}

		protected abstract String readArguments(ILaunchConfiguration configuration, ILaunchConfigurationAccessor accessor);

		/** Read an argument with the given variable name.
		 *
		 * @param name the name of the variable.
		 * @param defaultValue the default value.
		 * @return the read value, or the default value.
		 */
		public String arg(String name, String defaultValue) {
			final String varName = VariableNames.toPropertyName(name);
			final String value = this.arguments.get(varName);
			if (Strings.isNullOrEmpty(value)) {
				return defaultValue;
			}
			return value;
		}

		/** Read an argument with the given variable name.
		 *
		 * @param name the name of the variable.
		 * @param defaultValue the default value.
		 * @return the read value, or the default value.
		 */
		public boolean arg(String name, boolean defaultValue) {
			final String varName = VariableNames.toPropertyName(name);
			final String value = this.arguments.get(varName);
			if (Strings.isNullOrEmpty(value)) {
				return defaultValue;
			}
			try {
				return Boolean.parseBoolean(value);
			} catch (Throwable exception) {
				//
			}
			return defaultValue;
		}

		/** Read an argument with the given variable name.
		 *
		 * @param name the name of the variable.
		 * @param defaultValue the default value.
		 * @return the read value, or the default value.
		 */
		public int arg(String name, int defaultValue) {
			final String varName = VariableNames.toPropertyName(name);
			final String value = this.arguments.get(varName);
			if (Strings.isNullOrEmpty(value)) {
				return defaultValue;
			}
			try {
				return Integer.parseInt(value);
			} catch (Throwable exception) {
				//
			}
			return defaultValue;
		}

		/** Read an argument with the given variable name.
		 *
		 * @param name the name of the variable.
		 * @param defaultValue the default value.
		 * @return the read value, or the default value.
		 */
		public byte arg(String name, byte defaultValue) {
			final String varName = VariableNames.toPropertyName(name);
			final String value = this.arguments.get(varName);
			if (Strings.isNullOrEmpty(value)) {
				return defaultValue;
			}
			try {
				return Byte.parseByte(value);
			} catch (Throwable exception) {
				//
			}
			return defaultValue;
		}

		/** Read an argument with the given variable name.
		 *
		 * @param name the name of the variable.
		 * @param defaultValue the default value.
		 * @return the read value, or the default value.
		 */
		public short arg(String name, short defaultValue) {
			final String varName = VariableNames.toPropertyName(name);
			final String value = this.arguments.get(varName);
			if (Strings.isNullOrEmpty(value)) {
				return defaultValue;
			}
			try {
				return Short.parseShort(value);
			} catch (Throwable exception) {
				//
			}
			return defaultValue;
		}

		/** Read an argument with the given variable name.
		 *
		 * @param name the name of the variable.
		 * @param defaultValue the default value.
		 * @return the read value, or the default value.
		 */
		public long arg(String name, long defaultValue) {
			final String varName = VariableNames.toPropertyName(name);
			final String value = this.arguments.get(varName);
			if (Strings.isNullOrEmpty(value)) {
				return defaultValue;
			}
			try {
				return Long.parseLong(value);
			} catch (Throwable exception) {
				//
			}
			return defaultValue;
		}

		/** Read an argument with the given variable name.
		 *
		 * @param name the name of the variable.
		 * @param defaultValue the default value.
		 * @return the read value, or the default value.
		 */
		public float arg(String name, float defaultValue) {
			final String varName = VariableNames.toPropertyName(name);
			final String value = this.arguments.get(varName);
			if (Strings.isNullOrEmpty(value)) {
				return defaultValue;
			}
			try {
				return Float.parseFloat(value);
			} catch (Throwable exception) {
				//
			}
			return defaultValue;
		}

		/** Read an argument with the given variable name.
		 *
		 * @param name the name of the variable.
		 * @param defaultValue the default value.
		 * @return the read value, or the default value.
		 */
		public double arg(String name, double defaultValue) {
			final String varName = VariableNames.toPropertyName(name);
			final String value = this.arguments.get(varName);
			if (Strings.isNullOrEmpty(value)) {
				return defaultValue;
			}
			try {
				return Double.parseDouble(value);
			} catch (Throwable exception) {
				//
			}
			return defaultValue;
		}

		/** Read an argument with the given variable name.
		 *
		 * @param name the name of the variable.
		 * @param defaultValue the default value.
		 * @return the read value, or the default value.
		 */
		public char arg(String name, char defaultValue) {
			final String varName = VariableNames.toPropertyName(name);
			final String value = this.arguments.get(varName);
			if (Strings.isNullOrEmpty(value)) {
				return defaultValue;
			}
			try {
				return value.charAt(0);
			} catch (Throwable exception) {
				//
			}
			return defaultValue;
		}

	}

	/**
	 * Arguments from a launch configuration.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.12
	 */
	public static class InputExtraSreArguments extends InputExtraArguments<InputExtraSreArguments> {

		InputExtraSreArguments(String contributorId) {
			super(contributorId);
		}

		@Override
		protected String readArguments(ILaunchConfiguration configuration, ILaunchConfigurationAccessor accessor) {
			return accessor.getExtraSRELaunchingArguments(configuration, getContributorId());
		}

	}

	/**
	 * Arguments from a launch configuration.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.12
	 */
	public static class InputExtraJreArguments extends InputExtraArguments<InputExtraJreArguments> {

		InputExtraJreArguments(String contributorId) {
			super(contributorId);
		}

		@Override
		protected String readArguments(ILaunchConfiguration configuration, ILaunchConfigurationAccessor accessor) {
			return accessor.getExtraJRELaunchingArguments(configuration, getContributorId());
		}

	}

}
