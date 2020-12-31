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

package io.sarl.lang.util;

import com.google.common.base.Strings;

/** Command-line Utilities.
 *
 * <p>TODO: Move to the new version of AFC.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class CliUtilities {

	private static final String UNIX_LOPT = "--"; //$NON-NLS-1$

	private static final String WINDOWS_LOPT = "/"; //$NON-NLS-1$

	private static final String UNIX_SOPT = "-"; //$NON-NLS-1$

	private static final String WINDOWS_SOPT = "/"; //$NON-NLS-1$

	private static final String EQUAL_SIGN = "="; //$NON-NLS-1$

	private static final String DEFINITION_PREFIX = "D"; //$NON-NLS-1$

	private CliUtilities() {
		//
	}

	private static boolean isUnixCompliant() {
		final String os = System.getProperty("os.name").trim().toLowerCase(); //$NON-NLS-1$
		if (os.indexOf("windows") >= 0) { //$NON-NLS-1$
			return false;
		}
		return true;
	}

	/** Replies the characters to be used to mark the last option on the command line.
	 * This function is OS dependent.
	 *
	 * @return the command-line option prefix.
	 * @since 0.12
	 */
	public static String getCommandLineLastOptionPrefix() {
		if (isUnixCompliant()) {
			return UNIX_LOPT;
		}
		return ""; //$NON-NLS-1$
	}

	/** Replies the characters to be used as long-option prefix on the command line.
	 * This function is OS dependent.
	 *
	 * @return the command-line option prefix.
	 * @since 0.12
	 */
	public static String getCommandLineLongOptionPrefix() {
		if (isUnixCompliant()) {
			return UNIX_LOPT;
		}
		return WINDOWS_LOPT;
	}

	/** Replies the characters to be used as short-option prefix on the command line.
	 * This function is OS dependent.
	 *
	 * @return the command-line option prefix.
	 * @since 0.12
	 */
	public static String getCommandLineShortOptionPrefix() {
		if (isUnixCompliant()) {
			return UNIX_SOPT;
		}
		return WINDOWS_SOPT;
	}

	/** Replies the option prefixed with the characters to be used as option prefix on the command line.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @return the command-line option.
	 * @since 0.12
	 */
	public static String getCommandLineOption(String name) {
		if (name.length() > 1) {
			return getCommandLineLongOptionPrefix() + name;
		}
		return getCommandLineShortOptionPrefix() + name;
	}

	/** Replies the option prefixed with the characters to be used as option prefix on the command line and postfixed
	 * with the given boolean value.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @param value the value to put in the command-line option.
	 * @return the command-line option.
	 * @since 0.12
	 */
	public static String getCommandLineOption(String name, boolean value) {
		return getCommandLineOption(name) + EQUAL_SIGN + value;
	}

	/** Replies the option prefixed with the characters to be used as option prefix on the command line and postfixed
	 * with the given boolean value.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @param value the value to put in the command-line option.
	 * @return the command-line option.
	 * @since 0.12
	 */
	public static String getCommandLineOption(String name, long value) {
		return getCommandLineOption(name) + EQUAL_SIGN + value;
	}

	/** Replies the option prefixed with the characters to be used as option prefix on the command line and postfixed
	 * with the given boolean value.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @param value the value to put in the command-line option.
	 * @return the command-line option.
	 * @since 0.12
	 */
	public static String getCommandLineOption(String name, double value) {
		return getCommandLineOption(name) + EQUAL_SIGN + value;
	}

	/** Replies the option prefixed with the characters to be used as option prefix on the command line and postfixed
	 * with the given boolean value.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @param value the value to put in the command-line option.
	 * @return the command-line option.
	 * @since 0.12
	 */
	public static String getCommandLineOption(String name, String value) {
		assert !Strings.isNullOrEmpty(name);
		assert !Strings.isNullOrEmpty(value);
		final StringBuilder buf = new StringBuilder();
		buf.append(getCommandLineOption(name));
		buf.append(EQUAL_SIGN).append(value);
		return buf.toString();
	}

	/** Replies the option for definition a property.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @param value the value to put in the command-line option.
	 * @return the command-line option.
	 * @since 0.12
	 */
	public static String getCommandLineDefinition(String name, boolean value) {
		return getCommandLineDefinition(name, Boolean.toString(value));
	}

	/** Replies the option for definition a property.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @param value the value to put in the command-line option.
	 * @return the command-line option.
	 * @since 0.12
	 */
	public static String getCommandLineDefinition(String name, long value) {
		return getCommandLineDefinition(name, Long.toString(value));
	}

	/** Replies the option for definition a property.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @param value the value to put in the command-line option.
	 * @return the command-line option.
	 * @since 0.12
	 */
	public static String getCommandLineDefinition(String name, double value) {
		return getCommandLineDefinition(name, Double.toString(value));
	}

	/** Replies the option for definition a property.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @param value the value to put in the command-line option.
	 * @return the command-line option.
	 * @since 0.12
	 */
	public static String getCommandLineDefinition(String name, String value) {
		assert !Strings.isNullOrEmpty(name);
		assert !Strings.isNullOrEmpty(value);
		final StringBuilder buf = new StringBuilder();
		buf.append(getCommandLineOption(DEFINITION_PREFIX));
		buf.append(name);
		buf.append(EQUAL_SIGN).append(value);
		return buf.toString();
	}

	/** Replies the option prefixed with the characters to be used as option prefix on the command line.
	 * This function is for Unix.
	 *
	 * @param name the name of the option.
	 * @return the command-line option.
	 * @since 0.12
	 */
	public static String getUnixCommandLineOption(String name) {
		if (name.length() > 1) {
			return getUnixCommandLineLongOptionPrefix() + name;
		}
		return getUnixCommandLineShortOptionPrefix() + name;
	}

	/** Replies the option prefixed with the characters to be used as option prefix on the command line and postfixed
	 * with the given boolean value.
	 * This function is for Unix.
	 *
	 * @param name the name of the option.
	 * @param value the value to put in the command-line option.
	 * @return the command-line option.
	 * @since 0.12
	 */
	public static String getUnixCommandLineOption(String name, String value) {
		assert !Strings.isNullOrEmpty(name);
		assert !Strings.isNullOrEmpty(value);
		final StringBuilder buf = new StringBuilder();
		buf.append(getUnixCommandLineOption(name));
		buf.append(EQUAL_SIGN).append(value);
		return buf.toString();
	}

	/** Replies the characters to be used as short-option prefix on the command line.
	 * This function is for Unix.
	 *
	 * @return the command-line option prefix.
	 * @since 0.12
	 */
	public static String getUnixCommandLineShortOptionPrefix() {
		return UNIX_SOPT;
	}

	/** Replies the characters to be used as long-option prefix on the command line on Unix.
	 *
	 * @return the command-line option prefix.
	 * @since 0.12
	 */
	public static String getUnixCommandLineLongOptionPrefix() {
		return UNIX_LOPT;
	}

	/** Replies the characters to be used to mark the last option on the command line.
	 * This function is for Unix.
	 *
	 * @return the command-line option prefix.
	 * @since 0.12
	 */
	public static String getUnixCommandLineLastOptionPrefix() {
		return UNIX_LOPT;
	}

}
