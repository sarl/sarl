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

package io.sarl.eclipse.launching.dialog;

import org.arakhne.afc.vmutil.OperatingSystem;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Spinner;

/**
 * Additional utilities for the launch configuration tabs.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class SarlSwtFactory extends SWTFactory {

	private static final int DEFAULT_INCREMENT = 1;

	private static final int DEFAULT_PAGE_INCREMENT = 10;

	private static final String UNIX_LOPT = "--"; //$NON-NLS-1$

	private static final String WINDOWS_LOPT = "/"; //$NON-NLS-1$

	private static final String UNIX_SOPT = "-"; //$NON-NLS-1$

	private static final String WINDOWS_SOPT = "/"; //$NON-NLS-1$

	private static final String EQUAL_SIGN = "="; //$NON-NLS-1$

	private static final String DEFINITION_PREFIX = "D"; //$NON-NLS-1$

	private SarlSwtFactory() {
		//
	}

	/** Replies the characters to be used to mark the last option on the command line.
	 * This function is OS dependent.
	 *
	 * @return the command-line option prefix.
	 */
	public static String getCommandLineLastOptionPrefix() {
		if (OperatingSystem.getCurrentOS().isUnixCompliant()) {
			return UNIX_LOPT;
		}
		return ""; //$NON-NLS-1$
	}

	/** Replies the characters to be used as long-option prefix on the command line.
	 * This function is OS dependent.
	 *
	 * @return the command-line option prefix.
	 */
	public static String getCommandLineLongOptionPrefix() {
		if (OperatingSystem.getCurrentOS().isUnixCompliant()) {
			return UNIX_LOPT;
		}
		return WINDOWS_LOPT;
	}

	/** Replies the characters to be used as short-option prefix on the command line.
	 * This function is OS dependent.
	 *
	 * @return the command-line option prefix.
	 */
	public static String getCommandLineShortOptionPrefix() {
		if (OperatingSystem.getCurrentOS().isUnixCompliant()) {
			return UNIX_SOPT;
		}
		return WINDOWS_SOPT;
	}

	/** Replies the option prefixed with the characters to be used as option prefix on the command line.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @return the command-line option.
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
	 * @return the command-line option.
	 */
	public static String getCommandLineOption(String name, boolean value) {
		return getCommandLineOption(name) + EQUAL_SIGN + value;
	}

	/** Replies the option for definition a property.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @param value the value of the option.
	 * @return the command-line option.
	 */
	public static String getCommandLineDefinition(String name, boolean value) {
		return getCommandLineOption(DEFINITION_PREFIX) + name + "=" + value;
	}

	/** Replies the option for definition a property.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @param value the value of the option.
	 * @return the command-line option.
	 */
	public static String getCommandLineDefinition(String name, long value) {
		return getCommandLineOption(DEFINITION_PREFIX) + name + "=" + value;
	}

	/** Replies the option for definition a property.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @param value the value of the option.
	 * @return the command-line option.
	 */
	public static String getCommandLineDefinition(String name, double value) {
		return getCommandLineOption(DEFINITION_PREFIX) + name + "=" + value;
	}

	/** Replies the option for definition a property.
	 * This function is OS dependent.
	 *
	 * @param name the name of the option.
	 * @param value the value of the option.
	 * @return the command-line option.
	 */
	public static String getCommandLineDefinition(String name, String value) {
		return getCommandLineOption(DEFINITION_PREFIX) + name + "=" + value;
	}

	/** Create a spinner component.
	 *
	 * @param parent the parent component.
	 * @param hspan the horizontal span (number of columns)
	 * @param min the minimum value to be selected into the spinner.
	 * @param max the maximum value to be selected into the spinner.
	 * @param increment the increment for the selected value.
	 * @param pageIncrement the page increment for the selected value.
	 * @return the spinner.
	 */
	public static Spinner createSpinner(Composite parent, int hspan, int min, int max, int increment, int pageIncrement) {
		final Spinner spinner = new Spinner(parent, SWT.SINGLE | SWT.BORDER);
		spinner.setFont(parent.getFont());
		spinner.setMinimum(min);
		spinner.setMaximum(max);
		spinner.setIncrement(increment);
		spinner.setPageIncrement(pageIncrement);
		spinner.setDigits(0);
		final GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.horizontalSpan = hspan;
		spinner.setLayoutData(gd);
		return spinner;
	}

	/** Create a spinner component.
	 *
	 * @param parent the parent component.
	 * @param hspan the horizontal span (number of columns)
	 * @param min the minimum value to be selected into the spinner.
	 * @param max the maximum value to be selected into the spinner.
	 * @return the spinner.
	 */
	public static Spinner createSpinner(Composite parent, int hspan, int min, int max) {
		return createSpinner(parent, hspan, min, max, DEFAULT_INCREMENT, DEFAULT_PAGE_INCREMENT);
	}

}
