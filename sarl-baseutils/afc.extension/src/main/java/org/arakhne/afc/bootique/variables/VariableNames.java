package org.arakhne.afc.bootique.variables;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Strings;

//FIXME: Move to Arakhne
public final class VariableNames {

	/** Prefix {@code bq.} for properties.
	 */
	public static final String BOOTIQUE_PROPERTY_PREFIX = "bq."; //$NON-NLS-1$

	private VariableNames() {
		//
	}

	/** Replies the name of an property that corresponds to the name of a bootique variable.
	 *
	 * @param bootiqueVariable the name of the bootique variable.
	 * @return the name of the property.
	 */
	public static String toPropertyName(String bootiqueVariable) {
		if (Strings.isNullOrEmpty(bootiqueVariable)) {
			return null;
		}
		return BOOTIQUE_PROPERTY_PREFIX + bootiqueVariable;
	}

	/** Replies the name of an environment variable that corresponds to the name of a bootique variable.
	 *
	 * @param bootiqueVariable the name of the bootique variable.
	 * @return the name of the environment variable.
	 */
	public static String toEnvironmentVariableName(String bootiqueVariable) {
		if (Strings.isNullOrEmpty(bootiqueVariable)) {
			return null;
		}
		final StringBuilder name = new StringBuilder();
		final Pattern pattern = Pattern.compile("((?:[a-z0_9_]+)|(?:[A-Z]+[^A-Z]*))"); //$NON-NLS-1$
		for (final String component : bootiqueVariable.split("[^a-zA-Z0_9_]+")) { //$NON-NLS-1$
			final Matcher matcher = pattern.matcher(component);
			while (matcher.find()) {
				final String word = matcher.group(1);
				if (name.length() > 0) {
					name.append("_"); //$NON-NLS-1$
				}
				name.append(word.toUpperCase());
			}
		}
		return name.toString();
	}

	/** Replies the name of an property that corresponds to the name of a bootique variable.
	 *
	 * @param bootiqueVariable the name of the bootique variable.
	 * @return the name of the property.
	 */
	public static String basename(String bootiqueVariable) {
		final int idx = bootiqueVariable.lastIndexOf('.');
		if (idx >= 0) {
			return bootiqueVariable.substring(idx + 1);
		}
		return bootiqueVariable;
	}

}
