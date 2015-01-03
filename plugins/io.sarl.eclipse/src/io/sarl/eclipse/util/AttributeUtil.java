/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.eclipse.util;

import java.util.Map;

import com.google.common.base.Strings;


/** Utilities for maps of attributes.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class AttributeUtil {

	/**
	 */
	private AttributeUtil() {
		//
	}

	/** Read a boolean value from the given attributes.
	 *
	 * @param attributes - the attributes.
	 * @param name - the name of the attribute.
	 * @param defaultValue - the default value.
	 * @return the value of the attribute.
	 */
	public static boolean get(Map<String, ?> attributes, String name, boolean defaultValue) {
		if (attributes != null && !Strings.isNullOrEmpty(name)) {
			Object value = attributes.get(name);
			if (value != null) {
				if ("true".equalsIgnoreCase(value.toString())) { //$NON-NLS-1$
					return true;
				}
				if ("false".equalsIgnoreCase(value.toString())) { //$NON-NLS-1$
					return false;
				}
			}
		}
		return defaultValue;
	}

	/** Read a value from the given attributes.
	 *
	 * @param attributes - the attributes.
	 * @param name - the name of the attribute.
	 * @param defaultValue - the default value.
	 * @return the value of the attribute.
	 */
	public static String get(Map<String, ?> attributes, String name, String defaultValue) {
		if (attributes != null && !Strings.isNullOrEmpty(name)) {
			Object value = attributes.get(name);
			if (value != null && !Strings.isNullOrEmpty(value.toString())) {
				return value.toString();
			}
		}
		return defaultValue;
	}

}
