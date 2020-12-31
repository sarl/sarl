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

package io.sarl.maven.bootiqueapp.utils;

import com.google.common.base.Strings;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Utilities methods for accessing the properties and the environment variables.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public final class SystemProperties {

	private SystemProperties() {
		//
	}

	/** Replies the value of a property or an environment variable.
	 *
	 * <p>The function seach for the following values:<ul>
	 * <li>a property with the given {@code name},</li>
	 * <li>an environment variable with the given {@code name},</li>
	 * <li>a property with the name {@code "bq."+name},</li>
	 * <li>an environment variable with the name {@code "bq."+name}.</li>
	 * </ul>
	 *
	 * @param name the name of the property or an environment variable.
	 * @return the value, or {@code null}.
	 */
	@Pure
	@Inline(value = "getValue($1, null)")
	public static String getValue(String name) {
		return getValue(name, null);
	}

	/** Replies the value of a property or an environment variable.
	 *
	 * <p>The function seach for the following values:<ul>
	 * <li>a property with the given {@code name},</li>
	 * <li>an environment variable with the given {@code name},</li>
	 * <li>a property with the name {@code "bq."+name},</li>
	 * <li>an environment variable with the name {@code "bq."+name}.</li>
	 * </ul>
	 *
	 * @param name the name of the property or an environment variable.
	 * @param defaultValue the default value.
	 * @return the value, or the default value.
	 */
	@Pure
	public static String getValue(String name, String defaultValue) {
		if (!Strings.isNullOrEmpty(name)) {
			String value;
			try {
				value = System.getProperty(name, null);
				if (!Strings.isNullOrEmpty(value)) {
					return value;
				}
			} catch (Throwable exception) {
				//
			}
			try {
				value = System.getenv(name);
				if (!Strings.isNullOrEmpty(value)) {
					return value;
				}
			} catch (Throwable exception) {
				//
			}
			final String bqName = "bq." + name; //$NON-NLS-1$
			try {
				value = System.getProperty(bqName, null);
				if (!Strings.isNullOrEmpty(value)) {
					return value;
				}
			} catch (Throwable exception) {
				//
			}
			try {
				value = System.getenv(bqName);
				if (!Strings.isNullOrEmpty(value)) {
					return value;
				}
			} catch (Throwable exception) {
				//
			}
		}
		return defaultValue;
	}

}
