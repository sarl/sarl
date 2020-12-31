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

package io.sarl.lang.extralanguage.compiler;

import com.google.common.base.Strings;

/** Utilities classes for the output configurations that are dedicated to the extra language generators.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public final class ExtraLanguageOutputConfigurations {

	/** String of characters that should be appended to the output configuration's name
	 * in order to recognized the configuration as being associated to an extra language generator.
	 */
	public static final String NAME_POSTFIX = ".extraLanguageGeneratorConfiguration"; //$NON-NLS-1$

	private ExtraLanguageOutputConfigurations() {
		//
	}

	/** Create and reply a name for an output configuration dedicated to the extra language generators.
	 *
	 * <p>Usually, the value of {@link #NAME_POSTFIX} is appended to the given identifier.
	 *
	 * @param generatorID the identifier of the generator.
	 * @return the name of the configuration.
	 */
	public static String createOutputConfigurationName(String generatorID) {
		return generatorID + NAME_POSTFIX;
	}

	/** Replies if the given name is one for an output configuration that is associated
	 * to a extra language generator.
	 *
	 * <p>Usually, the name has the postfix {@link #NAME_POSTFIX}.
	 *
	 * @param name the name to test.
	 * @return {@code true} if the given name is for an output configuration associated to an extra language generator.
	 */
	public static boolean isExtraLanguageOutputConfiguration(String name) {
		return Strings.nullToEmpty(name).endsWith(NAME_POSTFIX);
	}

}
