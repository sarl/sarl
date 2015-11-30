/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.typing;

import com.google.inject.Singleton;

/**
 * Constants used by the XExpression helpers.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see SARLXExpressionHelper
 */
@Singleton
public final class XExpressionConstants {

	/** Regular expression pattern that matches the names of functions usually
	 * considered as pure.
	 */
	public static final String SPECIAL_PURE_FUNCTION_NAME_PATTERN =
			"^(((is)|(get)|(has))[A-Z].*)|(equals)|(hashCode)|(clone)|(toString)$"; //$NON-NLS-1$;

	private XExpressionConstants() {
		//
	}

}
