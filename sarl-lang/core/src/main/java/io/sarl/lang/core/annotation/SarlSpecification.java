/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.core.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import io.sarl.lang.core.SARLVersion;

/** Annotation for marking a JvmElement with the version number of the SARL specification.
 *
 * @author $Author: sgalland$
 * @version core 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 */
@Target({ ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
public @interface SarlSpecification {

	/** Replies the SARL code that is the cause of the generation.
	 *
	 * @return the SARL specification version.
	 */
	String value() default SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING;

}
