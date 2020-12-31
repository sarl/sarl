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

package io.sarl.lang.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Annotation for marking an element to be part of the private API as a feature provider.
 *
 * <p>Any reference to a private API type from the SARL code is forbidden, except if the
 * caller's type is also marked with this annotation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Target({ ElementType.TYPE, ElementType.METHOD, ElementType.CONSTRUCTOR, ElementType.FIELD })
@Retention(RetentionPolicy.CLASS)
public @interface PrivateAPI {

	/** Indicate if the annotated element is a caller to the private API.
	 *
	 * @return {@code true} if the annotated element is a caller to the private API, not a private API itself.
	 *     {@code false} if the annotated element is both a caller to the private API, and a private API itself.
	 */
	boolean isCallerOnly() default false;

}
