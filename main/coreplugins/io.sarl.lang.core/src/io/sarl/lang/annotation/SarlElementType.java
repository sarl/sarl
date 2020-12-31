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

/** Annotation for marking a JvmElement with the specific type of SARL element.
 *
 * <p>This annotation is attached to the JvmElements that represent SARL specific
 * type declarations, e.g. agent, behavior, etc.
 *
 * <p>This annotation is usually used for simulating quickly the "A instanceof B",
 * wheree A is a JvmElement (not an Xbase element), and B is a Xbase type.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Target({ ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
public @interface SarlElementType {

	/** Replies the SARL type.
	 *
	 * <p>The replies value is the ID of the ECore type, e.g.
	 * {@code SarlPackage::SARL_AGENT} and {@code SarlPackage::SARL_BEHAVIOR}.
	 *
	 * @return the SARL element type
	 */
	int value();

}
