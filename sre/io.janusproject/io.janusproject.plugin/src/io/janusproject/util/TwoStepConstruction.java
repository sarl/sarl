/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject.util;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation that permits to mark the classes that must be constructed in two explicit steps.
 * <p>
 * The steps are:
 * <ol>
 * <li>invoke the constructor (explicit call);</li>
 * <li>injection mechanism (implicit, done during the constructor call);</li>
 * <li>finalization of the construction, eg. <code>postConstruction()</code>.</li>
 * </ol>
 *
 * @author $Author: galland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Target({ ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
public @interface TwoStepConstruction {

	/**
	 * Replies the names of the functions that are used for post construction.
	 *
	 * @return the names.
	 */
	String[] names() default { "postConstruction" };

}
