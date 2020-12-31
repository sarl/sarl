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
package io.sarl.tests.api;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


/** Annotation for specifying the scope of the unit test.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.METHOD, ElementType.TYPE })
public @interface TestScope {

	/** The test is valid for a run in Maven Tycho.
	 *
	 * @return <code>true</code> if the unit test could be run in Tycho.
	 */
	boolean tycho() default true;

	/** The test is valid for a run in Eclipse.
	 *
	 * @return <code>true</code> if the unit test could be run in Eclipse.
	 */
	boolean eclipse() default true;

	/** The test is valid for a run in an environment in which the Maven central repository is available.
	 *
	 * @return <code>true</code> if the unit test could be run only if a connection to the Maven
	 * central repository is possible.
	 */
	boolean needmavencentral() default false;

}
