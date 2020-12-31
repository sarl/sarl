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


/** Annotation for specifying additional bundles in the classpath of the project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.METHOD })
public @interface TestClasspath {

	/** The test is valid for a run in Tycho.
	 *
	 * @return <code>true</code> if the unit test could be run in Tycho.
	 */
	String[] value() default {};
	
	/** Indicates if the default bundles of the SARL library must be included on
	 * the classpath.
	 *
	 * <p>If the default bundles are not included, the classpath will contains only
	 * the bundles given by {@link #value()}.
	 *
	 * @return <code>true</code> if the bundles are included; <code>false</code> if
	 * the classpath will contains only the values given by {@link #value()}
	 */
	boolean includeDefaultBundles() default true;

}
