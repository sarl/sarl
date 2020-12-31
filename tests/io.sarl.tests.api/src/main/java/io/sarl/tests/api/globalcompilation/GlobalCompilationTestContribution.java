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

package io.sarl.tests.api.globalcompilation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.junit.jupiter.api.TestTemplate;
import org.junit.jupiter.api.extension.ExtendWith;

/** Annotation for marking the tests as a part of the global compilation test suite.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
@TestTemplate
@ExtendWith(ResourceSetGlobalCompilationUnitExtension.class)
public @interface GlobalCompilationTestContribution {

	/** Replies if the validation process must be run in test methods.
	 *
	 * @return {@code true} if the validation is run in test methods.
	 * @since 0.11
	 */
	boolean getValidate() default ResourceSetGlobalCompilationContext.DEFAULT_VALIDATION_IN_TEST_METHODS;

}
