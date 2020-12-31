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
package io.sarl.tests.api.extensions;

import org.eclipse.xtext.util.JavaVersion;
import org.junit.jupiter.api.extension.ConditionEvaluationResult;
import org.junit.jupiter.api.extension.ExecutionCondition;
import org.junit.jupiter.api.extension.ExtensionContext;

import io.sarl.lang.SARLVersion;

/** JUnit 5 extension that tests if the Java version used for running the tests
 * is at least the minimal Java version for compiling the SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class JavaVersionCheckExtension implements ExecutionCondition {

	@Override
	public ConditionEvaluationResult evaluateExecutionCondition(ExtensionContext context) {
		// Check if the minimal version of Java is used for running the tests.
		final JavaVersion cVersion = JavaVersion.fromQualifier(System.getProperty("java.specification.version"));
		if (cVersion == null) {
			return ConditionEvaluationResult.disabled("You must use JDK " + SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT + " or higher for running the tests.");
		}
		final JavaVersion mVersion = JavaVersion.fromQualifier(SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
		if (mVersion == null || !cVersion.isAtLeast(mVersion)) {
			return ConditionEvaluationResult.disabled("You must use JDK " + SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT + " or higher for running the tests.");
		}
		final JavaVersion xVersion = JavaVersion.fromQualifier(SARLVersion.INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
		// If null the max version that is specified into the SARL configuration is not yey supported by Xtext enumeration
		if (xVersion != null && cVersion.isAtLeast(xVersion)) {
			return ConditionEvaluationResult.disabled("You must use JDK strictly below " + SARLVersion.INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT + " for running the tests.");
		}
		return ConditionEvaluationResult.enabled("supported version of JDK");
	}

}
