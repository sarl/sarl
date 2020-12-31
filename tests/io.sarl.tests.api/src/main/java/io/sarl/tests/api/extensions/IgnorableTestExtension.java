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

import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;

import org.junit.jupiter.api.extension.ConditionEvaluationResult;
import org.junit.jupiter.api.extension.ExecutionCondition;
import org.junit.jupiter.api.extension.ExtensionContext;

import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.TestScope;
import io.sarl.tests.api.tools.TestReflections;
import io.sarl.tests.api.tools.TestUtils;

/** JUnit 5 extension that tests if the test is ignorable.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class IgnorableTestExtension implements ExecutionCondition {

	/** URL of the Maven central repository.
	 */
	public static final String MAVEN_CENTRAL_REPOSITORY_URL = "http://repo1.maven.org/maven2/io/sarl/lang/io.sarl.lang.core/0.2.0/io.sarl.lang.core-0.2.0.pom";

	/** Timeout for connecting to the Maven central server (in milliseconds).
	 */
	public static final int MAVEN_CENTRAL_TIMEOUT = 15000;

	private boolean isIgnorable(ExtensionContext context) {
		final Class<?> type = context.getRequiredTestClass();
		if (AbstractSarlTest.class.isAssignableFrom(type)
				&& context.getTestInstance().isPresent()) {
			final Object instance = context.getTestInstance().get();
			if (instance != null) {
				try {
					final Object r = TestReflections.invoke(instance, "isIgnorable", context);
					if (r instanceof Boolean) {
						return ((Boolean) r).booleanValue();
					}
				} catch (Throwable e) {
					//
				}
			}
		}
		return false;
	}

	@Override
	public ConditionEvaluationResult evaluateExecutionCondition(ExtensionContext context) {
		// This test is working only in Eclipse, or Maven/Tycho.
		TestScope scope = context.getRequiredTestClass().getAnnotation(TestScope.class);
		if (scope == null) {
			Class<?> enclosingType = context.getRequiredTestClass();
			while (scope == null && enclosingType != null) {
				scope = enclosingType.getAnnotation(TestScope.class);
				enclosingType = enclosingType.getEnclosingClass();
			}
		}
		if (scope != null) {
			if (!scope.tycho() && !scope.eclipse()) {
				return ConditionEvaluationResult.disabled("not running on the current framework");
			}
			if (scope.tycho() || scope.eclipse()) {
				boolean isEclipse = TestUtils.isEclipseRuntimeEnvironment();
				if (scope.tycho()) {
					if (isEclipse) {
						return ConditionEvaluationResult.disabled("cannot run Tycho-specific test in Eclipse");
					}
				} else if (!isEclipse) {
					return ConditionEvaluationResult.disabled("cannot run Eclipse-specific test outside Eclipse");
				}
			}
			if (scope.needmavencentral()) {
				boolean canAccessNetwork = true;
				try {
					URL central = new URL(MAVEN_CENTRAL_REPOSITORY_URL);
					URLConnection connection = central.openConnection();
					connection.setConnectTimeout(MAVEN_CENTRAL_TIMEOUT);
					try (InputStream is = connection.getInputStream()) {
						byte[] buffer = new byte[128];
						int length = is.read(buffer);
						while (length > 0) {
							length = is.read(buffer);
						}
					}
				} catch (Exception exception) {
					canAccessNetwork = false;
				}
				if (!canAccessNetwork) {
					return ConditionEvaluationResult.disabled("cannot have access to Maven central server");
				}
			}
		}
		//
		if (isIgnorable(context)) {
			return ConditionEvaluationResult.disabled("this test is dynamically ignored.");
		}
		//
		return ConditionEvaluationResult.enabled("no dynamic condition for disabling this test");
	}

}
