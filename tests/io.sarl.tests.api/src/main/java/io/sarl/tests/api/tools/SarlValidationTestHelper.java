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
package io.sarl.tests.api.tools;

import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.testing.validation.ValidationTestHelper;
import org.eclipse.xtext.validation.Issue;

/** Helper for testing the validation results.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class SarlValidationTestHelper extends ValidationTestHelper {

	public SarlValidationTestHelper() {
		//
	}

	@Override
	public StringBuilder getIssuesAsString(Resource resource, Iterable<Issue> issues, StringBuilder result) {
		return super.getIssuesAsString(resource, issues, result);
	}

	@Override
	public Iterable<Issue> matchIssues(Resource resource, EClass objectType, String code, int offset, int length,
			Severity severity, List<Issue> validate, String... messageParts) {
		return super.matchIssues(resource, objectType, code, offset, length, severity, validate, messageParts);
	}

}
