/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.validation.Issue;

/** Set of utility classes that provide additional assertion functions on SARL issues.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version tests.api 0.15.1 20250911-224823
 * @mavengroupid io.sarl.baseutils
 * @mavenartifactid tests.api
 * @since 0.11
 */
public final class TestIssues {

	private TestIssues() {
		//
	}

	/** Replies if the given issue has the given parts within its message.
	 *
	 * @param issue the issue to test.
	 * @param messageParts the parts of the error message to search for.
	 * @return {@code true} if all parts are found.
	 */
	public static boolean isIssueMessage(Issue issue, String... messageParts) {
		for (var messagePart : messageParts) {
			if (!issue.getMessage().toLowerCase().contains(messagePart.toLowerCase())) {
				return false;
			}
		}
		return true;
	}

	/** Replies the string representation of issues.
	 *
	 * @param model the parsed model.
	 * @param issues the issues.
	 * @param result the result.
	 * @return {@code result}.
	 */
	public static StringBuilder getIssuesAsString(EObject model, Iterable<Issue> issues, StringBuilder result) {
		for(var issue : issues) {
			final var uri = issue.getUriToProblem();
			result.append(issue.getSeverity());
			result.append(" ("); //$NON-NLS-1$
			result.append(issue.getCode());
			result.append(") '"); //$NON-NLS-1$
			result.append(issue.getMessage());
			result.append("'"); //$NON-NLS-1$
			if (uri != null) {
				var eObject = model.eResource().getResourceSet().getEObject(uri, true);
				result.append(" on "); //$NON-NLS-1$
				result.append(eObject.eClass().getName());
			}
			result.append("\n"); //$NON-NLS-1$
		}
		return result;
	}

}
