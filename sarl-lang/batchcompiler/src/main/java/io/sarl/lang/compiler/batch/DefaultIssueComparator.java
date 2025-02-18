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

package io.sarl.lang.compiler.batch;

import java.util.Comparator;

import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.validation.Issue;

/** Comparator of issues.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DefaultIssueComparator implements Comparator<Issue> {

	private static int compareSafe(Integer n1, Integer n2) {
		if (n1 == null) {
			return n2 == null ? 0 : -1;
		}
		if (n2 == null) {
			return 1;
		}
		return Integer.compare(n1.intValue(), n2.intValue());
	}

	private static int compareSafe(Severity s1, Severity s2) {
		if (s1 == null) {
			return s2 == null ? 0 : -1;
		}
		if (s2 == null) {
			return 1;
		}
		return s1.compareTo(s2);
	}

	private static int compareSafe(String s1, String s2) {
		if (s1 == null) {
			return s2 == null ? 0 : -1;
		}
		if (s2 == null) {
			return 1;
		}
		return s1.compareTo(s2);
	}

	@Override
	public int compare(Issue issue1, Issue issue2) {
		if (issue1 == issue2) {
			return 0;
		}
		if (issue1 == null) {
			return -1;
		}
		if (issue2 == null) {
			return 1;
		}
		final var u1 = issue1.getUriToProblem();
		final var u2 = issue2.getUriToProblem();
		var cmp = 0;
		if (u1 != u2 && u1 != null && u2 != null) {
			cmp = u1.toFileString().compareTo(u2.toFileString());
		}
		if (cmp != 0) {
			return cmp;
		}
		cmp = compareSafe(issue1.getLineNumber(), issue2.getLineNumber());
		if (cmp != 0) {
			return cmp;
		}
		cmp = compareSafe(issue1.getColumn(), issue2.getColumn());
		if (cmp != 0) {
			return cmp;
		}
		cmp = compareSafe(issue1.getSeverity(), issue2.getSeverity());
		if (cmp != 0) {
			return cmp;
		}
		cmp = compareSafe(issue1.getMessage(), issue2.getMessage());
		if (cmp != 0) {
			return cmp;
		}
		return Integer.compare(System.identityHashCode(issue1), System.identityHashCode(issue2));
	}

}
