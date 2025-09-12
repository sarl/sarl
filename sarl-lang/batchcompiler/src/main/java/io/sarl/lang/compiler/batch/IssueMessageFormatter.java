/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.validation.Issue;

/** Formatter for the issue messages.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@FunctionalInterface
public interface IssueMessageFormatter {

	/** Format the message for the given issue.
	 *
	 * @param severity the severity that is considered by the compiler. This severity must be stronger than the one specified in the issue.
	 * @param issue the issue.
	 * @param uri URI to the problem.
	 * @return the message; or {@code null} for using the default formatter.
	 */
	String format(Severity severity, Issue issue, org.eclipse.emf.common.util.URI uri);

}
