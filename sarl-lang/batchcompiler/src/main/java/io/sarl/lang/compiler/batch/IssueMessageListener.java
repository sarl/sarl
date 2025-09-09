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

import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.validation.Issue;

/** Listener for the issue messages.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version batchcompiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid batchcompiler
 */
@FunctionalInterface
public interface IssueMessageListener {

	/** Replies the message for the given issue.
	 *
	 * @param severity the severity that was considered by the batch compiler. It may be stronger alert level that those in the {@code issue}.
	 * @param issue the issue.
	 * @param uri URI to the problem.
	 * @param message the formatted message.
	 */
	void onIssue(Severity severity, Issue issue, org.eclipse.emf.common.util.URI uri, String message);

}
