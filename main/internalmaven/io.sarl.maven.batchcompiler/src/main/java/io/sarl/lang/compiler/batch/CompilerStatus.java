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

package io.sarl.lang.compiler.batch;

import org.eclipse.xtext.xbase.lib.Pure;

/** The type of statut that is reached by a Java batch compiler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public enum CompilerStatus {

	/** Compilation success.
	 */
	COMPILATION_SUCCESS {
		@Override
		public boolean isSuccess() {
			return true;
		}
		@Override
		public String getFailureExplanation() {
			return null;
		}
	},
	/** Compilation failure.
	 */
	COMPILATION_FAILURE {
		@Override
		public boolean isSuccess() {
			return false;
		}
		@Override
		public String getFailureExplanation() {
			return Messages.CompilerStatus_0;
		}
	},
	/** No source code found.
	 */
	NOTHING_TO_COMPILE {
		@Override
		public boolean isSuccess() {
			return false;
		}
		@Override
		public String getFailureExplanation() {
			return Messages.CompilerStatus_1;
		}
	},
	/** Compilation is manually canceled.
	 */
	CANCELED {
		@Override
		public boolean isSuccess() {
			return false;
		}
		@Override
		public String getFailureExplanation() {
			return Messages.CompilerStatus_2;
		}
	};

	/** Replies if the status is a success or an error.
	 *
	 * @return {@code true} if the compiler has terminated on a success.
	 */
	@Pure
	public abstract boolean isSuccess();

	/** Replies the standard text that explains the failing status. 
	 *
	 * @return the explanation text; or {@code null} if the status is successfull.
	 */
	@Pure
	public abstract String getFailureExplanation();

}
