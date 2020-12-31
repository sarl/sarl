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

/** Exception that is thrown when the SARL batch compiler cannot be created.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class InvalidSarlBatchCompilerError extends Error {

	private static final long serialVersionUID = 2124788845031567919L;

	/** Constructor with the standard error message.
	 */
	public InvalidSarlBatchCompilerError() {
		super(Messages.InvalidSarlBatchCompilerException_0);
	}
	
	/** Constructor with the standard error message and a cause.
	 *
	 * @param cause the cause of the exception.
	 */
	public InvalidSarlBatchCompilerError(Throwable cause) {
		super(Messages.InvalidSarlBatchCompilerException_0, cause);
	}

}
