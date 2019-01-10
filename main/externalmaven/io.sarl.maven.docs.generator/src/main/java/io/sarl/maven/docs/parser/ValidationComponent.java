/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.maven.docs.parser;

import java.io.File;

/** A component description for validation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ValidationComponent {

	private boolean isCompilable;

	private boolean isExecutable;

	private File file;

	private int lineno;

	private String code;

	/** Change the flag that indicates if the component is compilable with success.
	 *
	 * @param couldBeCompiled {@code true} if the component is compilable.
	 */
	public void setCompilable(boolean couldBeCompiled) {
		this.isCompilable = couldBeCompiled;
	}

	/** Replies if the component is compilable with success.
	 *
	 * @return {@code true} if the component is compilable.
	 */
	public boolean isCompilable() {
		return this.isCompilable;
	}

	/** Change the flag that indicates if the component is executable.
	 *
	 * @param executable {@code true} if the component is executable.
	 */
	public void setExecutable(boolean executable) {
		this.isExecutable = executable;
	}

	/** Replies if the component is executable.
	 *
	 * @return {@code true} if the component is executable.
	 */
	public boolean isExecutable() {
		return this.isExecutable;
	}

	/** Change the line number.
	 *
	 * @param lineno the line number.
	 */
	public void setLineno(int lineno) {
		this.lineno = lineno;
	}

	/** Replies the line number.
	 *
	 * @return the line number.
	 */
	public int getLineno() {
		return this.lineno;
	}

	/** Change the file.
	 *
	 * @param file the file.
	 */
	public void setFile(File file) {
		this.file = file;
	}

	/** Replies the file.
	 *
	 * @return the file.
	 */
	public File getFile() {
		return this.file;
	}

	/** Change the code to be process by the validation component.
	 *
	 * @param code the code to process.
	 */
	public void setCode(String code) {
		this.code = code;
	}

	/** Replies the code to be process by the validation component.
	 *
	 * @return the code to process.
	 */
	public String getCode() {
		return this.code;
	}

	@Override
	public String toString() {
		return getCode();
	}

}
