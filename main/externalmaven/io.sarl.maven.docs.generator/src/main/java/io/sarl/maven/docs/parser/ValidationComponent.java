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

package io.sarl.maven.docs.parser;

import java.io.File;

import org.arakhne.afc.vmutil.json.JsonBuffer;
import org.arakhne.afc.vmutil.json.JsonableObject;

/** A component description for validation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ValidationComponent implements JsonableObject {

	private boolean isCompilable;

	private boolean isExecutable;

	private File file;

	private int lineno;

	private int endLineno;

	private int offset;

	private int length;

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
	public void setLinenoInSourceFile(int lineno) {
		this.lineno = lineno;
	}

	/** Replies the line number.
	 *
	 * @return the line number.
	 */
	public int getLinenoInSourceFile() {
		return this.lineno;
	}

	/** Change the end line number.
	 *
	 * @param lineno the end line number.
	 */
	public void setEndLinenoInSourceFile(int lineno) {
		this.endLineno = lineno;
		if (this.endLineno < 0 || this.endLineno < this.lineno) {
			this.endLineno = this.lineno;
		}
	}

	/** Replies the end line number.
	 *
	 * @return the end line number.
	 */
	public int getEndLinenoInSourceFile() {
		return this.endLineno;
	}

	/** Change the offset.
	 *
	 * @param offset the offset of the component within its file.
	 * @since 0.11
	 */
	public void setOffsetInSourceFile(int offset) {
		this.offset = offset;
	}

	/** Replies the offset.
	 *
	 * @return the offset of the component within its file.
	 */
	public int getOffsetInSourceFile() {
		return this.offset;
	}

	/** Change the length.
	 *
	 * @param length the length of the component within its file.
	 * @since 0.11
	 */
	public void setLengthInSourceFile(int length) {
		this.length = length;
	}

	/** Replies the length.
	 *
	 * @return the length of the component within its file.
	 * @since 0.11
	 */
	public int getLengthInSourceFile() {
		return this.length;
	}

	/** Change the file.
	 *
	 * @param file the file.
	 */
	public void setSourceFile(File file) {
		this.file = file;
	}

	/** Replies the file.
	 *
	 * @return the file.
	 */
	public File getSourceFile() {
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
	public void toJson(JsonBuffer buffer) {
		buffer.add("sourceFile", getSourceFile());
		buffer.add("linenoInSourceFile", getLinenoInSourceFile());
		buffer.add("endLinenoInSourceFile", getEndLinenoInSourceFile());
		buffer.add("offsetInSourceFile", getOffsetInSourceFile());
		buffer.add("lengthInSourceFile", getLengthInSourceFile());
		buffer.add("compilable", isCompilable());
		buffer.add("executable", isExecutable());
		buffer.add("code", getCode());
	}

	@Override
	public String toString() {
		return JsonBuffer.toString(this);
	}

}
