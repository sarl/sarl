/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.generator.parser;

import java.io.File;

import org.arakhne.afc.vmutil.json.JsonBuffer;
import org.arakhne.afc.vmutil.json.JsonableObject;
import org.eclipse.xtend.lib.annotations.Data;

/** Data on the validation component.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
@Data
public class ValidationComponentData implements JsonableObject {

	/** Source file. */
	public File file;

	/** Line number. */
	public int lineno = -1;

	/** End line number. */
	public int endLineno = -1;

	/** Offset. */
	public int offset = -1;

	/** Length. */
	public int length = -1;

	/** Validation code. */
	public String code;

	@Override
	public void toJson(JsonBuffer buffer) {
		buffer.add("file", this.file); //$NON-NLS-1$
		buffer.add("lineno", Integer.valueOf(this.lineno)); //$NON-NLS-1$
		buffer.add("endLineno", Integer.valueOf(this.endLineno)); //$NON-NLS-1$
		buffer.add("offset", Integer.valueOf(this.offset)); //$NON-NLS-1$
		buffer.add("length", Integer.valueOf(this.length)); //$NON-NLS-1$
		buffer.add("code", Integer.valueOf(this.code)); //$NON-NLS-1$
	}

	@Override
	public String toString() {
		return JsonBuffer.toString(this);
	}

}
