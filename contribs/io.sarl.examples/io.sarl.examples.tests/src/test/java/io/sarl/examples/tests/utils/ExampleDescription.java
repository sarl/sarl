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

package io.sarl.examples.tests.utils;

import java.io.File;

import org.arakhne.afc.vmutil.json.JsonBuffer;
import org.arakhne.afc.vmutil.json.JsonableObject;
import org.eclipse.xtend.lib.annotations.Data;

/** Utilities for the example's tests.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
@Data
public class ExampleDescription implements JsonableObject {

	/** Name of the example.
	 */
	public String name;

	/** Name of the file that contains the example's code.
	 */
	public File archive;

	/** Name of the folder in which the source could be read.
	 */
	public File sourceFolder;

	@Override
	public void toJson(JsonBuffer buffer) {
		buffer.add("name", this.name);
		buffer.add("archive", this.archive);
		buffer.add("sourceFolder", this.sourceFolder);
	}

	@Override
	public String toString() {
		final JsonBuffer buffer = new JsonBuffer();
		toJson(buffer);
		return buffer.toString();
	}

	
}
