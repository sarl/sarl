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

package io.sarl.lang.mwe2.externalspec;

/** Appendable for XML output that provides high level methods.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public interface IXmlStyleAppendable extends IStyleAppendable {

	/** Open the tag.
	 *
	 * @param tag the name of the tag.
	 * @param nameValuePairs the parameters of the tag (name-value pairs).
	 * @return the appendable for the inside.
	 */
	IXmlStyleCloseable open(String tag, String... nameValuePairs);

	/** Open and close the tag.
	 *
	 * @param tag the name of the tag.
	 * @param nameValuePairs the parameters of the tag (name-value pairs).
	 */
	void appendTag(String tag, String... nameValuePairs);

	/** Create a tag with a value.
	 *
	 * @param tag the name of the tag.
	 * @param value the value.
	 * @param nameValuePairs the parameters of the tag (name-value pairs).
	 */
	void appendTagWithValue(String tag, String value, String... nameValuePairs);

}
