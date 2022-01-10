/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2022 the original authors or authors.
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

package io.sarl.docs.doclet2.framework;

import java.util.Collection;

import jdk.javadoc.doclet.Doclet;
import jdk.javadoc.doclet.DocletEnvironment;
import jdk.javadoc.doclet.Taglet;
import jdk.javadoc.doclet.Taglet.Location;

/** Manager of taglets.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public interface TagletManager {

	/** Add the instance of taglet.
	 *
	 * @param taglet the taglet to add.
	 */
	void addTaglet(Taglet taglet);

	/** Replies all the taglets.
	 *
	 * @return the taglets.
	 */
	Collection<Taglet> getAllTaglets();

	/** Replies the block taglet with the given name.
	 *
	 * @param location is the expected location. 
	 * @param name is the name of the taglet to search for.
	 * @return the taglet or {@code null}.
	 */
	Taglet getBlockTaglet(Location location, String name);

	/** Replies the inline taglet with the given name.
	 *
	 * @param name is the name of the taglet to search for.
	 * @return the taglet or {@code null}.
	 */
	Taglet getInlineTaglet(String name);

	/** Initialize all the registered taglets.
	 *
	 * @param environment the generation environment.
	 * @param doclet the generaion doclet.
	 */
	void init(DocletEnvironment env, Doclet doclet);

}
