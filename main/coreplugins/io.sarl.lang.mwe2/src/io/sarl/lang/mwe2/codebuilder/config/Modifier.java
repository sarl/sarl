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

package io.sarl.lang.mwe2.codebuilder.config;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Describes a modifier for a rule.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class Modifier {

	private String type;

	private final List<String> modifier = new ArrayList<>();

	/** Change the name of the type.
	 *
	 * @param name the name of the type.
	 */
	public void setType(String name) {
		if (!Strings.isEmpty(name)) {
			this.type = name;
		}
	}

	/** Change the name of the type.
	 *
	 * @return the name of type.
	 */
	@Pure
	public String getType() {
		return this.type;
	}

	/** Change the modifier.
	 *
	 * @param modifier the modifier.
	 */
	public void addModifier(String modifier) {
		if (!Strings.isEmpty(modifier)) {
			this.modifier.add(modifier);
		}
	}

	/** Change the modifiers.
	 *
	 * @return the modifiers.
	 */
	@Pure
	public List<String> getModifiers() {
		return this.modifier;
	}

}
