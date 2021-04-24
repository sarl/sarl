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

package io.sarl.lang.sarl.actionprototype;

import java.io.Serializable;

import org.eclipse.xtext.util.Strings;

/**
 * Container of an argument name that could be changed according to the generated code.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class DynamicArgumentName implements Cloneable, Serializable {

	private static final long serialVersionUID = -3797441145684942274L;

	private String argument;

	/** Constructor.
	 *
	 * @param argument the default argument.
	 */
	public DynamicArgumentName(String argument) {
		this.argument = argument;
	}

	/** Replies the argument.
	 *
	 * @return the argument.
	 */
	public String getArgument() {
		return this.argument;
	}

	/** Change the argument.
	 *
	 * @param arg the argument.
	 */
	public void setArgument(String arg) {
		this.argument = arg;
	}

	@Override
	public DynamicArgumentName clone() {
		try {
			final DynamicArgumentName prototype = (DynamicArgumentName) super.clone();
			return prototype;
		} catch (CloneNotSupportedException e) {
			throw new Error(e);
		}
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		}

		if (obj == null) {
			return false;
		}

		if (this.getClass() == obj.getClass()) {
			final DynamicArgumentName arg = (DynamicArgumentName) obj;
			return Strings.equal(this.argument, arg.argument);
		}
		return false;
	}

	@Override
	public int hashCode() {
		int hash = 1;
		hash = 31 * hash + ((this.argument == null) ? 0 : this.argument.hashCode());
		return hash;
	}

	@Override
	public String toString() {
		return this.argument; //$NON-NLS-1$
	}

}
