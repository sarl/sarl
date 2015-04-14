/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.actionprototype;

import java.io.Serializable;

import com.google.common.base.Objects;

/**
 * The definition of an qualified action name.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class QualifiedActionName implements Cloneable, Serializable, Comparable<QualifiedActionName> {

	private static final long serialVersionUID = -5590450855487852546L;

	private String resourceID;
	private String declaringType;
	private String functionName;

	/**
	 * @param resourceID - the name of the resource where the action is defined.
	 * @param declaringType - the fully qualified name of the declaring type of the action.
	 * @param functionName - the name of the action.
	 */
	protected QualifiedActionName(String resourceID, String declaringType, String functionName) {
		this.functionName = functionName;
		this.resourceID = resourceID;
		this.declaringType = declaringType;
	}

	/** Replies the ID of the resource.
	 *
	 * @return the ID.
	 */
	public String getResourceID() {
		return this.resourceID;
	}

	/** Replies the fully qualified name of the declaring type in which the action is defined.
	 *
	 * @return the ID.
	 */
	public String getDeclaringType() {
		return this.declaringType;
	}

	/** Replies the name of the function.
	 *
	 * @return the name.
	 */
	public String getActionName() {
		return this.functionName;
	}

	@Override
	public QualifiedActionName clone() {
		try {
			return (QualifiedActionName) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new Error(e);
		}
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		}
		if (obj instanceof QualifiedActionName) {
			QualifiedActionName k = (QualifiedActionName) obj;
			return Objects.equal(this.resourceID, k.resourceID)
					&& Objects.equal(this.declaringType, k.declaringType)
					&& Objects.equal(this.functionName, k.functionName);
		}
		return false;
	}

	@Override
	public int hashCode() {
		int h = 1;
		h = 31 * h + this.resourceID.hashCode();
		h = 31 * h + this.declaringType.hashCode();
		h = 31 * h + this.functionName.hashCode();
		return h;
	}

	@Override
	public String toString() {
		return getContainerID() + "#" + this.functionName; //$NON-NLS-1$
	}

	/** Replies the identifier that corresponds to the resource and the declaring type.
	 *
	 * @return the container identifier.
	 */
	public String getContainerID() {
		return this.resourceID + "/" + this.declaringType; //$NON-NLS-1$
	}

	@Override
	public int compareTo(QualifiedActionName o) {
		if (o == null) {
			return Integer.MAX_VALUE;
		}
		int cmp = this.resourceID.compareTo(o.resourceID);
		if (cmp != 0) {
			return cmp;
		}
		cmp = this.declaringType.compareTo(o.declaringType);
		if (cmp != 0) {
			return cmp;
		}
		return this.functionName.compareTo(o.functionName);
	}

}
