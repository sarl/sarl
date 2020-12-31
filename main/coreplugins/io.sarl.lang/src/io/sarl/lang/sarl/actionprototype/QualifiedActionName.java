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
import java.lang.ref.WeakReference;

import com.google.common.base.Objects;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.util.Strings;

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

	private final WeakReference<JvmIdentifiableElement> declaringType;

	private String functionName;

	/** Constructor.
	 * @param resourceID the name of the resource where the action is defined.
	 * @param declaringType the declaring type of the action.
	 * @param functionName the name of the action.
	 */
	protected QualifiedActionName(String resourceID, JvmIdentifiableElement declaringType, String functionName) {
		this.functionName = Strings.emptyIfNull(functionName);
		this.resourceID = Strings.emptyIfNull(resourceID);
		this.declaringType = new WeakReference<>(declaringType);
	}

	/** Replies the ID of the resource.
	 *
	 * @return the ID.
	 */
	public String getResourceID() {
		return this.resourceID;
	}

	/** Replies the declaring type in which the action is defined.
	 *
	 * @return the container.
	 */
	public JvmIdentifiableElement getDeclaringType() {
		return this.declaringType.get();
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

		if (obj == null) {
			return false;
		}

		if (this.getClass() == obj.getClass()) {
			final QualifiedActionName k = (QualifiedActionName) obj;
			return Objects.equal(this.resourceID, k.resourceID)
					&& Objects.equal(
							this.declaringType.get().getQualifiedName(),
							k.declaringType.get().getQualifiedName())
					&& Objects.equal(this.functionName, k.functionName);
		}
		return false;
	}

	@Override
	public int hashCode() {
		int hash = 1;
		hash = 31 * hash + this.resourceID.hashCode();
		hash = 31 * hash + this.declaringType.hashCode();
		hash = 31 * hash + this.functionName.hashCode();
		return hash;
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
		return this.resourceID + "/" + this.declaringType.get().getQualifiedName(); //$NON-NLS-1$
	}

	@Override
	public int compareTo(QualifiedActionName otherName) {
		if (otherName == null) {
			return Integer.MAX_VALUE;
		}
		int cmp = this.resourceID.compareTo(otherName.resourceID);
		if (cmp != 0) {
			return cmp;
		}
		cmp = this.declaringType.get().getQualifiedName().compareTo(
				otherName.declaringType.get().getQualifiedName());
		if (cmp != 0) {
			return cmp;
		}
		return this.functionName.compareTo(otherName.functionName);
	}

}
