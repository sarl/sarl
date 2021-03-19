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

package io.sarl.lang.core;

import java.io.Serializable;
import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

/**
 * Unique Identifier for a {@link Space}.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SpaceID extends SRESpecificDataContainer implements Serializable, Comparable<SpaceID>, Cloneable {

	private static final long serialVersionUID = 8764568066583474825L;

	private final UUID id;

	private final UUID contextID;

	private final transient Class<? extends SpaceSpecification<?>> spaceSpec;

	/** Constructs a space ID.
	 *
	 * @param contextID the unique ID of the context that contains the space (cannot be {@code null}).
	 * @param id the unique ID of the space (cannot be {@code null}).
	 * @param spaceSpec the specification of the space for which the ID must be created
	 * 	   (could be {@code null}).
	 */
	public SpaceID(UUID contextID, UUID id, Class<? extends SpaceSpecification<?>> spaceSpec) {
		assert contextID != null;
		assert id != null;
		this.id = id;
		this.contextID = contextID;
		this.spaceSpec = spaceSpec;
	}

	@Override
	@Pure
	public SpaceID clone() {
		try {
			return (SpaceID) super.clone();
		} catch (Throwable exception) {
			throw new Error(exception);
		}
	}

	/**
	 * Returns a Unique Identifier for the space.
	 *
	 * @return this space's UUID
	 */
	@Pure
	public UUID getID() {
		return this.id;
	}

	/**
	 * Return the UUID of the context where the space was created.
	 *
	 * @return the context's id
	 */
	@Pure
	public UUID getContextID() {
		return this.contextID;
	}

	/**
	 * Replies the {@link SpaceSpecification} this space respects.
	 *
	 * @return The {@link SpaceSpecification} of this space
	 */
	@Pure
	public Class<? extends SpaceSpecification<?>> getSpaceSpecification() {
		return this.spaceSpec;
	}

	@Override
	@Pure
	public int hashCode() {
		int result = 1;
		result = 31 * result + (this.contextID == null ? 0 : this.contextID.hashCode());
		result = 31 * result + (this.id == null ? 0 : this.id.hashCode());
		return result;
	}

	@Override
	@Pure
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof SpaceID)) {
			return false;
		}

		final SpaceID other = (SpaceID) obj;

		if (!equalsContext(other)) {
			return false;
		}

		return equalsID(other);
	}

	private boolean equalsContext(SpaceID other) {
		if (this.contextID == null) {
			return other.contextID == null;
		}
		return this.contextID.equals(other.contextID);
	}

	private boolean equalsID(SpaceID other) {
		if (this.id == null) {
			return other.id == null;
		}
		return this.id.equals(other.id);
	}

	@Override
	@Pure
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.add("type", getClass().getSimpleName()); //$NON-NLS-1$
		builder.add("id", this.id); //$NON-NLS-1$
		builder.add("contextID", this.contextID); //$NON-NLS-1$
		builder.add("spaceSpec", this.spaceSpec); //$NON-NLS-1$
		return builder.toString();
	}

	@Override
	@Pure
	public int compareTo(SpaceID otherID) {
		final int cmp = this.contextID.compareTo(otherID.contextID);
		if (cmp != 0) {
			return cmp;
		}
		return this.id.compareTo(otherID.id);
	}

}
