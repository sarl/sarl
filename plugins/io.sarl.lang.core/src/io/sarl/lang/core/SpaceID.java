/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

/**
 * Unique Identifier for a {@link Space}.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SpaceID implements Serializable, Comparable<SpaceID> {

	private static final long serialVersionUID = 8764568066583474825L;

	private final UUID id;
	private final UUID contextID;
	private final transient Class<? extends SpaceSpecification<?>> spaceSpec;

	/** Constructs a space ID.
	 *
	 * @param contextID - the unique ID of the context that contains the space (cannot be <code>null</code>).
	 * @param id - the unique ID of the space (cannot be <code>null</code>).
	 * @param spaceSpec - the specification of the space for which the ID must be created
	 * 	(could be <code>null</code>).
	 */
	public SpaceID(UUID contextID, UUID id, Class<? extends SpaceSpecification<?>> spaceSpec) {
		assert (contextID != null);
		assert (id != null);
		this.id = id;
		this.contextID = contextID;
		this.spaceSpec = spaceSpec;
	}

	/**
	 * Returns a Unique Identifier for the space.
	 *
	 * @return this space's UUID
	 */
	public UUID getID() {
		return this.id;
	}

	/**
	 * Return the UUID of the context where the space was created.
	 *
	 * @return the context's id
	 */
	public UUID getContextID() {
		return this.contextID;
	}

	/**
	 * Replies the {@link SpaceSpecification} this space respects.
	 *
	 * @return The {@link SpaceSpecification} of this space
	 */
	public Class<? extends SpaceSpecification<?>> getSpaceSpecification() {
		return this.spaceSpec;
	}

	@Override
	public int hashCode() {
		int result = 1;
		result = 31 * result + ((this.contextID == null) ? 0 : this.contextID.hashCode());
		result = 31 * result + ((this.id == null) ? 0 : this.id.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}

		SpaceID other = (SpaceID) obj;

		if (!equalsContext(other)) {
			return false;
		}

		return equalsID(other);
	}

	private boolean equalsContext(SpaceID other) {
		if (this.contextID == null) {
			if (other.contextID != null) {
				return false;
			}
		} else if (!this.contextID.equals(other.contextID)) {
			return false;
		}
		return true;
	}

	private boolean equalsID(SpaceID other) {
		if (this.id == null) {
			if (other.id != null) {
				return false;
			}
		} else if (!this.id.equals(other.id)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "SpaceID [id=" + this.id //$NON-NLS-1$
				+ ", contextID=" + this.contextID //$NON-NLS-1$
				+ ", spaceSpec=" + this.spaceSpec //$NON-NLS-1$
				+ "]"; //$NON-NLS-1$
	}

	@Override
	public int compareTo(SpaceID o) {
		int cmp = this.contextID.compareTo(o.contextID);
		if (cmp != 0) {
			return cmp;
		}
		return this.id.compareTo(o.id);
	}

}
