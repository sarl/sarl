/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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
 * Unique Identifier for a {@link Space}
 * 
 * The {@link SpaceID}
 * 
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */

public class SpaceID implements Serializable, Comparable<SpaceID>{

	/**
	 * 
	 */
	private static final long serialVersionUID = 8764568066583474825L;
	
	private final UUID id;
	private final UUID contextID;
	private final transient Class<? extends SpaceSpecification> spaceSpec;

	/**
	 * @param contextID
	 * @param id
	 * @param spaceSpec
	 */
	public SpaceID(UUID contextID, UUID id, Class<? extends SpaceSpecification> spaceSpec) {
		this.id = id;
		this.contextID = contextID;
		this.spaceSpec = spaceSpec;
	}

	/**
	 * Returns a Unique Identifier for the space
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
	public Class<? extends SpaceSpecification> getSpaceSpecification() {
		return this.spaceSpec;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.contextID == null) ? 0 : this.contextID.hashCode());
		result = prime * result + ((this.id == null) ? 0 : this.id.hashCode());
		return result;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		SpaceID other = (SpaceID) obj;
		if (this.contextID == null) {
			if (other.contextID != null)
				return false;
		} else if (!this.contextID.equals(other.contextID))
			return false;
		if (this.id == null) {
			if (other.id != null)
				return false;
		} else if (!this.id.equals(other.id))
			return false;
		return true;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String toString() {
		return "SpaceID [id=" + this.id + ", contextID=" + this.contextID + ", spaceSpec=" + this.spaceSpec + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	}

	@Override
	public int compareTo(SpaceID o) {
		return this.id.compareTo(o.getID());
	}

}
