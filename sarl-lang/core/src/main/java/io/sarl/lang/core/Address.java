/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

/**
 * This class describes all the addresses used by the space to identify its
 * participants.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version core 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 */
public class Address extends SRESpecificDataContainer implements Serializable, Comparable<Address>, Cloneable {

	private static final long serialVersionUID = 1519844913685586094L;

	/** Identifier of the participant.
	 */
	private final UUID participantId;

	/** Identifier of the interaction space.
	 */
	private final SpaceID spaceId;

	/**
	 * Create an address based on the specified identifier.
	 *
	 * @param spaceId the identifier of the space related to this address.
	 * @param participantId is the identifier of the participant.
	 */
	public Address(SpaceID spaceId, UUID participantId) {
		assert participantId != null;
		assert spaceId != null;
		this.participantId = participantId;
		this.spaceId = spaceId;
	}

	@Override
	@Pure
	public Address clone() {
		try {
			return (Address) super.clone();
		} catch (Throwable exception) {
			throw new Error(exception);
		}
	}

	@Override
	@Pure
	public String toString() {
		final var builder = new ToStringBuilder(this);
		builder.add("type", getClass().getSimpleName()); //$NON-NLS-1$
		builder.add("participantId", this.participantId); //$NON-NLS-1$
		builder.add("spaceId", this.spaceId); //$NON-NLS-1$
		return builder.toString();
	}

	/**
	 * Replies the participant identifier associated to this address.
	 *
	 * @return the participant identifier associated to this address.
	 * @deprecated since 0.12, see {@link #getID()}
	 */
	@Deprecated(forRemoval = true, since = "0.12")
	@Pure
	@Inline("getID()")
	public UUID getUUID() {
		return getID();
	}

	/**
	 * Replies the participant identifier associated to this address.
	 *
	 * @return the participant identifier associated to this address.
	 * @since 0.12
	 */
	@Pure
	public UUID getID() {
		return this.participantId;
	}

	@Override
	@Pure
	public int hashCode() {
		var result = 1;
		result = 31 * result + ((this.participantId == null) ? 0 : this.participantId.hashCode());
		result = 31 * result + ((this.spaceId == null) ? 0 : this.spaceId.hashCode());
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

		if (obj instanceof Address cvalue) {
			return equals(cvalue);
		}
		return false;
	}

	/**
	 * Test if this address and the given one are equal.
	 *
	 * @param address is the address to be compared.
	 * @return {@code true} if this address and the given one are equal,
	 *         otherwise {@code false}
	 */
	@Pure
	public boolean equals(Address address) {
		return address != null && this.participantId.equals(address.getID())
				&& this.spaceId.equals(address.getSpaceID());
	}

	/**
	 * Compares this object with the specified object for order. Returns a
	 * negative integer, zero, or a positive integer as this object is less
	 * than, equal to, or greater than the specified object.
	 *
	 * <p>The implementor must ensure {@code sgn(x.compareTo(y)) ==
	 * -sgn(y.compareTo(x))} for all {@code x} and {@code y}. (This
	 * implies that {@code x.compareTo(y)} must throw an exception iff
	 * {@code y.compareTo(x)} throws an exception.)
	 *
	 * <p>The implementor must also ensure that the relation is transitive:
	 * {@code (x.compareTo(y)&gt;0 &amp;&amp; y.compareTo(z)&gt;0)} implies
	 * {@code x.compareTo(z)&gt;0}.
	 *
	 * <p>Finally, the implementor must ensure that {@code x.compareTo(y)==0}
	 * implies that {@code sgn(x.compareTo(z)) == sgn(y.compareTo(z))}, for all
	 * {@code z}.
	 *
	 * <p>It is strongly recommended, but <i>not</i> strictly required that
	 * {@code (x.compareTo(y)==0) == (x.equals(y))}. Generally speaking, any
	 * class that implements the {@code Comparable} interface and violates this
	 * condition should clearly indicate this fact. The recommended language is
	 * "Note: this class has a natural ordering that is inconsistent with
	 * equals."
	 *
	 * <p>In the foregoing description, the notation {@code sgn(}<i>expression</i>
	 * {@code )} designates the mathematical <i>signum</i> function, which is
	 * defined to return one of {@code -1}, {@code 0}, or {@code 1} according
	 * to whether the value of <i>expression</i> is negative, zero or positive.
	 *
	 * @param address is the address to be compared.
	 * @return a negative integer, zero, or a positive integer as this object is
	 *         less than, equal to, or greater than the specified object.
	 */
	@Override
	@Pure
	public int compareTo(Address address) {
		if (address == null) {
			return 1;
		}
		return this.participantId.compareTo(address.getID());
	}

	/** Replies the ID of the space related to this address.
	 *
	 * @return the space ID.
	 * @since 0.7
	 */
	@Pure
	public SpaceID getSpaceID() {
		return this.spaceId;
	}

	/** Replies the ID of the space related to this address.
	 *
	 * @return the space ID.
	 * @deprecated since 0.7, see {@link #getSpaceID()} for replacement.
	 */
	@Pure
	@Deprecated(forRemoval = true, since = "0.7")
	public SpaceID getSpaceId() {
		return getSpaceID();
	}

	/** Replies if the identifier in this address is equal to the given identifier.
	 * It is equivalent to {@code getID().equals(id)}.
	 *
	 * @param id the identifier to test.
	 * @return {@code true} if the identifier is equal to the address' identifier.
	 * @since 0.14
	 */
	@Pure
	@Inline(value = "(($1) != null && ($1).equals($0getID()))", constantExpression = true)
	public boolean operator_equals(UUID id) {
		return id != null && id.equals(getID());
	}

	/** Replies if the identifier in this address is equal to the given identifier.
	 * It is equivalent to {@code getID().equals(id)}.
	 *
	 * @param id the identifier to test.
	 * @return {@code true} if the identifier is equal to the address' identifier.
	 * @since 0.14
	 */
	@Pure
	@Inline(value = "(($1) != null && ($1).equals($0getSpaceID()))", constantExpression = true)
	public boolean operator_equals(SpaceID id) {
		return id != null && id.equals(getSpaceID());
	}


	/** Replies if the identifier in this address is not equal to the given identifier.
	 * It is equivalent to {@code !getID().equals(id)}.
	 *
	 * @param id the identifier to test.
	 * @return {@code false} if the identifier is equal to the address' identifier.
	 * @since 0.14
	 */
	@Pure
	@Inline(value = "(($1) == null || !($1).equals($0getID()))", constantExpression = true)
	public boolean operator_notEquals(UUID id) {
		return id == null || !id.equals(getID());
	}

	/** Replies if the identifier in this address is not equal to the given identifier.
	 * It is equivalent to {@code !getID().equals(id)}.
	 *
	 * @param id the identifier to test.
	 * @return {@code false} if the identifier is equal to the address' identifier.
	 * @since 0.14
	 */
	@Pure
	@Inline(value = "(($1) == null || !($1).equals($0getSpaceID()))", constantExpression = true)
	public boolean operator_notEquals(SpaceID id) {
		return id == null || !id.equals(getSpaceID());
	}

}
