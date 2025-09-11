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

import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

/** Elementary interaction unit inside an {@link EventSpace} An event is the
 * specification of some occurrence in a Space that may potentially trigger
 * effects by a listener. Within a Space, the notion of {@link Scope} enables to
 * precisely control/filter the potential recipients of an event.
 *
 * @author <a href="http://www.sebastianrodriguez.com.ar/">Sebastian Rodriguez</a>
 * @version core 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 */
public abstract class Event implements Serializable {

	private static final long serialVersionUID = -4314428111329600890L;

	/** Sender of the event.
	 */
	private Address source;

	/** Constructs an Event without source.
	 * The source must be set with {@link #setSource(Address)}
	 * by the creator of the event, or by the sending mechanism,
	 * before sending the event on the event bus.
	 */
	public Event() {
		//
	}

	/** Constructs an Event with a source.
	 * @param source source of the event.
	 */
	public Event(Address source) {
		this.source = source;
	}

	@Override
	@Pure
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj != null && getClass().equals(obj.getClass())) {
			final var event = (Event) obj;
			return (this.source == null && event.source == null)
					|| (this.source != null && this.source.equals(event.source));
		}
		return false;
	}

	@Override
	@Pure
	public int hashCode() {
		var hash = 31 + getClass().hashCode();
		if (this.source != null) {
			hash = hash * 31 + this.source.hashCode();
		}
		return hash;
	}

	/**
	 * The source of the event.
	 *
	 * @return the source of the event.
	 */
	@Pure
	public Address getSource() {
		return this.source;
	}

	/**
	 * The address of the source of this event.
	 *
	 * @param source the source of the event.
	 */
	public void setSource(Address source) {
		this.source = source;
	}

	@Override
	@Pure
	public final String toString() {
		final var builder = new ToStringBuilder(this);
		toString(builder);
		return builder.toString();
	}

	/** fill the given builder with the string representation of this object.
	 *
	 * @param builder the string builder.
	 * @since 0.7
	 */
	@Pure
	protected void toString(ToStringBuilder builder) {
		builder.add("type", getClass().getSimpleName()); //$NON-NLS-1$
		builder.add("source", this.source); //$NON-NLS-1$
	}

	/** Replies if the event was emitted by an entity with the given address.
	 *
	 * @param address the address of the emitter to test.
	 * @return {@code true} if the given event was emitted by
	 *     an entity with the given address; otherwise {@code false}.
	 * @since 0.2
	 */
	@Pure
	public boolean isFrom(Address address) {
		return (address != null) && address.equals(getSource());
	}

	/** Replies if the event was emitted by an entity with the given identifier.
	 *
	 * @param entityId the identifier of the emitter to test.
	 * @return {@code true} if the given event was emitted by
	 *     an entity with the given identifier; otherwise {@code false}.
	 * @since 0.2
	 */
	@Pure
	public boolean isFrom(UUID entityId) {
		final var iSource = getSource();
		return (entityId != null) && (iSource != null)
				&& entityId.equals(iSource.getID());
	}

	/** Types of generic bound.
	 * The upper bounds corresponds to the type that a generic type must inherit.
	 * The lower bounds corresponds to the that that a generic type must be a super type.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version core 0.15.1 20250911-224823
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid core
	 * @since 0.14
	 */
	public enum BoundType {

		/** Upper bound, usually corresponds to an {@code extends}.
		 */
		UPPER_BOUND,

		/** Lower bound, usually corresponds to a {@code super}.
		 */
		LOWER_BOUND

	}

	/** Definition of a bound for generic types.
	 *
	 * @param type the type associated to this generic type bound.
	 * @param direction the type of bound for the record.
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version core 0.15.1 20250911-224823
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid core
	 * @since 0.14
	 */
	public record Bound(Class<?> type, BoundType direction)  {

		/** Creation a bound with the given type and direction.
		 * If the arguments are {@code null}, then the default values are considered (see the documentation of each argument).
		 *
		 * @param type the type associated to this generic type bound. If it is {@code null}, {@code Object} is assumed.
		 * @param direction the type of bound for the record. If it is {@code null}, {@link BoundType#UPPER_BOUND} is assumed.
		 * @return the bound.
		 */
		public static Bound of(Class<?> type, BoundType direction) {
			final var t = type == null ? Object.class : type;
			final var d = direction == null ? BoundType.UPPER_BOUND : direction;
			return new Bound(t, d);
		}

	}

}
