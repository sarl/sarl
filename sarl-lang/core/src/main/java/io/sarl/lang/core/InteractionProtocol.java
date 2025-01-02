/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

/**
 * Agents in SARL can interact following an interaction protocol.
 * A protocol is the support of the interaction between agents respecting the rules
 * defined in the protocol.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public abstract class InteractionProtocol implements Serializable {

	private static final long serialVersionUID = -8579052625862523434L;

	/** Constructs an interaction protolwithout source.
	 * The source must be set with {@link #setSource(Address)}
	 * by the creator of the event, or by the sending mechanism,
	 * before sending the event on the event bus.
	 */
	public InteractionProtocol() {
		//
	}

	@Override
	@Pure
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj != null && getClass().equals(obj.getClass())) {
			return true;
		}
		return false;
	}

	@Override
	@Pure
	public int hashCode() {
		var hash = 31 + getClass().hashCode();
		return hash;
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
	}

}
