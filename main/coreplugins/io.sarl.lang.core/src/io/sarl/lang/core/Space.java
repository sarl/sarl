/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.util.SynchronizedSet;

/**
 * Agents in SARL can interact only via Interaction Spaces. A space is the support of the event-driven interaction between agents respecting the rules
 * defined in various Space Specifications.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface Space {

    /**
     * Replies the Identification of this Interaction {@link Space}.
     *
     * @return the space's id
     * @deprecated replaced by {@link #getSpaceID()}, to increase readability in the SARL code
     */
    @Pure
    @Deprecated
    @Inline("getSpaceID()")
    default SpaceID getID() {
    	return getSpaceID();
    }

    /**
     * Replies the Identification of this Interaction {@link Space}.
     *
     * @return the space's id
     */
    @Pure
    SpaceID getSpaceID();

    /**
     * Returns the IDs of all agents interacting in this space all over the network.
     *
     * @return participants IDs
     */
    @Pure
    SynchronizedSet<UUID> getParticipants();

}
