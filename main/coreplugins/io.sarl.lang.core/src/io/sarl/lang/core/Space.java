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

import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Agents in SARL can interact only via Interaction Spaces.
 * A space is the support of the event-driven interaction between agents respecting the rules
 * defined in various Space Specifications.
 *
 * <p>Participants to the space are software entities, e.g. agents that are participating
 * to the interaction in the space.
 * Two types of participant are forseen:<ul>
 * <li><i>strong participant</i>: this is the standard or regular type. If the space has a
 * strong participant, it is considered as an not empty space and cannot be destroyed from the system.</li>
 * <li><i>weak participant</i>: this is a special type. If the space has only weak participants,
 * i.e. no strong participant is involved, it is considered as en empty space and could be destroy from the system.</li>
 * <li></li>
 * </ul>
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface Space {

	/**
	 * Replies the Identification of this Interaction {@link Space}.
	 *
	 * @return the space's id
	 */
	@Pure
	SpaceID getSpaceID();

	/**
	 * Replies if the space could be considered as empty.
	 * Usually an empty space is a space without relevant participant, i.e. weak participants are
	 * ignored by this function.
	 *
	 * @return {@code true} if the space could be considered as empty.
	 * @since 0.11
	 */
	@Pure
	@Inline(value = "($0getNumberOfStrongParticipants() == 0)", constantExpression = true)
	default boolean isPseudoEmpty() {
		return getNumberOfStrongParticipants() == 0;
	}

	/**
	 * Replies if the space is empty or the given identifier is associated to the only one participant to the space.
	 * Weak participants are ignored by this function.
	 *
	 * @param id the identifier to test.
	 * @return {@code true} if space if empty or the identifier is associated to the only one participant.
	 * @since 0.11
	 */
	@Pure
	boolean isPseudoEmpty(UUID id);

	/**
	 * Replies the number of strong participants to the space.
	 * This function ignores the weak participants.
	 *
	 * @return the number of participants.
	 * @since 0.11
	 */
	@Pure
	int getNumberOfStrongParticipants();

	/**
	 * Replies the number of strong participants to the space.
	 * This function ignores the weak participants.
	 *
	 * @return the number of participants.
	 * @since 0.11
	 */
	@Pure
	int getNumberOfWeakParticipants();

	/** Apply the given procedure to each of the strong participants.
	 * This function ignores the weak participants.
	 *
	 * @param callback the lambda to invoke.
	 * @since 0.11
	 */
	void forEachStrongParticipant(Procedure1<? super UUID> callback);

	/** Apply the given procedure to each of the weak participants.
	 * This function ignores the strong participants.
	 *
	 * @param callback the lambda to invoke.
	 * @since 0.11
	 */
	void forEachWeakParticipant(Procedure1<? super UUID> callback);

}
