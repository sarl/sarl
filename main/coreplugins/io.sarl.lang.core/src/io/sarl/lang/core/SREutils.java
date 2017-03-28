/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import org.eclipse.xtext.xbase.lib.Pure;

/** Utilities for accessing to the part of the SARL API that is dedicated to the
 * SARL run-time environments (SRE).
 *
 * <p>These utilities are provided for enabling the implementation of high-performance
 * SRE by giving storage area into the instances of SARL concepts.
 *
 * <p>The data that is set and get by the functions in this utility class are SRE-dependent.
 * The instance type of the data is known only by the SRE.
 *
 * <p>Note that any SRE-specific data stored into a SARL object has the following properties: <ul>
 * <li>it is transient, i.e. it is not serialized;</li>
 * <li>it is used in the implementation of the {@link Object#equals(Object)} function;</li>
 * <li>it is used in the implementation of the {@link Object#hashCode()} function;</li>
 * <li>it is used in the implementation of the {@link Object#toString()} function.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
public final class SREutils {

	private SREutils() {
		//
	}

	/** Replies the data associated to this agent trait by the SRE.
	 *
	 * @param <S> the type of the data.
	 * @param type the type of the data.
	 * @param trait the trait.
	 * @return the SRE-specific data.
	 */
	@Pure
	public static <S> S getSreSpecificData(AgentTrait trait, Class<S> type) {
		assert trait != null;
		return trait.getSreSpecificData(type);
	}

	/** Change the data associated to this agent trait by the SRE.
	 *
	 * @param trait the trait.
	 * @param data the SRE-specific data.
	 */
	public static void setSreSpecificData(AgentTrait trait, Object data) {
		trait.setSreSpecificData(data);
	}

}
