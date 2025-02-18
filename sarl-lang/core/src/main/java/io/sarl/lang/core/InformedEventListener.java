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

import io.sarl.lang.core.annotation.PrivateAPI;

/** 
 * Entity capable listening to events inside an Interaction {@link Space}.
 * 
 * <p>{@link Space}s in SARL are event driven. Most of the time,
 * {@link EventListener}s in a {@link Space} will be {@link Agent}s. However,
 * any entity implementing the {@link EventListener} can interact inside an
 * {@link EventSpace} (e.g. UI)
 *
 * <p>This specific listening entity knows its owner and provides access to it.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10 in Janus SRE
 * @since 0.15 in SARL lang core
 */
public interface InformedEventListener extends EventListener {

	/** Replies the owner of this listener.
	 *
	 * <p>This function is part of the private API and should not be invoked
	 * outside the implementation of the SRE.
	 *
	 * @return the agent instance.
	 */
	@PrivateAPI
	Agent getOwnerInstance();

}
