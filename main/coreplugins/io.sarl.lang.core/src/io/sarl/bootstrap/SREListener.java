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

package io.sarl.bootstrap;

import java.util.EventListener;

/**
 * Observer on SRE events.
 *
 * <p>This observer will be notified depending on the the implementation of the SRE.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public interface SREListener extends EventListener {

	/** Invoked when the SRE is started.
	 *
	 * @param sre the started SRE.
	 */
	void sreStarted(SREBootstrap sre);

	/** Invoked when the SRE is stopped.
	 *
	 * @param sre the stopped SRE.
	 */
	void sreStopped(SREBootstrap sre);

}
