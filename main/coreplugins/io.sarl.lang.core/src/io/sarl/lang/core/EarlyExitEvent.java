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


/** Event describing an early exit from a block of code when it
 * is fired by a SARL action.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @deprecated no replacement.
 */
@Deprecated
public abstract class EarlyExitEvent extends Event {

	private static final long serialVersionUID = -5601857950368054559L;

	/** Constructs an EarlyExitEvent without source.
	 * The source must be set with {@link #setSource(Address)}
	 * by the creator of the event, or by the emitting mechanism,
	 * before sending the event on the event bus.
	 */
	public EarlyExitEvent() {
		//
	}

	/** Constructs an EarlyExitEvent with a source.
	 * @param source source of the event.
	 */
	public EarlyExitEvent(Address source) {
		super(source);
	}

}
