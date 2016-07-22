/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject.kernel.services.jdk.network;

import io.janusproject.services.network.AbstractEventEncrypter;
import io.janusproject.services.network.EventEncrypter;
import io.janusproject.services.network.EventEnvelope;

/**
 * A utility implementation of the {@link EventEncrypter} that does not apply any encryption algorithm.
 *
 * <p>
 * The main use of the class should be development to be able to easy see what's being transfered on the wire.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class PlainTextEventEncrypter extends AbstractEventEncrypter {

	@Override
	public void encrypt(EventEnvelope envelope) {
		assert (envelope != null) : "Parameter 'envelope' must not be null"; //$NON-NLS-1$
		// Do nothing.
	}

	@Override
	public void decrypt(EventEnvelope envelope) {
		assert (envelope != null) : "Parameter 'envelope' must not be null"; //$NON-NLS-1$
		// Do nothing.
	}

}
