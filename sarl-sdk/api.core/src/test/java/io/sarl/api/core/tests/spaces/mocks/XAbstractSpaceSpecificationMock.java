/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
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

package io.sarl.api.core.tests.spaces.mocks;

import org.eclipse.xtext.xbase.lib.Functions.Function1;

import io.sarl.api.core.spaces.AbstractSpaceSpecification;
import io.sarl.api.core.spaces.SpaceComponentFactory;
import io.sarl.api.core.spaces.SpaceParticipantListenerFactory;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.SpaceID;
import jakarta.inject.Provider;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public final class XAbstractSpaceSpecificationMock extends AbstractSpaceSpecification<EventSpace> {

	private final Function1<SpaceID, EventSpace> callback;
	
	public XAbstractSpaceSpecificationMock(Provider<SpaceComponentFactory> spaceComponentFactoryProvider,
			Provider<SpaceParticipantListenerFactory> spaceParticipantListenerFactoryProvider, Function1<SpaceID, EventSpace> callback) {
		super(spaceComponentFactoryProvider, spaceParticipantListenerFactoryProvider);
		this.callback = callback;
	}

	@Override
	public EventSpace create(SpaceID id, Object... params) {
		return this.callback.apply(id);
	}
	
}
