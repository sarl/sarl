/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

import static io.sarl.tests.api.tools.TestMockito.mock;
import static org.mockito.Mockito.when;

import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;
import java.util.logging.Logger;

import io.sarl.api.core.spaces.AbstractEventSpace;
import io.sarl.api.core.spaces.Participant;
import io.sarl.api.core.spaces.SpaceParticipantListener;
import io.sarl.lang.core.SpaceID;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public final class XAbstractEventSpaceMock extends AbstractEventSpace {
	
	public XAbstractEventSpaceMock(ConcurrentHashMap<UUID, Participant> participants1,
			ConcurrentHashMap<UUID, Participant> participants2, SpaceID spaceId) {
		super(spaceId, mock(SpaceParticipantListener.class), createLoggerMock(),
			participants1, participants2);
	}

	public static Supplier<Logger> createLoggerMock() {
		var loggerSupplier = mock(Supplier.class);
		var logger = mock(Logger.class);
		when(loggerSupplier.get()).thenReturn(logger);
		return loggerSupplier;
	}
	
}
