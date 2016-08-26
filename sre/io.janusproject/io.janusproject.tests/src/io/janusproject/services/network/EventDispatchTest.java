/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.janusproject.services.network;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

import java.util.HashMap;
import java.util.Map;

import io.janusproject.testutils.AbstractJanusTest;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import io.sarl.lang.core.Event;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.SpaceID;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class EventDispatchTest extends AbstractJanusTest {

	@Nullable
	private SpaceID spaceId;

	@Nullable
	private Event event;

	@Nullable
	private Scope<?> scope;

	@Nullable
	private Map<String, String> headers;

	@Nullable
	private EventDispatch dispatch;

	@Before
	public void setUp() {
		this.spaceId = Mockito.mock(SpaceID.class);
		this.event = Mockito.mock(Event.class);
		this.scope = Mockito.mock(Scope.class);
		this.headers = new HashMap<>();
		this.dispatch = new EventDispatch(this.spaceId, this.event, this.scope, this.headers);
	}

	@Test
	public void getEvent() {
		assertSame(this.event, this.dispatch.getEvent());
	}

	@Test
	public void getScope() {
		assertSame(this.scope, this.dispatch.getScope());
	}

	@Test
	public void getCustomHeaders() {
		assertSame(this.headers, this.dispatch.getCustomHeaders());
	}

	@Test
	public void changeCustomHeaders() {
		this.dispatch.getCustomHeaders().put("xx-test-xx", "xx-value-xx"); //$NON-NLS-1$ //$NON-NLS-2$
		assertEquals("xx-value-xx", this.dispatch.getCustomHeaders().get("xx-test-xx")); //$NON-NLS-1$ //$NON-NLS-2$
		assertEquals("xx-value-xx", this.headers.get("xx-test-xx")); //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Test
	public void getSpaceID() {
		assertSame(this.spaceId, this.dispatch.getSpaceID());
	}

}
