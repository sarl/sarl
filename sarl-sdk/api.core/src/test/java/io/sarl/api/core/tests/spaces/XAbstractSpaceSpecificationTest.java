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

package io.sarl.api.core.tests.spaces;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

import java.util.logging.Logger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.sarl.api.core.spaces.AbstractSpaceSpecification;
import io.sarl.api.core.spaces.OpenEventSpace;
import io.sarl.api.core.spaces.SpaceComponentFactory;
import io.sarl.api.core.spaces.SpaceParticipantListenerFactory;
import io.sarl.api.core.tests.spaces.mocks.XAbstractSpaceSpecificationMock;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.tests.api.extensions.JavaVersionCheckExtension;
import io.sarl.tests.api.extensions.ContextInitExtension;
import io.sarl.tests.api.extensions.PropertyRestoreExtension;

/** 
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version api.core 0.15.1 20250911-224825
 * @mavengroupid io.sarl.sdk
 * @mavenartifactid api.core
 */
@ExtendWith({
	ContextInitExtension.class,
	JavaVersionCheckExtension.class,
	PropertyRestoreExtension.class
})
@DisplayName("AbstractSpaceSpecification")
@Tag("unit")
@Tag("api")
@SuppressWarnings("all")
public class XAbstractSpaceSpecificationTest {

	private SpaceComponentFactory spaceComponentFactory;

	private SpaceParticipantListenerFactory spaceParticipantListenerFactory;
	
	private EventSpace space;
	
	private AbstractSpaceSpecification<EventSpace> test;
	
	@BeforeEach
	public void setUp() {
		this.spaceComponentFactory = mock(SpaceComponentFactory.class);
		this.spaceParticipantListenerFactory = mock(SpaceParticipantListenerFactory.class);
		this.space = mock(EventSpace.class);
		this.test = new XAbstractSpaceSpecificationMock(() -> this.spaceComponentFactory, () -> this.spaceParticipantListenerFactory, it -> this.space);
	}

	@DisplayName("getSpaceComponentFactory")
	@Test
	public void getSpaceComponentFactory() {
		assertSame(this.spaceComponentFactory, this.test.getSpaceComponentFactory());
	}

	@DisplayName("setSpaceComponentFactory(null)")
	@Test
	public void setSpaceComponentFactory_null() {
		this.test.setSpaceComponentFactory(null);
		// Factory is created on the fly by the provider
		assertSame(this.spaceComponentFactory, this.test.getSpaceComponentFactory());
	}

	@DisplayName("setSpaceComponentFactory")
	@Test
	public void setSpaceComponentFactory() {
		var m = mock(SpaceComponentFactory.class);
		this.test.setSpaceComponentFactory(m);
		assertSame(m, this.test.getSpaceComponentFactory());
	}

	@DisplayName("setSpaceComponentFactoryProvider(null)")
	@Test
	public void setSpaceComponentFactoryProvider_null() {
		this.test.setSpaceComponentFactoryProvider(null);
		assertNull(this.test.getSpaceComponentFactory());
	}

	@DisplayName("setSpaceComponentFactoryProvider")
	@Test
	public void setSpaceComponentFactoryProvider() {
		var m = mock(SpaceComponentFactory.class);
		this.test.setSpaceComponentFactoryProvider(() -> m);
		assertSame(m, this.test.getSpaceComponentFactory());
	}

	@DisplayName("getSpaceParticipantListenerFactory")
	@Test
	public void getSpaceParticipantListenerFactory() {
		assertSame(this.spaceParticipantListenerFactory, this.test.getSpaceParticipantListenerFactory());
	}

	@DisplayName("setSpaceParticipantListenerFactory(null)")
	@Test
	public void setSpaceParticipantListenerFactory_null() {
		this.test.setSpaceParticipantListenerFactory(null);
		// Factory is created on the fly by the provider
		assertSame(this.spaceParticipantListenerFactory, this.test.getSpaceParticipantListenerFactory());
	}

	@DisplayName("setSpaceParticipantListenerFactory")
	@Test
	public void setSpaceParticipantListenerFactory() {
		var m = mock(SpaceParticipantListenerFactory.class);
		this.test.setSpaceParticipantListenerFactory(m);
		assertSame(m, this.test.getSpaceParticipantListenerFactory());
	}

	@DisplayName("setSpaceParticipantListenerFactoryProvider(null)")
	@Test
	public void setSpaceParticipantListenerFactoryProvider_null() {
		this.test.setSpaceParticipantListenerFactoryProvider(null);
		assertNull(this.test.getSpaceParticipantListenerFactory());
	}

	@DisplayName("setSpaceParticipantListenerFactoryProvider")
	@Test
	public void setSpaceParticipantListenerFactoryProvider() {
		var m = mock(SpaceParticipantListenerFactory.class);
		this.test.setSpaceParticipantListenerFactoryProvider(() -> m);
		assertSame(m, this.test.getSpaceParticipantListenerFactory());
	}

	@DisplayName("getDefaultSpace")
	@Test
	public void getDefaultSpace() {
		assertNull(this.test.getDefaultSpace());
	}

	@DisplayName("setDefaultSpace(null)")
	@Test
	public void setDefaultSpace_null() {
		this.test.setDefaultSpace(null);
		assertNull(this.test.getDefaultSpace());

		var s = mock(OpenEventSpace.class);
		this.test.setDefaultSpace(s);
		assertSame(s, this.test.getDefaultSpace());

		this.test.setDefaultSpace(null);
		assertNull(this.test.getDefaultSpace());
	}

	@DisplayName("setDefaultSpace")
	@Test
	public void setDefaultSpace() {
		var s = mock(OpenEventSpace.class);
		this.test.setDefaultSpace(s);
		assertSame(s, this.test.getDefaultSpace());
	}

	@DisplayName("setDefaultSpaceProvider(null)")
	@Test
	public void setDefaultSpaceProvider_null() {
		this.test.setDefaultSpaceProvider(null);
		assertNull(this.test.getDefaultSpace());

		var s = mock(OpenEventSpace.class);
		this.test.setDefaultSpaceProvider(() -> s);
		assertSame(s, this.test.getDefaultSpace());

		this.test.setDefaultSpaceProvider(null);
		assertSame(s, this.test.getDefaultSpace());
	}

	@DisplayName("setDefaultSpaceProvider")
	@Test
	public void setDefaultSpaceProvider() {
		var s = mock(OpenEventSpace.class);
		this.test.setDefaultSpaceProvider(() -> s);
		assertSame(s, this.test.getDefaultSpace());
	}

	@DisplayName("getLoggerProvider")
	@Test
	public void getLoggerProvider() {
		assertNotNull(this.test.getLoggerProvider());
	}

	@DisplayName("setLoggerProvider(null)")
	@Test
	public void setLoggerProvider_null() {
		this.test.setLoggerProvider(null);
		assertNotNull(this.test.getLoggerProvider());

		var s = mock(Logger.class);
		this.test.setLoggerProvider(id -> s);
		assertNotNull(this.test.getLoggerProvider());
		assertSame(s, this.test.getLoggerProvider().apply(mock(SpaceID.class)));

		this.test.setLoggerProvider(null);
		assertNotNull(this.test.getLoggerProvider());
	}

	@DisplayName("setLoggerProvider")
	@Test
	public void setLoggerProvider() {
		var s = mock(Logger.class);
		this.test.setLoggerProvider(id -> s);
		assertNotNull(this.test.getLoggerProvider());
		assertSame(s, this.test.getLoggerProvider().apply(mock(SpaceID.class)));
	}

}
