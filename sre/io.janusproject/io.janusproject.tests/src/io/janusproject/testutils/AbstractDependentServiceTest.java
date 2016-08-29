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
package io.janusproject.testutils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import io.janusproject.services.DependentService;

import org.junit.Assume;
import org.junit.Test;

import com.google.common.util.concurrent.Service;

/**
 * Abstract class that permits to test the implementation of a service.
 *
 * @param <S> - the type of the service.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public abstract class AbstractDependentServiceTest<S extends DependentService> extends AbstractServiceTest<S> {

	/**
	 * Type of the testing service.
	 */
	protected final Class<? super S> serviceType;

	/**
	 * @param type - the type of the service to test.
	 */
	public AbstractDependentServiceTest(Class<? super S> type) {
		Assume.assumeNotNull(type);
		this.serviceType = type;
	}

	@AvoidServiceStartForTest
	@Test
	public void getServiceType() {
		assertNotNull(this.service);
		Class<? extends Service> servType = this.service.getServiceType();
		assertNotNull(servType);
		assertTrue(servType.isInterface());
		assertEquals(this.serviceType, servType);
	}

	/**
	 * Test the dependencies.
	 * 
	 * The weak dependencies are defined in {@link DependentService#getServiceDependencies()}.
	 */
	@AvoidServiceStartForTest
	@Test
	public abstract void getServiceDependencies();

	/**
	 * Test the weak dependencies.
	 * 
	 * The weak dependencies are defined in {@link DependentService#getServiceWeakDependencies()}.
	 */
	@AvoidServiceStartForTest
	@Test
	public abstract void getServiceWeakDependencies();

}
