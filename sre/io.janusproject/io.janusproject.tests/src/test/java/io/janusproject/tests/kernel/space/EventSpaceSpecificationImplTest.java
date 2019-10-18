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
package io.janusproject.tests.kernel.space;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Comparator;
import java.util.UUID;
import java.util.concurrent.locks.ReadWriteLock;

import com.google.inject.Injector;
import com.google.inject.Provider;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.janusproject.kernel.space.EventSpaceSpecificationImpl;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.distributeddata.DMap;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.lang.core.SpaceID;
import io.sarl.tests.api.ManualMocking;
import io.sarl.tests.api.Nullable;
import io.sarl.util.concurrent.NoReadWriteLock;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@ManualMocking
public class EventSpaceSpecificationImplTest extends AbstractJanusTest {

	@Nullable
	private SpaceID spaceId;

	@Mock
	private DistributedDataStructureService structureFactory;

	@Mock
	private Injector injector;

	@InjectMocks
	private EventSpaceSpecificationImpl specification;

	@Mock
	private ContextSpaceService contextSpaceService;

	@Before
	public void setUp() {
		this.spaceId = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), EventSpaceSpecification.class);
		MockitoAnnotations.initMocks(this);
		when(this.injector.getInstance(any(Class.class))).thenAnswer((it) -> {
			Object obj = null;
			if (DistributedDataStructureService.class.equals(it.getArgument(0))) {
				obj = this.structureFactory;
			} else if (ContextSpaceService.class.equals(it.getArgument(0))) {
				obj = this.contextSpaceService;
			}
			return obj;
		});
		when(this.injector.getProvider(any(Class.class))).thenAnswer((it) -> {
			Provider<?> provider = null;
			if (ReadWriteLock.class.equals(it.getArgument(0))) {
				provider = () -> NoReadWriteLock.SINGLETON;
			}
			return provider;
		});
		DMap<Object, Object> mapMock = mock(DMap.class);
		when(this.structureFactory.getMap(anyString(), any(Comparator.class))).thenReturn(mapMock);
		when(this.structureFactory.getMap(anyString())).thenReturn(mapMock);
	}

	@Test
	public void create() {
		EventSpace space = this.specification.create(this.spaceId, "a", "b", "c"); //$NON-NLS-1$ //$NON-NLS-2$//$NON-NLS-3$
		assertNotNull(space);
		assertSame(this.spaceId, space.getSpaceID());
	}

}
