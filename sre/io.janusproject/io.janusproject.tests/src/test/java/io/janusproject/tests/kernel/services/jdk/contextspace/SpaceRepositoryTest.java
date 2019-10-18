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
package io.janusproject.tests.kernel.services.jdk.contextspace;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.UUID;
import java.util.concurrent.locks.ReadWriteLock;

import com.google.inject.Injector;
import com.google.inject.Provider;
import javassist.Modifier;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.internal.verification.Times;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import io.janusproject.kernel.services.jdk.contextspace.SpaceRepository;
import io.janusproject.services.contextspace.SpaceRepositoryListener;
import io.janusproject.services.distributeddata.DMap;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.tests.testutils.AbstractJanusTest;
import io.janusproject.util.TwoStepConstruction;

import io.sarl.core.OpenEventSpace;
import io.sarl.core.OpenEventSpaceSpecification;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.tests.api.Nullable;
import io.sarl.util.RestrictedAccessEventSpace;
import io.sarl.util.RestrictedAccessEventSpaceSpecification;
import io.sarl.util.concurrent.Collections3;
import io.sarl.util.concurrent.NoReadWriteLock;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SpaceRepositoryTest extends AbstractJanusTest {

	@Nullable
	private DMap<SpaceID, Object[]> spaceIDs;

	@Nullable
	private DistributedDataStructureService dds;

	@Nullable
	private Injector injector;

	@Nullable
	private SpaceRepositoryListener listener;

	@Nullable
	private SpaceRepository repository;

	@Nullable
	private SpaceID spaceID;

	@Nullable
	private Object[] params;

	@Nullable
	private OpenEventSpaceSpecification spaceSpecification;

	@Nullable
	private OpenEventSpace space;

	@Before
	public void setUp() throws Exception {
		this.spaceIDs = mock(DMap.class);
		this.dds = mock(DistributedDataStructureService.class);
		this.injector = mock(Injector.class);
		when(this.injector.getProvider(any(Class.class))).thenAnswer((it) -> {
			Provider<?> provider = null;
			if (ReadWriteLock.class.equals(it.getArgument(0))) {
				provider = () -> NoReadWriteLock.SINGLETON;
			}
			return provider;
		});
		this.listener = mock(SpaceRepositoryListener.class);
		this.spaceID = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class);
		this.params = new Object[] { "PARAM" }; //$NON-NLS-1$
		//
		when(this.dds.<SpaceID, Object[]> getMap(ArgumentMatchers.any(), ArgumentMatchers.any()))
				.thenReturn(this.spaceIDs);
		when(this.dds.<SpaceID, Object[]> getMap(ArgumentMatchers.any())).thenReturn(this.spaceIDs);
		//
		this.repository = this.reflect.newInstance(SpaceRepository.class, "thename", //$NON-NLS-1$
				this.dds, this.injector, this.listener);
	}

	private void initMocks() throws Exception {
		when(this.spaceIDs.containsKey(this.spaceID)).thenReturn(true);
		when(this.spaceIDs.keySet()).thenReturn(new HashSet<>(Collections.singleton(this.spaceID)));
		this.spaceSpecification = mock(OpenEventSpaceSpecification.class);
		when(this.injector.getInstance(OpenEventSpaceSpecification.class)).thenReturn(this.spaceSpecification);
		this.space = mock(OpenEventSpace.class);
		when(this.spaceSpecification.create(this.spaceID, this.params)).thenReturn(this.space);
		when(this.space.getSpaceID()).thenReturn(this.spaceID);
	}

	private void baseInit() throws Exception {
		this.spaceSpecification = mock(OpenEventSpaceSpecification.class);
		when(this.injector.getInstance(OpenEventSpaceSpecification.class)).thenReturn(this.spaceSpecification);
		this.space = mock(OpenEventSpace.class);
		when(this.spaceSpecification.create(this.spaceID, this.params)).thenReturn(this.space);
		when(this.space.getSpaceID()).thenReturn(this.spaceID);
	}

	private void initRepository() throws Exception {
		initMocks();
		this.reflect.invoke(this.repository, "ensureLocalSpaceDefinition", this.spaceID, this.params);
	}

	@Test
	public void ensureSpaceDefinition() throws Exception {
		initMocks();
		//
		this.reflect.invoke(this.repository, "ensureLocalSpaceDefinition", this.spaceID, this.params);
		//
		ArgumentCaptor<SpaceID> argument1 = ArgumentCaptor.forClass(SpaceID.class);
		ArgumentCaptor<Object[]> argument2 = ArgumentCaptor.forClass(Object[].class);
		verify(this.spaceSpecification, new Times(1)).create(argument1.capture(), argument2.capture());
		assertSame(this.spaceID, argument1.getValue());
		assertEquals("PARAM", argument2.getValue()); //$NON-NLS-1$
		assertSame(this.space, this.repository.getSpace(this.spaceID));
		//
		ArgumentCaptor<Space> argument3 = ArgumentCaptor.forClass(Space.class);
		ArgumentCaptor<Boolean> argument4 = ArgumentCaptor.forClass(Boolean.class);
		verify(this.listener, new Times(1)).spaceCreated(argument3.capture(), argument4.capture());
		assertSame(this.space, argument3.getValue());
		assertFalse(argument4.getValue());
	}

	@Test
	public void twoStepConstruction() throws Exception {
		TwoStepConstruction annotation = SpaceRepository.class.getAnnotation(TwoStepConstruction.class);
		assertNotNull(annotation);
		for (String name : annotation.names()) {
			for (Method method : SpaceRepository.class.getMethods()) {
				if (name.equals(method.getName())) {
					assertTrue(Modifier.isPackage(method.getModifiers()) || Modifier.isPublic(method.getModifiers()));
					break;
				}
			}
		}
	}

	@Test
	public void ensureSpaceDefinition_recall() throws Exception {
		initMocks();
		//
		this.reflect.invoke(this.repository, "ensureLocalSpaceDefinition", this.spaceID, this.params);
		//
		this.reflect.invoke(this.repository, "ensureLocalSpaceDefinition", this.spaceID, this.params);
		//
		ArgumentCaptor<SpaceID> argument1 = ArgumentCaptor.forClass(SpaceID.class);
		ArgumentCaptor<Object[]> argument2 = ArgumentCaptor.forClass(Object[].class);
		verify(this.spaceSpecification, new Times(1)).create(argument1.capture(), argument2.capture());
		assertSame(this.spaceID, argument1.getValue());
		assertEquals("PARAM", argument2.getValue()); //$NON-NLS-1$
		assertSame(this.space, this.repository.getSpace(this.spaceID));
		//
		ArgumentCaptor<Space> argument3 = ArgumentCaptor.forClass(Space.class);
		ArgumentCaptor<Boolean> argument4 = ArgumentCaptor.forClass(Boolean.class);
		verify(this.listener, new Times(1)).spaceCreated(argument3.capture(), argument4.capture());
		assertSame(this.space, argument3.getValue());
		assertFalse(argument4.getValue());
	}

	@Test
	public void removeSpaceDefinition() throws Exception {
		initRepository();
		when(this.space.getParticipants()).thenReturn(Collections3.<UUID> emptySynchronizedSet());
		when(this.spaceIDs.containsKey(this.spaceID)).thenReturn(false);
		//
		this.reflect.invoke(this.repository, "removeLocalSpaceDefinition", this.spaceID, true);
		//
		assertNull(this.repository.getSpace(this.spaceID));
		//
		ArgumentCaptor<Space> argument3 = ArgumentCaptor.forClass(Space.class);
		ArgumentCaptor<Boolean> argument4 = ArgumentCaptor.forClass(Boolean.class);
		verify(this.listener, new Times(1)).spaceDestroyed(argument3.capture(), argument4.capture());
		assertSame(this.space, argument3.getValue());
		assertTrue(argument4.getValue());
	}

	@Test
	public void getSpaces() throws Exception {
		initRepository();
		//
		Collection<? extends Space> spaces = this.repository.getSpaces();
		assertNotNull(spaces);
		assertEquals(1, spaces.size());
		assertTrue(spaces.contains(this.space));
	}

	@Test
	public void getSpaceSpaceID() throws Exception {
		initRepository();
		//
		Space space = this.repository.getSpace(this.spaceID);
		assertSame(this.space, space);
		//
		assertNull(this.repository.getSpace(mock(SpaceID.class)));
	}

	@Test
	public void getSpacesClass_EventSpace() throws Exception {
		initRepository();
		//
		Collection<EventSpace> spaces = this.repository.getSpaces(EventSpaceSpecification.class);
		assertNotNull(spaces);
		assertEquals(0, spaces.size());
		assertFalse(spaces.contains(this.space));
	}

	@Test
	public void getSpacesClass_OpenEventSpace() throws Exception {
		initRepository();
		//
		Collection<OpenEventSpace> spaces = this.repository.getSpaces(OpenEventSpaceSpecification.class);
		assertNotNull(spaces);
		assertEquals(1, spaces.size());
		assertTrue(spaces.contains(this.space));
	}

	@Test
	public void getSpacesClass_RestrictedAccessEventSpace() throws Exception {
		initRepository();
		//
		Collection<RestrictedAccessEventSpace> spaces = this.repository.getSpaces(RestrictedAccessEventSpaceSpecification.class);
		assertNotNull(spaces);
		assertEquals(0, spaces.size());
		assertFalse(spaces.contains(this.space));
	}

	@Test
	public void createSpace_singlecreation() throws Exception {
		baseInit();
		//
		OpenEventSpace space = this.repository.createSpace(this.spaceID, OpenEventSpaceSpecification.class, this.params);
		//
		assertSame(this.space, space);
		ArgumentCaptor<SpaceID> argument1 = ArgumentCaptor.forClass(SpaceID.class);
		ArgumentCaptor<Object[]> argument2 = ArgumentCaptor.forClass(Object[].class);
		verify(this.spaceSpecification, new Times(1)).create(argument1.capture(), argument2.capture());
		assertSame(this.spaceID, argument1.getValue());
		assertEquals("PARAM", argument2.getValue()); //$NON-NLS-1$
		assertSame(this.space, this.repository.getSpace(this.spaceID));
		//
		ArgumentCaptor<Space> argument3 = ArgumentCaptor.forClass(Space.class);
		ArgumentCaptor<Boolean> argument4 = ArgumentCaptor.forClass(Boolean.class);
		verify(this.listener, new Times(1)).spaceCreated(argument3.capture(), argument4.capture());
		assertSame(this.space, argument3.getValue());
		assertTrue(argument4.getValue());
	}

	@Test
	public void createSpace_doublecreation() throws Exception {
		baseInit();
		//
		OpenEventSpace space1 = this.repository.createSpace(this.spaceID, OpenEventSpaceSpecification.class, this.params);
		OpenEventSpace space2 = this.repository.createSpace(this.spaceID, OpenEventSpaceSpecification.class, this.params);
		//
		assertSame(this.space, space1);
		assertNull(space2);
		ArgumentCaptor<SpaceID> argument1 = ArgumentCaptor.forClass(SpaceID.class);
		ArgumentCaptor<Object[]> argument2 = ArgumentCaptor.forClass(Object[].class);
		verify(this.spaceSpecification, new Times(1)).create(argument1.capture(), argument2.capture());
		assertSame(this.spaceID, argument1.getValue());
		assertEquals("PARAM", argument2.getValue()); //$NON-NLS-1$
		assertSame(this.space, this.repository.getSpace(this.spaceID));
		//
		ArgumentCaptor<Space> argument3 = ArgumentCaptor.forClass(Space.class);
		ArgumentCaptor<Boolean> argument4 = ArgumentCaptor.forClass(Boolean.class);
		verify(this.listener, new Times(1)).spaceCreated(argument3.capture(), argument4.capture());
		assertSame(this.space, argument3.getValue());
	}

	@Test
	public void getOrCreateSpaceWithSpec_singlecreation() throws Exception {
		baseInit();
		//
		OpenEventSpace space = this.repository.getOrCreateSpaceWithSpec(this.spaceID, OpenEventSpaceSpecification.class,
				this.params);
		//
		assertSame(this.space, space);
		ArgumentCaptor<SpaceID> argument1 = ArgumentCaptor.forClass(SpaceID.class);
		ArgumentCaptor<Object[]> argument2 = ArgumentCaptor.forClass(Object[].class);
		verify(this.spaceSpecification, new Times(1)).create(argument1.capture(), argument2.capture());
		assertSame(this.spaceID, argument1.getValue());
		assertEquals("PARAM", argument2.getValue()); //$NON-NLS-1$
		assertSame(this.space, this.repository.getSpace(this.spaceID));
		//
		ArgumentCaptor<Space> argument3 = ArgumentCaptor.forClass(Space.class);
		ArgumentCaptor<Boolean> argument4 = ArgumentCaptor.forClass(Boolean.class);
		verify(this.listener, new Times(1)).spaceCreated(argument3.capture(), argument4.capture());
		assertSame(this.space, argument3.getValue());
		assertTrue(argument4.getValue());
	}

	@Test
	public void getOrCreateSpaceWithSpec_doublecreation() throws Exception {
		baseInit();
		//
		OpenEventSpace space1 = this.repository.getOrCreateSpaceWithSpec(this.spaceID, OpenEventSpaceSpecification.class,
				this.params);
		OpenEventSpace space2 = this.repository.getOrCreateSpaceWithSpec(this.spaceID, OpenEventSpaceSpecification.class,
				this.params);
		//
		assertSame(this.space, space1);
		assertSame(this.space, space2);
		ArgumentCaptor<SpaceID> argument1 = ArgumentCaptor.forClass(SpaceID.class);
		ArgumentCaptor<Object[]> argument2 = ArgumentCaptor.forClass(Object[].class);
		verify(this.spaceSpecification, new Times(1)).create(argument1.capture(), argument2.capture());
		assertSame(this.spaceID, argument1.getValue());
		assertEquals("PARAM", argument2.getValue()); //$NON-NLS-1$
		assertSame(this.space, this.repository.getSpace(this.spaceID));
		//
		ArgumentCaptor<Space> argument3 = ArgumentCaptor.forClass(Space.class);
		ArgumentCaptor<Boolean> argument4 = ArgumentCaptor.forClass(Boolean.class);
		verify(this.listener, new Times(1)).spaceCreated(argument3.capture(), argument4.capture());
		assertSame(this.space, argument3.getValue());
		assertTrue(argument4.getValue());
	}

	@Test
	public void getOrCreateSpaceWithID_singlecreation() throws Exception {
		baseInit();
		//
		OpenEventSpace space = this.repository.getOrCreateSpaceWithID(this.spaceID, OpenEventSpaceSpecification.class,
				this.params);
		//
		assertSame(this.space, space);
		ArgumentCaptor<SpaceID> argument1 = ArgumentCaptor.forClass(SpaceID.class);
		ArgumentCaptor<Object[]> argument2 = ArgumentCaptor.forClass(Object[].class);
		verify(this.spaceSpecification, new Times(1)).create(argument1.capture(), argument2.capture());
		assertSame(this.spaceID, argument1.getValue());
		assertEquals("PARAM", argument2.getValue()); //$NON-NLS-1$
		assertSame(this.space, this.repository.getSpace(this.spaceID));
		//
		ArgumentCaptor<Space> argument3 = ArgumentCaptor.forClass(Space.class);
		ArgumentCaptor<Boolean> argument4 = ArgumentCaptor.forClass(Boolean.class);
		verify(this.listener, new Times(1)).spaceCreated(argument3.capture(), argument4.capture());
		assertSame(this.space, argument3.getValue());
		assertTrue(argument4.getValue());
	}

	@Test
	public void getOrCreateSpaceWithID_doublecreation() throws Exception {
		baseInit();
		//
		OpenEventSpace space1 = this.repository.getOrCreateSpaceWithID(this.spaceID, OpenEventSpaceSpecification.class,
				this.params);
		OpenEventSpace space2 = this.repository.getOrCreateSpaceWithID(this.spaceID, OpenEventSpaceSpecification.class,
				this.params);
		//
		assertSame(this.space, space1);
		assertSame(this.space, space2);
		ArgumentCaptor<SpaceID> argument1 = ArgumentCaptor.forClass(SpaceID.class);
		ArgumentCaptor<Object[]> argument2 = ArgumentCaptor.forClass(Object[].class);
		verify(this.spaceSpecification, new Times(1)).create(argument1.capture(), argument2.capture());
		assertSame(this.spaceID, argument1.getValue());
		assertEquals("PARAM", argument2.getValue()); //$NON-NLS-1$
		assertSame(this.space, this.repository.getSpace(this.spaceID));
		//
		ArgumentCaptor<Space> argument3 = ArgumentCaptor.forClass(Space.class);
		ArgumentCaptor<Boolean> argument4 = ArgumentCaptor.forClass(Boolean.class);
		verify(this.listener, new Times(1)).spaceCreated(argument3.capture(), argument4.capture());
		assertSame(this.space, argument3.getValue());
		assertTrue(argument4.getValue());
	}

	@Test
	public void destroy_notinit() throws Exception {
		this.repository.destroy();
		verifyZeroInteractions(this.listener);
	}

	@Test
	public void destroy_baseinit() throws Exception {
		baseInit();
		this.repository.destroy();
		verifyZeroInteractions(this.listener);
	}

	@Test
	public void destroy_initmocks() throws Exception {
		initMocks();
		this.repository.destroy();
		verifyZeroInteractions(this.listener);
	}

	@Test
	public void destroy_initrepository() throws Exception {
		initRepository();
		when(this.space.getParticipants()).thenReturn(Collections3.<UUID> emptySynchronizedSet());
		this.repository.destroy();
		ArgumentCaptor<Space> argument = ArgumentCaptor.forClass(Space.class);
		ArgumentCaptor<Boolean> argument4 = ArgumentCaptor.forClass(Boolean.class);
		verify(this.listener, new Times(1)).spaceDestroyed(argument.capture(), argument4.capture());
		assertSame(this.space, argument.getValue());
		assertTrue(argument4.getValue());
	}

	@Test
	public void destroy_hasparticipant() throws Exception {
		initRepository();
		when(this.space.getParticipants()).thenReturn(Collections3.synchronizedSingleton(UUID.randomUUID()));
		this.repository.destroy();
		ArgumentCaptor<Space> argument = ArgumentCaptor.forClass(Space.class);
		ArgumentCaptor<Boolean> argument4 = ArgumentCaptor.forClass(Boolean.class);
		verify(this.listener, new Times(1)).spaceDestroyed(argument.capture(), argument4.capture());
		assertSame(this.space, argument.getValue());
		assertTrue(argument4.getValue());
	}

	@Test
	public void createSpace_bug92() throws Exception {
		initMocks();
		//
		final UUID contextID = UUID.randomUUID();
		when(this.spaceSpecification.create(ArgumentMatchers.any())).thenAnswer(new Answer<Space>() {
			@Override
			public Space answer(InvocationOnMock invocation) throws Throwable {
				Space sp = mock(OpenEventSpace.class);
				SpaceID spId = (SpaceID) invocation.getArguments()[0];
				when(sp.getSpaceID()).thenReturn(spId);
				when(sp.toString()).thenReturn(spId.toString());
				return sp;
			}
		});
		//
		OpenEventSpace space1 = this.repository
				.createSpace(new SpaceID(contextID, UUID.fromString("22222222-2222-2222-2222-222222222222"), //$NON-NLS-1$
						OpenEventSpaceSpecification.class), OpenEventSpaceSpecification.class);
		OpenEventSpace space2 = this.repository
				.createSpace(new SpaceID(contextID, UUID.fromString("77777777-7777-7777-7777-777777777777"), //$NON-NLS-1$
						OpenEventSpaceSpecification.class), OpenEventSpaceSpecification.class);
		OpenEventSpace space3 = this.repository
				.createSpace(new SpaceID(contextID, UUID.fromString("77777777-7777-7777-7777-777777777777"), //$NON-NLS-1$
						OpenEventSpaceSpecification.class), OpenEventSpaceSpecification.class);
		//
		assertNotNull(space1);
		assertNotNull(space2);
		assertNull(space3);
		assertNotSame(space1, space2);
		assertNotEquals(space1.getSpaceID(), space2.getSpaceID());
	}

	@Test
	public void getOrCreateSpaceWithSpec_bug92() throws Exception {
		initMocks();
		//
		final UUID contextID = UUID.randomUUID();
		when(this.spaceSpecification.create(ArgumentMatchers.any())).thenAnswer(new Answer<Space>() {
			@Override
			public Space answer(InvocationOnMock invocation) throws Throwable {
				Space sp = mock(OpenEventSpace.class);
				SpaceID spId = (SpaceID) invocation.getArguments()[0];
				when(sp.getSpaceID()).thenReturn(spId);
				when(sp.toString()).thenReturn(spId.toString());
				return sp;
			}
		});
		//
		OpenEventSpace space1 = this.repository
				.getOrCreateSpaceWithSpec(new SpaceID(contextID, UUID.fromString("22222222-2222-2222-2222-222222222222"), //$NON-NLS-1$
						OpenEventSpaceSpecification.class), OpenEventSpaceSpecification.class);
		OpenEventSpace space2 = this.repository
				.getOrCreateSpaceWithSpec(new SpaceID(contextID, UUID.fromString("77777777-7777-7777-7777-777777777777"), //$NON-NLS-1$
						OpenEventSpaceSpecification.class), OpenEventSpaceSpecification.class);
		OpenEventSpace space3 = this.repository
				.getOrCreateSpaceWithSpec(new SpaceID(contextID, UUID.fromString("77777777-7777-7777-7777-777777777777"), //$NON-NLS-1$
						OpenEventSpaceSpecification.class), OpenEventSpaceSpecification.class);
		//
		assertNotNull(space1);
		assertNotNull(space2);
		assertNotNull(space3);
		assertSame(space1, space2);
		assertSame(space2, space3);
	}

	@Test
	public void getOrCreateSpaceWithID_bug92() throws Exception {
		initMocks();
		//
		final UUID contextID = UUID.randomUUID();
		when(this.spaceSpecification.create(ArgumentMatchers.any())).thenAnswer(new Answer<Space>() {
			@Override
			public Space answer(InvocationOnMock invocation) throws Throwable {
				Space sp = mock(OpenEventSpace.class);
				SpaceID spId = (SpaceID) invocation.getArguments()[0];
				when(sp.getSpaceID()).thenReturn(spId);
				when(sp.toString()).thenReturn(spId.toString());
				return sp;
			}
		});
		//
		OpenEventSpace space1 = this.repository
				.getOrCreateSpaceWithID(new SpaceID(contextID, UUID.fromString("22222222-2222-2222-2222-222222222222"), //$NON-NLS-1$
						OpenEventSpaceSpecification.class), OpenEventSpaceSpecification.class);
		OpenEventSpace space2 = this.repository
				.getOrCreateSpaceWithID(new SpaceID(contextID, UUID.fromString("77777777-7777-7777-7777-777777777777"), //$NON-NLS-1$
						OpenEventSpaceSpecification.class), OpenEventSpaceSpecification.class);
		OpenEventSpace space3 = this.repository
				.getOrCreateSpaceWithID(new SpaceID(contextID, UUID.fromString("77777777-7777-7777-7777-777777777777"), //$NON-NLS-1$
						OpenEventSpaceSpecification.class), OpenEventSpaceSpecification.class);
		//
		assertNotNull(space1);
		assertNotNull(space2);
		assertNotNull(space3);
		assertNotSame(space1, space2);
		assertSame(space2, space3);
	}

}
