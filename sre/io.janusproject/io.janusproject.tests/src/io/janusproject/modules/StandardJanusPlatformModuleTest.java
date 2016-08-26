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
package io.janusproject.modules;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.Iterator;

import org.arakhne.afc.vmutil.OperatingSystem;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.google.inject.Binder;
import com.google.inject.Module;

import io.janusproject.JanusConfig;
import io.janusproject.modules.eventserial.NetworkEventModule;
import io.janusproject.modules.hazelcast.HazelcastModule;
import io.janusproject.modules.kernel.LocalDistributedDataStructureServiceModule;
import io.janusproject.modules.kernel.LocalInfrastructureServiceModule;
import io.janusproject.modules.kernel.LocalKernelDiscoveryServiceModule;
import io.janusproject.modules.nonetwork.NoNetworkModule;
import io.janusproject.modules.zeromq.ZeroMQNetworkModule;
import io.janusproject.testutils.AbstractJanusTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({ StandardJanusPlatformModuleTest.AndroidTests.class, StandardJanusPlatformModuleTest.LinuxTests.class,
		StandardJanusPlatformModuleTest.WindowsTests.class, StandardJanusPlatformModuleTest.MacOSTests.class, })
@SuppressWarnings("all")
public class StandardJanusPlatformModuleTest {

	public static void assertContainsModules(Iterable<? extends Module> iterable, Class... moduleTypes) {
		AbstractJanusTest.assertContainsCollection(new ModuleIterable(iterable),
				new ClassIterable(Arrays.<Class<?>> asList(moduleTypes)));
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class AndroidTests extends AbstractJanusTest {

		@Mock
		private Binder binder;

		@InjectMocks
		private StandardJanusPlatformModule module;

		@Before
		public void setUp() {
			MockitoAnnotations.initMocks(this);
			OperatingSystem.setCurrentOS(OperatingSystem.ANDROID);
		}

		@After
		public void tearDown() {
			OperatingSystem.setCurrentOS(null);
		}

		@Test
		public void testNetworkIsDisabledWhenOffline() {
			System.setProperty(JanusConfig.OFFLINE, Boolean.TRUE.toString());
			//
			this.module.configure();
			//
			ArgumentCaptor<Module> arg = ArgumentCaptor.forClass(Module.class);
			verify(this.binder, times(6)).install(arg.capture());
			assertContainsModules(arg.getAllValues(),
					// Mandatory modules
					BootModule.class, StandardCoreModule.class,
					// Offline modules
					LocalInfrastructureServiceModule.class, LocalDistributedDataStructureServiceModule.class,
					LocalKernelDiscoveryServiceModule.class, NoNetworkModule.class);
		}

		@Test
		public void testNetworkIsDisabledWhenNotOffline() {
			System.setProperty(JanusConfig.OFFLINE, Boolean.FALSE.toString());
			//
			this.module.configure();
			//
			ArgumentCaptor<Module> arg = ArgumentCaptor.forClass(Module.class);
			verify(this.binder, times(6)).install(arg.capture());
			assertContainsModules(arg.getAllValues(),
					// Mandatory modules
					BootModule.class, StandardCoreModule.class,
					// Offline modules
					LocalInfrastructureServiceModule.class, LocalDistributedDataStructureServiceModule.class,
					LocalKernelDiscoveryServiceModule.class, NoNetworkModule.class);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class LinuxTests extends AbstractJanusTest {

		@Mock
		private Binder binder;

		@InjectMocks
		private StandardJanusPlatformModule module;

		@Before
		public void setUp() {
			MockitoAnnotations.initMocks(this);
			OperatingSystem.setCurrentOS(OperatingSystem.LINUX);
		}

		@After
		public void tearDown() {
			OperatingSystem.setCurrentOS(null);
		}

		@Test
		public void testNetworkIsDisabledWhenOffline() {
			System.setProperty(JanusConfig.OFFLINE, Boolean.TRUE.toString());
			//
			this.module.configure();
			//
			ArgumentCaptor<Module> arg = ArgumentCaptor.forClass(Module.class);
			verify(this.binder, times(6)).install(arg.capture());
			assertContainsModules(arg.getAllValues(),
					// Mandatory modules
					BootModule.class, StandardCoreModule.class,
					// Offline modules
					LocalInfrastructureServiceModule.class, LocalDistributedDataStructureServiceModule.class,
					LocalKernelDiscoveryServiceModule.class, NoNetworkModule.class);
		}

		@Test
		public void testNetworkIsEnabledWhenNotOffline() {
			System.setProperty(JanusConfig.OFFLINE, Boolean.FALSE.toString());
			//
			this.module.configure();
			//
			ArgumentCaptor<Module> arg = ArgumentCaptor.forClass(Module.class);
			verify(this.binder, times(5)).install(arg.capture());
			assertContainsModules(arg.getAllValues(),
					// Mandatory modules
					BootModule.class, StandardCoreModule.class,
					// Offline modules
					HazelcastModule.class, NetworkEventModule.class, ZeroMQNetworkModule.class);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class WindowsTests extends AbstractJanusTest {

		@Mock
		private Binder binder;

		@InjectMocks
		private StandardJanusPlatformModule module;

		@Before
		public void setUp() {
			MockitoAnnotations.initMocks(this);
			OperatingSystem.setCurrentOS(OperatingSystem.WIN);
		}

		@After
		public void tearDown() {
			OperatingSystem.setCurrentOS(null);
		}

		@Test
		public void testNetworkIsDisabledWhenOffline() {
			System.setProperty(JanusConfig.OFFLINE, Boolean.TRUE.toString());
			//
			this.module.configure();
			//
			ArgumentCaptor<Module> arg = ArgumentCaptor.forClass(Module.class);
			verify(this.binder, times(6)).install(arg.capture());
			assertContainsModules(arg.getAllValues(),
					// Mandatory modules
					BootModule.class, StandardCoreModule.class,
					// Offline modules
					LocalInfrastructureServiceModule.class, LocalDistributedDataStructureServiceModule.class,
					LocalKernelDiscoveryServiceModule.class, NoNetworkModule.class);
		}

		@Test
		public void testNetworkIsEnabledWhenNotOffline() {
			System.setProperty(JanusConfig.OFFLINE, Boolean.FALSE.toString());
			//
			this.module.configure();
			//
			ArgumentCaptor<Module> arg = ArgumentCaptor.forClass(Module.class);
			verify(this.binder, times(5)).install(arg.capture());
			assertContainsModules(arg.getAllValues(),
					// Mandatory modules
					BootModule.class, StandardCoreModule.class,
					// Offline modules
					HazelcastModule.class, NetworkEventModule.class, ZeroMQNetworkModule.class);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class MacOSTests extends AbstractJanusTest {

		@Mock
		private Binder binder;

		@InjectMocks
		private StandardJanusPlatformModule module;

		@Before
		public void setUp() {
			MockitoAnnotations.initMocks(this);
			OperatingSystem.setCurrentOS(OperatingSystem.MACOSX);
		}

		@After
		public void tearDown() {
			OperatingSystem.setCurrentOS(null);
		}

		@Test
		public void testNetworkIsDisabledWhenOffline() {
			System.setProperty(JanusConfig.OFFLINE, Boolean.TRUE.toString());
			//
			this.module.configure();
			//
			ArgumentCaptor<Module> arg = ArgumentCaptor.forClass(Module.class);
			verify(this.binder, times(6)).install(arg.capture());
			assertContainsModules(arg.getAllValues(),
					// Mandatory modules
					BootModule.class, StandardCoreModule.class,
					// Offline modules
					LocalInfrastructureServiceModule.class, LocalDistributedDataStructureServiceModule.class,
					LocalKernelDiscoveryServiceModule.class, NoNetworkModule.class);
		}

		@Test
		public void testNetworkIsEnabledWhenNotOffline() {
			System.setProperty(JanusConfig.OFFLINE, Boolean.FALSE.toString());
			//
			this.module.configure();
			//
			ArgumentCaptor<Module> arg = ArgumentCaptor.forClass(Module.class);
			verify(this.binder, times(5)).install(arg.capture());
			assertContainsModules(arg.getAllValues(),
					// Mandatory modules
					BootModule.class, StandardCoreModule.class,
					// Offline modules
					HazelcastModule.class, NetworkEventModule.class, ZeroMQNetworkModule.class);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class ModuleIterable implements Iterable<String> {

		private final Iterable<? extends Module> iterable;

		public ModuleIterable(Iterable<? extends Module> iterable) {
			this.iterable = iterable;
		}

		@Override
		public Iterator<String> iterator() {
			return new ModuleIterator(this.iterable.iterator());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class ModuleIterator implements Iterator<String> {

		private final Iterator<? extends Module> iterator;

		public ModuleIterator(Iterator<? extends Module> iterator) {
			this.iterator = iterator;
		}

		@Override
		public boolean hasNext() {
			return this.iterator.hasNext();
		}

		@Override
		public String next() {
			Module module = this.iterator.next();
			return module.getClass().getName();
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class ClassIterable implements Iterable<String> {

		private final Iterable<? extends Class<?>> iterable;

		public ClassIterable(Iterable<? extends Class<?>> iterable) {
			this.iterable = iterable;
		}

		@Override
		public Iterator<String> iterator() {
			return new ClassIterator(this.iterable.iterator());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class ClassIterator implements Iterator<String> {

		private final Iterator<? extends Class<?>> iterator;

		public ClassIterator(Iterator<? extends Class<?>> iterator) {
			this.iterator = iterator;
		}

		@Override
		public boolean hasNext() {
			return this.iterator.hasNext();
		}

		@Override
		public String next() {
			Class<?> type = this.iterator.next();
			return type.getName();
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

}
