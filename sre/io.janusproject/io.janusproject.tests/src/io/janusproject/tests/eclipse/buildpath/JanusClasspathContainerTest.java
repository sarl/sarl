/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.janusproject.tests.eclipse.buildpath;

import static org.junit.Assert.*;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.collect.Iterables;
import io.janusproject.eclipse.buildpath.JanusClasspathContainer;
import io.janusproject.eclipse.buildpath.Messages;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.junit.Before;
import org.junit.Test;

import io.sarl.eclipse.util.BundleUtil.IBundleDependencies;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.TestScope;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class JanusClasspathContainerTest extends AbstractSarlTest {

	private static final String[] ECLIPSE_DEPENDENCIES = {
			"aopalliance",
			"com.google.code.gson",
			"com.google.guava",
			"com.google.inject",
			"com.google.inject.multibindings",
			"com.hazelcast",
			"io.janusproject.plugin",
			"io.sarl.core",
			"io.sarl.lang.core",
			"io.sarl.util",
			"javax.cache.api",
			"javax.inject",
			"javax.servlet",
			"org.apache.commons.cli",
			"org.apache.log4j",
			"org.arakhne.afc.core.util",
			"org.arakhne.afc.core.vmutils",
			"org.eclipse.core.contenttype",
			"org.eclipse.core.jobs",
			"org.eclipse.core.runtime",
			"org.eclipse.equinox.app",
			"org.eclipse.equinox.common",
			"org.eclipse.equinox.preferences",
			"org.eclipse.equinox.registry",
			"org.eclipse.osgi",
			"org.eclipse.osgi.compatibility.state",
			"org.eclipse.osgi.services",
			"org.eclipse.xtext.logging",
			"org.eclipse.xtext.xbase.lib",
			"org.slf4j.api",
			"org.slf4j.impl.log4j12",
			"org.zeromq.jeromq",
	};
	
	private static final String[] TYCHO_DEPENDENCIES = {
			"com.google.code.gson",
			"com.google.guava",
			"com.google.inject",
			"com.google.inject.multibindings",
			"com.hazelcast",
			"io.janusproject.plugin",
			"io.sarl.core",
			"io.sarl.lang.core",
			"io.sarl.util",
			"javax.cache.api",
			"javax.inject",
			"org.aopalliance",
			"org.apache.commons.cli",
			"org.apache.log4j",
			"org.arakhne.afc.core.util",
			"org.arakhne.afc.core.vmutils",
			"org.eclipse.osgi",
			"org.eclipse.xtext.xbase.lib",
			"org.zeromq.jeromq",
	};

	@NonNullByDefault
	private IPath containerPath;

	@NonNullByDefault
	private JanusClasspathContainer container;

	@Before
	public void setUp() {
		this.containerPath = mock(IPath.class);
		this.container = new JanusClasspathContainer(this.containerPath);
	}
	
	private static boolean isReferenceLibrary(String reference, IClasspathEntry entry) {
		IPath path = entry.getPath();
		String str = path.lastSegment();
		if ("classes".equals(str)) {
			str = path.segment(path.segmentCount() - 3);
			return str.equals(reference);
		}
		return str.startsWith(reference + "_");
	}
	
	@Test
	public void toStr() {
		IBundleDependencies deps = JanusClasspathContainer.getJanusPlatformClasspath();
		String str = deps.toString();
		//System.out.println(str);
	}

	@Test
	public void getDescription() {
		assertEquals(Messages.JanusClasspathContainer_0, this.container.getDescription());
	}

	@Test
	public void getKind() {
		assertEquals(IClasspathContainer.K_APPLICATION, this.container.getKind());
	}

	@Test
	public void getPath() {
		assertSame(this.containerPath, this.container.getPath());
	}

	@Test
	@TestScope(eclipse = true, tycho = false)
	public void getBundleDependencies_withEclipse() {
		Iterable<String> iterable = this.container.getBundleDependencies();
		assertNotNull(iterable);
		assertContains(iterable, ECLIPSE_DEPENDENCIES);
	}

	@Test
	@TestScope(eclipse = false, tycho = true)
	public void getBundleDependencies_withTycho() {
		Iterable<String> iterable = this.container.getBundleDependencies();
		assertNotNull(iterable);
		assertContains(iterable, TYCHO_DEPENDENCIES);
	}

	@Test
	@TestScope(eclipse = true, tycho = false)
	public void getClasspathEntries_withEclipse() {
		IClasspathEntry[] iterable = this.container.getClasspathEntries();
		assertNotNull(iterable);
		assertEquals(ECLIPSE_DEPENDENCIES.length, iterable.length);
	}

	@Test
	@TestScope(eclipse = false, tycho = true)
	public void getClasspathEntries_withTycho() {
		IClasspathEntry[] iterable = this.container.getClasspathEntries();
		assertNotNull(iterable);
		assertEquals(TYCHO_DEPENDENCIES.length, iterable.length);
	}

}
