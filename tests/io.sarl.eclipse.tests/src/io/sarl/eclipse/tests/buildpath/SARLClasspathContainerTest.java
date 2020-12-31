/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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
package io.sarl.eclipse.tests.buildpath;

import static org.junit.Assert.*;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.collect.Iterables;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.junit.Before;
import org.junit.Test;

import io.sarl.eclipse.buildpath.Messages;
import io.sarl.eclipse.buildpath.SARLClasspathContainer;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.TestScope;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLClasspathContainerTest extends AbstractSarlUiTest {

	@NonNullByDefault
	private IPath containerPath;

	@NonNullByDefault
	private SARLClasspathContainer container;

	@Before
	public void setUp() {
		this.containerPath = mock(IPath.class);
		this.container = new SARLClasspathContainer(this.containerPath);
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
	public void getDescription() {
		assertEquals(Messages.SARLClasspathContainer_0, this.container.getDescription());
	}

	@Test
	public void getKind() {
		assertEquals(IClasspathContainer.K_SYSTEM, this.container.getKind());
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
		assertPartlyContains(iterable,
			"com.google.guava",
			"com.google.inject",
			"io.sarl.core",
			"io.sarl.lang.core",
			"io.sarl.util",
			"javax.inject",
			"org.eclipse.osgi",
			"org.eclipse.osgi.compatibility.state",
			"org.eclipse.xtext.xbase.lib"
			);
	}

	@Test
	@TestScope(eclipse = false, tycho = true)
	public void getBundleDependencies_withTycho() {
		Iterable<String> iterable = this.container.getBundleDependencies();
		assertNotNull(iterable);
		assertPartlyContains(iterable,
				"io.sarl.lang.core",
				"javax.inject",
				"org.eclipse.xtext.xbase.lib",
				"com.google.guava",
				"org.eclipse.osgi",
				"org.eclipse.osgi.compatibility.state",
				//
				"io.sarl.util",
				//
				"io.sarl.core");
	}

	@Test
	@TestScope(eclipse = true, tycho = false)
	public void getClasspathEntries_withEclipse() {
		IClasspathEntry[] iterable = this.container.getClasspathEntries();
		assertNotNull(iterable);
		assertEquals(12, iterable.length);
	}

	@Test
	@TestScope(eclipse = false, tycho = true)
	public void getClasspathEntries_withTycho() {
		IClasspathEntry[] iterable = this.container.getClasspathEntries();
		assertNotNull(iterable);
		assertEquals(8, iterable.length);
	}

}
