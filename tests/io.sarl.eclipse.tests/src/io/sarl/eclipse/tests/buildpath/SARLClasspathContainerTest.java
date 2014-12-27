/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import static org.mockito.Mockito.mock;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import io.sarl.eclipse.buildpath.Messages;
import io.sarl.eclipse.buildpath.SARLClasspathContainer;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.google.common.base.Strings;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLClasspathContainerTest extends Assert {

	private IPath containerPath;
	private SARLClasspathContainer container;
	
	@Before
	public void setUp() {
		this.containerPath = mock(IPath.class);
		this.container = new SARLClasspathContainer(this.containerPath);
	}
	
	@After
	public void tearDown() {
		this.container = null;
		this.containerPath = null;
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
	public void getClasspathEntries() {
		IClasspathEntry[] entries = this.container.getClasspathEntries();
		List<IClasspathEntry> list = new ArrayList<>(Arrays.asList(entries));
		for (String reference : SARLClasspathContainer.SARL_REFERENCE_LIBRARIES) {
			Iterator<IClasspathEntry> iterator = list.iterator();
			boolean found = false;
			while (!found && iterator.hasNext()) {
				IClasspathEntry entry = iterator.next();
				if (isReferenceLibrary(reference, entry)) {
					found = true;
					iterator.remove();
				}
			}
			if (!found) {
				fail("Expecting the reference library: " + reference
						+ "in: " + Arrays.toString(entries));
			}
		}
		if (!list.isEmpty()) {
			fail("Unexpected reference library: " + list.toString()
					+ "in: " + Arrays.toString(entries));
		}
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

}
