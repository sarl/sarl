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
package io.sarl.eclipse.tests.explorer;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jface.viewers.Viewer;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;

import io.sarl.eclipse.explorer.HiddenFileFilter;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class HiddenFileFilterTest extends AbstractSarlTest {

	@NonNullByDefault
	private HiddenFileFilter filter;

	@Mock
	private Viewer viewer;

	@Mock
	private Object parent;

	@Before
	public void setUp() {
		this.filter = new HiddenFileFilter();
	}

	@Test
	public void eclipseHiddenFile() {
		IFile element = mock(IFile.class);
		when(element.isHidden()).thenReturn(true);
		when(element.isDerived()).thenReturn(false);
		when(element.isPhantom()).thenReturn(false);
		when(element.getRawLocation()).thenReturn(null);
		assertFalse(this.filter.select(this.viewer, parent, element));
	}

	@Test
	public void eclipseDerivedFile() {
		IFile element = mock(IFile.class);
		when(element.isHidden()).thenReturn(false);
		when(element.isDerived()).thenReturn(true);
		when(element.isPhantom()).thenReturn(false);
		when(element.getRawLocation()).thenReturn(null);
		assertFalse(this.filter.select(this.viewer, parent, element));
	}

	@Test
	public void osHiddenFile() {
		IFile element = mock(IFile.class);
		when(element.isHidden()).thenReturn(false);
		when(element.isDerived()).thenReturn(false);
		when(element.isPhantom()).thenReturn(false);
		IPath path = mock(IPath.class);
		when(element.getRawLocation()).thenReturn(path);
		File osfile = mock(File.class);
		when(path.toFile()).thenReturn(osfile);
		when(osfile.isHidden()).thenReturn(true);
		assertFalse(this.filter.select(this.viewer, parent, element));
	}

	@Test
	public void standardFile() {
		IFile element = mock(IFile.class);
		when(element.isHidden()).thenReturn(false);
		when(element.isDerived()).thenReturn(false);
		when(element.isPhantom()).thenReturn(false);
		IPath path = mock(IPath.class);
		when(element.getRawLocation()).thenReturn(path);
		File osfile = mock(File.class);
		when(path.toFile()).thenReturn(osfile);
		when(osfile.isHidden()).thenReturn(false);
		assertTrue(this.filter.select(this.viewer, parent, element));
	}

	@Test
	public void standardFolder() {
		IFolder element = mock(IFolder.class);
		assertTrue(this.filter.select(this.viewer, parent, element));
	}

}
