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
package io.sarl.eclipse.tests.navigator;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import io.sarl.eclipse.navigator.ContentProvider;
import io.sarl.eclipse.navigator.ISARLProjectElement;
import io.sarl.eclipse.navigator.SARLProjectWorkbenchRoot;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

import org.junit.Before;
import org.junit.Test;

/**
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class ContentProviderTest extends AbstractSarlTest {

	@Nullable
	private ContentProvider provider;
	@Nullable
	private SARLProjectWorkbenchRoot root;
	@Nullable
	private ISARLProjectElement element;
	@Nullable
	private ISARLProjectElement child1;
	@Nullable
	private ISARLProjectElement child2;
	@Nullable
	private Object other;
	@Nullable
	private Object[] children;
	
	@Before
	public void setUp() {
		this.provider = new ContentProvider();
		this.root = mock(SARLProjectWorkbenchRoot.class);
		this.element = mock(ISARLProjectElement.class);
		this.child1 = mock(ISARLProjectElement.class);
		when(this.child1.getParent()).thenReturn(this.element);
		this.child2 = mock(ISARLProjectElement.class);
		when(this.child2.getParent()).thenReturn(this.element);
		this.children = new Object[] {
			child1,
			child2,
		};
		when(this.element.getChildren()).thenReturn(this.children);
		when(this.element.hasChildren()).thenReturn(true);
		this.other = mock(Object.class);
	}

	@Test
	public void getChildren_root() {
		Object[] objects = this.provider.getChildren(this.root);
		assertNotNull(objects);
		assertZero(objects.length);
	}

	@Test
	public void getChildren_element() {
		Object[] objects = this.provider.getChildren(this.element);
		assertNotNull(objects);
		assertArraySimilar(this.children, objects);
	}

	@Test
	public void getChildren_other() {
		Object[] objects = this.provider.getChildren(this.other);
		assertNotNull(objects);
		assertZero(objects.length);
	}

	@Test
	public void getElements_root() {
		Object[] objects = this.provider.getElements(this.root);
		assertNotNull(objects);
		assertZero(objects.length);
	}

	@Test
	public void getElements_element() {
		Object[] objects = this.provider.getElements(this.element);
		assertNotNull(objects);
		assertArraySimilar(this.children, objects);
	}

	@Test
	public void getElements_other() {
		Object[] objects = this.provider.getElements(this.other);
		assertNotNull(objects);
		assertZero(objects.length);
	}

	@Test
	public void getParent() {
		assertSame(this.element, this.child1.getParent());
		assertSame(this.element, this.child2.getParent());
	}

	@Test
	public void hasChildren() {
		assertTrue(this.provider.hasChildren(this.element));
		assertFalse(this.provider.hasChildren(this.child1));
		assertFalse(this.provider.hasChildren(this.child2));
	}

}
