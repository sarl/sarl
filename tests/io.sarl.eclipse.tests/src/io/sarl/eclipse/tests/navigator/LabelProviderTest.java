/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import static org.mockito.Mockito.*;
import static org.junit.Assert.*;

import java.util.UUID;

import io.sarl.eclipse.navigator.ISARLProjectElement;
import io.sarl.eclipse.navigator.LabelProvider;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

import org.eclipse.swt.graphics.Image;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class LabelProviderTest extends AbstractSarlTest {

	@Nullable
	private LabelProvider provider;

	@Before
	public void setUp() {
		this.provider = new LabelProvider();
	}

	@Test
	public void getImage_projectElement() {
		Image image = new Image(null, 1, 1);
		ISARLProjectElement element = mock(ISARLProjectElement.class);
		when(element.getImage()).thenReturn(image);
		assertSame(image, this.provider.getImage(element));
	}

	@Test
	public void getImage_otherElement() {
		assertNull(this.provider.getImage(UUID.randomUUID()));
	}

	@Test
	public void getText_projectElement() {
		String name = UUID.randomUUID().toString();
		ISARLProjectElement element = mock(ISARLProjectElement.class);
		when(element.getText()).thenReturn(name);
		assertEquals(name, this.provider.getText(element));
	}

	@Test
	public void getText_otherElement() {
		assertEquals("", this.provider.getText(UUID.randomUUID()));
	}

}
