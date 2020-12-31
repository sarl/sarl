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
package io.sarl.eclipse.tests.natures;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.junit.Before;
import org.junit.Test;

import io.sarl.eclipse.natures.SARLProjectNature;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLProjectNatureTest extends AbstractSarlTest {

	@NonNullByDefault
	private SARLProjectNature nature;

	@Before
	public void setUp() {
		this.nature = new SARLProjectNature();
	}

	@Test
	public void configure() {
		//
	}

	@Test
	public void deconfigure() {
		//
	}

	@Test
	public void getProject() {
		assertNull(this.nature.getProject());
	}

	@Test
	public void setProject() {
		IProject project = mock(IProject.class);
		this.nature.setProject(project);
		assertSame(project, this.nature.getProject());
	}

}
