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
package io.sarl.eclipse.tests.launching;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import io.sarl.eclipse.launching.sreproviding.StandardProjectSREProvider;
import io.sarl.eclipse.runtime.SARLRuntime;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.TestScope;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@TestScope(tycho = false)
@SuppressWarnings("all")
public class StandardProjectSREProviderTest extends AbstractSarlUiTest {

	@NonNullByDefault
	private String id;

	@NonNullByDefault
	private IProject project;

	@NonNullByDefault
	private StandardProjectSREProvider provider;

	@Before
	public void setUp() throws Exception {
		this.id = SARLRuntime.createUniqueIdentifier();
		this.project = mock(IProject.class);
		when(this.project.getPersistentProperty(ArgumentMatchers.any(QualifiedName.class))).thenAnswer(new Answer() {
		@Override
			public Object answer(InvocationOnMock invocation) throws Throwable {
				QualifiedName qn = (QualifiedName) invocation.getArguments()[0];
				String sqn = qn.toString();
				switch (sqn) {
				case "io.sarl.eclipse.launch.SRE:HAS_PROJECT_SPECIFIC":
					return Boolean.TRUE.toString();
				case "io.sarl.eclipse.launch.SRE:USE_SYSTEM_WIDE_SRE":
					return Boolean.FALSE.toString();
				case "io.sarl.eclipse.launch.SRE:SRE_INSTALL_ID":
					return StandardProjectSREProviderTest.this.id;
				}
				return null;
			}
		});
		this.provider = new StandardProjectSREProvider(this.project);
	}

	@Test
	public void hasProjectSpecificSREConfiguration() {
		assertTrue(this.provider.hasProjectSpecificSREConfiguration());
	}

	@Test
	public void isSystemSREUsed() {
		assertFalse(this.provider.isSystemSREUsed());
	}

	@Test
	public void getSREInstallIdentifier() {
		assertEquals(this.id, this.provider.getSREInstallIdentifier());
	}

}
