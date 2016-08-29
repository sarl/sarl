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
package io.janusproject.kernel.bic;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import io.janusproject.kernel.Kernel;
import io.janusproject.services.network.NetworkService;
import io.janusproject.testutils.AbstractJanusTest;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class MicroKernelSkillTest extends AbstractJanusTest {

	@Mock
	private NetworkService service;

	@Mock
	private Kernel kernel;

	@InjectMocks
	private MicroKernelSkill skill;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		Mockito.when(this.kernel.getService(Matchers.any(Class.class))).thenReturn(this.service);
	}

	@Test
	public void getKernel() {
		assertSame(this.kernel, this.skill.getKernel());
	}

	@Test
	public void uninstall() {
		assertSame(this.kernel, this.skill.getKernel());
		this.skill.uninstall();
		assertNull(this.skill.getKernel());
	}

	@Test
	public void getService() {
		assertSame(this.service, this.skill.getService(null));
	}

}
