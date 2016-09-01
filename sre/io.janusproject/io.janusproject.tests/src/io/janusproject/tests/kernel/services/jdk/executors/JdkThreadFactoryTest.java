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
package io.janusproject.tests.kernel.services.jdk.executors;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import java.lang.Thread.UncaughtExceptionHandler;

import io.janusproject.kernel.services.jdk.executors.JdkThreadFactory;
import io.janusproject.tests.testutils.AbstractJanusTest;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class JdkThreadFactoryTest extends AbstractJanusTest {

	@Nullable
	private UncaughtExceptionHandler handler;

	@Nullable
	private JdkThreadFactory factory;

	@Before
	public void setUp() {
		this.handler = Mockito.mock(UncaughtExceptionHandler.class);
		this.factory = new JdkThreadFactory(this.handler);
	}

	@Test
	public void newThread() {
		Thread t = this.factory.newThread(Mockito.mock(Runnable.class));
		assertNotNull(t);
		assertSame(this.handler, t.getUncaughtExceptionHandler());
	}

}
