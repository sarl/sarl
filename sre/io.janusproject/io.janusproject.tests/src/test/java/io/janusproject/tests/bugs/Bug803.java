/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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
package io.janusproject.tests.bugs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.*;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;

import io.janusproject.kernel.bic.LoggingSkill;
import io.janusproject.services.logging.LogService;

import io.sarl.lang.core.Agent;
import io.sarl.tests.api.AbstractSarlTest.ReflectExtensions;

/** Tests for issue #803: debug() not working in Logging capacity.
 *
 * <p>See: https://github.com/sarl/sarl/issues/803
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/803"
 */
@SuppressWarnings("all")
public class Bug803 {

	private LoggingSkill skill;

	private Logger logger;
	
	@Before
	public void setUp() throws Exception {
		ReflectExtensions reflect = new ReflectExtensions();
		Agent agent = mock(Agent.class);
		this.logger = spy(Logger.getLogger("fake"));
		this.skill = reflect.newInstance(LoggingSkill.class, agent);
		this.skill.setLoggingService(mock(LogService.class));
		this.skill.setLogger(this.logger);
	}

	@After
	public void tearDown() throws Exception {
		this.skill = null;
		this.logger = null;
	}
	
	@Test
	public void debugFlag() throws Exception {
		this.skill.setLogLevel(4);
		assertTrue(this.skill.isDebugLogEnabled());
		this.skill.debug("xxxxxxx");
		ArgumentCaptor<Level> level = ArgumentCaptor.forClass(Level.class);
		ArgumentCaptor<String> message = ArgumentCaptor.forClass(String.class);
		ArgumentCaptor<String[]> params = ArgumentCaptor.forClass(String[].class);
		verify(this.logger, times(1)).log(level.capture(), message.capture(), params.capture());
		assertSame(Level.FINE, level.getValue());
		assertEquals("xxxxxxx", message.getValue());
	}

}
