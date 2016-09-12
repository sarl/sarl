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
package io.janusproject.tests.kernel.bic;

import static org.junit.Assert.*;

import java.util.UUID;
import java.util.concurrent.TimeUnit;

import io.janusproject.kernel.bic.InternalEventBusCapacity;
import io.janusproject.kernel.bic.TimeSkill;
import io.janusproject.tests.testutils.AbstractJanusTest;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import io.sarl.core.Time;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class TimeSkillTest extends AbstractJanusTest {

	@Nullable
	private TimeSkill skill;

	@Before
	public void setUp() throws Exception {
		Agent agent = new TestAgent(this);
		this.skill = this.reflect.newInstance(TimeSkill.class, agent);
	}
	
	@Test
	public void getTime() {
		assertEpsilonEquals(System.currentTimeMillis() / 1000., this.skill.getTime());
	}

	@Test
	public void getTimeTimeUnit_null() {
		assertEpsilonEquals(System.currentTimeMillis() / 1000., this.skill.getTime(null));
	}

	@Test
	public void getTimeTimeUnit_seconds() {
		assertEpsilonEquals(System.currentTimeMillis() / 1000., this.skill.getTime(TimeUnit.SECONDS));
	}

	@Test
	public void getTimeTimeUnit_millis() {
		assertEpsilonEquals(System.currentTimeMillis(), this.skill.getTime(TimeUnit.MILLISECONDS));
	}

	@Test
	public void getOSTimeFactor() {
		assertEpsilonEquals(1, this.skill.getOSTimeFactor());
	}

	public static class TestAgent extends Agent {

		private final TimeSkillTest test;
		
		public TestAgent(TimeSkillTest test) {
			super(Mockito.mock(BuiltinCapacitiesProvider.class), UUID.randomUUID(), null);
			this.test = test;
		}

		@Override
		protected <S extends Capacity> S getSkill(Class<S> capacity) {
			if (Time.class.equals(capacity))
				return capacity.cast(this.test.skill);
			return super.getSkill(capacity);
		}

	}

}
