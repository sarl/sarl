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
package io.janusproject.tests.bugs.network;

import java.io.ObjectStreamException;
import java.util.Collection;
import java.util.UUID;

import com.google.common.base.Objects;
import org.junit.Ignore;
import org.junit.Test;

import io.janusproject.tests.testutils.AbstractJanusRunTest;

import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.Logging;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Scope;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.util.SerializableProxy;

/** Tests for issue #885: Guards are not evaluated when the event is fired across multiple kernels.
 *
 * <p>See: https://github.com/sarl/sarl/issues/885
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/885"
 */
@SuppressWarnings("all")
public class Bug885 extends AbstractJanusRunTest {

	@Test
	@Ignore
	public void runPing() throws Exception {
		runJanus(PingAgent.class, true, false, NO_TIMEOUT);
	}

	@Test
	@Ignore
	public void runPong() throws Exception {
		runJanus(PongAgent.class, true, false, NO_TIMEOUT);
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_EVENT)
	static class Ping extends Event {
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_EVENT)
	static class Pong extends Event {
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_AGENT)
	static class PongAgent extends TestingAgent {

		public PongAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			getSkill(Logging.class).info("ready to receive ping");
			return false;
		}

		@PerceptGuardEvaluator
		private void guardPing(Ping occurrence, Collection<Runnable> handlers) {
			handlers.add(() -> onPing(occurrence));
		}

		private void onPing(Ping occurrence) {
			getSkill(Logging.class).info("receive ping: " + occurrence);
			class $SerializableClosureProxy implements Scope<Address> {
				private final UUID uuid;
				public $SerializableClosureProxy(UUID uuid) {
					this.uuid = uuid;
				}
				@Override
				public boolean matches(final Address it) {
					return Objects.equal(it.getUUID(), this.uuid);
				}
			}
			Scope<Address> scope = new Scope<Address>() {
				@Override
				public boolean matches(Address it) {
					return Objects.equal(it.getUUID(), occurrence.getSource().getUUID());
				}
				private Object writeReplace() throws ObjectStreamException {
					return new SerializableProxy($SerializableClosureProxy.class, occurrence.getSource().getUUID());
				}
			};
			getSkill(DefaultContextInteractions.class).emit(new Pong(), scope);
		}

	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_AGENT)
	static class PingAgent extends TestingAgent {

		public PingAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			getSkill(Logging.class).info("initiate ping-pong protocol");
			getSkill(DefaultContextInteractions.class).emit(new Ping());
			return false;
		}

		@PerceptGuardEvaluator
		private void guardPong(Pong occurrence, Collection<Runnable> handlers) {
			handlers.add(() -> onPong(occurrence));
		}

		private void onPong(Pong occurrence) {
			getSkill(Logging.class).info("Receiver pong: " + occurrence);
			class $SerializableClosureProxy implements Scope<Address> {
				private final UUID uuid;
				public $SerializableClosureProxy(UUID uuid) {
					this.uuid = uuid;
				}
				@Override
				public boolean matches(final Address it) {
					return Objects.equal(it.getUUID(), this.uuid);
				}
			}
			Scope<Address> scope = new Scope<Address>() {
				@Override
				public boolean matches(Address it) {
					return Objects.equal(it.getUUID(), occurrence.getSource().getUUID());
				}
				private Object writeReplace() throws ObjectStreamException {
					return new SerializableProxy($SerializableClosureProxy.class, occurrence.getSource().getUUID());
				}
			};
			getSkill(DefaultContextInteractions.class).emit(new Ping(), scope);
		}

	}

}
