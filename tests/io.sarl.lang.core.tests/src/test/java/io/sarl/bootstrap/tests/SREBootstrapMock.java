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
package io.sarl.bootstrap.tests;

import java.util.UUID;

import io.sarl.bootstrap.SREBootstrap;
import io.sarl.bootstrap.SREListener;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SREBootstrapMock implements SREBootstrap {

	@Override
	public void startAgent(Class<? extends Agent> agentCls, Object... params) throws Exception {
		throw new IllegalStateException();
	}

	@Override
	public void startAgent(int nbAgents, Class<? extends Agent> agentCls, Object... params) throws Exception {
		throw new IllegalStateException();
	}

	@Override
	public void startAgentWithID(Class<? extends Agent> agentCls, UUID agentID, Object... params) throws Exception {
		throw new IllegalStateException();
	}

	@Override
	public AgentContext startWithoutAgent(boolean asCommandLineApp) {
		throw new IllegalStateException();
	}

	@Override
	public void shutdown(int timeout) throws InterruptedException {
		throw new IllegalStateException();
	}

	@Override
	public <T> T getService(Class<T> serviceType) {
		throw new IllegalStateException();
	}

	@Override
	public void addSREListener(SREListener listener) {
		throw new IllegalStateException();
	}

	@Override
	public void removeSREListener(SREListener listener) {
		throw new IllegalStateException();
	}

	@Override
	public void setCommandLineArguments(String[] arguments) {
		throw new IllegalStateException();
	}

}
