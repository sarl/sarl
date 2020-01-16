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

package io.janusproject;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.logging.Logger;

import com.google.common.util.concurrent.Service;

import io.janusproject.kernel.Kernel;

import io.sarl.bootstrap.SREBootstrap;
import io.sarl.lang.annotation.PrivateAPI;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;

/**
 * Represents an access point to the SARL run-time environment (SRE).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public final class Bootstrap implements SREBootstrap {

	private volatile Kernel kernel;

	/** Construct a bootstrap with the default initialization values for the bootstrap.
	 */
	public Bootstrap() {
		setOffline(JanusConfig.DEFAULT_OFFLINE_FLAG_FOR_BOOTSTRAP);
	}

	/** Change the current kernel.
	 *
	 * @param kernel the kernel.
	 */
	@PrivateAPI
	public void setKernel(Kernel kernel) {
		this.kernel = kernel;
	}

	@Override
	public boolean isRunning() {
		return this.kernel != null && this.kernel.isRunning();
	}

	@Override
	public void shutdown(boolean blocking) throws InterruptedException {
		final Kernel kern;
		synchronized (this) {
			kern = this.kernel;
			this.kernel = null;
		}
		if (kern != null) {
			final Runnable stop = kern.getStopBehavior();
			stop.run();
			if (blocking) {
				while (kern.isRunning()) {
					Thread.yield();
				}
			}
		}
	}

	@Override
	public UUID startAgent(Class<? extends Agent> agentCls, Object... params) throws Exception {
		final Kernel kern = this.kernel;
		if (kern == null) {
			synchronized (this) {
				this.kernel = Boot.startJanus(agentCls, params);
			}
			return Boot.getBootAgentIdentifier();
		}
		return kern.spawn(agentCls, params);
	}

	@Override
	public Iterable<UUID> startAgent(int nbAgents, Class<? extends Agent> agentCls, Object... params) throws Exception {
		Kernel kern = this.kernel;
		if (kern == null) {
			final List<UUID> spawned = new ArrayList<>();
			synchronized (this) {
				this.kernel = Boot.startJanus(agentCls, params);
				kern = this.kernel;
			}
			spawned.add(Boot.getBootAgentIdentifier());
			if (nbAgents > 1) {
				spawned.addAll(kern.spawn(nbAgents - 1, agentCls, params));
			}
			return spawned;
		}
		return kern.spawn(nbAgents, agentCls, params);
	}

	@Override
	public void startAgentWithID(Class<? extends Agent> agentCls, UUID agentId, Object... params) throws Exception {
		final Kernel kern = this.kernel;
		if (kern == null) {
			synchronized (this) {
				this.kernel = Boot.startJanus(agentCls, params);
				return;
			}
		}
		kern.spawn(agentId, agentCls, params);
	}

	@Override
	public UUID getBootAgentIdentifier() {
		return Boot.getBootAgentIdentifier();
	}

	@Override
	public AgentContext startWithoutAgent() {
		Kernel kern = this.kernel;
		if (kern == null) {
			synchronized (this) {
				this.kernel = Boot.startWithoutAgent();
				kern = this.kernel;
			}
		}
		return kern.getJanusContext();
	}

	@Override
	public Logger getKernelLogger() {
		final Kernel kern;
		synchronized (this) {
			kern = this.kernel;
		}
		if (kern != null) {
			return kern.getLogger();
		}
		return null;
	}

	@Override
	public void setOffline(boolean isOffline) {
		Boot.setOffline(isOffline);
	}

	@Override
	public void setRandomContextUUID() {
		Boot.setRandomContextUUID();
	}

	@Override
	public void setBootAgentTypeContextUUID() {
		Boot.setBootAgentTypeContextUUID();
	}

	@Override
	public void setSpecificContextUUID() {
		Boot.setDefaultContextUUID();
	}

	@Override
	public void setVerboseLevel(int level) {
		Boot.setVerboseLevel(level);
	}

	/** {@inheritDoc}
	 */
	@Override
	public <T> T getService(Class<T> serviceType) {
		if (Service.class.isAssignableFrom(serviceType)) {
			final Kernel kern;
			synchronized (this) {
				kern = this.kernel;
			}
			if (kern != null) {
				return kern.getService(serviceType.asSubclass(Service.class));
			}
		}
		return null;
	}

}
