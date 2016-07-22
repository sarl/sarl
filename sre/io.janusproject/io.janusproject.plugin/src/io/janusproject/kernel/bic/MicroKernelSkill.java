/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import java.lang.ref.WeakReference;

import com.google.common.util.concurrent.Service;
import io.janusproject.kernel.Kernel;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Skill;

/**
 * Janus implementation of an internal skill that provides access to the micro kernel.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class MicroKernelSkill extends Skill implements MicroKernelCapacity {

	private WeakReference<Kernel> kernel;

	/**
	 * @param agent - the owner of this skill.
	 * @param kernel - the reference to the local micro-kernel.
	 */
	MicroKernelSkill(Agent agent, Kernel kernel) {
		super(agent);
		this.kernel = new WeakReference<>(kernel);
	}

	@Override
	protected void uninstall() {
		this.kernel = null;
	}

	/**
	 * Replies the kernel.
	 *
	 * @return the kernel, or <code>null</code>.
	 */
	protected Kernel getKernel() {
		return this.kernel == null ? null : this.kernel.get();
	}

	@Override
	public <S extends Service> S getService(Class<S> type) {
		Kernel k = getKernel();
		if (k != null) {
			return k.getService(type);
		}
		return null;
	}

}
