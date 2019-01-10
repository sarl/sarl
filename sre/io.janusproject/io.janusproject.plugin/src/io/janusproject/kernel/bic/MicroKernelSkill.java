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

package io.janusproject.kernel.bic;

import java.lang.ref.WeakReference;

import com.google.common.util.concurrent.Service;

import io.janusproject.kernel.Kernel;

import io.sarl.lang.core.Agent;

/**
 * Janus implementation of an internal skill that provides access to the micro kernel.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class MicroKernelSkill extends BuiltinSkill implements MicroKernelCapacity {

	private static int installationOrder = -1;

	private WeakReference<Kernel> kernel;

	/** Constructor.
	 * @param agent the owner of this skill.
	 * @param kernel the reference to the local micro-kernel.
	 */
	MicroKernelSkill(Agent agent, Kernel kernel) {
		super(agent);
		this.kernel = new WeakReference<>(kernel);
	}

	@Override
	public int getInstallationOrder() {
		if (installationOrder < 0) {
			installationOrder = installationOrder(this);
		}
		return installationOrder;
	}

	@Override
	protected void uninstall(UninstallationStage stage) {
		if (stage == UninstallationStage.POST_DESTROY_EVENT) {
			final WeakReference<Kernel> kernelReference = this.kernel;
			this.kernel = null;
			if (kernelReference != null) {
				kernelReference.clear();
			}
		}
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
		final Kernel k = getKernel();
		if (k != null) {
			return k.getService(type);
		}
		return null;
	}

}
