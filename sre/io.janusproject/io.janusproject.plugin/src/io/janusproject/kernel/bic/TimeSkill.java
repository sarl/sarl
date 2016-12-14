/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import java.util.concurrent.TimeUnit;

import io.sarl.core.Time;
import io.sarl.lang.core.Agent;
import io.sarl.lang.scoping.batch.SARLTimeExtensions;

/**
 * Janus implementation of SARL's {@link Time} built-in capacity.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class TimeSkill extends BuiltinSkill implements Time {

	private static int installationOrder = -1;

	/**
	 * @param agent - owner of this skill.
	 */
	TimeSkill(Agent agent) {
		super(agent);
	}

	@Override
	public int getInstallationOrder() {
		if (installationOrder < 0) {
			installationOrder = installationOrder(this);
		}
		return installationOrder;
	}

	@Override
	public double getTime(TimeUnit timeUnit) {
		final double currentTime = System.currentTimeMillis();
		final TimeUnit tu = (timeUnit == null) ? TimeUnit.SECONDS : timeUnit;
		return SARLTimeExtensions.convertFromTo(currentTime, TimeUnit.MILLISECONDS, tu);
	}

	@Override
	public double getTime() {
		return SARLTimeExtensions.convertFromTo(System.currentTimeMillis(), TimeUnit.MILLISECONDS, TimeUnit.SECONDS);
	}

	@Override
	public double getOSTimeFactor() {
		return 1;
	}

}
