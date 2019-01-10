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

import java.util.concurrent.TimeUnit;

import io.sarl.core.Time;
import io.sarl.lang.core.Agent;
import io.sarl.lang.scoping.extensions.time.TimeExtensions;

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

	/** Constructor.
	 * @param agent owner of this skill.
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
		if (timeUnit == null || timeUnit == TimeUnit.SECONDS) {
			return System.currentTimeMillis() / (double) TimeExtensions.MILLIS_IN_SECOND;
		}
		if (timeUnit == TimeUnit.MILLISECONDS) {
			return currentTime;
		}
		return TimeExtensions.convertFromTo(currentTime, TimeUnit.MILLISECONDS, timeUnit);
	}

	@Override
	public double getTime() {
		return System.currentTimeMillis() / (double) TimeExtensions.MILLIS_IN_SECOND;
	}

	@Override
	public double getOSTimeFactor() {
		return 1;
	}

	@Override
	public double toOSTime(double timeValue) {
		return timeValue;
	}

	@Override
	public double fromOSTime(double timeValue) {
		return timeValue;
	}

	@Override
	public double toOSDuration(double timeDuration) {
		return timeDuration;
	}

	@Override
	public double fromOSDuration(double timeDuration) {
		return timeDuration;
	}

}
