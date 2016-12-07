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

import org.eclipse.osgi.util.NLS;

/** Messages.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Messages extends NLS {
	private static final String BUNDLE_NAME = "io.janusproject.kernel.bic.messages"; //$NON-NLS-1$
	public static String ExternalContextAccessSkill_0;
	public static String ExternalContextAccessSkill_1;
	public static String InternalEventBusSkill_0;
	public static String InternalEventBusSkill_1;
	public static String InternalEventBusSkill_2;
	public static String InternalEventBusSkill_3;
	public static String LoggingSkill_0;
	public static String SchedulesSkill_0;
	public static String SchedulesSkill_1;
	public static String SchedulesSkill_2;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
