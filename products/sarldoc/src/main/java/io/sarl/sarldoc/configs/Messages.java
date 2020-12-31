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

package io.sarl.sarldoc.configs;

import org.eclipse.osgi.util.NLS;

/** Messages for the SARL batch compiler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @ExcludeFromApidoc
 */
@SuppressWarnings("all")
public class Messages extends NLS {
	private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages"; //$NON-NLS-1$
	public static String Placement_0;
	public static String Placement_1;
	public static String Placement_2;
	public static String Placement_3;
	public static String Placement_4;
	public static String Placement_5;
	public static String Placement_6;
	public static String Placement_7;
	public static String SarldocConfig_0;
	public static String SarldocConfig_1;
	public static String SarldocConfig_2;
	public static String SarldocConfig_3;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
