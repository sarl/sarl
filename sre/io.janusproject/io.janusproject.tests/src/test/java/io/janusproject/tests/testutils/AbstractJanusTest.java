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
package io.janusproject.tests.testutils;

import java.util.Properties;

import io.janusproject.JanusConfig;
import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import io.sarl.tests.api.AbstractSarlTest;

/**
 * Abstract class that is providing useful tools for unit tests.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractJanusTest extends AbstractSarlTest {

	/**
	 * This rule permits to clean automatically the fields at the end of the test.
	 */
	@Rule
	public TestWatcher rootJanusWatchter = new TestWatcher() {
		@Override
		protected void starting(Description description) {
			// Clear the system properties
			resetProperties();
		}

		@Override
		protected void finished(Description description) {
			// Clear the system properties
			resetProperties();
		}
	};

	/**
	 * Remove all the system properties related to Janus.
	 */
	public static void resetProperties() {
		Properties tmp = new Properties();
		JanusConfig.getDefaultValues(tmp);
		Properties props = System.getProperties();
		for (Object name : tmp.keySet()) {
			props.remove(name);
		}
	}

}
