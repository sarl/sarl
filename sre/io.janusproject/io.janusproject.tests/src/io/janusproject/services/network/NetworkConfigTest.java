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
package io.janusproject.services.network;

import static org.junit.Assert.assertEquals;

import java.util.Properties;

import org.junit.Test;

import com.google.common.base.Charsets;

import io.janusproject.kernel.services.gson.GsonEventSerializer;
import io.janusproject.kernel.services.jdk.network.PlainTextEventEncrypter;
import io.janusproject.testutils.AbstractJanusTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class NetworkConfigTest extends AbstractJanusTest {

	@Test
	public void getDefaultValues() {
		Properties defs = new Properties();
		NetworkConfig.getDefaultValues(defs);

		// Use hard-coded string to ensure retro compatibility
		assertEquals("", defs.get("network.encrypter.aes.key")); //$NON-NLS-1$ //$NON-NLS-2$
		assertEquals(GsonEventSerializer.class.getName(), defs.get("network.serializer.class")); //$NON-NLS-1$
		assertEquals(PlainTextEventEncrypter.class.getName(), defs.get("network.encrypter.class")); //$NON-NLS-1$
		assertEquals(Charsets.UTF_8.name(), defs.get("network.serializer.charset")); //$NON-NLS-1$
	}

}
