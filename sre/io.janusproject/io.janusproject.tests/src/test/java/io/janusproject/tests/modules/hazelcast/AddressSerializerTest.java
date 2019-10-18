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
package io.janusproject.tests.modules.hazelcast;

import java.util.UUID;

import com.hazelcast.nio.serialization.StreamSerializer;
import org.junit.Before;
import org.junit.Test;

import io.sarl.core.OpenEventSpaceSpecification;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.SpaceID;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class AddressSerializerTest extends AbstractSerializerTest {

	@Nullable
	private Class<?> type;

	@Nullable
	private StreamSerializer serializer;

	@Before
	public void setUp() throws Exception {
		this.type = this.reflect.forName("io.janusproject.modules.hazelcast.AddressSerializer");
		this.serializer = (StreamSerializer) this.reflect.newInstance(this.type);
	}

	@Test
	public void getTypeId() throws Exception {
		assertEquals(
				this.reflect.getStatic(this.type, "ADDRESS_CLASS_TYPE"),
				this.reflect.invoke(this.serializer, "getTypeId"));
	}

	@Test
	public void writeRead() throws Exception {
		assertWriteRead(new Address(new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class),
				UUID.randomUUID()), this.serializer);
	}

}
