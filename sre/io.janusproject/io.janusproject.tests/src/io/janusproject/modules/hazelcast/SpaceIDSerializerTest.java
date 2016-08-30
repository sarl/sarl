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
package io.janusproject.modules.hazelcast;

import static org.junit.Assert.assertEquals;

import java.util.UUID;

import org.eclipse.jdt.annotation.Nullable;
import org.junit.Before;
import org.junit.Test;

import io.sarl.lang.core.SpaceID;
import io.sarl.util.OpenEventSpaceSpecification;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SpaceIDSerializerTest extends AbstractSerializerTest {

	@Nullable
	private SpaceIDSerializer serializer;

	@Before
	public void setUp() {
		this.serializer = new SpaceIDSerializer();
	}

	@Test
	public void getTypeId() {
		assertEquals(SpaceIDSerializer.SPACE_ID_CLASS_TYPE, this.serializer.getTypeId());
	}

	@Test
	public void writeRead() throws Exception {
		assertWriteRead(new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class), this.serializer);
	}

}
