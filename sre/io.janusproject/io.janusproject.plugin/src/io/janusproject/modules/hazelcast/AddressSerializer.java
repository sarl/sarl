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

package io.janusproject.modules.hazelcast;

import java.io.IOException;
import java.util.UUID;

import com.hazelcast.nio.ObjectDataInput;
import com.hazelcast.nio.ObjectDataOutput;
import com.hazelcast.nio.serialization.StreamSerializer;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.SpaceID;

/**
 * Serializer for Janus addresses.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class AddressSerializer implements StreamSerializer<Address> {

	/**
	 * Unique identifier for the {@link Address} type.
	 */
	public static final int ADDRESS_CLASS_TYPE = 19119;

	@Override
	public int getTypeId() {
		return ADDRESS_CLASS_TYPE;
	}

	@Override
	public void destroy() {
		//
	}

	@Override
	public void write(ObjectDataOutput out, Address object) throws IOException {
		out.writeObject(object.getUUID());
		out.writeObject(object.getSpaceId());

	}

	@Override
	public Address read(ObjectDataInput in) throws IOException {
		UUID id = in.readObject();
		SpaceID sid = in.readObject();
		return new Address(sid, id);
	}

}
