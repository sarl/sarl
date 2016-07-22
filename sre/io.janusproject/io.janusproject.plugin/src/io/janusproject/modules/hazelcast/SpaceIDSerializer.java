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
import org.arakhne.afc.vmutil.locale.Locale;

import io.sarl.lang.core.SpaceID;
import io.sarl.lang.core.SpaceSpecification;

/**
 * Serializer of identifiers of spaces.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class SpaceIDSerializer implements StreamSerializer<SpaceID> {

	/**
	 * Unique identifier for the {@link SpaceID} type.
	 */
	public static final int SPACE_ID_CLASS_TYPE = 19118;

	@Override
	public int getTypeId() {
		return SPACE_ID_CLASS_TYPE;
	}

	@Override
	public void destroy() {
		//
	}

	@Override
	public void write(ObjectDataOutput out, SpaceID object) throws IOException {
		out.writeObject(object.getContextID());
		out.writeObject(object.getID());
		out.writeUTF(object.getSpaceSpecification().getCanonicalName());

	}

	@SuppressWarnings("unchecked")
	@Override
	public SpaceID read(ObjectDataInput in) throws IOException {
		try {
			UUID cid = in.readObject();
			UUID id = in.readObject();
			String specCls = in.readUTF();
			if (cid != null && id != null && specCls != null) {
				Class<?> type = Class.forName(specCls);
				if (SpaceSpecification.class.isAssignableFrom(type)) {
					return new SpaceID(cid, id, (Class<? extends SpaceSpecification<?>>) type);
				}
			}
			throw new IOException(Locale.getString("BUILD_ERROR", cid, id, specCls)); //$NON-NLS-1$
		} catch (ClassNotFoundException e) {
			throw new IOException(Locale.getString("SPECIFICATION_CLASS_NOT_FOUND"), e); //$NON-NLS-1$
		}

	}

}
