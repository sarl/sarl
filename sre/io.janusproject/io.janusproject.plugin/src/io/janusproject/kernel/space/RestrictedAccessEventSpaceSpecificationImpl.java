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

package io.janusproject.kernel.space;

import java.security.acl.Acl;
import java.security.acl.Permission;

import com.google.inject.Inject;
import com.google.inject.Injector;
import io.janusproject.services.distributeddata.DistributedDataStructureService;

import io.sarl.lang.core.SpaceID;
import io.sarl.util.RestrictedAccessEventSpace;
import io.sarl.util.RestrictedAccessEventSpaceSpecification;

/**
 * Default implementation of the specification of a restricted-access event space.
 *
 * <p>
 * The initialization parameters of {@link #create(SpaceID, Object...)} must contain an instance of {@link Acl}. This instance is
 * the Access Control List. The first parameter that is a {@link Permission} will be assumed as the permission to have to be
 * allowed to access to the space.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class RestrictedAccessEventSpaceSpecificationImpl implements RestrictedAccessEventSpaceSpecification {

	@Inject
	private Injector injector;

	@Override
	public RestrictedAccessEventSpace create(SpaceID id, Object... params) {
		Acl acl = null;
		Permission permission = null;
		for (Object o : params) {
			if (o instanceof Acl) {
				acl = (Acl) o;
			} else if (o instanceof Permission) {
				permission = (Permission) o;
			}
		}
		if (acl != null) {
			if (permission == null) {
				permission = new RegistrationPermission();
			}
			RestrictedAccessEventSpaceImpl space = new RestrictedAccessEventSpaceImpl(id, acl, permission,
					this.injector.getInstance(DistributedDataStructureService.class));
			this.injector.injectMembers(space);
			return space;
		}
		throw new IllegalArgumentException();
	}

}
