/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.util;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Scope;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Scope using {@link Address} for {@link EventSpace}'s
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class AddressScope implements Scope<Address>, Serializable {

	private static final long serialVersionUID = -1176306257753661102L;

	private final String SCOPE_ID = "aid://";

	private Collection<Address> addresses = null;

	AddressScope(Address... addrs) {
		this.addresses  = Collections.newSetFromMap(new ConcurrentHashMap<Address,Boolean>());
		Collections.addAll(this.addresses , addrs);
	}

	/**
	 * {@inheritDoc}
	 */
	public String getRepresentation() {
		return SCOPE_ID + addresses.toString();
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean matches(Address address) {
		return this.addresses.contains(address);
	}
	
	public final static AddressScope getScope(Address... addresses){
		return new AddressScope(addresses);
	}

}

