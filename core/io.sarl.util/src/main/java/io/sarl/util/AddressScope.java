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

import java.util.Set;
import java.util.TreeSet;

/**
 * Scope using {@link Address} for <var>EventSpace</var>'s
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class AddressScope implements Scope<Address> {

	private static final long serialVersionUID = -1176306257753661102L;

	private static final String SCOPE_ID = "aid://"; //$NON-NLS-1$

	private Set<Address> addresses = null;

	/**
	 * @param addrs
	 */
	AddressScope(Address... addrs) {		
	    this.addresses = new TreeSet<Address>();
	    for(Address adr : addrs) {
	        if (adr!=null) {
	        	this.addresses.add(adr);
	        }
	    }
	}

	@Override
	public String toString() {
		return SCOPE_ID + this.addresses.toString();
	}

	@Override
	public boolean matches(Address address) {
		assert(address != null);
		return this.addresses.contains(address);
	}

	/** Create an scope restricted to the given addresses.
	 * 
	 * @param addresses
	 * @return the scope restricted to the given addresses.
	 */
	public final static AddressScope getScope(Address... addresses){
		return new AddressScope(addresses);
	}

}

