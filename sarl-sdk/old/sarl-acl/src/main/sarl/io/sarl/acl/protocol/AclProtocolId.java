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
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.acl.protocol;

import org.eclipse.xtext.xbase.lib.Pure;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.base.Strings;

/**
 * This enumeration describes all available protocols that are FIPA or not.
 * 
 * @author $Author: sgalland$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $Groupid$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public enum AclProtocolId {
	/** Unknown protocol.
	 */
	NONE("none"), //$NON-NLS-1$

	/** SARL Ping-Pong protocol.
	 */
	SARL_PINGPONG("sarl-pingpong"), //$NON-NLS-1$

	/** FIPA Request protocol.
	 *
	 * @see <a href="http://www.fipa.org/specs/fipa00026/SC00026H.html">FIPA Request Interaction Protocol Specification</a>
	 */
	FIPA_REQUEST("fipa-request"), //$NON-NLS-1$

	/** FIPA Propose protocol.
	 * 
	 * @see <a href="http://www.fipa.org/specs/fipa00036/SC00036H.html">FIPA Propose
	 *      Interaction Protocol Specification</a>
	 */
	FIPA_PROPOSE("fipa-propose"), //$NON-NLS-1$
	
	/** FIPA Query protocol.
	 * 
	 * @see <a href="http://www.fipa.org/specs/fipa00027/SC00027H.html">FIPA Query
	 *      Interaction Protocol Specification</a>
	 */
	FIPA_QUERY("fipa-query"), //$NON-NLS-1$

	/** FIPA Request-When protocol.
	 * 
	 * @see <a href="http://www.fipa.org/specs/fipa00028/SC00028H.html">FIPA Request-When
	 *      Interaction Protocol Specification</a>
	 */
	FIPA_REQUEST_WHEN("fipa-request-when"); //$NON-NLS-1$

	private final String code;
	
	AclProtocolId(String code) {
		this.code = code;
	}
	
	/** Replies the identifier of the ACL representation.
	 *
	 * @return the id.
	 */
	@Pure
	public String getCode() {
		return this.code;
	}

	/** Replies the default FIPA protocol.
	 *
	 * @return the default protocol.
	 */
	@Pure
	public static AclProtocolId getDefault() {
		return NONE;
	}
	
	/** Replies the Json string representation of this protocol.
	 *
	 * @return the Json string representation.
	 */
	@JsonValue
	@Pure
	public String toJsonString() {
		return name().toLowerCase();
	}

	/** Parse the given case insensitive string for obtaining the protocol.
	 *
	 * @param name the string to parse.
	 * @return the protocol.
	 * @throws NullPointerException when the specified name is null
	 */
	@JsonCreator
	@Pure
	public static AclProtocolId valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			throw new NullPointerException("name is null"); //$NON-NLS-1$
		}
		final String ucname = name.toUpperCase();
		final String lcname = name.toLowerCase();
		try {
			for (final AclProtocolId protocol : values()) {
				if (ucname.equals(protocol.name())) {
					return protocol;
				}
				if (lcname.equals(protocol.getCode())) {
					return protocol;
				}
			}
		} catch (Throwable exception) {
			//
		}
		throw new IllegalArgumentException("illegal value for name: " + name); //$NON-NLS-1$
	}	

	/** Replies the Json labels for the types of ACL protocol.
	 *
	 * @return the labels.
	 */
	@Pure
	public static String getJsonLabels() {
		final StringBuilder buffer = new StringBuilder();
		boolean first = true;
		for (final AclProtocolId type : values()) {
			if (first) {
				first = false;
			} else {
				buffer.append(", "); //$NON-NLS-1$
			}
			buffer.append(type.toJsonString());
		}
		return buffer.toString();
	}

}
