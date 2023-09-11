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

package io.sarl.acl.message;

import org.eclipse.xtext.xbase.lib.Pure;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.base.Strings;

/**
 * This enumeration describes all available ACL representations as defined by FIPA.
 * 
 * @see <a href="http://www.fipa.org/repository/aclreps.php3">FIPA ACL Representation Specifications</a>
 * 
 * @author $Author: sgalland$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $Groupid$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public enum AclRepresentation {
	/** Use array of byte for the representation.
	 */
	BIT_EFFICIENT("fipa.acl.rep.bitefficient.std"), //$NON-NLS-1$

	/** Use string of character for the representation.
	 */
	STRING("fipa.acl.rep.string.std"), //$NON-NLS-1$

	/** Use Json string for the representation (not FIPA compliant).
	 */
	JSON("sarl.acl.rep.json.std"), //$NON-NLS-1$

	/** Use XML for the representation.
	 */
	XML("fipa.acl.rep.xml.std"); //$NON-NLS-1$
	  
	private final String fipaName;
	
	private AclRepresentation(String fipaName) {
		this.fipaName = fipaName;
	}

	/** Replies the identifier of the representation.
	 *
	 * @return the id.
	 */
	@Pure
	public String getFipaName() {
		return this.fipaName;
	}

	/** Replies the default ACL representation.
	 *
	 * @return the default representation.
	 */
	@Pure
	public static AclRepresentation getDefault() {
		return BIT_EFFICIENT;
	}
	
	/** Replies the Json string representation of this type.
	 *
	 * @return the Json string representation.
	 */
	@JsonValue
	@Pure
	public String toJsonString() {
		return name().toLowerCase();
	}

	/** Parse the given case insensitive string for obtaining the representation.
	 *
	 * @param name the string to parse.
	 * @return the type.
	 * @throws NullPointerException when the specified name is null
	 * @throws IllegalArgumentException when the specified name cannot be found and no default value is provided.
	 */
	@JsonCreator
	@Pure
	public static AclRepresentation valueOfCaseInsensitive(String name) {
		return valueOfCaseInsensitive(name, null);
	}

	/** Parse the given case insensitive string for obtaining the representation.
	 *
	 * @param name the string to parse.
	 * @param defaultValue is the value to reply if the given {@code name} does not correspond to a representation.
	 *     If the default value is {@code null} and the {@code name} cannot be found, the exception
	 *     {@link IllegalArgumentException} is thrown.
	 * @return the type.
	 * @throws NullPointerException when the specified name is null
	 * @throws IllegalArgumentException when the specified name cannot be found and no default value is provided.
	 */
	@Pure
	public static AclRepresentation valueOfCaseInsensitive(String name, AclRepresentation defaultValue) {
		if (Strings.isNullOrEmpty(name)) {
			if (defaultValue != null) {
				return defaultValue;
			}
			throw new NullPointerException("name is null"); //$NON-NLS-1$
		}
		final String ucname = name.toUpperCase();
		try {
			for (final AclRepresentation representation : values()) {
				if (ucname.equals(representation.name())) {
					return representation;
				}
				if (name.equals(representation.getFipaName())) {
					return representation;
				}
			}
		} catch (Throwable exception) {
			//
		}
		if (defaultValue != null) {
			return defaultValue;
		}
		throw new IllegalArgumentException("illegal value for name: " + name); //$NON-NLS-1$
	}	

	/** Replies the Json labels for the types of ACL representation.
	 *
	 * @return the labels.
	 */
	@Pure
	public static String getJsonLabels() {
		final StringBuilder buffer = new StringBuilder();
		boolean first = true;
		for (final AclRepresentation type : values()) {
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
