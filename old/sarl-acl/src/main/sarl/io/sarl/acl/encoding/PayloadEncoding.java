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

package io.sarl.acl.encoding;

import org.eclipse.xtext.xbase.lib.Pure;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.base.Strings;

/**
 * This enumeration corresponds to the different type of encoding (UTF-8, ISO, ASCII...)
 * 
 * @author $Author: sgalland$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $Groupid$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public enum PayloadEncoding {
	
	/**
	 * UTF8 Enconding, the default one.
	 */
	UTF8("UTF-8"), //$NON-NLS-1$
	
	/**
	 * UTF16 Enconding.
	 */
	UTF16("UTF-16"); //$NON-NLS-1$

	private final String code;
	
	PayloadEncoding(String code) {
		this.code = code;
	}

	/** Replies the code associated to this type.
	 *
	 * @return the code representation.
	 */
	@Pure
	public String getCode() {
		return this.code;
	}

	/** Replies the default encoding.
	 *
	 * @return the default encoding.
	 */
	@Pure
	public static PayloadEncoding getDefault() {
		return UTF8;
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

	/** Parse the given case insensitive string for obtaining the encoding.
	 *
	 * @param name the string to parse.
	 * @return the type.
	 * @throws NullPointerException when the specified name is null
	 */
	@JsonCreator
	@Pure
	public static PayloadEncoding valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			throw new NullPointerException("name is null"); //$NON-NLS-1$
		}
		final String ucname = name.toUpperCase();
		try {
			for (final PayloadEncoding encoding : values()) {
				if (ucname.equals(encoding.name())) {
					return encoding;
				}
				if (ucname.equals(encoding.getCode())) {
					return encoding;
				}
			}
		} catch (Throwable exception) {
			//
		}
		throw new IllegalArgumentException("illegal value for name: " + name); //$NON-NLS-1$
	}	

	/** Replies the Json labels for the types of encoding.
	 *
	 * @return the labels.
	 */
	@Pure
	public static String getJsonLabels() {
		final StringBuilder buffer = new StringBuilder();
		boolean first = true;
		for (final PayloadEncoding type : values()) {
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
