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

package io.sarl.acl.encoding.bitefficient.constant;

import org.eclipse.xtext.xbase.lib.Pure;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.base.Strings;

/** Defines the token from the numbers according to the FIPA specification.
 * 
 * @author $Author: flacreus$
 * @author $Author: sroth$
 * @author $Author: cstentz$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $Groupid$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 * @see <a href="http://www.fipa.org/specs/fipa00069/SC00069G.html">FIPA ACL Message Representation in Bit-Efficient Specification</a> 
 */
public enum NumberToken {

	/** Padding.
	 * 
	 * <p>Code {@code 0x00}.
	 */
	PADDING('\0', (byte) 0x00), //$NON-NLS-1$

	/** {@code 0}.
	 *
	 * <p>Code {@code 0x01}.
	 */
	ZERO('0', (byte) 0x01), //$NON-NLS-1$

	/** {@code 1}.
	 * 
	 * <p>Code {@code 0x02}.
	 */
	ONE('1', (byte) 0x02), //$NON-NLS-1$

	/** {@code 2}.
	 * 
	 * <p>Code {@code 0x03}.
	 */
	TWO('2', (byte) 0x03), //$NON-NLS-1$

	/** {@code 3}.
	 * 
	 * <p>Code {@code 0x04}.
	 */
	THREE('3', (byte) 0x04), //$NON-NLS-1$

	/** {@code 4}.
	 * 
	 * <p>Code {@code 0x05}.
	 */
	FOUR('4', (byte) 0x05), //$NON-NLS-1$

	/** {@code 5}.
	 * 
	 * <p>Code {@code 0x06}.
	 */
	FIVE('5', (byte) 0x06), //$NON-NLS-1$

	/** {@code 6}.
	 * 
	 * <p>Code {@code 0x07}.
	 */
	SIX('6', (byte) 0x07), //$NON-NLS-1$

	/** {@code 7}.
	 * 
	 * <p>Code {@code 0x08}.
	 */
	SEVEN('7', (byte) 0x08), //$NON-NLS-1$

	/** {@code 8}.
	 * 
	 * <p>Code {@code 0x09}.
	 */
	EIGHT('8', (byte) 0x09), //$NON-NLS-1$

	/** {@code 9}.
	 * 
	 * <p>Code {@code 0x0A}.
	 */
	NINE('9', (byte) 0x0A), //$NON-NLS-1$

	/** {@code +}.
	 * 
	 * <p>Code {@code 0x0B}.
	 */
	PLUS('+', (byte) 0x0B), //$NON-NLS-1$

	/** {@code E}.
	 * 
	 * <p>Code {@code 0x0C}.
	 */
	EXPONENT('E', (byte) 0x0C), //$NON-NLS-1$

	/** {@code -}.
	 * 
	 * <p>Code {@code 0x0D}.
	 */
	MINUS('-', (byte) 0x0D), //$NON-NLS-1$

	/** {@code .}.
	 * 
	 * <p>Code {@code 0x0E}.
	 */
	DOT('.', (byte) 0x0E); //$NON-NLS-1$

	private final char token;

	private final byte code;

	private NumberToken(char token, byte code) {
		this.token = token;
		this.code = code;
	}

	/** Replies the code from the FIPA specification.
	 * 
	 * @return the code.
	 */
	@Pure
	public byte getBinaryCode() {
		return this.code;
	}

	/** Replies the code of the token that is corresponding to the given character.
	 * 
	 * @param token the token to search for.
	 * @return the code, or the code {@link #PADDING} if the character is not
	 *     recognized.
	 */
	@Pure
	public static byte getBinaryCode(char token) {
		final char uctoken = Character.toUpperCase(token);
		for (NumberToken value : values()) { 
			if (value.getToken() == uctoken) { 
				return value.getBinaryCode();
			} 
		}
		return PADDING.getBinaryCode(); 
	}

	/** Replies the string-representation of the token from the FIPA specification.
	 * 
	 * @return the string-representation of the token.
	 */
	@Pure
	public char getToken() {
		return this.token;
	}

	/** Replies the string-representation of the given code.
	 * 
	 * @param code the code to search for.
	 * @return the string representation of the given code,
	 *     or the string-representation of {@link #PADDING} if the
	 *     code is not recognized.
	 */
	@Pure
	public static char getToken(byte code) {
		for (NumberToken value : values()) { 
			if (value.getBinaryCode() == code) { 
				return value.getToken();
			} 
		}
		return PADDING.getToken();
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

	/** Parse the given case insensitive string for obtaining the message type.
	 *
	 * @param name the string to parse.
	 * @return the type.
	 * @throws NullPointerException when the specified name is null
	 */
	@JsonCreator
	@Pure
	public static NumberToken valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			throw new NullPointerException("name is null"); //$NON-NLS-1$
		}
		final String ucname = name.toUpperCase();
		final char ctoken = ucname.charAt(0);
		final String lcname = name.toLowerCase();
		try {
			for (final NumberToken token : values()) {
				if (ucname.equals(token.name())) {
					return token;
				}
				if (lcname.equals(token.toJsonString())) {
					return token;
				}
				if (ctoken == token.getToken()) {
					return token;
				}
			}
		} catch (Throwable exception) {
			//
		}
		throw new IllegalArgumentException("illegal value for name: " + name); //$NON-NLS-1$
	}	

	/** Replies the Json labels for the types of messages.
	 *
	 * @return the labels.
	 */
	@Pure
	public static String getJsonLabels() {
		final StringBuilder buffer = new StringBuilder();
		boolean first = true;
		for (final NumberToken token : values()) {
			if (first) {
				first = false;
			} else {
				buffer.append(", "); //$NON-NLS-1$
			}
			buffer.append(token.toJsonString());
		}
		return buffer.toString();
	}

}
