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

import io.sarl.acl.message.Performative;

/**
 * This enumeration describes all available constant for
 * Predefined Msg Type as defined by FIPA for Bit-Efficient encoding, 
 * and their setter (used for decoding process - java reflection tips)
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
public enum PredefinedMessageType {

	/** Accept a proposal.
	 *
	 * <p>Code: {@code 0x01}.
	 */
	ACCEPT_PROPOSAL(Performative.ACCEPT_PROPOSAL, (byte) 0x01),

	/** Agree.
	 *
	 * <p>Code: {@code 0x02}.
	 */
	AGREE(Performative.AGREE, (byte) 0x02),
	
	/** Cancel.
	 * 
	 * <p>Code: {@code 0x03}.
	 */
	CANCEL(Performative.CANCEL, (byte) 0x03),

	/** Call for proposal.
	 * 
	 * <p>Code: {@code 0x04}.
	 */
	CFP(Performative.CFP, (byte) 0x04),

	/** Confirm.
	 *
	 * <p>Code: {@code 0x05}.
	 */
	CONFIRM(Performative.CONFIRM, (byte) 0x05),

	/** Disconfirm.
	 * 
	 * <p>Code: {@code 0x06}.
	 */
	DISCONFIRM(Performative.DISCONFIRM, (byte) 0x06),

	/** Failure.
	 * 
	 * <p>Code: {@code 0x07}.
	 */
	FAILURE(Performative.FAILURE, (byte) 0x07),

	/** Inform.
	 * 
	 * <p>Code: {@code 0x08}.
	 */
	INFORM(Performative.INFORM, (byte) 0x08),

	/** Inform if.
	 * 
	 * <p>Code: {@code 0x09}.
	 */
	INFORM_IF(Performative.INFORM_IF, (byte) 0x09),

	/** Inform ref.
	 * 
	 * <p>Code: {@code 0x0A}.
	 */
	INFORM_REF(Performative.INFORM_REF, (byte) 0x0A),

	/** Not understood.
	 * 
	 * <p>Code: {@code 0x0B}.
	 */
	NOT_UNDERSTOOD(Performative.NOT_UNDERSTOOD, (byte) 0x0B),

	/** Propagate.
	 * 
	 * <p>Code: {@code 0x0C}.
	 */
	PROPAGATE(Performative.PROPAGATE, (byte) 0x0C),

	/** Propose.
	 * 
	 * <p>Code: {@code 0x0D}.
	 */
	PROPOSE(Performative.PROPOSE, (byte) 0x0D),

	/** Proxy.
	 * 
	 * <p>Code: {@code 0x0E}.
	 */
	PROXY(Performative.PROXY, (byte) 0x0E),

	/** Query if.
	 * 
	 * <p>Code: {@code 0x0F}.
	 */
	QUERY_IF(Performative.QUERY_IF, (byte) 0x0F),

	/** Query ref.
	 *
	 * <p>Code: {@code 0x10}.
	 */
	QUERY_REF(Performative.QUERY_REF, (byte) 0x10),

	/** Refuse.
	 *
	 * <p>Code: {@code 0x11}.
	 */
	REFUSE(Performative.REFUSE, (byte) 0x11),

	/** Reject proposal.
	 * 
	 * <p>Code: {@code 0x12}.
	 */
	REJECT_PROPOSAL(Performative.REJECT_PROPOSAL, (byte) 0x12),

	/** Request.
	 * 
	 * <p>Code: {@code 0x13}.
	 */
	REQUEST(Performative.REQUEST, (byte) 0x13),

	/** REquest when.
	 * 
	 * <p>Code: {@code 0x14}.
	 */
	REQUEST_WHEN(Performative.REQUEST_WHEN, (byte) 0x14),

	/** Request whenever.
	 * 
	 * <p>Code: {@code 0x15}.
	 */
	REQUEST_WHENEVER(Performative.REQUEST_WHENEVER, (byte) 0x15),

	/** Subscribe.
	 * 
	 * <p>Code: {@code 0x16}.
	 */
	SUBSCRIBE(Performative.SUBSCRIBE, (byte) 0x16);

	private final Performative performative;

	private final byte code;

	private PredefinedMessageType(Performative performative, byte code) {
		this.performative = performative;
		this.code = code;
	}

	/** Replies the binary code from the FIPA specification.
	 * 
	 * @return the code.
	 */
	@Pure
	public byte getBinaryCode() {
		return this.code;
	}

	/** Replies the binary code associated to the given performative.
	 * 
	 * @param performative the performative to search for.
	 * @return the code of the performative, or {@code -1} if none.
	 */
	@Pure
	public static byte getBinaryCode(Performative performative) {
		for (PredefinedMessageType value : values()) { 
			if (value.getPerformative() == performative) { 
				return value.getBinaryCode();
			} 
		}
		return -1;
	}

	/** Replies the performative associated to this message type.
	 * 
	 * @return the performative.
	 */
	@Pure
	public Performative getPerformative() {
		return this.performative;
	}

	/** Replies the performative associated to the given code.
	 * 
	 * @param code the code to search for.
	 * @return the performative for the code, or {@link Performative#NONE} if the code is not associated to
	 *     a performative.
	 */
	@Pure
	public static Performative getPerformative(byte code) {
		for (PredefinedMessageType value : values()) { 
			if (value.getBinaryCode() == code) { 
				return value.getPerformative();
			} 
		}
		return Performative.NONE;
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
	public static PredefinedMessageType valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			throw new NullPointerException("name is null"); //$NON-NLS-1$
		}
		final String ucname = name.toUpperCase();
		final String lcname = name.toLowerCase();
		try {
			for (final PredefinedMessageType type : values()) {
				if (ucname.equals(type.name())) {
					return type;
				}
				if (lcname.equals(type.toJsonString())) {
					return type;
				}
				if (lcname.equals(type.performative.getFipaName())) {
					return type;
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
		for (final PredefinedMessageType type : values()) {
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
