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
 * This enumeration describes all available performatives as defined by FIPA.
 * 
 * @see <a href="http://www.fipa.org/specs/fipa00037/SC00037J.html">FIPA
 *      Communicative Act Library Specification</a>
 * 
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $Groupid$
 * @mavenartifactid $ArtifactId$
 * @since 0.1
 */
public enum Performative {
	/** No performative.
	 */
	NONE("none"), //$NON-NLS-1$
	/** Accept a proposal.
	 */
	ACCEPT_PROPOSAL("accept-proposal"), //$NON-NLS-1$
	/** Agree.
	 */
	AGREE("agree"), //$NON-NLS-1$
	/** Cancel.
	 */
	CANCEL("cancel"), //$NON-NLS-1$
	/** Call-for-proposal.
	 */
	CFP("cfp"), //$NON-NLS-1$
	/** Confirm.
	 */
	CONFIRM("confirm"), //$NON-NLS-1$
	/** Disconfirm.
	 */
	DISCONFIRM("disconfirm"), //$NON-NLS-1$
	/** Failure.
	 */
	FAILURE("failure"), //$NON-NLS-1$
	/** Inform.
	 */
	INFORM("inform"), //$NON-NLS-1$
	/** Conditional inform.
	 */
	INFORM_IF("inform-if"), //$NON-NLS-1$
	/** Inform reference.
	 */
	INFORM_REF("inform-ref"), //$NON-NLS-1$
	/** Not understood.
	 */
	NOT_UNDERSTOOD("not-understood"), //$NON-NLS-1$
	/** Propose.
	 */
	PROPOSE("propose"), //$NON-NLS-1$  
	/** Conditional query.
	 */
	QUERY_IF("query-if"), //$NON-NLS-1$
	/** Query reference.
	 */
	QUERY_REF("query-ref"), //$NON-NLS-1$ 
	/** Refuse.
	 */
	REFUSE("refuse"), //$NON-NLS-1$
	/** Reject a proposal.
	 */
	REJECT_PROPOSAL("reject-proposal"), //$NON-NLS-1$
	/** Request.
	 */
	REQUEST("request"), //$NON-NLS-1$ 
	/** Conditional request.
	 */
	REQUEST_WHEN("request-when"), //$NON-NLS-1$
	/** Request.
	 */
	REQUEST_WHENEVER("request-whenever"), //$NON-NLS-1$
	/** Subscribe.
	 */
	SUBSCRIBE("subscribe"), //$NON-NLS-1$
	/** Proxy.
	 */
	PROXY("proxy"), //$NON-NLS-1$ 
	/** Propagate.
	 */
	PROPAGATE("propagate"); //$NON-NLS-1$

	private final String name;

	Performative(String name) {
		this.name = name;
	}

	/** Replies the FIPA performative.
	 * @return the string name of the performative.
	 */
	@Pure
	public String getFipaName() {
		return this.name;
	}


	/** Replies the default performative.
	 *
	 * @return the default performative.
	 */
	@Pure
	public static Performative getDefault() {
		return NONE;
	}
	
	/** Replies the Json string representation of this performative.
	 *
	 * @return the Json string representation.
	 */
	@JsonValue
	@Pure
	public String toJsonString() {
		return name().toLowerCase();
	}

	/** Parse the given case insensitive string for obtaining the performative.
	 *
	 * @param name the string to parse.
	 * @return the performative.
	 * @throws NullPointerException when the specified name is null
	 */
	@JsonCreator
	@Pure
	public static Performative valueOfCaseInsensitive(String name) {
		if (Strings.isNullOrEmpty(name)) {
			throw new NullPointerException("name is null"); //$NON-NLS-1$
		}
		final String ucname = name.toUpperCase();
		final String lcname = name.toLowerCase();
		try {
			for (final Performative representation : values()) {
				if (ucname.equals(representation.name())) {
					return representation;
				}
				if (lcname.equals(representation.getFipaName())) {
					return representation;
				}
			}
		} catch (Throwable exception) {
			//
		}
		throw new IllegalArgumentException("illegal value for name: " + name); //$NON-NLS-1$
	}	

	/** Replies the Json labels for the types of performative.
	 *
	 * @return the labels.
	 */
	@Pure
	public static String getJsonLabels() {
		final StringBuilder buffer = new StringBuilder();
		boolean first = true;
		for (final Performative type : values()) {
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
