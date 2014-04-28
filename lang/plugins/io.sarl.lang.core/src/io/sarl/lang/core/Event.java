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
package io.sarl.lang.core;

import java.io.Serializable;

/**
 * Elementary interaction unit inside an {@link EventSpace} An event is the
 * specification of some occurrence in a Space that may potentially trigger
 * effects by a listener. Within a Space, the notion of {@link Scope} enables to
 * precisely control/filter the potential recipients of an event.
 * 
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class Event implements Serializable {

	private static final long serialVersionUID = -9077741474332211244L;

	private Address source = null;

	/**
	 * The source of the event.
	 * 
	 * @return the source of the event
	 */
	public Address getSource() {
		return this.source;
	}

	/**
	 * The address of the source of this event.
	 * 
	 * @param source
	 */
	public void setSource(Address source) {
		this.source = source;
	}

	/**
	 * Returns a String representation of the Event E1 attributes only.
	 * @return the string representation of the attributes of this Event.
	 */
	protected String attributesToString() {
		StringBuilder result = new StringBuilder();
		result.append("source = "); //$NON-NLS-1$
		result.append(this.source);
		return result.toString();
	}
	
	@Override
	public String toString() {
		return getClass().getSimpleName()+" ["+attributesToString()+"]"; //$NON-NLS-1$ //$NON-NLS-2$
	}
}
