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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.core;

import org.eclipse.xtext.xbase.lib.Pure;

/** This class defines the protected API of an object that could contains SRE specific data.
 *
 * <p>Any SRE-specific data into this object is assumed to be transient (not serializable).
 *
 * @author <a href="http://www.ciad-lab.fr/author-10836/">St&eacute;phane Galland</a>
 * @version io.sarl.lang.core 0.12.0 20210527-171007
 * @mavengroupid io.sarl.lang
 * @mavenartifactid io.sarl.lang.core
 * @since 0.6
 */
public abstract class SRESpecificDataContainer {

	private transient volatile Object sreSpecificData;

	/** Replies the data associated to this agent trait by the SRE.
	 *
	 * @param <S> the type of the data.
	 * @param type the type of the data.
	 * @return the SRE-specific data.
	 * @since 0.5
	 */
	@Pure
	<S> S $getSreSpecificData(Class<S> type) {
		return type.cast(this.sreSpecificData);
	}

	/** Change the data associated to this agent trait by the SRE.
	 *
	 * @param data the SRE-specific data.
	 * @since 0.5
	 */
	void $setSreSpecificData(Object data) {
		this.sreSpecificData = data;
	}

}
