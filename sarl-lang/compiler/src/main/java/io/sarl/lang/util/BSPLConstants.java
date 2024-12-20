/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.util;

import java.util.Set;
import java.util.TreeSet;

/**
 * Constants for BSPL protocol components.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @author $Author: mbaldoni$
 * @author $Author: cbaroglio$
 * @author $Author: rmicalizio$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public final class BSPLConstants {

	/** BSPL keyword {@code in}.
	 */
	public static final String IN = "in"; //$NON-NLS-1$
	
	/** BSPL keyword {@code out}.
	 */
	public static final String OUT = "out"; //$NON-NLS-1$

	/** BSPL keyword {@code nil}.
	 */
	public static final String NIL = "nil"; //$NON-NLS-1$

	/** BSPL keyword {@code any}.
	 */
	public static final String ANY = "any"; //$NON-NLS-1$

	/** BSPL keyword {@code opt}.
	 */
	public static final String OPT = "opt"; //$NON-NLS-1$

	/** BSPL keyword {@code key}.
	 */
	public static final String KEY = "key"; //$NON-NLS-1$

	/** List of the modifiers that may be used in BSPL protocol specification.
	 *
	 * @see #MESSAGE_TARGET_MODIFIERS
	 */
	public static final Set<String> MODIFIERS = new TreeSet<>();
	
	/** List of the modifiers that may be used for a specific message target in BSPL protocol specification.
	 * It is a subset of {@link #MODIFIERS}.
	 *
	 * @see #MODIFIERS
	 */
	public static final Set<String> MESSAGE_TARGET_MODIFIERS = new TreeSet<>();

	static {
		MODIFIERS.add(IN);
		MODIFIERS.add(OUT);
		MODIFIERS.add(NIL);
		MODIFIERS.add(ANY);
		MODIFIERS.add(OPT);
		MODIFIERS.add(KEY);

		MESSAGE_TARGET_MODIFIERS.add(IN);
		MESSAGE_TARGET_MODIFIERS.add(OUT);
	}

	private BSPLConstants() {
		//
	}

}
