/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.bspl.lang.bspl.impl;

import java.util.Collection;

/**
 * Utilities for custom EOjbects.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public final class CustomEObjects {

	private CustomEObjects() {
		//
	}

	/** Replies if the modifiers correspond to an input parameter. An input parameter is marked with {@code in} or {@code any} or without other modifier.
	 * 
	 * @param modifiers the list of modifiers to test.
	 * @return {@code true} if the modifiers correspond to an input parameter.
	 */
	public static boolean isInputModifier(Collection<String> modifiers) {
		var empty = true;
		for (final var modifier : modifiers) {
			if ("in".equals(modifier) || "any".equals(modifier)) { //$NON-NLS-1$ //$NON-NLS-2$
				return true;
			}
			if ("nil".equals(modifier) || "opt".equals(modifier)) { //$NON-NLS-1$ //$NON-NLS-2$
				return false;
			}
			if ("out".equals(modifier)) { //$NON-NLS-1$
				empty = false;
			}
		}
		return empty;
	}

	/** Replies if the modifiers correspond to an output parameter. An input parameter is marked with {@code out} or {@code any} or without other modifier.
	 * 
	 * @param modifiers the list of modifiers to test.
	 * @return {@code true} if the modifiers correspond to an input parameter.
	 */
	public static boolean isOutputModifier(Collection<String> modifiers) {
		var empty = true;
		for (final var modifier : modifiers) {
			if ("out".equals(modifier) || "any".equals(modifier)) { //$NON-NLS-1$ //$NON-NLS-2$
				return true;
			}
			if ("nil".equals(modifier) || "opt".equals(modifier)) { //$NON-NLS-1$ //$NON-NLS-2$
				return false;
			}
			if ("in".equals(modifier)) { //$NON-NLS-1$
				empty = false;
			}
		}
		return empty;
	}

	/** Replies if the modifiers correspond to an input/output parameter. An input/input parameter is marked with {@code any} or ({@code in} and {@code out}) or without other modifier.
	 * 
	 * @param modifiers the list of modifiers to test.
	 * @return {@code true} if the modifiers correspond to an input parameter.
	 */
	public static boolean isAnyModifier(Collection<String> modifiers) {
		var in = false;
		var out = false;
		var empty = true;
		for (final var modifier : modifiers) {
			if ("any".equals(modifier)) { //$NON-NLS-1$
				return true;
			}
			if ("nil".equals(modifier) || "opt".equals(modifier)) { //$NON-NLS-1$ //$NON-NLS-2$
				return false;
			}
			if ("out".equals(modifier)) { //$NON-NLS-1$
				out = true;
				empty = false;
			} else if ("in".equals(modifier)) { //$NON-NLS-1$
				in = true;
				empty = false;
			}
		}
		return empty || (in && out);
	}

	/** Replies if the modifiers correspond to a key parameter. A key parameter is marked with {@code key}.
	 * 
	 * @param modifiers the list of modifiers to test.
	 * @return {@code true} if the modifiers correspond to an input parameter.
	 */
	public static boolean isKeyModifier(Collection<String> modifiers) {
		return modifiers.contains("key"); //$NON-NLS-1$
	}

	/** Replies if the modifiers correspond to a nil parameter. A nil parameter is marked with {@code nil}.
	 * 
	 * @param modifiers the list of modifiers to test.
	 * @return {@code true} if the modifiers correspond to an input parameter.
	 */
	public static boolean isNilModifier(Collection<String> modifiers) {
		return modifiers.contains("nil"); //$NON-NLS-1$
	}

	/** Replies if the modifiers correspond to an optional parameter. An optional parameter is marked with {@code opt}.
	 * 
	 * @param modifiers the list of modifiers to test.
	 * @return {@code true} if the modifiers correspond to an input parameter.
	 */
	public static boolean isOptionalModifier(Collection<String> modifiers) {
		return modifiers.contains("opt"); //$NON-NLS-1$
	}

}
