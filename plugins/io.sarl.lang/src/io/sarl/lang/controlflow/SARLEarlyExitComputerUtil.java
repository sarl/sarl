/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

package io.sarl.lang.controlflow;

import io.sarl.lang.annotation.EarlyExit;
import io.sarl.lang.util.Utils;

import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmTypeReference;

/** Utilities related to early-exit flags.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class SARLEarlyExitComputerUtil {

	private static final String EARLY_EXIT_EVENT = "io.sarl.core.Destroy"; //$NON-NLS-1$

	private SARLEarlyExitComputerUtil() {
		//
	}

	/** Replies if the given event is an event that causes an early exit of the
	 * function was is calling the firing function.
	 *
	 * @param reference - the event reference.
	 * @return <code>true</code> if the event may causes early exit of the function,
	 * otherwise <code>false</code>.
	 */
	public static boolean isEarlyExitEvent(JvmTypeReference reference) {
		if (reference != null) {
			//TODO: Introduce inheritance testing. Should be solved by annotations' introduction in SARL.
			return EARLY_EXIT_EVENT.equals(reference.getIdentifier());
		}
		return false;
	}

	/** Replies if the given statement is annotated with the "early-exist" annotation.
	 *
	 * @param element - the element to test.
	 * @return <code>true</code> if the given element is annotated with the "early-flag"
	 * annotation, otherwise <code>false</code>.
	 */
	public static boolean isEarlyExitAnnotatedElement(Object element) {
		return ((element instanceof JvmAnnotationTarget)
				&& (Utils.hasAnnotation((JvmAnnotationTarget) element, EarlyExit.class)));
	}

}

