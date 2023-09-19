/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.typesystem.cast;

import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.typesystem.computation.ILinkingCandidate;

/** Linking candidate for cast operator.
*
* @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
* @version compiler 0.13.0 20230919-093056
* @mavengroupid io.sarl.lang
* @mavenartifactid compiler
* @since 0.9
*/
public interface ICastOperatorLinkingCandidate extends ILinkingCandidate {

	/**
	 * Returns the feature that should be called in place of the cast operator.
	 * @return the feature.
	 */
	JvmOperation getOperation();

	/**
	 * Returns the receiver that should be used for calling the feature.
	 * @return the receiver expression, or {@code null} if no receiver.
	 */
	XExpression getReceiver();

	/**
	 * Returns the argument that should be used for calling the feature.
	 * @return the argument expression, or {@code null} if none.
	 */
	XExpression getArgument();

}
