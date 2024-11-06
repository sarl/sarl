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

package io.sarl.lang.extralanguage.compiler;

import org.eclipse.xtext.xbase.lib.Procedures.Procedure3;

/** Initializer for the extra language converters.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.6
 */
@FunctionalInterface
public interface IExtraLanguageConversionInitializer {

	/** Initialize the conversions mapping.
	 *
	 * @param result the function to invoke for initializing the element.
	 *     The first formal parameter is the simple name of the source.
	 *     The second formal parameter is the full name of source of the conversion.
	 *     The third formal parameter is the target of the conversion.
	 */
	void initializeConversions(Procedure3<? super String, ? super String, ? super String> result);

}
