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

package io.sarl.lang.compiler;

import org.eclipse.xtend.core.parser.XtendPartialParsingHelper;

/** A customized partial parsing helper that falls eagerly back to a full parse
 * as soon as the new token sequence would be different from the old one.
 * This approach allows to benefit from partial parsing whenever a token content
 * is edited (e.g. the content of a comment, identifier or string literal)
 * while not producing bogus lookahead information.
 *
 * <p>Mostly copied and refactored from the default implementation.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.9
 */
public class SarlPartialParserHelper extends XtendPartialParsingHelper {
	//
}
