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

package io.sarl.lang.extralanguage.compiler;

import org.eclipse.xtext.xbase.lib.Functions.Function0;

/** Provider of keywords for the extra language.
 *
 * @author $Author: sgalland$
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.8
 */
public interface IExtraLanguageKeywordProvider {

	/** Replies the lambda that permits to get the keyword that is equivalent to "this".
	 *
	 * @return the lambda.
	 */
	Function0<? extends String> getThisKeywordLambda();

	/** Replies the lambda that permits to get the keyword that is equivalent to "super".
	 *
	 * @return the lambda.
	 */
	Function0<? extends String> getSuperKeywordLambda();

	/** Replies the lambda that permits to get the keyword that is equivalent to "null".
	 *
	 * @return the lambda.
	 */
	Function0<? extends String> getNullKeywordLambda();

}
