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

package io.sarl.lang.extralanguage;

import java.util.Collection;
import java.util.function.Predicate;

/** Tool for obtaining all the contributions as an extra-language components.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.8
 */
public interface IExtraLanguageContributions {

	/** Replies all the registered contributions.
	 *
	 * @return the collection of the contributions.
	 */
	Collection<IExtraLanguageContribution> getContributions();

	/** Change the lambda that is used for checking if a extra-language is enabled.
	 *
	 * @param checker the checker.
	 */
	void setContributionChecker(Predicate<IExtraLanguageContribution> checker);

}
