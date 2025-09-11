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

package io.sarl.lang.scoping;

import java.util.List;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.xtext.xbase.scoping.batch.ImplicitlyImportedFeatures;

import io.sarl.lang.core.scoping.extensions.cast.GeneralCastImplicitlyImportedFeatures;
import io.sarl.lang.core.scoping.extensions.numbers.cast.NumberCastImplicitlyImportedFeatures;
import io.sarl.lang.core.scoping.extensions.numbers.comparison.NumberComparisonImplicitlyImportedFeatures;
import io.sarl.lang.core.scoping.extensions.time.TimeExtensions;
import io.sarl.lang.core.scoping.extensions.uuid.UUIDExtensions;


/** Provider of the implicitly imported features in the SARL language.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 */
@Singleton
public class SARLImplicitlyImportedFeatures extends ImplicitlyImportedFeatures {

	@Inject
	private NumberComparisonImplicitlyImportedFeatures numberComparisonFeatures;

	/*XXX: @Inject
	private NumberArithmeticImplicitlyImportedFeatures numberArithmeticFeatures;*/

	@Inject
	private NumberCastImplicitlyImportedFeatures numberCastFeatures;

	@Inject
	private GeneralCastImplicitlyImportedFeatures generalCastFeatures;

	/** Construct the provider.
	 */
	public SARLImplicitlyImportedFeatures() {
		super();
	}

	@Override
	protected List<Class<?>> getExtensionClasses() {
		final var xtextList = super.getExtensionClasses();

		xtextList.add(0, TimeExtensions.class);

		// Add features related to numbers.
		this.numberComparisonFeatures.getImportedFeatures(xtextList);
		//TODO: this.numberArithmeticFeatures.getImportedFeatures(xtextList);
		this.numberCastFeatures.getImportedFeatures(xtextList);
		this.generalCastFeatures.getImportedFeatures(xtextList);

		// Insert at the beginning for ensuring the SARL extension is selected before any Xtext extension.
		xtextList.add(0, UUIDExtensions.class);

		return xtextList;
	}

}

