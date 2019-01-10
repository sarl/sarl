/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import javax.inject.Inject;

import com.google.inject.Singleton;
import org.eclipse.xtext.xbase.scoping.batch.ImplicitlyImportedFeatures;

import io.sarl.lang.bugfixes.pending.pr106.PR106ArrayExtensions;
import io.sarl.lang.bugfixes.pending.pr106.PR106ArrayLiterals;
import io.sarl.lang.scoping.extensions.numbers.arithmetic.NumberArithmeticImplicitlyImportedFeatures;
import io.sarl.lang.scoping.extensions.numbers.cast.NumberCastImplicitlyImportedFeatures;
import io.sarl.lang.scoping.extensions.numbers.comparison.NumberComparisonImplicitlyImportedFeatures;
import io.sarl.lang.scoping.extensions.time.TimeExtensions;


/** Provider of the implicitly imported features in the SARL language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class SARLImplicitlyImportedFeatures extends ImplicitlyImportedFeatures {

	@Inject
	private NumberComparisonImplicitlyImportedFeatures numberComparisonFeatures;

	@Inject
	private NumberArithmeticImplicitlyImportedFeatures numberArithmeticFeatures;

	@Inject
	private NumberCastImplicitlyImportedFeatures numberCastFeatures;

	/** Construct the provider.
	 */
	public SARLImplicitlyImportedFeatures() {
		super();
	}

	@Override
	protected List<Class<?>> getStaticImportClasses() {
		final List<Class<?>> xtextList = super.getStaticImportClasses();
		// Insert at the beginning for ensuring the SARL extension is selected before any Xtext extension.
		xtextList.add(0, PR106ArrayLiterals.class);
		return xtextList;
	}

	@Override
	protected List<Class<?>> getExtensionClasses() {
		final List<Class<?>> xtextList = super.getExtensionClasses();
		// Insert at the beginning for ensuring the SARL extension is selected before any Xtext extension.
		xtextList.add(0, PR106ArrayExtensions.class);
		xtextList.add(0, TimeExtensions.class);

		// Add features related to numbers.
		this.numberComparisonFeatures.getImportedFeatures(xtextList);
		this.numberArithmeticFeatures.getImportedFeatures(xtextList);
		this.numberCastFeatures.getImportedFeatures(xtextList);

		return xtextList;
	}

}

