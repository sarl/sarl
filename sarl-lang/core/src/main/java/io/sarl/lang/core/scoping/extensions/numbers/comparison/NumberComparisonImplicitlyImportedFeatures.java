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

package io.sarl.lang.core.scoping.extensions.numbers.comparison;

import java.util.List;

import jakarta.inject.Singleton;

/** Provide static functions related to the comparison of numbers.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.7
 * @see "https://github.com/sarl/sarl/issues/767"
 */
@Singleton
public class NumberComparisonImplicitlyImportedFeatures {

	/** Constructor.
	 */
	public NumberComparisonImplicitlyImportedFeatures() {
		//
	}

	/** Fill the given list with the implicitly imported features.
	 *
	 * @param features the list to fill.
	 */
	@SuppressWarnings("static-method")
	public void getImportedFeatures(List<Class<?>> features) {
		features.add(AtomicIntegerComparisonExtensions.class);
		features.add(AtomicLongComparisonExtensions.class);
		features.add(ByteComparisonExtensions.class);
		features.add(DoubleComparisonExtensions.class);
		features.add(FloatComparisonExtensions.class);
		features.add(IntegerComparisonExtensions.class);
		features.add(LongComparisonExtensions.class);
		features.add(NumberComparisonExtensions.class);
		features.add(PrimitiveByteComparisonExtensions.class);
		features.add(PrimitiveDoubleComparisonExtensions.class);
		features.add(PrimitiveFloatComparisonExtensions.class);
		features.add(PrimitiveIntComparisonExtensions.class);
		features.add(PrimitiveLongComparisonExtensions.class);
		features.add(PrimitiveShortComparisonExtensions.class);
		features.add(ShortComparisonExtensions.class);
	}

}
