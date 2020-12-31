/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

// THIS FILE IS AUTO-GENERATED. DO NOT CHANGE MANUALLY

package io.sarl.lang.scoping.extensions.numbers.arithmetic;

import java.util.List;
import javax.inject.Singleton;

/** Provide static functions related to the arithmetic of numbers.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/sarl/sarl/issues/767"
 */
@Singleton
public class NumberArithmeticImplicitlyImportedFeatures {

	/** Fill the given list with the implicitly imported features.
	 *
	 * @param features the list to fill.
	 */
	@SuppressWarnings("static-method")
	public void getImportedFeatures(List<Class<?>> features) {
		features.add(AtomicIntegerArithmeticExtensions.class);
		features.add(AtomicLongArithmeticExtensions.class);
		features.add(ByteArithmeticExtensions.class);
		features.add(DoubleArithmeticExtensions.class);
		features.add(FloatArithmeticExtensions.class);
		features.add(IntegerArithmeticExtensions.class);
		features.add(LongArithmeticExtensions.class);
		features.add(NumberArithmeticExtensions.class);
		features.add(PrimitiveByteArithmeticExtensions.class);
		features.add(PrimitiveDoubleArithmeticExtensions.class);
		features.add(PrimitiveFloatArithmeticExtensions.class);
		features.add(PrimitiveIntArithmeticExtensions.class);
		features.add(PrimitiveLongArithmeticExtensions.class);
		features.add(PrimitiveShortArithmeticExtensions.class);
		features.add(ShortArithmeticExtensions.class);
	}

}
