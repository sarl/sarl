/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

package io.sarl.lang.scoping.numbers;

import java.util.List;

import com.google.inject.Singleton;

/** Provide static functions related to the casting of numbers.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/sarl/sarl/issues/767"
 */
@Singleton
public class NumberCastImplicitlyImportedFeatures {

	/** Fill the given list with the implicitly imported features.
	 *
	 * @param features the list to fill.
	 */
	@SuppressWarnings("static-method")
	public void getImportedFeatures(List<Class<?>> features) {
		features.add(IntegerCastExtensions.class);
		features.add(PrimitiveShortCastExtensions.class);
		features.add(AtomicLongCastExtensions.class);
		features.add(LongCastExtensions.class);
		features.add(AtomicIntegerCastExtensions.class);
		features.add(PrimitiveByteCastExtensions.class);
		features.add(PrimitiveDoubleCastExtensions.class);
		features.add(ShortCastExtensions.class);
		features.add(ByteCastExtensions.class);
		features.add(PrimitiveFloatCastExtensions.class);
		features.add(FloatCastExtensions.class);
		features.add(DoubleCastExtensions.class);
		features.add(PrimitiveLongCastExtensions.class);
		features.add(PrimitiveIntCastExtensions.class);
	}

}
