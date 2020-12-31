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

package io.sarl.lang.scoping.extensions.numbers.cast;

import java.util.List;
import javax.inject.Singleton;

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
		features.add(AtomicLongCastExtensions.class);
		features.add(AtomicIntegerCastExtensions.class);
		features.add(AtomicDoubleCastExtensions.class);
		features.add(BigIntegerCastExtensions.class);
		features.add(BigDecimalCastExtensions.class);
		features.add(NumberCastExtensions.class);
		features.add(PrimitiveByteCastExtensions.class);
		features.add(PrimitiveDoubleCastExtensions.class);
		features.add(PrimitiveFloatCastExtensions.class);
		features.add(PrimitiveIntCastExtensions.class);
		features.add(PrimitiveLongCastExtensions.class);
		features.add(PrimitiveShortCastExtensions.class);
	}

}
