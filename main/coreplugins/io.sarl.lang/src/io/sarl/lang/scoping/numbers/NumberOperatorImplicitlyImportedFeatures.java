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

/** Provide static functions related to the operators on numbers.
 *
 * <p>The following extensions have been added due to issue:
 * <a ref="https://github.com/eclipse/xtext-extras/issues/186">Xtext Issue 186</a>.
 * Since the possible patch was discarded from Xbase, these extensions are present within SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@Singleton
public class NumberOperatorImplicitlyImportedFeatures {

	/** Fill the given list with the implicitly imported features.
	 *
	 * @param features the list to fill.
	 */
	@SuppressWarnings("static-method")
	public void getImportedFeatures(List<Class<?>> features) {
		features.add(PrimitiveByteOperatorExtensions.class);
		features.add(PrimitiveIntOperatorExtensions.class);
		features.add(PrimitiveShortOperatorExtensions.class);
		features.add(PrimitiveDoubleOperatorExtensions.class);
		features.add(PrimitiveFloatOperatorExtensions.class);
		features.add(PrimitiveLongOperatorExtensions.class);
		features.add(IntegerOperatorExtensions.class);
		features.add(ShortOperatorExtensions.class);
		features.add(DoubleOperatorExtensions.class);
		features.add(FloatOperatorExtensions.class);
		features.add(ByteOperatorExtensions.class);
		features.add(LongOperatorExtensions.class);
		features.add(AtomicLongOperatorExtensions.class);
		features.add(AtomicIntegerOperatorExtensions.class);
		features.add(NumberOperatorExtensions.class);
	}

}
