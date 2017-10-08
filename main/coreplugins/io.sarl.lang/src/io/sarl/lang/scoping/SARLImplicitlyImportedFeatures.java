/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import com.google.inject.Singleton;
import org.eclipse.xtext.xbase.scoping.batch.ImplicitlyImportedFeatures;

import io.sarl.lang.scoping.batch.SARLMapExtensions;
import io.sarl.lang.scoping.batch.SARLTimeExtensions;
import io.sarl.lang.scoping.numbers.AtomicIntegerExtensions;
import io.sarl.lang.scoping.numbers.AtomicLongExtensions;
import io.sarl.lang.scoping.numbers.ByteExtensions;
import io.sarl.lang.scoping.numbers.DoubleExtensions;
import io.sarl.lang.scoping.numbers.FloatExtensions;
import io.sarl.lang.scoping.numbers.IntegerExtensions;
import io.sarl.lang.scoping.numbers.LongExtensions;
import io.sarl.lang.scoping.numbers.NumberExtensions;
import io.sarl.lang.scoping.numbers.PrimitiveByteExtensions;
import io.sarl.lang.scoping.numbers.PrimitiveDoubleExtensions;
import io.sarl.lang.scoping.numbers.PrimitiveFloatExtensions;
import io.sarl.lang.scoping.numbers.PrimitiveIntExtensions;
import io.sarl.lang.scoping.numbers.PrimitiveLongExtensions;
import io.sarl.lang.scoping.numbers.PrimitiveShortExtensions;
import io.sarl.lang.scoping.numbers.ShortExtensions;


/** Provider of the implicitly imported features in the SARL language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class SARLImplicitlyImportedFeatures extends ImplicitlyImportedFeatures {

	/** Construct the provider.
	 */
	public SARLImplicitlyImportedFeatures() {
		super();
	}

	@Override
	protected List<Class<?>> getExtensionClasses() {
		final List<Class<?>> xtextList = super.getExtensionClasses();
		// Insert at the beginning for ensuring the SARL extension is selected before any Xtext extension.
		xtextList.add(0, SARLMapExtensions.class);
		xtextList.add(0, SARLTimeExtensions.class);

		// The following extensions have been added due to issue:
		// https://github.com/eclipse/xtext-extras/issues/186
		// Since the possible patch was discarded from Xbase, these extensions are present within SARL.
		xtextList.add(PrimitiveShortExtensions.class);
		xtextList.add(PrimitiveByteExtensions.class);
		xtextList.add(PrimitiveFloatExtensions.class);
		xtextList.add(PrimitiveIntExtensions.class);
		xtextList.add(PrimitiveDoubleExtensions.class);
		xtextList.add(PrimitiveLongExtensions.class);
		xtextList.add(ShortExtensions.class);
		xtextList.add(LongExtensions.class);
		xtextList.add(FloatExtensions.class);
		xtextList.add(ByteExtensions.class);
		xtextList.add(IntegerExtensions.class);
		xtextList.add(DoubleExtensions.class);
		xtextList.add(AtomicLongExtensions.class);
		xtextList.add(AtomicIntegerExtensions.class);
		xtextList.add(NumberExtensions.class);
		return xtextList;
	}

}

