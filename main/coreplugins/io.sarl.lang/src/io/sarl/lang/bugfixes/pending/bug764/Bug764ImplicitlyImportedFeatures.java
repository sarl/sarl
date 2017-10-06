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

package io.sarl.lang.bugfixes.pending.bug764;

import java.util.List;

import com.google.inject.Singleton;

import io.sarl.lang.scoping.SARLImplicitlyImportedFeatures;


/** Provider of the implicitly imported features in the SARL language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@Singleton
public class Bug764ImplicitlyImportedFeatures extends SARLImplicitlyImportedFeatures {

	/** Construct the provider.
	 */
	public Bug764ImplicitlyImportedFeatures() {
		super();
	}

	@Override
	protected List<Class<?>> getExtensionClasses() {
		final List<Class<?>> xtextList = super.getExtensionClasses();
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

