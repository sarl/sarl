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

package io.sarl.lang.jvmmodel.fragments.bspl.impl;

import java.util.Collections;

import com.google.common.base.Strings;

import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.JvmGenericTypeFactory;
import io.sarl.lang.jvmmodel.fragments.JvmGenericTypeProvider;
import io.sarl.lang.jvmmodel.fragments.bspl.IProtocolInferrerFragment;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlProtocol;

/** Fragment for inferred the protocols to the JVM model.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class ProtocolInferrerFragment extends AbstractJvmModelInferrerFragment implements IProtocolInferrerFragment {

	@Override
	public void transform(SarlProtocol source, JvmGenericTypeProvider inferredJvmTypes, IBaseJvmModelInferrer baseInferrer) {
		// Issue #356: do not generate if the space has no name.
		assert source != null;
		assert inferredJvmTypes != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}

		// First type
		
		// Create the generation context that is used by the other transformation functions.
		final var type0 = inferredJvmTypes.getGenericType(0);
		final var context0 = baseInferrer.openContext(source, type0, Collections.singleton(SarlField.class));
		try {
			type0.setInterface(true);
			type0.setAbstract(false);
			setVisibility(type0, source);
			type0.setStatic(false);
			type0.setStrictFloatingPoint(false);
			type0.setFinal(false);
			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(type0);
		} finally {
			baseInferrer.closeContext(context0);
		}

		// Second type
		
		// Create the generation context that is used by the other transformation functions.
		final var type1 = inferredJvmTypes.getGenericType(1);
		final var context1 = baseInferrer.openContext(source, type1, Collections.singleton(SarlField.class));
		try {
			type1.setInterface(false);
			type1.setAbstract(false);
			setVisibility(type1, source);
			type1.setStatic(false);
			type1.setStrictFloatingPoint(false);
			type1.setFinal(false);
			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(type0);
		} finally {
			baseInferrer.closeContext(context1);
		}
	}

	@Override
	public void prepareTransform(SarlProtocol source, JvmGenericTypeFactory inferredJvmTypes, IBaseJvmModelInferrer baseInferrer) {
		inferredJvmTypes
			.createReceiver(0, source.getName())
			.createReceiver(1, source.getName() + "Impl"); //$NON-NLS-1$
	}

}
