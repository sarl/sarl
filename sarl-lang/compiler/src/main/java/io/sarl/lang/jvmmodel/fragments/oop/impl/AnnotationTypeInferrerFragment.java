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

package io.sarl.lang.jvmmodel.fragments.oop.impl;

import java.util.Collections;
import java.util.function.BiConsumer;

import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtext.common.types.JvmAnnotationType;

import com.google.common.base.Strings;

import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerTypeFragment;
import io.sarl.lang.jvmmodel.fragments.oop.IAnnotationTypeInferrerFragment;
import io.sarl.lang.sarl.SarlEnumLiteral;
import io.sarl.lang.util.Utils;

/** Fragment for inferred the annotation types to the JVM model.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.15
 */
public class AnnotationTypeInferrerFragment extends AbstractJvmModelInferrerTypeFragment implements IAnnotationTypeInferrerFragment {

	@Override
	public void transform(XtendAnnotationType source, JvmAnnotationType inferredJvmType, IBaseJvmModelInferrer baseInferrer,
			BiConsumer<XtendAnnotationType, JvmAnnotationType> inheritedTransformer) {
		// Issue #356: do not generate if the annotation type has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Issue #363: do not generate the annotation if the SARL library is incompatible.
		if (!Utils.isCompatibleSARLLibraryOnClasspath(this.jvmTypeReferences, source)) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final var context = baseInferrer.openContext(source, inferredJvmType,
				Collections.singleton(SarlEnumLiteral.class));
		try {
			// Standard OOP generation
			inheritedTransformer.accept(source, inferredJvmType);

			// Override the visibility
			setVisibility(inferredJvmType, source);

			// Add SARL synthetic functions
			appendSyntheticDefaultValuedParameterMethods(
					baseInferrer, source, inferredJvmType, true, context);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(baseInferrer, context, source, inferredJvmType);

			// Add the type of SARL Element
			appendSARLElementType(baseInferrer, source, inferredJvmType);

			// Add the type of SARL Element
			appendInjectableAnnotationIfInjectable(baseInferrer, inferredJvmType, context);

			// Add the Xbase annotation that avoid JaCoCo to analyze the generate code
			appendXbaseGeneratedAnnotation(baseInferrer, inferredJvmType);
		} finally {
			baseInferrer.closeContext(context);
		}
	}

}
