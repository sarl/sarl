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

import java.util.Arrays;
import java.util.function.BiConsumer;

import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtext.common.types.JvmGenericType;

import com.google.common.base.Strings;

import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerTypeFragment;
import io.sarl.lang.jvmmodel.fragments.oop.IClassInferrerFragment;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.util.Utils;

/** Fragment for inferred the classes to the JVM model.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class ClassInferrerFragment extends AbstractJvmModelInferrerTypeFragment implements IClassInferrerFragment {

	@Override
	public void transform(XtendClass source, JvmGenericType inferredJvmType, IBaseJvmModelInferrer baseInferrer,
			BiConsumer<XtendClass, JvmGenericType> inheritedTransformer) {
		// Issue #356: do not generate if the class has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Issue #363: do not generate the class if the SARL library is incompatible.
		if (!Utils.isCompatibleSARLLibraryOnClasspath(this.jvmTypeReferences, source)) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final var context = baseInferrer.openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlConstructor.class, SarlAction.class));
		try {
			// Initialize the context with inheriting features
			Utils.populateInheritanceContext(
					inferredJvmType,
					source.getExtends(),
					source.getImplements(),
					context.getInheritedFinalOperations(),
					context.getInheritedOverridableOperations(),
					null,
					context.getInheritedOperationsToImplement(),
					null,
					this.sarlSignatureProvider);

			// Standard OOP generation
			inheritedTransformer.accept(source, inferredJvmType);

			// Override the visibility
			setVisibility(inferredJvmType, source);

			// Change the injectable flag
			context.setInjectable(inferredJvmType.getExtendedClass());

			// Add SARL synthetic functions
			appendSyntheticDefaultValuedParameterMethods(
					baseInferrer, source, inferredJvmType, true, context);

			// Add functions dedicated to comparisons (equals, hashCode, etc.)
			appendComparisonFunctions(baseInferrer, context, source, inferredJvmType);

			// Add clone functions if the generated type is cloneable
			appendCloneFunctionIfCloneable(baseInferrer, context, source, inferredJvmType);

			// Add the default constructors for the behavior, if not already added
			appendDefaultConstructors(baseInferrer, source, inferredJvmType);

			// Add serialVersionUID field if the generated type is serializable
			appendSerialNumberIfSerializable(baseInferrer, context, source, inferredJvmType);

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
