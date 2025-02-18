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

package io.sarl.lang.jvmmodel.fragments.aop.impl;

import java.util.Collections;
import java.util.TreeSet;

import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmVisibility;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;

import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerTypeFragment;
import io.sarl.lang.jvmmodel.fragments.aop.ICapacityInferrerFragment;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;
import io.sarl.lang.util.Utils;

/** Fragment for inferred the capacities to the JVM model.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class CapacityInferrerFragment extends AbstractJvmModelInferrerTypeFragment implements ICapacityInferrerFragment {

	@Override
	public void transform(SarlCapacity source, JvmGenericType inferredJvmType, IBaseJvmModelInferrer baseInferrer) {
		// Issue #356: do not generate if the capacity has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final var context = baseInferrer.openContext(source, inferredJvmType,
				Collections.singleton(SarlAction.class));
		try {
			// Initialize the context with inheriting features
			Utils.populateInheritanceContext(
					inferredJvmType,
					null,
					source.getExtends(),
					context.getInheritedFinalOperations(),
					context.getInheritedOverridableOperations(),
					null,
					context.getInheritedOperationsToImplement(),
					null,
					this.sarlSignatureProvider);

			// Copy the documentation
			this.jvmTypeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			inferredJvmType.setInterface(true);
			inferredJvmType.setAbstract(true);
			setVisibility(inferredJvmType, source);
			inferredJvmType.setStatic(false);
			inferredJvmType.setStrictFloatingPoint(false);
			inferredJvmType.setFinal(false);

			// Generate the annotations.
			baseInferrer.translateAnnotationsTo(source.getAnnotations(), inferredJvmType);

			// Generate the extended types.
			appendConstrainedExtends(baseInferrer, context, inferredJvmType, Capacity.class, SarlCapacity.class, source.getExtends());

			// Issue #363: do not generate the capacity if the SARL library is incompatible.
			if (Utils.isCompatibleSARLLibraryOnClasspath(this.jvmTypeReferences, source)) {
				// Generate the members of the generated type.
				appendAOPMembers(baseInferrer, inferredJvmType, source, context);
			}

			// Add the @FunctionalInterface
			appendFunctionalInterfaceAnnotation(baseInferrer, inferredJvmType);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(baseInferrer, context, source, inferredJvmType);

			// Add the type of SARL Element
			appendSARLElementType(baseInferrer, source, inferredJvmType);

			// Add the type of SARL Element
			appendInjectableAnnotationIfInjectable(baseInferrer, inferredJvmType, context);

			// Add the Xbase annotation that avoid JaCoCo to analyze the generate code
			appendXbaseGeneratedAnnotation(baseInferrer, inferredJvmType);

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			baseInferrer.closeContext(context);
		}

		// Generate the context aware wrapper
		appendCapacityContextAwareWrapper(baseInferrer, source, inferredJvmType);
	}

	/** Initialize the SARL capacity context-aware wrapper.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 * @since 0.6
	 */
	private void appendCapacityContextAwareWrapper(IBaseJvmModelInferrer baseInferrer,  SarlCapacity source,
			JvmGenericType inferredJvmType) {
		final var innerType = this.jvmTypesFactory.createJvmGenericType();
		innerType.setInterface(false);
		innerType.setAbstract(false);
		innerType.setVisibility(JvmVisibility.PUBLIC);
		innerType.setStatic(true);
		innerType.setStrictFloatingPoint(false);
		innerType.setFinal(false);
		final var innerTypeName = Capacity.ContextAwareCapacityWrapper.class.getSimpleName();
		innerType.setSimpleName(innerTypeName);

		inferredJvmType.getMembers().add(innerType);

		this.jvmTypeBuilder.setDocumentation(innerType, "@ExcludeFromApidoc"); //$NON-NLS-1$

		final var typeParameter = this.jvmTypesFactory.createJvmTypeParameter();
		typeParameter.setName("C"); //$NON-NLS-1$
		final var constraint = this.jvmTypesFactory.createJvmUpperBound();
		constraint.setTypeReference(baseInferrer.getJvmTypeReferenceBuilder().typeRef(inferredJvmType));
		typeParameter.getConstraints().add(constraint);
		innerType.getTypeParameters().add(typeParameter);

		final var extendedTypeIterator = inferredJvmType.getExtendedInterfaces().iterator();
		if (extendedTypeIterator.hasNext()) {
			final var extendedType = extendedTypeIterator.next();
			final var superType = baseInferrer.getJvmTypeReferenceBuilder().typeRef(
					extendedType.getQualifiedName() + "$" + innerTypeName, //$NON-NLS-1$
					baseInferrer.getJvmTypeReferenceBuilder().typeRef(typeParameter));
			innerType.getSuperTypes().add(superType);
		}

		innerType.getSuperTypes().add(baseInferrer.getJvmTypeReferenceBuilder().typeRef(inferredJvmType));

		final var constructor = this.jvmTypesFactory.createJvmConstructor();
		constructor.setVisibility(JvmVisibility.PUBLIC);
		innerType.getMembers().add(constructor);
		final var parameter1 = this.jvmTypesFactory.createJvmFormalParameter();
		parameter1.setName("capacity"); //$NON-NLS-1$
		parameter1.setParameterType(baseInferrer.getJvmTypeReferenceBuilder().typeRef(typeParameter));
		constructor.getParameters().add(parameter1);
		final var parameter2 = this.jvmTypesFactory.createJvmFormalParameter();
		parameter2.setName("caller"); //$NON-NLS-1$
		parameter2.setParameterType(baseInferrer.getJvmTypeReferenceBuilder().typeRef(AgentTrait.class));
		constructor.getParameters().add(parameter2);
		setBody(constructor, it -> {
			it.append("super(capacity, caller);"); //$NON-NLS-1$
		});

		final var createdActions = new TreeSet<ActionPrototype>();
		for (final var sourceType : Iterables.concat(
				Collections.singletonList(inferredJvmType),
				Iterables.transform(Iterables.skip(inferredJvmType.getExtendedInterfaces(), 1), it -> {
					return (JvmGenericType) it.getType();
				}))) {
			copyNonStaticPublicJvmOperations(baseInferrer, sourceType, innerType, createdActions, false, (operation, it) -> {
				it.append("try {"); //$NON-NLS-1$
				it.newLine();
				it.append("  ensureCallerInLocalThread();"); //$NON-NLS-1$
				it.newLine();
				it.append("  "); //$NON-NLS-1$
				if (operation.getReturnType() != null && !Objects.equal("void", operation.getReturnType().getIdentifier())) { //$NON-NLS-1$
					it.append("return "); //$NON-NLS-1$
				}
				it.append("this.capacity."); //$NON-NLS-1$
				it.append(operation.getSimpleName());
				it.append("("); //$NON-NLS-1$
				var first = true;
				for (final var fparam : operation.getParameters()) {
					if (first) {
						first = false;
					} else {
						it.append(", "); //$NON-NLS-1$
					}
					it.append(fparam.getName());
				}
				it.append(");"); //$NON-NLS-1$
				it.newLine();
				it.append("} finally {"); //$NON-NLS-1$
				it.newLine();
				it.append("  resetCallerInLocalThread();"); //$NON-NLS-1$
				it.newLine();
				it.append("}"); //$NON-NLS-1$
			});
		}
	}

}
