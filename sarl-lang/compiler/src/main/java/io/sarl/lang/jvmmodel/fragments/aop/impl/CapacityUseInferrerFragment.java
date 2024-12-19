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

package io.sarl.lang.jvmmodel.fragments.aop.impl;

import java.util.Collections;

import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.core.AtomicSkillReference;
import io.sarl.lang.core.annotation.ImportedCapacityFeature;
import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.ICapacityUseInferrerFragment;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.util.Utils;

/** Fragment for inferred the capacity uses to the JVM model.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class CapacityUseInferrerFragment extends AbstractJvmModelInferrerFragment implements ICapacityUseInferrerFragment {

	@Override
	public void transform(SarlCapacityUses source, JvmGenericType container,
			IBaseJvmModelInferrer baseInferrer) {
		final var context = baseInferrer.getContext(container);
		if (context == null) {
			return;
		}
		for (final var capacityType : source.getCapacities()) {
			final var type = capacityType.getType();
			if (type instanceof JvmGenericType
					/*&& this.inheritanceHelper.isSubTypeOf(capacityType, Capacity.class, SarlCapacity.class)*/
					&& !context.getGeneratedCapacityUseFields().contains(capacityType.getIdentifier())) {
				// Generate the buffer field
				final var fieldName = Utils.createNameForHiddenCapacityImplementationAttribute(capacityType.getIdentifier());
				final var field = this.jvmTypesFactory.createJvmField();
				container.getMembers().add(field);
				field.setVisibility(JvmVisibility.PRIVATE);
				field.setSimpleName(fieldName);
				field.setTransient(true);
				final var skillClearableReference = this.jvmTypeReferences.getTypeForName(AtomicSkillReference.class, container);
				field.setType(skillClearableReference);

				this.associator.associatePrimary(source, field);

				addAnnotationSafe(baseInferrer, field, Extension.class);
				try {
					field.getAnnotations().add(annotationClassRef(baseInferrer, ImportedCapacityFeature.class,
							Collections.singletonList(capacityType)));
				} catch (Throwable ex) {
					// Ignore this error
				}
				appendGeneratedAnnotation(baseInferrer, field, baseInferrer.getContext(container));

				// Generate the calling function
				final var methodName = Utils.createNameForHiddenCapacityImplementationCallingMethodFromFieldName(
						fieldName);
				final var operation = this.jvmTypesFactory.createJvmOperation();
				container.getMembers().add(operation);
				operation.setVisibility(JvmVisibility.PRIVATE);
				operation.setReturnType(cloneWithTypeParametersAndProxies(capacityType, operation, baseInferrer));
				operation.setSimpleName(methodName);

				this.associator.associatePrimary(source, operation);

				setBody(operation, it -> {
					it.append("if (this.").append(fieldName).append(" == null || this."); //$NON-NLS-1$ //$NON-NLS-2$
					it.append(fieldName).append(".get() == null) {"); //$NON-NLS-1$
					it.increaseIndentation();
					it.newLine();
					it.append("this.").append(fieldName).append(" = ") //$NON-NLS-1$ //$NON-NLS-2$
					.append(SarlUtils.HIDDEN_MEMBER_CHARACTER).append("getSkill("); //$NON-NLS-1$
					it.append(capacityType.getType()).append(".class);"); //$NON-NLS-1$
					it.decreaseIndentation();
					it.newLine();
					it.append("}"); //$NON-NLS-1$
					it.newLine();
					it.append("return ").append(SarlUtils.HIDDEN_MEMBER_CHARACTER) //$NON-NLS-1$
					.append("castSkill(").append(capacityType.getType()).append(".class, this.") //$NON-NLS-1$ //$NON-NLS-2$
					.append(fieldName).append(");"); //$NON-NLS-1$
				});

				// Add the annotation dedicated to this particular method
				/*context.getPostFinalizationElements().add(() -> {
					final String inlineExpression = Utils.HIDDEN_MEMBER_CHARACTER
							+ "castSkill(" + capacityType.getSimpleName() //$NON-NLS-1$
							+ ".class, ($0" + fieldName //$NON-NLS-1$
							+ " == null || $0" + fieldName //$NON-NLS-1$
							+ ".get() == null) ? ($0" + fieldName //$NON-NLS-1$
							+ " = $0" + Utils.HIDDEN_MEMBER_CHARACTER + "getSkill(" //$NON-NLS-1$ //$NON-NLS-2$
							+ capacityType.getSimpleName()
							+ ".class)) : $0" + fieldName + ")"; //$NON-NLS-1$ //$NON-NLS-2$;
					this.inlineExpressionCompiler.appendInlineAnnotation(
							operation, source.eResource().getResourceSet(), inlineExpression, capacityType);
				});*/
				appendGeneratedAnnotation(baseInferrer, operation, context);
				if (context.getGeneratorConfig2().isGeneratePureAnnotation()) {
					addAnnotationSafe(baseInferrer, operation, Pure.class);
				}

				context.addGeneratedCapacityUseField(capacityType.getIdentifier());
				context.incrementSerial(capacityType.getIdentifier().hashCode());
			}
		}
	}

}
