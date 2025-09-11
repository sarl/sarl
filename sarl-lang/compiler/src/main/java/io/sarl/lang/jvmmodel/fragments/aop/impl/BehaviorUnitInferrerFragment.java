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

import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.lib.Pure;

import com.google.common.base.Strings;

import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.Messages;
import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.IBehaviorUnitInferrerFragment;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.util.Utils;

/** Fragment for inferred the behavior units to the JVM model.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.15
 */
public class BehaviorUnitInferrerFragment extends AbstractJvmModelInferrerFragment implements IBehaviorUnitInferrerFragment {

	@Override
	public void transform(SarlBehaviorUnit source, JvmGenericType container,
			IBaseJvmModelInferrer baseInferrer) {
		final var context = baseInferrer.getContext(container);
		if (source.getName() != null && !Strings.isNullOrEmpty(source.getName().getSimpleName()) && context != null) {
			final var guard = source.getGuard();

			final boolean isTrueGuard;

			// Check the guard value
			if (guard == null) {
				isTrueGuard = true;
			} else if (guard instanceof XBooleanLiteral literal) {
				if (literal.isIsTrue()) {
					isTrueGuard = true;
				} else {
					// The guard is always false => no need to generate the code
					return;
				}
			} else {
				isTrueGuard = false;
			}

			final var voidType = baseInferrer.getJvmTypeReferenceBuilder().typeRef(Void.TYPE);

			//----------------
			// Body function
			//----------------
			// Name
			final var bodyMethodName = Utils.createNameForHiddenEventHandlerMethod(source.getName(), context.getBehaviorUnitIndex());
			// Operation
			final var bodyOperation = this.jvmTypesFactory.createJvmOperation();
			bodyOperation.setAbstract(false);
			bodyOperation.setNative(false);
			bodyOperation.setSynchronized(false);
			bodyOperation.setStrictFloatingPoint(false);
			bodyOperation.setFinal(false);
			bodyOperation.setVisibility(JvmVisibility.PRIVATE);
			bodyOperation.setStatic(false);
			bodyOperation.setSimpleName(bodyMethodName);
			bodyOperation.setReturnType(voidType);
			// Add to container
			container.getMembers().add(bodyOperation);
			this.associator.associatePrimary(source, bodyOperation);
			// First parameter: occurrence
			var jvmParam = this.jvmTypesFactory.createJvmFormalParameter();
			jvmParam.setName(this.grammarKeywordAccess.getOccurrenceKeyword());
			jvmParam.setParameterType(this.jvmTypeBuilder.cloneWithProxies(source.getName()));
			this.associator.associate(source, jvmParam);
			bodyOperation.getParameters().add(jvmParam);
			// Body
			setBody(baseInferrer, bodyOperation, source.getExpression());
			// Annotations
			baseInferrer.translateAnnotationsTo(source.getAnnotations(), bodyOperation);
			if (context.getGeneratorConfig2().isGeneratePureAnnotation()
					&& !this.services.getExpressionHelper().hasSideEffects(source.getExpression())) {
				addAnnotationSafe(baseInferrer, bodyOperation, Pure.class);
			}

			final var evaluators = context.ensureGuardEvaluationCodeFor(source, this.jvmTypeReferences);
			assert evaluators != null;

			if (isTrueGuard) {
				evaluators.add(it -> {
					it.append(RUNNABLE_COLLECTION);
					it.append(".add(() -> "); //$NON-NLS-1$
					it.append(bodyMethodName);
					it.append("("); //$NON-NLS-1$
					it.append(this.grammarKeywordAccess.getOccurrenceKeyword());
					it.append("));"); //$NON-NLS-1$
				});
			} else {
				assert guard != null;

				//----------------
				// Guard function
				//----------------
				// Name
				final var guardMethodName = Utils.createNameForHiddenGuardEvaluatorMethod(source.getName(), context.getBehaviorUnitIndex());
				// Operation
				final var guardOperation = this.jvmTypesFactory.createJvmOperation();
				guardOperation.setAbstract(false);
				guardOperation.setNative(false);
				guardOperation.setSynchronized(false);
				guardOperation.setStrictFloatingPoint(false);
				guardOperation.setFinal(false);
				guardOperation.setVisibility(JvmVisibility.PRIVATE);
				guardOperation.setStatic(false);
				guardOperation.setSimpleName(guardMethodName);
				guardOperation.setReturnType(baseInferrer.getJvmTypeReferenceBuilder().typeRef(Boolean.TYPE));
				// Add to container
				container.getMembers().add(guardOperation);
				this.associator.associate(source, guardOperation);
				this.associator.associatePrimary(guard, guardOperation);
				// First parameter: it
				jvmParam = this.jvmTypesFactory.createJvmFormalParameter();
				jvmParam.setName(this.grammarKeywordAccess.getItKeyword());
				jvmParam.setParameterType(this.jvmTypeBuilder.cloneWithProxies(source.getName()));
				this.associator.associate(source, jvmParam);
				guardOperation.getParameters().add(jvmParam);
				// Second parameter: occurrence
				jvmParam = this.jvmTypesFactory.createJvmFormalParameter();
				jvmParam.setName(this.grammarKeywordAccess.getOccurrenceKeyword());
				jvmParam.setParameterType(this.jvmTypeBuilder.cloneWithProxies(source.getName()));
				this.associator.associate(source, jvmParam);
				guardOperation.getParameters().add(jvmParam);
				// Body
				setBody(baseInferrer, guardOperation, guard);
				// Annotations
				if (context.getGeneratorConfig2().isGeneratePureAnnotation()) {
					addAnnotationSafe(baseInferrer, guardOperation, Pure.class);
				}

				//------------------
				// Handler function
				//------------------
				evaluators.add(it -> {
					it.append("if ("); //$NON-NLS-1$
					it.append(guardMethodName);
					it.append("("); //$NON-NLS-1$
					it.append(this.grammarKeywordAccess.getOccurrenceKeyword());
					it.append(", "); //$NON-NLS-1$
					it.append(this.grammarKeywordAccess.getOccurrenceKeyword());
					it.append(")) {"); //$NON-NLS-1$
					it.increaseIndentation();
					it.newLine();
					it.append(RUNNABLE_COLLECTION);
					it.append(".add(() -> "); //$NON-NLS-1$
					it.append(bodyMethodName);
					it.append("("); //$NON-NLS-1$
					it.append(this.grammarKeywordAccess.getOccurrenceKeyword());
					it.append("));"); //$NON-NLS-1$
					it.decreaseIndentation();
					it.newLine();
					it.append("}"); //$NON-NLS-1$
				});
			}

			context.setBehaviorUnitIndex(context.getBehaviorUnitIndex() + 1);
			context.incrementSerial(bodyMethodName.hashCode());
		} else {
			baseInferrer.logInternalError(Messages.SARLJvmModelInferrer_10);
		}
	}

}
