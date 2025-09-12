/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.XbaseGenerated;
import org.eclipse.xtext.xtype.XComputedTypeReference;

import com.google.common.base.Strings;

import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.annotation.DefaultValue;
import io.sarl.lang.core.annotation.DefaultValueSource;
import io.sarl.lang.core.annotation.DefaultValueUse;
import io.sarl.lang.core.annotation.EarlyExit;
import io.sarl.lang.core.annotation.FiredEvent;
import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.jvmmodel.GenerationContext;
import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerExecutableFragment;
import io.sarl.lang.jvmmodel.fragments.oop.IActionInferrerFragment;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.util.Utils;

/** Fragment for inferred the actions (functions) to the JVM model.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class ActionInferrerFragment extends AbstractJvmModelInferrerExecutableFragment implements IActionInferrerFragment {

	@Override
	public void transform(XtendFunction source, JvmGenericType container, boolean allowDispatch,
			IBaseJvmModelInferrer baseInferrer) {
		final var context = baseInferrer.getContext(container);
		assert context != null;
		// Compute the operation name
		// Issue #355: null or empty name is possible.
		final var originalFunctionName = source.getName();
		if (!Strings.isNullOrEmpty(originalFunctionName)) {
			if (Utils.isNameForJavaMainFunction(originalFunctionName)
					&& source.getDeclaringType() instanceof SarlClass
					&& source.isStatic() && !source.isDispatch()
					&& !context.isMainFunctionGenerated()
					&& !context.isMainFunctionManuallyDefined()) {
				translateMainFunction(context, source, container, baseInferrer);
			} else {
				translateRegularFunction(context, source, container, allowDispatch, baseInferrer);
			}			
		}
	}

	/** Transform a regular function.
	 *
	 * @param context the generation context.
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @since 0.15
	 */
	@SuppressWarnings("null")
	protected void translateMainFunction(GenerationContext context, XtendFunction source,
			JvmGenericType container, IBaseJvmModelInferrer baseInferrer) {
		final var functionName = Utils.getNameForJavaMainFunction();

		// Create the main function
		final var operation = this.jvmTypesFactory.createJvmOperation();
		container.getMembers().add(operation);
		this.associator.associatePrimary(source, operation);
		operation.setSimpleName(functionName);
		operation.setVisibility(JvmVisibility.PUBLIC);
		operation.setStrictFloatingPoint(source.isStrictFloatingPoint());
		operation.setStatic(true);
		operation.setSynchronized(false);
		operation.setNative(false);
		operation.setDefault(false);
		operation.setAbstract(false);
		operation.setFinal(false);
		operation.setVarArgs(false);
		operation.setReturnType(this.jvmTypeReferences.getTypeForName(void.class, container));

		// Determine if the source parameters are compatible with a main function
		final var isVarArgsFromSource = Utils.isVarArg(source.getParameters());
		var isValidParameter = false;
		if (source.getParameters().size() == 1) {
			final var providedArg = source.getParameters().get(0);
			var argType = Utils.toLightweightTypeReference(providedArg.getParameterType(), this.services);
			if (!isVarArgsFromSource && argType.isArray()) {
				argType = argType.getComponentType();
			}
			isValidParameter = argType.isType(String.class);
		}
		
		// Determine if the source return type is compatible with a main function
		final var operationReturnType = source.getReturnType();
		final var returnType = Utils.toLightweightTypeReference(operationReturnType, this.services);
		final var isValidReturnType = returnType == null || returnType.isPrimitiveVoid(); 

		// Create the mandatory formal parameter
		final var jvmParamArgs = this.jvmTypesFactory.createJvmFormalParameter();
		final String jvmParamArgsName;
		if (isValidParameter) {
			jvmParamArgsName = source.getParameters().get(0).getName();
		} else {
			jvmParamArgsName = this.grammarKeywordAccess.getItKeyword();
		}
		jvmParamArgs.setName(jvmParamArgsName);
		final var stringType = this.jvmTypeReferences.getTypeForName(String.class, container);
		jvmParamArgs.setParameterType(this.jvmTypeReferences.createArrayType(stringType));
		operation.getParameters().add(jvmParamArgs);

		// Exceptions
		for (final var exception : source.getExceptions()) {
			operation.getExceptions().add(this.jvmTypeBuilder.cloneWithProxies(exception));
		}

		// Compute the identifier of the action.
		final var actionKey = this.sarlSignatureProvider.createQualifiedActionName(container, functionName);
		final var actionSignatures = this.sarlSignatureProvider.createPrototypeFromJvmModel(
				context.getActionPrototypeContext(this.sarlSignatureProvider), actionKey, false,
				operation.getParameters());
		final var actSigKey = this.sarlSignatureProvider.createActionPrototype(
				functionName, actionSignatures.getFormalParameterTypes());

		// Add the body
		Runnable differedGeneration = null;
		if (isValidParameter && isValidReturnType) {
			setBody(baseInferrer, operation, source.getExpression());
		} else {
			// Because we need to add additional statements, Xtext imposes to create an internal
			// function to support the provided XExpression, and then we could generate our
			// own code that is calling this extra function.
			final var extractOperationName = SarlUtils.HIDDEN_MEMBER_CHARACTER + operation.getSimpleName() + '_' + context.getActionIndex();

			final var generateReturnWrapper = !isValidReturnType;
			final var generateParameterWrapper = !isValidParameter;
			setBody(operation, it -> {
				String returnCodeVar = null;
				if (generateReturnWrapper) {
					returnCodeVar = it.declareUniqueNameVariable(operation, "returnCode"); //$NON-NLS-1$
					it.append("final ").append(returnType).append(" "); //$NON-NLS-1$ //$NON-NLS-2$
					it.append(returnCodeVar).append(" = "); //$NON-NLS-1$
				}
				it.append(extractOperationName);
				it.append("("); //$NON-NLS-1$
				final var stringTypeReference = Utils.toLightweightTypeReference(stringType, this.services);
				var i = 0;
				for (final var parameter : source.getParameters()) {
					if (i > 0) {
						it.append(", "); //$NON-NLS-1$
					}
					var paramType = Utils.toLightweightTypeReference(parameter.getParameterType(), this.services);
					if (paramType.isArray()) {
						paramType = paramType.getComponentType();
					}
					if (paramType.isAssignableFrom(stringTypeReference)) {
						if (generateParameterWrapper) {
							it.append(jvmParamArgsName).append("["); //$NON-NLS-1$
							it.append(Integer.toString(i)).append("]"); //$NON-NLS-1$
						} else {
							it.append(parameter.getName());
						}
					} else {
						// A cast is needed from String to the type of the provided argument.
						// Since an error is generated in this case, we simply put the default value.
						it.append(java.util.Objects.toString(this.defaultValueProvider.getDefaultValue(parameter.getParameterType().getType())));
					}
					++i;
				}
				it.append(");"); //$NON-NLS-1$
				if (generateReturnWrapper) {
					it.newLine();
					it.append(System.class).append(".exit("); //$NON-NLS-1$
					if (returnType.isPrimitive()) {
						if (!returnType.isType(int.class)) {
							it.append("(int) "); //$NON-NLS-1$
						}
						it.append(returnCodeVar);
					} else {
						it.append(returnCodeVar);
						it.append(" == null ? 255 : "); //$NON-NLS-1$
						if (returnType.isSubtypeOf(Number.class)) {
							it.append(returnCodeVar);
							it.append(".intValue()"); //$NON-NLS-1$
						} else {
							it.append("0"); //$NON-NLS-1$
						}
					}
					it.append(");"); //$NON-NLS-1$
				}
			});

			differedGeneration = () -> {
				final var operation2 = ActionInferrerFragment.this.jvmTypesFactory.createJvmOperation();
				container.getMembers().add(operation2);
				operation2.setSimpleName(extractOperationName);
				operation2.setVisibility(JvmVisibility.PRIVATE);
				operation2.setVarArgs(isVarArgsFromSource);
				operation2.setAbstract(false);
				operation2.setDeprecated(false);
				operation2.setStatic(true);
				operation2.setFinal(false);
				operation2.setNative(false);
				operation2.setStrictFloatingPoint(false);
				operation2.setSynchronized(false);
				operation2.setDefault(false);
				operation2.setAbstract(false);
				this.associator.associate(source, operation2);

				for (final var exception : operation.getExceptions()) {
					operation2.getExceptions().add(ActionInferrerFragment.this.jvmTypeBuilder
							.cloneWithProxies(exception));
				}

				final JvmTypeReference returnType2;
				if (operationReturnType instanceof XComputedTypeReference) {
					returnType2 = this.jvmTypeReferences.createDelegateTypeReference(operationReturnType);
				} else {
					returnType2 = cloneWithTypeParametersAndProxies(operationReturnType, operation2, baseInferrer);
				}
				operation2.setReturnType(returnType2);

				// Compute the identifier of the action.
				final var actionKey2 = this.sarlSignatureProvider.createQualifiedActionName(container, extractOperationName);
				final var actionSignatures2 = this.sarlSignatureProvider.createPrototypeFromSarlModel(
						context.getActionPrototypeContext(this.sarlSignatureProvider), actionKey2, isVarArgsFromSource,
						source.getParameters());
				final var actSigKey2 = this.sarlSignatureProvider.createActionPrototype(
						extractOperationName, actionSignatures2.getFormalParameterTypes());

				// Add the formal parameters to the second operation
				var oparams = actionSignatures2.getOriginalParameterTypes();
				if (oparams == null) {
					oparams = Collections.emptyList();
				}
				translateSarlFormalParametersForSyntheticOperation(
						operation2, container, isVarArgsFromSource,
						oparams, baseInferrer);

				setBody(baseInferrer, operation2, source.getExpression());

				// Copy the annotations from the original operations, and that are not overridden above
				for (final JvmAnnotationReference annotation : operation.getAnnotations()) {
					final var id = annotation.getAnnotation().getIdentifier();
					if (!DefaultValueSource.class.getName().equals(id)
							&& !DefaultValueUse.class.getName().equals(id)
							&& !Pure.class.getName().equals(id)
							&& !EarlyExit.class.getName().equals(id)
							&& !FiredEvent.class.getName().equals(id)
							&& !Inline.class.getName().equals(id)
							&& !XbaseGenerated.class.getName().equals(id)
							&& !GENERATED_NAME.equals(id)) {
						try {
							final var clone = baseInferrer.getJvmAnnotationReferenceBuilder().annotationRef(id);
							if (clone != null) {
								for (final var annotationValue : annotation.getExplicitValues()) {
									clone.getExplicitValues().add(EcoreUtil.copy(annotationValue));
								}
								operation2.getAnnotations().add(clone);
							}
						} catch (IllegalArgumentException exception) {
							// ignore
						}
					}
				}

				// Copy and clean the documentation
				copyAndCleanDocumentationTo(operation, operation2);

				// Update the two collections that describes the implemented and implementable operations.
				context.doLocalOperationDefinition(actSigKey2, operation2);
			};
		}

		// User Annotations
		baseInferrer.translateAnnotationsTo(source.getAnnotations(), operation);
		if (!isValidParameter || !isValidReturnType) {
			appendGeneratedAnnotation(baseInferrer, operation, context);
		}

		copyAndCleanDocumentationTo(source, operation);

		// Update the context
		context.doLocalOperationDefinition(actSigKey, operation);
		if (differedGeneration != null) {
			context.getPreFinalizationElements().add(differedGeneration);
		}
		context.setActionIndex(context.getActionIndex() + 1);
		context.incrementSerial(actSigKey.hashCode());
	}


	/** Transform a regular function.
	 *
	 * @param context the generation context.
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 * @param allowDispatch indicates if dispatch function is allowed in the context.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @since 0.15
	 */
	protected void translateRegularFunction(GenerationContext context, XtendFunction source,
			JvmGenericType container, boolean allowDispatch, IBaseJvmModelInferrer baseInferrer) {
		final var originalFunctionName = source.getName();
		final var sourceNameBuffer = new StringBuilder(originalFunctionName);
		if (allowDispatch && source.isDispatch()) {
			sourceNameBuffer.insert(0, "_"); //$NON-NLS-1$
		}
		final var sourceName = sourceNameBuffer.toString();

		// Create the principal function
		final var operation = this.jvmTypesFactory.createJvmOperation();
		container.getMembers().add(operation);
		this.associator.associatePrimary(source, operation);
		operation.setSimpleName(sourceName);
		setVisibility(operation, source);
		operation.setStrictFloatingPoint(source.isStrictFloatingPoint());
		operation.setStatic(source.isStatic());
		operation.setSynchronized(source.isSynchonized());
		operation.setNative(source.isNative());
		boolean enableFunctionBody;
		if (container.isInterface()) {
			enableFunctionBody = false;
			if (!Utils.toLightweightTypeReference(container, this.services).isSubtypeOf(Capacity.class)) {
				if (operation.isStatic()) {
					enableFunctionBody = true;
				} else if (source.getExpression() != null && !operation.isAbstract()) {
					enableFunctionBody = true;
				}
			}
			operation.setDefault(enableFunctionBody && !operation.isStatic());
			operation.setAbstract(!enableFunctionBody);
			operation.setFinal(false);
		} else {
			operation.setDefault(false);
			enableFunctionBody = !source.isAbstract();
			operation.setAbstract(!enableFunctionBody);
			operation.setFinal(enableFunctionBody && source.isFinal());
		}

		// Type parameters
		baseInferrer.copyAndFixTypeParameters(source.getTypeParameters(), operation);

		// Compute the identifier of the action.
		final var actionKey = this.sarlSignatureProvider.createQualifiedActionName(
				container, sourceName);

		// Compute the different action prototypes associated to the action to create.
		final var isVarArgs = Utils.isVarArg(source.getParameters());
		final var actionSignatures = this.sarlSignatureProvider.createPrototypeFromSarlModel(
				context.getActionPrototypeContext(this.sarlSignatureProvider),
				actionKey,
				isVarArgs, source.getParameters());

		// Compute the action prototype of the action without optional parameter
		final var actSigKey = this.sarlSignatureProvider.createActionPrototype(
				sourceName,
				actionSignatures.getFormalParameterTypes());

		// Generate the parameters
		final var paramList = actionSignatures.getOriginalParameterTypes();
		translateSarlFormalParameters(
				context,
				operation, container, isVarArgs,
				source.getParameters(),
				container.isInterface(), paramList,
				true,
				baseInferrer);

		// Get the super function
		final var inheritedOperation = context.getInheritedOperation(actSigKey);

		// Infer the return type
		final var selectedReturnType = inferFunctionReturnType(source, operation, inheritedOperation, baseInferrer);
		operation.setReturnType(selectedReturnType);

		// Exceptions
		for (final var exception : source.getExceptions()) {
			operation.getExceptions().add(this.jvmTypeBuilder.cloneWithProxies(exception));
		}

		// Add the body
		if (enableFunctionBody) {
			setBody(baseInferrer, operation, source.getExpression());
		}

		// User Annotations
		baseInferrer.translateAnnotationsTo(source.getAnnotations(), operation);

		// Add @Inline annotation
		if (context.getGeneratorConfig2().isGenerateInlineAnnotation()
				&& !source.isAbstract() && !container.isInterface()
				&& (source.isStatic() || source.isFinal() || container.isFinal())
				&& context.getParentContext() == null
				&& this.jvmAnnotationFinder.findAnnotation(operation, Inline.class) == null) {
			context.getPostFinalizationElements().add(
					() -> this.inlineExpressionCompiler.appendInlineAnnotation(operation, source));
		}

		// Add @Override annotation
		if (source.isOverride()
				&& this.jvmAnnotationFinder.findAnnotation(operation, Override.class) == null
				&& this.jvmTypeReferences.findDeclaredType(Override.class, source) != null) {
			addAnnotationSafe(baseInferrer, operation, Override.class);
		}

		// Add @Pure annotation
		final boolean addDynamicPureAnnotationGenerator;
		final var hasExplicitPureAnnotation = this.jvmAnnotationFinder.findAnnotation(operation, Pure.class) != null;
		if (!hasExplicitPureAnnotation 
				&& context.getGeneratorConfig2().isGeneratePureAnnotation()
				&& this.jvmTypeReferences.findDeclaredType(Pure.class, source) != null) {
			addDynamicPureAnnotationGenerator = inheritedOperation == null;
			if (addDynamicPureAnnotationGenerator) {
				this.operationHelper.attachPureAnnotationAdapter(operation, (op, helper) -> {
					return Boolean.valueOf(helper.isPurableOperation(source));
				});
			} else {
				this.operationHelper.attachPureAnnotationAdapter(operation, (op, helper) -> {
					return Boolean.valueOf(helper.isPureOperation(inheritedOperation));
				});
			}
		} else {
			addDynamicPureAnnotationGenerator = false;
		}

		// Detecting if the action is an early-exit action.
		// If true, the Java code is annotated to be usable by the SARL validator.
		final List<JvmTypeReference> firedEvents;
		final boolean isEarlyExit;
		if (source instanceof SarlAction action) {
			firedEvents = action.getFiredEvents();
			isEarlyExit = this.earlyExitComputer.isEarlyExitOperation(action);
			if (isEarlyExit) {
				addAnnotationSafe(baseInferrer, operation, EarlyExit.class);
			}
		} else {
			firedEvents = Collections.emptyList();
			isEarlyExit = false;
		}

		// Put the fired SARL events as Java annotations for being usable by the SARL validator.
		if (!firedEvents.isEmpty()) {
			operation.getAnnotations().add(annotationClassRef(baseInferrer, FiredEvent.class, firedEvents));
		}

		// 1. Ensure that the Java annotations related to the default value are really present.
		//    They may be not present if the generated action is a specific version of an inherited
		//    action with default values for parameters.
		// 2. Update the two collections that describes the implemented and implementable operations.
		var implementedOperation = context.getInheritedOperationsToImplement().get(actSigKey);
		if (implementedOperation == null) {
			implementedOperation = context.getInheritedOverridableOperations().get(actSigKey);
		}
		// Put the annotations that were defined in the implemented operation
		if (implementedOperation != null) {
			if (this.jvmAnnotationFinder.findAnnotation(implementedOperation, DefaultValueSource.class) != null
					&& this.jvmAnnotationFinder.findAnnotation(operation, DefaultValueSource.class) == null) {
				addAnnotationSafe(baseInferrer, operation, DefaultValueSource.class);
			}
			// Reinject the @DefaultValue annotations
			final var oparams = implementedOperation.getParameters();
			final var cparams = operation.getParameters();
			assert oparams.size() == cparams.size();
			for (var i = 0; i < oparams.size(); ++i) {
				final var op = oparams.get(i);
				final var cp = cparams.get(i);
				final var ovalue = this.annotationUtils.findStringValue(op, DefaultValue.class);
				if (ovalue != null
						&& this.jvmAnnotationFinder.findAnnotation(cp, DefaultValue.class) == null) {
					addAnnotationSafe(baseInferrer, cp,
							DefaultValue.class,
							this.sarlSignatureProvider.qualifyDefaultValueID(
									implementedOperation.getDeclaringType().getIdentifier(),
									ovalue));
				}
			}
		}

		// Add the principal operation into the list of locally-defined operations
		context.doLocalOperationDefinition(actSigKey, operation);

		copyAndCleanDocumentationTo(source, operation);

		final Runnable differedGeneration = () -> {
			// Generate the Java functions that correspond to the action with the parameter default values applied.
			for (final var otherSignature : actionSignatures.getInferredParameterTypes().entrySet()) {
				final var ak = this.sarlSignatureProvider.createActionPrototype(
						sourceName,
						otherSignature.getKey());
				if (ak != null) {
					final var inheritedOp = context.getInheritedImplementedOperation(ak);
					translateSarlFormalParametersForLocalHiddenDefaultValues(context, inheritedOp);
					final var localOp = context.getLocalOperation(ak);
					if (inheritedOp == null && localOp == null) {
						// Generate the additional constructor that is invoke the main constructor previously generated.
						final var operation2 = this.jvmTypesFactory.createJvmOperation();
						container.getMembers().add(operation2);
						operation2.setSimpleName(operation.getSimpleName());
						operation2.setVisibility(operation.getVisibility());
						operation2.setVarArgs(operation.isVarArgs());
						operation2.setAbstract(operation.isAbstract());
						operation2.setDeprecated(operation.isDeprecated());
						operation2.setStatic(operation.isStatic());
						operation2.setFinal(!operation.isStatic() && !container.isInterface());
						operation2.setNative(false);
						operation2.setStrictFloatingPoint(false);
						operation2.setSynchronized(false);
						this.associator.associate(source, operation2);

						copyTypeParametersFromJvmOperation(operation, operation2, baseInferrer);

						for (final var exception : operation.getExceptions()) {
							operation2.getExceptions().add(this.jvmTypeBuilder.cloneWithProxies(exception));
						}

						final JvmTypeReference returnType2;
						if (selectedReturnType instanceof XComputedTypeReference) {
							returnType2 = this.jvmTypeReferences.createDelegateTypeReference(selectedReturnType);
						} else {
							returnType2 = cloneWithTypeParametersAndProxies(selectedReturnType, operation2, baseInferrer);
						}
						operation2.setReturnType(returnType2);

						final var args = translateSarlFormalParametersForSyntheticOperation(
								operation2, container, isVarArgs,
								otherSignature.getValue(), baseInferrer);

						operation2.setDefault(container.isInterface());
						operation2.setAbstract(false);
						setBody(operation2, it -> {
							final var type = operation2.getReturnType();
							if (!this.jvmTypeReferences.is(type, void.class)) {
								it.append("return "); //$NON-NLS-1$
							}
							final var ltr = Utils.toLightweightTypeReference(returnType2, this.services);
							if (Utils.containsGenericType(ltr)) {
								final var typeId = ltr.getRawTypeReference().getType().getQualifiedName('.');
								final var javaId = ltr.getRawTypeReference().getJavaIdentifier();
								final var fullId = ltr.getJavaIdentifier().replaceAll(
										Pattern.quote(javaId), Matcher.quoteReplacement(typeId));
								it.append("("); //$NON-NLS-1$
								it.append(fullId);
								it.append(")"); //$NON-NLS-1$
							}
							it.append(sourceName);
							it.append("("); //$NON-NLS-1$
							it.append(IterableExtensions.join(args, ", ")); //$NON-NLS-1$
							it.append(");"); //$NON-NLS-1$
						});

						// @Override annotation
						if (source.isOverride()
								&& this.jvmAnnotationFinder.findAnnotation(operation,
										Override.class) == null
										&& this.jvmTypeReferences.findDeclaredType(
												Override.class, source) != null) {
							addAnnotationSafe(baseInferrer, operation, Override.class);
						}

						// @DefaultValueUse annotation
						addAnnotationSafe(baseInferrer, operation2, DefaultValueUse.class,
								actionSignatures.getFormalParameterTypes().toString());
						appendGeneratedAnnotation(baseInferrer, operation2, context);

						// @EarlyExit annotation
						// If the main action is an early-exit action, the additional operation
						// is also an early-exit operation.
						if (isEarlyExit) {
							addAnnotationSafe(baseInferrer, operation2, EarlyExit.class);
						}

						// @FiredEvent annotation
						// Put the fired SARL events as Java annotations for being usable by the SARL validator.
						if (!firedEvents.isEmpty()) {
							operation2.getAnnotations().add(
									annotationClassRef(baseInferrer, FiredEvent.class, firedEvents));
						}

						// @Pure annotation
						if (addDynamicPureAnnotationGenerator) {
							this.operationHelper.attachPureAnnotationAdapter(operation2, (op, helper) -> {
								return Boolean.valueOf(helper.isPureOperation(operation));
							});
						} else if (hasExplicitPureAnnotation) {
							addAnnotationSafe(baseInferrer, operation2, Pure.class);
						}

						// Copy the annotations from the original operations, and that are not overridden above
						for (final JvmAnnotationReference annotation : operation.getAnnotations()) {
							final var id = annotation.getAnnotation().getIdentifier();
							if (!DefaultValueSource.class.getName().equals(id)
									&& !DefaultValueUse.class.getName().equals(id)
									&& !Pure.class.getName().equals(id)
									&& !EarlyExit.class.getName().equals(id)
									&& !FiredEvent.class.getName().equals(id)
									&& !Inline.class.getName().equals(id)
									&& !XbaseGenerated.class.getName().equals(id)
									&& !GENERATED_NAME.equals(id)) {
								try {
									final var clone = baseInferrer.getJvmAnnotationReferenceBuilder().annotationRef(id);
									if (clone != null) {
										for (final var annotationValue : annotation.getExplicitValues()) {
											clone.getExplicitValues().add(EcoreUtil.copy(annotationValue));
										}
										operation2.getAnnotations().add(clone);
									}
								} catch (IllegalArgumentException exception) {
									// ignore
								}
							}
						}

						// Copy and clean the documentation
						copyAndCleanDocumentationTo(operation, operation2);

						// Update the two collections that describes the implemented and implementable operations.
						context.doLocalOperationDefinition(ak, operation2);
					}
				}
			}
		};

		context.getPreFinalizationElements().add(differedGeneration);
		context.setActionIndex(context.getActionIndex() + 1);
		context.incrementSerial(actSigKey.hashCode());
		context.setInjectable(operation);
	}

}
