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

package io.sarl.lang.jvmmodel.fragments;

import java.text.MessageFormat;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.typesystem.InferredTypeIndicator;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.inject.Inject;

import io.sarl.lang.compiler.IInlineExpressionCompiler;
import io.sarl.lang.controlflow.ISarlEarlyExitComputer;
import io.sarl.lang.core.annotation.DefaultValue;
import io.sarl.lang.core.annotation.DefaultValueSource;
import io.sarl.lang.jvmmodel.GenerationContext;
import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.Messages;
import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlContinueExpression;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.sarl.actionprototype.InferredStandardParameter;
import io.sarl.lang.sarl.actionprototype.InferredValuedParameter;
import io.sarl.lang.typesystem.IOperationHelper;
import io.sarl.lang.util.Utils;

/** Abstract implementation of a fragment that may be used for inferring executable to JVM model.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public abstract class AbstractJvmModelInferrerExecutableFragment extends AbstractJvmModelInferrerFragment {

	/** Compiler of expressions that may be used for inlining expressions for functions.
	 */
	@Inject
	protected IInlineExpressionCompiler inlineExpressionCompiler;

	/** Helper for defining operations.
	 */
	@Inject
	protected IOperationHelper operationHelper;

	/** Computer of early-exits for SARL.
	 */
	@Inject
	protected ISarlEarlyExitComputer earlyExitComputer;

	/** Generate a list of formal parameters with annotations for the default values.
	 *
	 * @param context the generation context.
	 * @param owner the JVM element to change.
	 * @param actionContainer the container of the action.
	 * @param varargs indicates if the signature has variadic parameter.
	 * @param params the parameters.
	 * @param isForInterface indicates if the formal parameters are for an interface ({@code true})
	 * 							or a class ({@code false}).
	 * @param paramSpec the specification of the parameter as computed by a {@link IActionPrototypeProvider}.
	 * @param ignoreOverridableOperations indicates if the operations are ignored if it is marked as overridable.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 */
	protected void translateSarlFormalParameters(
			GenerationContext context,
			JvmExecutable owner,
			JvmGenericType actionContainer,
			boolean varargs,
			List<? extends XtendParameter> params,
			final boolean isForInterface,
			List<InferredStandardParameter> paramSpec,
			boolean ignoreOverridableOperations,
			IBaseJvmModelInferrer baseInferrer) {
		if (paramSpec == null) {
			return;
		}
		var hasDefaultValue = false;
		final var isStaticOwner = owner instanceof JvmConstructor || (owner instanceof JvmOperation && ((JvmOperation) owner).isStatic());
		for (var i = 0; i < params.size(); ++i) {
			final var param = params.get(i);
			assert param != null;
			final var paramName = param.getName();
			final var paramType = param.getParameterType();

			if (!Strings.isNullOrEmpty(paramName) && paramType != null) {
				// "Standard" (Xtend) translation of the parameter
				baseInferrer.translateParameter(owner, param);
				final var createdParam = owner.getParameters().get(owner.getParameters().size() - 1);
				// Treat the default value
				if (i < paramSpec.size() && param instanceof SarlFormalParameter sarlParam) {
					if (sarlParam.getDefaultValue() != null) {
						final var defaultValue = sarlParam.getDefaultValue();
						assert defaultValue != null;
						hasDefaultValue = true;
						final var inferredParam = paramSpec.get(i);
						assert inferredParam != null;
						final Runnable creationCode1 = () -> {
							//
							// Generate an instance definition of the default value
							//
							final var namePostPart = inferredParam.getDefaultValueAnnotationValue();
							final var inferredType = skipTypeParameters(paramType, actionContainer);
							final var functionName = this.sarlSignatureProvider.createFunctionNameForDefaultValueID(namePostPart);
							final boolean addMethod;
							if (ignoreOverridableOperations) {
								final var prototype = this.sarlSignatureProvider.createActionPrototype(
										functionName,
										this.sarlSignatureProvider.createParameterTypesForVoid());
								addMethod = context.getInheritedImplementedOperation(prototype) == null;
							} else {
								addMethod = true;
							}
							if (addMethod) {
								final var method = this.jvmTypeBuilder.toMethod(defaultValue, functionName, inferredType, it -> {
									this.jvmTypeBuilder.setDocumentation(it,
											MessageFormat.format(Messages.SARLJvmModelInferrer_11, paramName));
									it.setReturnType(inferredType);
									it.setStatic(isStaticOwner);
									it.setFinal(!isStaticOwner && !isForInterface);
									it.setDefault(isForInterface && !isStaticOwner);
									if (isForInterface) {
										it.setVisibility(JvmVisibility.PUBLIC);
									} else {
										it.setVisibility(JvmVisibility.PRIVATE);
									}
								});

								actionContainer.getMembers().add(method);

								addAnnotationSafe(baseInferrer, method, Pure.class);

								final var rawCode = Utils.getSarlCodeFor(defaultValue);
								appendGeneratedAnnotation(baseInferrer, method, context, rawCode);

								setBody(baseInferrer, method, defaultValue);

								addAnnotationSafe(baseInferrer, createdParam, DefaultValue.class, namePostPart);

								this.associator.associate(defaultValue, method);
							}
						};
						context.addUserObject("translateSarlFormalParameters", creationCode1); //$NON-NLS-1$
					}
				}
			}
		}

		if (hasDefaultValue) {
			addAnnotationSafe(baseInferrer, owner, DefaultValueSource.class);
		}
	}

	/** Generate the local default values.
	 *
	 * @param context the generation context.
	 * @param inheritedOperation the reference to the inherited operation.
	 * @since 0.12
	 */
	@SuppressWarnings("static-method")
	protected void translateSarlFormalParametersForLocalHiddenDefaultValues(GenerationContext context, JvmOperation inheritedOperation) {
		final var on = context.consumeUserObject("translateSarlFormalParameters", Runnable.class); //$NON-NLS-1$
		if (inheritedOperation == null) {
			for (final var code : on) {
				code.run();
			}
		}
	}

	/** Generate a list arguments from the formal parameters in order to be used for a call into
	 * a synthetic operation, such as default-valued parameter function.
	 *
	 * @param owner the JVM element to change.
	 * @param actionContainer the container of the action.
	 * @param varargs indicates if the signature has variadic parameter.
	 * @param signature the description of the parameters.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @return the arguments to pass to the original function.
	 */
	protected List<String> translateSarlFormalParametersForSyntheticOperation(JvmExecutable owner, 
			JvmGenericType actionContainer,
			boolean varargs, List<InferredStandardParameter> signature,
			IBaseJvmModelInferrer baseInferrer) {
		final var arguments = CollectionLiterals.<String>newArrayList();
		for (final var parameterSpec : signature) {
			final var paramType = parameterSpec.getType();
			if (parameterSpec instanceof InferredValuedParameter cvalue) {
				final var argumentValue = new StringBuilder();
				final var jtype = paramType.getType();
				if (jtype instanceof JvmTypeParameter) {
					argumentValue.append("("); //$NON-NLS-1$
					argumentValue.append(paramType.getSimpleName());
					argumentValue.append(") "); //$NON-NLS-1$
				}
				argumentValue.append(this.sarlSignatureProvider.toJavaArgument(
						actionContainer.getIdentifier(),
						cvalue.getCallingArgument()));
				arguments.add(argumentValue.toString());
			} else {
				final var param = parameterSpec.getParameter();
				final var paramName = parameterSpec.getName();
				if (!Strings.isNullOrEmpty(paramName) && paramType != null) {
					final var lastParam = this.jvmTypesFactory.createJvmFormalParameter();
					owner.getParameters().add(lastParam);
					lastParam.setName(paramName);
					if (owner instanceof JvmOperation) {
						lastParam.setParameterType(cloneWithTypeParametersAndProxies(paramType.toTypeReference(), owner, baseInferrer));
					} else {
						lastParam.setParameterType(this.jvmTypeBuilder.cloneWithProxies(paramType.toTypeReference()));
					}
					this.associator.associate(param, lastParam);
					arguments.add(paramName);
				}
			}
		}
		return arguments;
	}


	/** Infer the function's return type.
	 *
	 * @param body the body of the function.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @return the return type.
	 */
	protected JvmTypeReference inferFunctionReturnType(XExpression body, IBaseJvmModelInferrer baseInferrer) {
		var expr = body;
		var stop = false;
		while (!stop && expr instanceof XBlockExpression block) {
			switch (block.getExpressions().size()) {
			case 0:
				expr = null;
				break;
			case 1:
				expr = block.getExpressions().get(0);
				break;
			default:
				stop = true;
			}
		}
		if (expr == null || expr instanceof XAssignment || expr instanceof XVariableDeclaration
				|| expr instanceof SarlBreakExpression || expr instanceof SarlContinueExpression
				|| expr instanceof SarlAssertExpression) {
			return baseInferrer.getJvmTypeReferenceBuilder().typeRef(Void.TYPE);
		}
		return this.jvmTypeBuilder.inferredType(body);
	}

	/** Infer the return type for the given source function.
	 *
	 * @param source the source function.
	 * @param target the target operation.
	 * @param overriddenOperation reference to the overridden operation.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @return the inferred return type.
	 * @since 0.7
	 */
	protected JvmTypeReference inferFunctionReturnType(XtendFunction source, JvmOperation target,
			JvmOperation overriddenOperation, IBaseJvmModelInferrer baseInferrer) {
		// The return type is explicitly given
		if (source.getReturnType() != null) {
			return ensureValidType(source.eResource(), source.getReturnType(), baseInferrer);
		}

		// An super operation was detected => reuse its return type.
		if (overriddenOperation != null) {
			final var type = overriddenOperation.getReturnType();
			//return cloneWithProxiesFromOtherResource(type, target);
			return this.jvmTypeReferences.createDelegateTypeReference(type);
		}

		// Return type is inferred from the operation's expression.
		final var expression = source.getExpression();
		JvmTypeReference returnType = null;
		if (expression != null
				&& ((!(expression instanceof XBlockExpression))
						|| (!((XBlockExpression) expression).getExpressions().isEmpty()))) {
			returnType = inferFunctionReturnType(expression, baseInferrer);
		}
		return ensureValidType(source.eResource(), returnType, baseInferrer);
	}

	private JvmTypeReference ensureValidType(Resource targetResource, JvmTypeReference returnType,
			IBaseJvmModelInferrer baseInferrer) {
		// No return type could be inferred => assume "void"
		if (returnType == null) {
			return baseInferrer.getJvmTypeReferenceBuilder().typeRef(Void.TYPE);
		}
		// The given type is not associated to the target resource => force relocation.
		final var returnTypeResource = returnType.eResource();
		if (returnTypeResource != null && !Objects.equal(returnType.eResource(), targetResource)) {
			return this.jvmTypeBuilder.cloneWithProxies(returnType);
		}
		// A return type was inferred => use it as-is because it is not yet resolved to the concrete type.
		if (InferredTypeIndicator.isInferred(returnType)) {
			return returnType;
		}
		// A return was inferred and resolved => use it.
		return this.jvmTypeBuilder.cloneWithProxies(returnType);
	}

	/** Copy the type parameters from a JvmOperation.
	 *
	 * <p>This function differs from {@link #copyAndFixTypeParameters(List, org.eclipse.xtext.common.types.JvmTypeParameterDeclarator)}
	 * and {@link #copyTypeParameters(List, org.eclipse.xtext.common.types.JvmTypeParameterDeclarator)}
	 * in the fact that the type parameters were already generated and fixed. The current function supper generic types by
	 * clone the types references with {@link #cloneWithTypeParametersAndProxies(JvmTypeReference, JvmExecutable)}.
	 *
	 * @param fromOperation the operation from which the type parameters are copied.
	 * @param toOperation the operation that will receives the new type parameters.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 */
	protected void copyTypeParametersFromJvmOperation(JvmOperation fromOperation, JvmOperation toOperation,
			IBaseJvmModelInferrer baseInferrer) {
		Utils.copyTypeParametersFromJvmOperation(fromOperation, toOperation,
				baseInferrer.getJvmTypeReferenceBuilder(), this.jvmTypeBuilder,
				this.jvmTypeReferences, this.jvmTypesFactory);
	}

}
