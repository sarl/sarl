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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmUnknownTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.XbaseGenerated;
import org.eclipse.xtext.xbase.validation.ReadAndWriteTracking;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.collect.Sets;
import com.google.inject.Inject;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.annotation.DefaultValueUse;
import io.sarl.lang.core.annotation.Injectable;
import io.sarl.lang.core.annotation.NoEqualityTestFunctionsGeneration;
import io.sarl.lang.core.annotation.PerceptGuardEvaluator;
import io.sarl.lang.core.annotation.SarlElementType;
import io.sarl.lang.core.annotation.SarlSpecification;
import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.jvmmodel.GenerationContext;
import io.sarl.lang.jvmmodel.GenerationContext.BehaviorUnitGuardEvaluators;
import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;
import io.sarl.lang.sarl.actionprototype.InferredValuedParameter;
import io.sarl.lang.typesystem.InheritanceHelper;
import io.sarl.lang.util.Utils;
import io.sarl.lang.util.Utils.TypeParameterStatus;

/** Abstract implementation of a fragment that may be used for inferring type to JVM model.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.15
 */
public abstract class AbstractJvmModelInferrerTypeFragment extends AbstractJvmModelInferrerFragment {

	private static final String EQUALS_FUNCTION_NAME = "equals"; //$NON-NLS-1$

	private static final String HASHCODE_FUNCTION_NAME = "hashCode"; //$NON-NLS-1$

	private static final String CLONE_FUNCTION_NAME = "clone"; //$NON-NLS-1$

	private static final String SERIAL_FIELD_NAME = "serialVersionUID"; //$NON-NLS-1$

	private static final Set<Class<?>> EQUALITY_TEST_TYPES = new TreeSet<>((ele1, ele2) -> ele1.getName().compareTo(ele2.getName()));

	static {
		EQUALITY_TEST_TYPES.add(Boolean.TYPE);
		EQUALITY_TEST_TYPES.add(Boolean.class);
		EQUALITY_TEST_TYPES.add(Integer.TYPE);
		EQUALITY_TEST_TYPES.add(Integer.class);
		EQUALITY_TEST_TYPES.add(Character.TYPE);
		EQUALITY_TEST_TYPES.add(Character.class);
		EQUALITY_TEST_TYPES.add(Byte.TYPE);
		EQUALITY_TEST_TYPES.add(Byte.class);
		EQUALITY_TEST_TYPES.add(Short.TYPE);
		EQUALITY_TEST_TYPES.add(Short.class);
		EQUALITY_TEST_TYPES.add(Long.TYPE);
		EQUALITY_TEST_TYPES.add(Long.class);
		EQUALITY_TEST_TYPES.add(Float.TYPE);
		EQUALITY_TEST_TYPES.add(Float.class);
		EQUALITY_TEST_TYPES.add(Double.TYPE);
		EQUALITY_TEST_TYPES.add(Double.class);
		EQUALITY_TEST_TYPES.add(String.class);
		EQUALITY_TEST_TYPES.add(UUID.class);
	}

	@Inject
	private InheritanceHelper inheritanceHelper;

	/** The tracker of the read/write accesses to the fields.
	 *
	 * @since 0.15
	 */
	@Inject
	protected ReadAndWriteTracking readAndWriteTracking;

	/** Generate the missed operations that are the results from the generation of actions with default value parameters.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param source the SARL container.
	 * @param target the JVM feature container.
	 * @param ignoreOverridableOperations indicates if the operations must not be added if they are marked has
	 *     overridable.
	 * @param context description of the generation context in which the members must be considered.
	 */
	protected void appendSyntheticDefaultValuedParameterMethods(
			IBaseJvmModelInferrer baseInferrer,
			XtendTypeDeclaration source,
			JvmDeclaredType target,
			boolean ignoreOverridableOperations,
			GenerationContext context) {

		// Generate the different operations.
		final var differedGeneration = context.getPreFinalizationElements().iterator();
		while (differedGeneration.hasNext()) {
			final var runnable = differedGeneration.next();
			differedGeneration.remove();
			runnable.run();
		}

		// Generated the missed functions that are the result of the generation of operations
		// with default values.
		var actIndex = context.getActionIndex();

		final var inheritedOperations = context.getInheritedOperationsToImplement();

		for (final var missedOperation : inheritedOperations.entrySet()) {

			final var originalSignature = this.annotationUtils.findStringValue(
					missedOperation.getValue(), DefaultValueUse.class);
			if (!Strings.isNullOrEmpty(originalSignature)) {

				// Find the definition of the operation from the inheritance context.
				final var redefinedOperation = inheritedOperations.get(
						this.sarlSignatureProvider.createActionPrototype(
								missedOperation.getKey().getActionName(),
								this.sarlSignatureProvider.createParameterTypesFromString(originalSignature)));
				if (redefinedOperation != null) {
					final var parameterTypes = this.sarlSignatureProvider.createParameterTypesFromJvmModel(
							redefinedOperation.isVarArgs(), redefinedOperation.getParameters());
					final var qualifiedActionName = this.sarlSignatureProvider.createQualifiedActionName(
							missedOperation.getValue().getDeclaringType(),
							redefinedOperation.getSimpleName());

					// Retrieve the inferred prototype (including the prototypes with optional arguments)
					var redefinedPrototype = this.sarlSignatureProvider.getPrototypes(
							context.getActionPrototypeContext(this.sarlSignatureProvider),
							qualifiedActionName, parameterTypes);
					if (redefinedPrototype == null) {
						// The original operation was not parsed by the SARL compiler in the current run-time context.
						redefinedPrototype = this.sarlSignatureProvider.createPrototypeFromJvmModel(
								context.getActionPrototypeContext(this.sarlSignatureProvider),
								qualifiedActionName, redefinedOperation.isVarArgs(),
								redefinedOperation.getParameters());
					}

					// Retrieve the specification of the formal parameters that will be used for
					// determining the calling arguments.
					final var argumentSpec = redefinedPrototype.getInferredParameterTypes().get(
							missedOperation.getKey().getParametersTypes());

					// Create the missed java operation.
					final var op = this.jvmTypeBuilder.toMethod(
							source,
							missedOperation.getValue().getSimpleName(),
							missedOperation.getValue().getReturnType(),
							null);
					op.setVarArgs(missedOperation.getValue().isVarArgs());
					op.setFinal(true);

					final var arguments = new ArrayList<String>();

					// Create the formal parameters.
					for (final var parameter : argumentSpec) {
						if (parameter instanceof InferredValuedParameter inferredParameter) {
							arguments.add(this.sarlSignatureProvider.toJavaArgument(
									target.getIdentifier(), inferredParameter.getCallingArgument()));
						} else {
							arguments.add(parameter.getName());
							final var jvmParam = this.jvmTypesFactory.createJvmFormalParameter();
							jvmParam.setName(parameter.getName());
							jvmParam.setParameterType(this.jvmTypeBuilder.cloneWithProxies(parameter.getType().toTypeReference()));
							this.associator.associate(parameter.getParameter(), jvmParam);
							op.getParameters().add(jvmParam);
						}
					}

					// Create the body
					setBody(op,
							it -> {
								it.append(redefinedOperation.getSimpleName());
								it.append("("); //$NON-NLS-1$
								it.append(IterableExtensions.join(arguments, ", ")); //$NON-NLS-1$
								it.append(");"); //$NON-NLS-1$
							});

					// Add the annotations.
					addAnnotationSafe(baseInferrer, op, DefaultValueUse.class, originalSignature);
					appendGeneratedAnnotation(baseInferrer, op, context);

					// Add the operation in the container.
					target.getMembers().add(op);
					++actIndex;
				}
			}
		}
		context.setActionIndex(actIndex);
	}

	/** Append the @FunctionalInterface to the given type if it is a functional interface according
	 * to the Java 8 specification definition.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param type the type to update.
	 */
	protected void appendFunctionalInterfaceAnnotation(IBaseJvmModelInferrer baseInferrer, JvmGenericType type) {
		if (type != null && Utils.isFunctionalInterface(type, this.sarlSignatureProvider)
				&& this.jvmAnnotationFinder.findAnnotation(type, FunctionalInterface.class) == null) {
			addAnnotationSafe(baseInferrer, type, FunctionalInterface.class);
		}
	}

	/** Append the SARL specification version as an annotation to the given container.
	 *
	 * <p>The added annotation may be used by any underground platform for determining what is
	 * the version of the SARL specification that was used for generating the container.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	protected void appendSARLSpecificationVersion(IBaseJvmModelInferrer baseInferrer, GenerationContext context, XtendTypeDeclaration source,
			JvmDeclaredType target) {
		addAnnotationSafe(baseInferrer, target, SarlSpecification.class, SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING);
	}

	/** Append the SARL element type as an annotation to the given container.
	 *
	 * <p>The added annotation may be used by any underground platform for determining what is
	 * the type of the SARL element without invoking the costly "instanceof" operations.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	protected void appendSARLElementType(IBaseJvmModelInferrer baseInferrer, XtendTypeDeclaration source, JvmDeclaredType target) {
		addAnnotationSafe(target, SarlElementType.class, source.eClass().getClassifierID());
	}


	/** Append the injectable annotation to the given container.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param target the inferred JVM object.
	 * @param context the generation context.
	 * @since 0.12
	 */
	protected void appendInjectableAnnotationIfInjectable(IBaseJvmModelInferrer baseInferrer, JvmDeclaredType target, GenerationContext context) {
		if (context.isInjectable()) {
			addAnnotationSafe(baseInferrer, target, Injectable.class);
		}
	}

	/** Add the @XbaseGenerated annotation to the given target.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param target the target of the annotation.
	 */
	protected void appendXbaseGeneratedAnnotation(IBaseJvmModelInferrer baseInferrer, JvmAnnotationTarget target) {
		if (target instanceof JvmExecutable || target instanceof JvmDeclaredType) {
			addAnnotationSafe(baseInferrer, target, XbaseGenerated.class);
		}
	}

	/** Create the functions that permits to compare the object.
	 * The comparaison functions are {@link #equals(Object)} and {@link #hashCode()}.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	protected void appendComparisonFunctions(IBaseJvmModelInferrer baseInferrer, GenerationContext context, XtendTypeDeclaration source,
			JvmGenericType target) {
		if (isAppendComparisonFunctionsEnable(context, target)) {
			var isEqualsUserDefined = false;
			var isHashCodeUserDefined = false;
			for (final var operation : target.getDeclaredOperations()) {
				if (Objects.equal(EQUALS_FUNCTION_NAME, operation.getSimpleName())
						&& operation.getParameters().size() == 1
						&& Objects.equal(Object.class.getName(),
								operation.getParameters().get(0).getParameterType().getIdentifier())) {
					isEqualsUserDefined = true;
				} else if (Objects.equal(HASHCODE_FUNCTION_NAME, operation.getSimpleName())
						&& operation.getParameters().isEmpty()) {
					isHashCodeUserDefined = true;
				}
			}

			if (!isEqualsUserDefined || !isHashCodeUserDefined) {
				// Create a list of the declared non-static fields.
				final var declaredInstanceFields = new ArrayList<JvmField>();
				for (final var field : target.getDeclaredFields()) {
					if (isEqualityTestValidField(field)) {
						declaredInstanceFields.add(field);
					}
				}

				if (!declaredInstanceFields.isEmpty()) {
					final var finalOperations = new TreeMap<ActionPrototype, JvmOperation>();
					Utils.populateInheritanceContext(
							target, finalOperations, null, null, null, null, this.sarlSignatureProvider);

					var couldCreateEqualsFunction = true;
					if (!isEqualsUserDefined) {
						final var prototype = new ActionPrototype(EQUALS_FUNCTION_NAME,
								this.sarlSignatureProvider.createParameterTypesFromString(Object.class.getName()), false);
						couldCreateEqualsFunction = !finalOperations.containsKey(prototype);
					}

					var couldCreateHashCodeFunction = true;
					if (!isHashCodeUserDefined) {
						final var prototype = new ActionPrototype(HASHCODE_FUNCTION_NAME,
								this.sarlSignatureProvider.createParameterTypesForVoid(), false);
						couldCreateHashCodeFunction = !finalOperations.containsKey(prototype);
					}

					if (couldCreateEqualsFunction && couldCreateHashCodeFunction) {
						if (!isEqualsUserDefined) {
							final var op = toEqualsMethod(baseInferrer, source, target, declaredInstanceFields,
									context.getGeneratorConfig2().isGeneratePureAnnotation());
							if (op != null) {
								appendGeneratedAnnotation(baseInferrer, op, context);
								target.getMembers().add(op);
							}
						}

						if (!isHashCodeUserDefined) {
							final var op = toHashCodeMethod(baseInferrer, source, declaredInstanceFields,
									context.getGeneratorConfig2().isGeneratePureAnnotation());
							if (op != null) {
								appendGeneratedAnnotation(baseInferrer, op, context);
								target.getMembers().add(op);
							}
						}
					}
				}
			}
		}
	}

	private boolean isAppendComparisonFunctionsEnable(GenerationContext context, JvmGenericType target) {
		if (context.getGeneratorConfig2().isGenerateEqualityTestFunctions()) {
			var current = target;
			final var encounteredTypes = new TreeSet<String>();
			do {
				encounteredTypes.add(current.getIdentifier());
				if (this.jvmAnnotationFinder.findAnnotation(current, NoEqualityTestFunctionsGeneration.class) != null) {
					return false;
				}
				final var superType = current.getExtendedClass();
				current = null;
				if (superType != null) {
					final var type = superType.getType();
					if (type instanceof JvmGenericType cvalue) {
						current = cvalue;
						if (encounteredTypes.contains(current.getIdentifier())) {
							current = null;
						}
					}
				}
			} while (current != null);
			return true;
		}
		return false;
	}

	private boolean isEqualityTestValidField(JvmField field) {
		return !field.isStatic() && !SarlUtils.isHiddenMember(field.getSimpleName())
				&& this.jvmAnnotationFinder.findAnnotation(field, NoEqualityTestFunctionsGeneration.class) == null;
	}

	private boolean isEqualityTestValidField(JvmTypeReference reference) {
		for (final var type : EQUALITY_TEST_TYPES) {
			if (this.jvmTypeReferences.is(reference, type)) {
				return true;
			}
		}
		return false;
	}

	/** Generate the "equals()" operation.
	 * This function was deprecated in Xbase, and should be provided by DSL
	 * providers now.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param sarlElement the SARL element for which the "equals function must be generated.
	 * @param declaredType the declating type.
	 * @param jvmFields the fields declared in the container.
	 * @param generatePureAnnotation indicates if the {@code @Pure} annotation should be generated.
	 * @return the "equals" function.
	 */
	private JvmOperation toEqualsMethod(
			IBaseJvmModelInferrer baseInferrer,
			XtendTypeDeclaration sarlElement,
			final JvmDeclaredType declaredType,
			final Iterable<JvmField> jvmFields,
			boolean generatePureAnnotation) {
		if (sarlElement == null || declaredType == null) {
			return null;
		}

		final var result = this.jvmTypeBuilder.toMethod(sarlElement, EQUALS_FUNCTION_NAME,
				baseInferrer.getJvmTypeReferenceBuilder().typeRef(boolean.class), null);
		if (result == null) {
			return null;
		}
		addAnnotationSafe(baseInferrer, result, Override.class);
		if (generatePureAnnotation) {
			addAnnotationSafe(baseInferrer, result, Pure.class);
		}

		final var param = this.jvmTypesFactory.createJvmFormalParameter();
		param.setName("obj"); //$NON-NLS-1$
		param.setParameterType(baseInferrer.getJvmTypeReferenceBuilder().typeRef(Object.class));
		this.associator.associate(sarlElement, param);
		result.getParameters().add(param);
		setBody(result, new Procedures.Procedure1<ITreeAppendable>() {
			@Override
			public void apply(ITreeAppendable it) {
				var firstAttr = true;
				for (final var field : jvmFields) {
					if (isEqualityTestValidField(field.getType())) {
						if (firstAttr) {
							firstAttr = false;
							it.append("if (this == obj)").increaseIndentation(); //$NON-NLS-1$
							it.newLine().append("return true;").decreaseIndentation(); //$NON-NLS-1$
							it.newLine().append("if (obj == null)").increaseIndentation(); //$NON-NLS-1$
							it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
							it.newLine().append("if (getClass() != obj.getClass())").increaseIndentation(); //$NON-NLS-1$
							it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
							final var currentTypeName = new StringBuilder();
							currentTypeName.append(declaredType.getSimpleName());
							final var typeParameters = getTypeParametersFor(sarlElement);
							if (!typeParameters.isEmpty()) {
								currentTypeName.append("<"); //$NON-NLS-1$
								var first = true;
								for (final var typeParameter : typeParameters) {
									if (first) {
										first = false;
									} else {
										currentTypeName.append(", "); //$NON-NLS-1$
									}
									currentTypeName.append(typeParameter.getName());
								}
								currentTypeName.append(">"); //$NON-NLS-1$
							}
							it.newLine().append(currentTypeName).append(" other = ("); //$NON-NLS-1$
							it.append(currentTypeName).append(") obj;").newLine(); //$NON-NLS-1$
						}
						generateToEqualForField(it, field);
					}
				}
				it.append("return super.").append(EQUALS_FUNCTION_NAME); //$NON-NLS-1$
				it.append("(obj);"); //$NON-NLS-1$
			}

			private void generateToEqualForObjectNullity(ITreeAppendable it, JvmField field) {
				it.append("if (other.").append(field.getSimpleName()); //$NON-NLS-1$
				it.append(" == null) {").increaseIndentation(); //$NON-NLS-1$
				it.newLine().append("if (this.").append(field.getSimpleName()).append(" != null)").increaseIndentation(); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine().append("return false;").decreaseIndentation().decreaseIndentation(); //$NON-NLS-1$
				it.newLine().append("} else if (this."); //$NON-NLS-1$
				it.append(field.getSimpleName()).append(" == null)").increaseIndentation(); //$NON-NLS-1$
				it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
			}

			private void generateToEqualForObjectTest(ITreeAppendable it, JvmField field, String convertName) {
				if (convertName != null) {
					it.newLine().append("if (other.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(" != null && other.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(".").append(convertName).append("Value() != this.").append(field.getSimpleName()); //$NON-NLS-1$ //$NON-NLS-2$
					it.append(".").append(convertName).append("Value())").increaseIndentation(); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				} else {
					it.newLine().append("if (!").append(java.util.Objects.class); //$NON-NLS-1$
					it.append(".equals(this.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(", other.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append("))").increaseIndentation(); //$NON-NLS-1$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				}
			}

			private void generateToEqualForField(ITreeAppendable it, JvmField field) {
				final var refs = AbstractJvmModelInferrerTypeFragment.this.jvmTypeReferences;
				final var type = field.getType();
				if (refs.is(type, Boolean.TYPE)
						|| refs.is(type, Integer.TYPE)
						|| refs.is(type, Long.TYPE)
						|| refs.is(type, Character.TYPE)
						|| refs.is(type, Byte.TYPE)
						|| refs.is(type, Short.TYPE)) {
					it.append("if (other.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(" != this.").append(field.getSimpleName()).append(")").increaseIndentation(); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				} else if (refs.is(type, Double.TYPE)) {
					it.append("if (Double.doubleToLongBits(other.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(") != Double.doubleToLongBits(this.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append("))").increaseIndentation(); //$NON-NLS-1$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				} else if (refs.is(type, Double.class)) {
					generateToEqualForObjectNullity(it, field);
					it.newLine().append("if (other.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(" != null && Double.doubleToLongBits(other.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(".doubleValue()) != Double.doubleToLongBits(this.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(".doubleValue()))").increaseIndentation(); //$NON-NLS-1$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				} else if (refs.is(type, Float.TYPE)) {
					it.append("if (Float.floatToIntBits(other.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(") != Float.floatToIntBits(this.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append("))").increaseIndentation(); //$NON-NLS-1$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				} else if (refs.is(type, Float.class)) {
					generateToEqualForObjectNullity(it, field);
					it.newLine().append("if (other.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(" != null && Float.floatToIntBits(other.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(".floatValue()) != Float.floatToIntBits(this.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(".floatValue()))").increaseIndentation(); //$NON-NLS-1$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				} else if (refs.is(type, Byte.class)
						|| refs.is(type, Short.class)
						|| refs.is(type, Integer.class) || refs.is(type, AtomicInteger.class)
						|| refs.is(type, Long.class) || refs.is(type, AtomicLong.class)
						|| refs.is(type, Boolean.class)
						|| refs.is(type, Character.class)) {
					generateToEqualForObjectNullity(it, field);
					final String conv;
					if (refs.is(type, Byte.class)) {
						conv = "byte"; //$NON-NLS-1$
					} else if (refs.is(type, Short.class)) {
						conv = "short"; //$NON-NLS-1$
					} else if (refs.is(type, Integer.class)) {
						conv = "int"; //$NON-NLS-1$
					} else if (refs.is(type, Long.class)) {
						conv = "long"; //$NON-NLS-1$
					} else if (refs.is(type, Character.class)) {
						conv = "char"; //$NON-NLS-1$
					} else if (refs.is(type, Boolean.class)) {
						conv = "boolean"; //$NON-NLS-1$
					} else {
						conv = null;
					}
					generateToEqualForObjectTest(it, field, conv);
				} else {
					it.append("if (!").append(java.util.Objects.class); //$NON-NLS-1$
					it.append(".equals(this.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append(", other.").append(field.getSimpleName()); //$NON-NLS-1$
					it.append("))").increaseIndentation(); //$NON-NLS-1$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				}
				it.newLine();
			}
		});

		return result;
	}

	/** Replies the type parameters for the given type.
	 *
	 * @param type the type.
	 * @return the type parameters for the given type.
	 */
	@SuppressWarnings("static-method")
	private List<JvmTypeParameter> getTypeParametersFor(XtendTypeDeclaration type) {
		if (type instanceof XtendClass cvalue) {
			return cvalue.getTypeParameters();
		}
		if (type instanceof XtendInterface cvalue) {
			return cvalue.getTypeParameters();
		}
		return Collections.emptyList();
	}

	/** Generate the "hashCode()" operation.
	 * This function was deprecated in Xbase, and should be provided by DSL
	 * providers now.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param sarlElement the SARL element for which the "hashCode" msut be generated.
	 * @param jvmFields the fields declared in the container.
	 * @return the "hashCode" function.
	 */
	private JvmOperation toHashCodeMethod(
			IBaseJvmModelInferrer baseInferrer,
			XtendTypeDeclaration sarlElement,
			final Iterable<JvmField> jvmFields,
			boolean generatePureAnnotation) {
		if (sarlElement == null) {
			return null;
		}
		final var result = this.jvmTypeBuilder.toMethod(sarlElement, HASHCODE_FUNCTION_NAME,
				baseInferrer.getJvmTypeReferenceBuilder().typeRef(int.class), null);
		if (result == null) {
			return null;
		}
		addAnnotationSafe(baseInferrer, result, Override.class);
		if (generatePureAnnotation) {
			addAnnotationSafe(baseInferrer, result, Pure.class);
		}
		setBody(result, it -> {
			final var refs = this.jvmTypeReferences;
			it.append("int result = super.").append(HASHCODE_FUNCTION_NAME); //$NON-NLS-1$
			it.append("();"); //$NON-NLS-1$
			var firstAttr = true;
			for (final var field : jvmFields) {
				if (isEqualityTestValidField(field.getType())) {
					if (firstAttr) {
						firstAttr = false;
						it.newLine().append("final int prime = 31;"); //$NON-NLS-1$
					}
					final var type = field.getType();
					if (refs.is(type, Boolean.TYPE)) {
						it.newLine().append("result = prime * result + Boolean.hashCode(this."); //$NON-NLS-1$
						it.append(field.getSimpleName()).append(");"); //$NON-NLS-1$
					} else if (refs.is(type, Character.TYPE)) {
						it.newLine().append("result = prime * result + Character.hashCode(this."); //$NON-NLS-1$
						it.append(field.getSimpleName()).append(");"); //$NON-NLS-1$
					} else if (refs.is(type, Byte.TYPE)) {
						it.newLine().append("result = prime * result + Byte.hashCode(this."); //$NON-NLS-1$
						it.append(field.getSimpleName()).append(");"); //$NON-NLS-1$
					} else if (refs.is(type, Short.TYPE)) {
						it.newLine().append("result = prime * result + Short.hashCode(this."); //$NON-NLS-1$
						it.append(field.getSimpleName()).append(");"); //$NON-NLS-1$
					} else if (refs.is(type, Integer.TYPE)) {
						it.newLine().append("result = prime * result + Integer.hashCode(this."); //$NON-NLS-1$
						it.append(field.getSimpleName()).append(");"); //$NON-NLS-1$
					} else if (refs.is(type, Long.TYPE)) {
						it.newLine().append("result = prime * result + Long.hashCode(this."); //$NON-NLS-1$
						it.append(field.getSimpleName()).append(");"); //$NON-NLS-1$
					} else if (refs.is(type, Float.TYPE)) {
						it.newLine().append("result = prime * result + Float.hashCode(this."); //$NON-NLS-1$
						it.append(field.getSimpleName()).append(");"); //$NON-NLS-1$
					} else if (refs.is(type, Double.TYPE)) {
						it.newLine().append("result = prime * result + Double.hashCode(this."); //$NON-NLS-1$
						it.append(field.getSimpleName()).append(");"); //$NON-NLS-1$
					} else {
						it.newLine().append("result = prime * result + "); //$NON-NLS-1$
						it.append(java.util.Objects.class).append(".hashCode(this."); //$NON-NLS-1$
						it.append(field.getSimpleName()).append(");"); //$NON-NLS-1$
					}
				}
			}
			it.newLine().append("return result;"); //$NON-NLS-1$
		});

		return result;
	}

	/** Append the clone function only if the type is a subtype of {@link Cloneable}.
	 *
	 * <p>The clone function replies a value of the current type, not {@code Object}.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 * @since 0.6
	 */
	protected void appendCloneFunctionIfCloneable(
			IBaseJvmModelInferrer baseInferrer,
			GenerationContext context, XtendTypeDeclaration source,
			JvmGenericType target) {
		if (!target.isInterface() && this.inheritanceHelper.isSubTypeOf(target, Cloneable.class, null)) {
			appendCloneFunction(baseInferrer, context, source, target);
		}
	}

	/** Append the clone function.
	 *
	 * <p>The clone function replies a value of the current type, not {@code Object}.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 * @since 0.6
	 */
	private void appendCloneFunction(
			IBaseJvmModelInferrer baseInferrer, GenerationContext context,
			XtendTypeDeclaration source, JvmGenericType target) {
		if (!isAppendCloneFunctionsEnable(context)) {
			return;
		}
		for (final var operation : target.getDeclaredOperations()) {
			if (CLONE_FUNCTION_NAME.equals(operation.getSimpleName())) {
				return;
			}
		}

		final var standardPrototype = new ActionPrototype(CLONE_FUNCTION_NAME,
				this.sarlSignatureProvider.createParameterTypesForVoid(), false);

		final var finalOperations = new TreeMap<ActionPrototype, JvmOperation>();
		Utils.populateInheritanceContext(
				target, finalOperations, null, null, null, null, this.sarlSignatureProvider);

		if (!finalOperations.containsKey(standardPrototype)) {
			final var genericParameters = new JvmTypeReference[target.getTypeParameters().size()];
			for (var i = 0; i < target.getTypeParameters().size(); ++i) {
				final var typeParameter = target.getTypeParameters().get(i);
				genericParameters[i] = baseInferrer.getJvmTypeReferenceBuilder().typeRef(typeParameter);
			}
			final var myselfReference = baseInferrer.getJvmTypeReferenceBuilder().typeRef(target, genericParameters);
			final var operation = this.jvmTypeBuilder.toMethod(
					source, CLONE_FUNCTION_NAME, myselfReference, null);
			target.getMembers().add(operation);
			operation.setVisibility(JvmVisibility.PUBLIC);
			addAnnotationSafe(baseInferrer, operation, Override.class);
			if (context.getGeneratorConfig2().isGeneratePureAnnotation()) {
				addAnnotationSafe(baseInferrer, operation, Pure.class);
			}
			final var myselfReference2 = Utils.toLightweightTypeReference(
					operation.getReturnType(), this.services);
			setBody(operation, it -> {
				it.append("try {"); //$NON-NLS-1$
				it.increaseIndentation().newLine();
				it.append("return (").append(myselfReference2).append(") super.");  //$NON-NLS-1$//$NON-NLS-2$
				it.append(CLONE_FUNCTION_NAME).append("();"); //$NON-NLS-1$
				it.decreaseIndentation().newLine();
				it.append("} catch (").append(Throwable.class).append(" exception) {"); //$NON-NLS-1$ //$NON-NLS-2$
				it.increaseIndentation().newLine();
				it.append("throw new ").append(Error.class).append("(exception);"); //$NON-NLS-1$ //$NON-NLS-2$
				it.decreaseIndentation().newLine();
				it.append("}"); //$NON-NLS-1$
			});
			appendGeneratedAnnotation(baseInferrer, operation, context);
		}
	}

	private static boolean isAppendCloneFunctionsEnable(GenerationContext context) {
		return context.getGeneratorConfig2().isGenerateCloneFunctions();
	}

	/** Add the default constructors.
	 *
	 * <p>The default constructors have the same signature as the constructors of the super class.
	 *
	 * <p>This function adds the default constructors if no constructor was already added. This condition
	 * is determined with a call to {@link GenerationContext#hasConstructor()}.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param source the SARL element in which no constructor was specified. This SARL element should be
	 *     associated to the {@code target} element.
	 * @param target the JVM type that is receiving the default constructor.
	 * @see GenerationContext#hasConstructor()
	 */
	protected void appendDefaultConstructors(IBaseJvmModelInferrer baseInferrer, XtendTypeDeclaration source, JvmGenericType target) {
		final var context =  baseInferrer.getContext(target);
		if (!context.hasConstructor()) {

			// Special case: if a value was not set, we cannot create implicit constructors
			// in order to have the issue message "The blank final field may not have been initialized".

			final var notInitializedValueField = Iterables.any(source.getMembers(), it -> {
				if (it instanceof XtendField op) {
					if (op.isFinal() && !op.isStatic() && op.getInitialValue() == null) {
						return true;
					}
				}
				return false;
			});

			if (!notInitializedValueField) {
				// Add the default constructors for the agent, if not already added
				final var reference = target.getExtendedClass();
				if (reference != null) {
					final var type = reference.getType();
					if (type instanceof JvmGenericType cvalue) {
						copyVisibleJvmConstructors(
								baseInferrer, context, cvalue,
								target, source, Sets.newTreeSet(),
								JvmVisibility.PUBLIC);
					}
				}
			}
		}
	}

	/** Replies the constructors from the super type that are visible.
	 *
	 * @return the visible inherited constructors.
	 * @since 0.15
	 */
	@SuppressWarnings("static-method")
	protected Iterable<JvmConstructor> getVisibleInheritedJvmConstructors(JvmGenericType source, JvmGenericType target) {
		final var samePackage = Objects.equal(source.getPackageName(), target.getPackageName());
		final var constructors = Iterables.transform(Iterables.filter(source.getMembers(), it -> {
			if (it instanceof JvmConstructor op) {
				return op.getVisibility() != JvmVisibility.PRIVATE
						&& (op.getVisibility() != JvmVisibility.DEFAULT || samePackage);
			}
			return false;
		}), it -> (JvmConstructor) it);
		return constructors;
	}

	/** Copy the JVM constructors from the source to the destination.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the current generation context.
	 * @param source the source.
	 * @param target the destination.
	 * @param sarlSource the SARL source element. If {@code null}, the generated constructors will not be associated to the SARL element.
	 * @param createdConstructors the set of constructors that are created before (input) or during (output) the invocation.
	 * @param minimalVisibility the minimal visibility to apply to the created constructors.
	 * @since 0.10
	 */
	private void copyVisibleJvmConstructors(
			IBaseJvmModelInferrer baseInferrer,
			GenerationContext context,
			JvmGenericType source, JvmGenericType target,
			XtendTypeDeclaration sarlSource, Set<ActionParameterTypes> createdConstructors,
			JvmVisibility minimalVisibility) {
		final var constructors = getVisibleInheritedJvmConstructors(source, target);

		// Sort the constructor in order to always add them in the same order.
		final var sortedConstructors = new TreeSet<Pair<JvmConstructor, ActionParameterTypes>>((elt1, elt2) -> elt1.getValue().compareTo(elt2.getValue()));
		for (final var constructor : constructors) {
			final var types = this.sarlSignatureProvider.createParameterTypesFromJvmModel(
					constructor.isVarArgs(), constructor.getParameters());
			sortedConstructors.add(new Pair<>(constructor, types));
		}

		for (final var pair : sortedConstructors) {
			if (createdConstructors.add(pair.getValue())) {
				final var constructor = pair.getKey();
				final var newCons = this.jvmTypesFactory.createJvmConstructor();
				newCons.setDeprecated(constructor.isDeprecated());
				newCons.setSimpleName(target.getSimpleName());
				target.getMembers().add(newCons);

				for (final var parameter : constructor.getParameters()) {
					final var newParam = this.jvmTypesFactory.createJvmFormalParameter();
					newParam.setName(parameter.getSimpleName());
					newCons.getParameters().add(newParam);

					final var originalParamTypeReference = parameter.getParameterType();
					final var paramType = cloneWithTypeParametersAndProxies(originalParamTypeReference, newCons, baseInferrer);
					assert originalParamTypeReference != paramType;
					newParam.setParameterType(paramType);

					for (final var annotationReference : parameter.getAnnotations()) {
						if (this.annotationUtils.findAnnotation(newParam, annotationReference.getAnnotation().getQualifiedName()) == null) {
							final var annotation = EcoreUtil.copy(annotationReference);
							if (annotation != null) {
								newParam.getAnnotations().add(annotation);
								this.associator.removeAllAssociation(annotation);
							}
						}
					}

					this.associator.removeAllAssociation(paramType);
					this.associator.removeAllAssociation(newParam);
				}

				newCons.setVarArgs(constructor.isVarArgs());

				var visibility = constructor.getVisibility();
				if (visibility != null && minimalVisibility != null && minimalVisibility.compareTo(visibility) > 0) {
					visibility = minimalVisibility;
				}
				newCons.setVisibility(visibility);

				setBody(newCons, it -> {
					it.append(this.grammarKeywordAccess.getSuperKeyword());
					it.append("("); //$NON-NLS-1$
					var first = true;
					for (final var parameter : newCons.getParameters()) {
						if (first) {
							first = false;
						} else {
							it.append(", "); //$NON-NLS-1$
						}
						it.append(parameter.getSimpleName());
					}
					it.append(");"); //$NON-NLS-1$
				});

				copyAndCleanDocumentationTo(constructor, newCons);

				appendGeneratedAnnotation(baseInferrer, newCons, baseInferrer.getContext(target));

				for (final var annotationReference : constructor.getAnnotations()) {
					final var annotationType = annotationReference.getAnnotation();
					if (isAccessibleTypeAccordingToJavaSpecifications(baseInferrer, context, annotationType)
							&& (this.annotationUtils.findAnnotation(newCons, annotationType.getQualifiedName()) == null)) {
						final var annotation = EcoreUtil.copy(annotationReference);
						if (annotation != null) {
							newCons.getAnnotations().add(annotation);
						}
					}
				}

				this.associator.removeAllAssociation(newCons);
			}
		}
	}

	/** Replies if the given type is accessible according to the Java specifications.
	 * Indeed, since Java11, several types, e.g. {@code HotSpotIntrinsicCandidate} annotation,
	 * are inaccessible because they are in private modules.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the generation context.
	 * @param type the type to test.
	 * @return {@code true} if the type is accessible.
	 */
	@SuppressWarnings("static-method")
	private boolean isAccessibleTypeAccordingToJavaSpecifications(IBaseJvmModelInferrer baseInferrer,
			GenerationContext context, JvmDeclaredType type) {
		// TODO find and use an API-oriented way to have access to the module access definitions.
		final var packageName = type.getPackageName();
		if (!Strings.isNullOrEmpty(packageName)) {
			return !packageName.contains(".internal"); //$NON-NLS-1$
		}
		return true;
	}

	/** Append the serial number field if and only if the container type is serializable.
	 *
	 * <p>The serial number field is computed from the given context and from the generated fields.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 * @see #appendSerialNumber(IBaseJvmModelInferrer, GenerationContext, XtendTypeDeclaration, JvmGenericType)
	 */
	protected void appendSerialNumberIfSerializable(IBaseJvmModelInferrer baseInferrer,
			GenerationContext context, XtendTypeDeclaration source, JvmGenericType target) {
		if (!target.isInterface() && this.inheritanceHelper.isSubTypeOf(target, Serializable.class, null)) {
			appendSerialNumber(baseInferrer, context, source, target);
		}
	}

	/** Append the serial number field.
	 *
	 * <p>The serial number field is computed from the given context and from the generated fields.
	 * The field is added if no field with name "serialVersionUID" was defined.
	 *
	 * <p>This function does not test if the field container is serializable.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 * @see #appendSerialNumberIfSerializable(IBaseJvmModelInferrer, GenerationContext, XtendTypeDeclaration, JvmGenericType)
	 */
	protected void appendSerialNumber(IBaseJvmModelInferrer baseInferrer, GenerationContext context,
			XtendTypeDeclaration source, JvmGenericType target) {
		if (!isAppendSerialNumbersEnable(context)) {
			return;
		}
		for (final var field : target.getDeclaredFields()) {
			if (SERIAL_FIELD_NAME.equals(field.getSimpleName())) {
				return;
			}
		}

		final var field = this.jvmTypesFactory.createJvmField();
		field.setSimpleName(SERIAL_FIELD_NAME);
		field.setVisibility(JvmVisibility.PRIVATE);
		field.setStatic(true);
		field.setTransient(false);
		field.setVolatile(false);
		field.setFinal(true);
		target.getMembers().add(field);
		field.setType(this.jvmTypeBuilder.cloneWithProxies(baseInferrer.getJvmTypeReferenceBuilder().typeRef(long.class)));
		final var serial = context.getSerial();
		this.jvmTypeBuilder.setInitializer(field, toStringConcatenation(serial + "L")); //$NON-NLS-1$
		appendGeneratedAnnotation(baseInferrer, field, context);
		this.readAndWriteTracking.markInitialized(field, null);
	}

	private static boolean isAppendSerialNumbersEnable(GenerationContext context) {
		return context.getGeneratorConfig2().isGenerateSerialNumberFields();
	}

	/** Generate the extended types for the given SARL statement.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the context of the generation.
	 * @param owner the JVM element to change.
	 * @param defaultJvmType the default JVM type.
	 * @param defaultSarlType the default SARL type.
	 * @param supertype the supertype.
	 */
	protected void appendConstrainedExtends(IBaseJvmModelInferrer baseInferrer, GenerationContext context,
			JvmGenericType owner, Class<?> defaultJvmType, Class<? extends XtendTypeDeclaration> defaultSarlType,
			JvmParameterizedTypeReference supertype) {
		final List<? extends JvmParameterizedTypeReference> supertypes;
		if (supertype == null) {
			supertypes = Collections.emptyList();
		} else {
			supertypes = Collections.singletonList(supertype);
		}
		appendConstrainedExtends(baseInferrer, context, owner, defaultJvmType, defaultSarlType, supertypes);
	}

	/** Generate the extended types for the given SARL statement.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the context of the generation.
	 * @param owner the JVM element to change.
	 * @param defaultJvmType the default JVM type.
	 * @param defaultSarlType the default Sarl type.
	 * @param supertypes the supertypes.
	 */
	protected void appendConstrainedExtends(IBaseJvmModelInferrer baseInferrer, GenerationContext context,
			JvmGenericType owner, Class<?> defaultJvmType, Class<? extends XtendTypeDeclaration> defaultSarlType,
			List<? extends JvmParameterizedTypeReference> supertypes) {
		boolean explicitType = false;
		final var ownerId = owner.getIdentifier();
		for (final var superType : supertypes) {
			String superTypeId;
			try {
				superTypeId = superType.getIdentifier();
			} catch (Exception ex) {
				baseInferrer.logInternalError(ex);
				superTypeId = null;
			}
			if (!Objects.equal(ownerId, superTypeId)
					&& superType.getType() instanceof JvmGenericType
					/*&& this.inheritanceHelper.isProxyOrSubTypeOf(superType, defaultJvmType, defaultSarlType, isInterface)*/) {
				owner.getSuperTypes().add(this.jvmTypeBuilder.cloneWithProxies(superType));
				context.incrementSerial(superType.getIdentifier().hashCode());
				explicitType = true;
			}
		}
		if (!explicitType) {
			final var type = baseInferrer.getJvmTypeReferenceBuilder().typeRef(defaultJvmType);
			if (!(type instanceof JvmUnknownTypeReference)) {
				owner.getSuperTypes().add(type);
			}
			context.incrementSerial(type.getIdentifier().hashCode());
		}
	}

	/** Generate the implemented types for the given SARL statement.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the context of the generation.
	 * @param owner the JVM element to change.
	 * @param defaultJvmType the default JVM type.
	 * @param defaultSarlType the default SARL type.
	 * @param implementedtypes the implemented types.
	 */
	protected void appendConstrainedImplements(IBaseJvmModelInferrer baseInferrer,
			GenerationContext context, JvmGenericType owner, Class<?> defaultJvmType,
			Class<? extends XtendTypeDeclaration> defaultSarlType,
			List<? extends JvmParameterizedTypeReference> implementedtypes) {
		var explicitType = false;
		for (final var superType : implementedtypes) {
			if (!Objects.equal(owner.getIdentifier(), superType.getIdentifier())
					&& superType.getType() instanceof JvmGenericType
					/*&& this.inheritanceHelper.isProxyOrSubTypeOf(superType, defaultJvmType, defaultSarlType, true)*/) {
				owner.getSuperTypes().add(this.jvmTypeBuilder.cloneWithProxies(superType));
				context.incrementSerial(superType.getIdentifier().hashCode());
				explicitType = true;
			}
		}
		if (!explicitType) {
			final var type = baseInferrer.getJvmTypeReferenceBuilder().typeRef(defaultJvmType);
			owner.getSuperTypes().add(type);
			context.incrementSerial(type.getIdentifier().hashCode());
		}
	}

	/** Copy the JVM operations from the source to the destination.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param source the source.
	 * @param target the destination.
	 * @param createdActions the set of actions that are created before (input) or during (output) the invocation.
	 * @param copyHiddenNames indicates if the operations with hidden name are copied.
	 * @param bodyBuilder the builder of the target's operations.
	 * @since 0.12
	 */
	protected void copyNonStaticPublicJvmOperations(IBaseJvmModelInferrer baseInferrer, JvmGenericType source,
			JvmGenericType target, Set<ActionPrototype> createdActions, boolean copyHiddenNames,
			Procedure2<? super JvmOperation, ? super ITreeAppendable> bodyBuilder) {
		final var operations = Iterables.transform(Iterables.filter(source.getMembers(), it -> {
			if (it instanceof JvmOperation op) {
				if (!op.isStatic() && op.getVisibility() == JvmVisibility.PUBLIC) {
					return copyHiddenNames || !SarlUtils.isHiddenMember(op.getSimpleName());
				}
				return false;
			}
			return false;
		}), it -> (JvmOperation) it);
		for (final var operation : operations) {
			final var types = this.sarlSignatureProvider.createParameterTypesFromJvmModel(
					operation.isVarArgs(), operation.getParameters());
			final var actSigKey = this.sarlSignatureProvider.createActionPrototype(
					operation.getSimpleName(), types);
			if (createdActions.add(actSigKey)) {
				final var newOp = this.jvmTypesFactory.createJvmOperation();
				target.getMembers().add(newOp);

				newOp.setAbstract(false);
				newOp.setFinal(false);
				newOp.setNative(false);
				newOp.setStatic(false);
				newOp.setSynchronized(false);
				newOp.setVisibility(JvmVisibility.PUBLIC);

				newOp.setDefault(operation.isDefault());
				newOp.setDeprecated(operation.isDeprecated());
				newOp.setSimpleName(operation.getSimpleName());
				newOp.setStrictFloatingPoint(operation.isStrictFloatingPoint());

				copyTypeParametersFromJvmOperation(baseInferrer, operation, newOp);

				for (final var exception : operation.getExceptions()) {
					newOp.getExceptions().add(cloneWithTypeParametersAndProxies(exception, newOp, baseInferrer));
				}

				for (final var parameter : operation.getParameters()) {
					final var newParam = this.jvmTypesFactory.createJvmFormalParameter();
					newOp.getParameters().add(newParam);
					newParam.setName(parameter.getSimpleName());
					newParam.setParameterType(cloneWithTypeParametersAndProxies(parameter.getParameterType(), newOp, baseInferrer));
				}

				newOp.setVarArgs(operation.isVarArgs());

				newOp.setReturnType(cloneWithTypeParametersAndProxies(operation.getReturnType(), newOp, baseInferrer));

				setBody(newOp, it -> bodyBuilder.apply(operation, it));
			}
		}
	}

	/** Copy the type parameters from a JvmOperation.
	 *
	 * <p>This function differs from {@link io.sarl.lang.jvmmodel.SARLJvmModelInferrer#copyAndFixTypeParameters(List, org.eclipse.xtext.common.types.JvmTypeParameterDeclarator)}
	 * and {@link io.sarl.lang.jvmmodel.SARLJvmModelInferrer#copyTypeParameters(List, org.eclipse.xtext.common.types.JvmTypeParameterDeclarator)}
	 * in the fact that the type parameters were already generated and fixed. The current function supper generic types by
	 * clone the types references with {@link #cloneWithTypeParametersAndProxies(JvmTypeReference, JvmExecutable, IBaseJvmModelInferrer)}.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param fromOperation the operation from which the type parameters are copied.
	 * @param toOperation the operation that will receives the new type parameters.
	 */
	protected void copyTypeParametersFromJvmOperation(IBaseJvmModelInferrer baseInferrer,
			JvmOperation fromOperation, JvmOperation toOperation) {
		Utils.copyTypeParametersFromJvmOperation(fromOperation, toOperation,
				baseInferrer.getJvmTypeReferenceBuilder(), this.jvmTypeBuilder,
				this.jvmTypeReferences, this.jvmTypesFactory);
	}

	/** Generate the code for the given SARL members in a agent-oriented container.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param featureContainerType the feature container.
	 * @param container the SARL container.
	 * @param context description of the generation context in which the members must be considered.
	 */
	protected void appendAOPMembers(IBaseJvmModelInferrer baseInferrer, JvmGenericType featureContainerType,
			XtendTypeDeclaration container, GenerationContext context) {

		Utils.populateInheritanceContext(
				featureContainerType,
				context.getInheritedFinalOperations(),
				context.getInheritedOverridableOperations(),
				null,
				context.getInheritedOperationsToImplement(),
				null,
				this.sarlSignatureProvider);

		final var delayedMembers = new LinkedList<XtendMember>();

		for (final var feature : container.getMembers()) {
			if (context.isSupportedMember(feature)) {
				if ((feature instanceof SarlCapacityUses)
						|| (feature instanceof SarlRequiredCapacity)) {
					delayedMembers.add(feature);
				} else {
					baseInferrer.transform(feature, featureContainerType, true);
				}
			}
		}

		for (final var feature : delayedMembers) {
			baseInferrer.transform(feature, featureContainerType, false);
		}

		// Add event handlers
		appendEventGuardEvaluators(baseInferrer, featureContainerType);

		// Add dispatch methods
		baseInferrer.appendSyntheticDispatchMethods(container, featureContainerType);

		// Add SARL synthetic functions
		appendSyntheticDefaultValuedParameterMethods(baseInferrer,  container,
				featureContainerType, true, context);
	}

	/** Append the guard evaluators.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param container the container type.
	 */
	private void appendEventGuardEvaluators(IBaseJvmModelInferrer baseInferrer, JvmGenericType container) {
		final var context = baseInferrer.getContext(container);
		if (context != null) {
			final var allEvaluators = context.getGuardEvaluationCodes();
			if (allEvaluators == null || allEvaluators.isEmpty()) {
				return;
			}

			final var guardDefs = new BehaviorUnitDefinitions();

			for (final var evaluators : allEvaluators) {
				final var behName = appendEventGuardEvaluatorForReflectMethod(baseInferrer, evaluators, container, context);
				final var functionNames = guardDefs.getFunctionsFor(evaluators.eventType());
				final List<JvmTypeReference> typeParameters = new ArrayList<>();
				final var result = Utils.forEachTypeParameterName(evaluators.eventType(), (name, i) -> {
					typeParameters.add(name);
				});
				functionNames.registerFunction(behName, result == TypeParameterStatus.EXPLICIT_DIRECT_TYPE ? typeParameters : Collections.emptyList());
			}

			var isRootType = true;
			final var superType = container.getExtendedClass();
			if (superType != null) {
				final var containerName = superType.getIdentifier();
				isRootType = containerName.equals(Agent.class.getName())
						|| containerName.equals(Behavior.class.getName())
						|| containerName.equals(Skill.class.getName());
			}

			appendEventGuardEvaluatorsForPolymorphicMethod(baseInferrer, guardDefs, isRootType, container, context);
		}
	}

	/** Append the guard evaluators for the reflection-based method.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param evaluators the guard evaluators to generate.
	 * @param container the receiver of the generated components.
	 * @param context the generation context.
	 * @return the name of the generated function.
	 * @since 0.12
	 */
	private String appendEventGuardEvaluatorForReflectMethod(
			IBaseJvmModelInferrer baseInferrer, BehaviorUnitGuardEvaluators evaluators,
			JvmGenericType container, GenerationContext context) {
		final var source = evaluators.source();
		final var sourceId = evaluators.eventType();
		final var voidType = baseInferrer.getJvmTypeReferenceBuilder().typeRef(Void.TYPE);
		final var runnableType = baseInferrer.getJvmTypeReferenceBuilder().typeRef(Runnable.class);
		final var collectionType = baseInferrer.getJvmTypeReferenceBuilder().typeRef(Collection.class, runnableType);

		// Force the event type to be expressed in its raw form in the guard function's prototype
		final var rawEventId = sourceId.getType();
		final var rawEventReference = this.jvmTypeReferences.createTypeRef(rawEventId);
		rawEventReference.getArguments().clear();

		// Determine the name of the operation for the behavior output
		final var behaviorUnitName = Utils.createNameForHiddenGuardGeneralEvaluatorMethod(sourceId);

		// Create the main function
		final var operation = this.jvmTypesFactory.createJvmOperation();

		// Annotation for the event bus

		appendGeneratedAnnotation(baseInferrer, operation, context);
		final var guardAnnotation = addAnnotationSafe(baseInferrer, operation, PerceptGuardEvaluator.class);
		if (guardAnnotation != null) {
			final var bounds  = Utils.getTypeParameterBoundsFor(sourceId, this.jvmTypeReferences);
			if (bounds != null && !bounds.isEmpty()) {
				JvmOperation boundOperation = null;
				for (final var guardAnnotationOperation : guardAnnotation.getAnnotation().getDeclaredOperations()) {
					if ("typeParameters".equals(guardAnnotationOperation.getSimpleName())) { //$NON-NLS-1$
						boundOperation = guardAnnotationOperation;
						break;
					}
				}
				final var generics = this.jvmTypesFactory.createJvmTypeAnnotationValue();
				for (final var boundType : bounds) {
					generics.getValues().add(this.jvmTypeBuilder.cloneWithProxies(boundType));
				}
				generics.setOperation(boundOperation);
				guardAnnotation.getExplicitValues().add(generics);
			}
		}

		// Guard evaluator unit parameters
		// - Event occurrence
		var jvmParam = this.jvmTypesFactory.createJvmFormalParameter();
		jvmParam.setName(this.grammarKeywordAccess.getOccurrenceKeyword());
		jvmParam.setParameterType(rawEventReference);
		this.associator.associate(source, jvmParam);
		operation.getParameters().add(jvmParam);
		// - List of runnables
		jvmParam = this.jvmTypesFactory.createJvmFormalParameter();
		jvmParam.setName(RUNNABLE_COLLECTION);
		jvmParam.setParameterType(this.jvmTypeBuilder.cloneWithProxies(collectionType));
		operation.getParameters().add(jvmParam);

		operation.setAbstract(false);
		operation.setNative(false);
		operation.setSynchronized(false);
		operation.setStrictFloatingPoint(false);
		operation.setFinal(false);
		operation.setVisibility(JvmVisibility.PRIVATE);
		operation.setStatic(false);
		operation.setSimpleName(behaviorUnitName);
		operation.setReturnType(this.jvmTypeBuilder.cloneWithProxies(voidType));
		container.getMembers().add(operation);

		setBody(operation, it -> {
			it.append("assert "); //$NON-NLS-1$
			it.append(this.grammarKeywordAccess.getOccurrenceKeyword());
			it.append(" != null;"); //$NON-NLS-1$
			it.newLine();
			it.append("assert "); //$NON-NLS-1$
			it.append(RUNNABLE_COLLECTION);
			it.append(" != null;"); //$NON-NLS-1$
			for (final var code : evaluators.evaluators()) {
				it.newLine();
				code.apply(it);
			}
		});

		this.associator.associatePrimary(source, operation);
		this.jvmTypeBuilder.copyDocumentationTo(source, operation);

		return behaviorUnitName;
	}

	/** Append the guard evaluators for the polymorphic method.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param guardDefs the definition of the guards.
	 * @param isRootType indicates if the containing type is considered as a root type from the polymorphic method
	 *     point of view.
	 * @param container the receiver of the generated components.
	 * @param context the generation context.
	 * @since 0.12
	 */
	private void appendEventGuardEvaluatorsForPolymorphicMethod(
			IBaseJvmModelInferrer baseInferrer, BehaviorUnitDefinitions guardDefs,
			boolean isRootType, JvmGenericType container, GenerationContext context) {
		final var voidType = baseInferrer.getJvmTypeReferenceBuilder().typeRef(Void.TYPE);

		// Function "$getSupportedEvents"
		final var eventTypeOperation = this.jvmTypesFactory.createJvmOperation();
		appendGeneratedAnnotation(baseInferrer, eventTypeOperation, context);
		addAnnotationSafe(baseInferrer, eventTypeOperation, Override.class);

		eventTypeOperation.setAbstract(false);
		eventTypeOperation.setNative(false);
		eventTypeOperation.setSynchronized(false);
		eventTypeOperation.setStrictFloatingPoint(false);
		eventTypeOperation.setFinal(false);
		eventTypeOperation.setVisibility(JvmVisibility.PUBLIC);
		eventTypeOperation.setStatic(false);
		eventTypeOperation.setSimpleName(SarlUtils.HIDDEN_MEMBER_CHARACTER + "getSupportedEvents"); //$NON-NLS-1$
		eventTypeOperation.setReturnType(this.jvmTypeBuilder.cloneWithProxies(voidType));

		final var jvmParam0 = this.jvmTypesFactory.createJvmFormalParameter();
		jvmParam0.setName("toBeFilled"); //$NON-NLS-1$
		jvmParam0.setParameterType(baseInferrer.getJvmTypeReferenceBuilder().typeRef(Set.class,
				baseInferrer.getJvmTypeReferenceBuilder().typeRef(Class.class,
						baseInferrer.getJvmTypeReferenceBuilder().wildcardExtends(
								baseInferrer.getJvmTypeReferenceBuilder().typeRef(Event.class)))));
		eventTypeOperation.getParameters().add(jvmParam0);

		container.getMembers().add(eventTypeOperation);

		setBody(eventTypeOperation, it -> {
			it.append("super.").append(SarlUtils.HIDDEN_MEMBER_CHARACTER); //$NON-NLS-1$
			it.append("getSupportedEvents(toBeFilled);"); //$NON-NLS-1$
			for (final var type : guardDefs.getEventTypes()) {
				it.newLine();
				it.append("toBeFilled.add("); //$NON-NLS-1$
				it.append(type.getType());
				it.append(".class);"); //$NON-NLS-1$
			}
		});

		// Function "$isSupportedEvent"
		final var eventSupportOperation = this.jvmTypesFactory.createJvmOperation();
		appendGeneratedAnnotation(baseInferrer, eventSupportOperation, context);
		addAnnotationSafe(baseInferrer, eventSupportOperation, Override.class);

		eventSupportOperation.setAbstract(false);
		eventSupportOperation.setNative(false);
		eventSupportOperation.setSynchronized(false);
		eventSupportOperation.setStrictFloatingPoint(false);
		eventSupportOperation.setFinal(false);
		eventSupportOperation.setVisibility(JvmVisibility.PUBLIC);
		eventSupportOperation.setStatic(false);
		eventSupportOperation.setSimpleName(SarlUtils.HIDDEN_MEMBER_CHARACTER + "isSupportedEvent"); //$NON-NLS-1$
		eventSupportOperation.setReturnType(baseInferrer.getJvmTypeReferenceBuilder().typeRef(boolean.class));

		final var jvmParam1 = this.jvmTypesFactory.createJvmFormalParameter();
		jvmParam1.setName("event"); //$NON-NLS-1$
		jvmParam1.setParameterType(baseInferrer.getJvmTypeReferenceBuilder().typeRef(Class.class,
				baseInferrer.getJvmTypeReferenceBuilder().wildcardExtends(
						baseInferrer.getJvmTypeReferenceBuilder().typeRef(Event.class))));
		eventSupportOperation.getParameters().add(jvmParam1);

		container.getMembers().add(eventSupportOperation);

		setBody(eventSupportOperation, it -> {
			for (final var type : guardDefs.getEventTypes()) {
				it.append("if ("); //$NON-NLS-1$
				it.append(type.getType());
				it.append(".class.isAssignableFrom(event)) {"); //$NON-NLS-1$
				it.increaseIndentation().newLine();
				it.append("return true;"); //$NON-NLS-1$
				it.decreaseIndentation().newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLine();
			}
			it.append("return "); //$NON-NLS-1$
			if (isRootType) {
				it.append("false"); //$NON-NLS-1$
			} else {
				it.append("super.").append(SarlUtils.HIDDEN_MEMBER_CHARACTER); //$NON-NLS-1$
				it.append("isSupportedEvent(event)"); //$NON-NLS-1$
			}
			it.append(";"); //$NON-NLS-1$
		});

		// Function "$evaluateBehaviorGuards"
		final var runnableType = baseInferrer.getJvmTypeReferenceBuilder().typeRef(Runnable.class);
		final var collectionType = baseInferrer.getJvmTypeReferenceBuilder().typeRef(Collection.class, runnableType);
		final var evaluateOperation = this.jvmTypesFactory.createJvmOperation();
		appendGeneratedAnnotation(baseInferrer, evaluateOperation, context);
		addAnnotationSafe(baseInferrer, evaluateOperation, Override.class);

		evaluateOperation.setAbstract(false);
		evaluateOperation.setNative(false);
		evaluateOperation.setSynchronized(false);
		evaluateOperation.setStrictFloatingPoint(false);
		evaluateOperation.setFinal(false);
		evaluateOperation.setVisibility(JvmVisibility.PUBLIC);
		evaluateOperation.setStatic(false);
		evaluateOperation.setSimpleName(SarlUtils.HIDDEN_MEMBER_CHARACTER + "evaluateBehaviorGuards"); //$NON-NLS-1$
		evaluateOperation.setReturnType(this.jvmTypeBuilder.cloneWithProxies(voidType));

		final var jvmParam4 = this.jvmTypesFactory.createJvmFormalParameter();
		jvmParam4.setName("eventType"); //$NON-NLS-1$
		jvmParam4.setParameterType(baseInferrer.getJvmTypeReferenceBuilder().typeRef(Class.class, baseInferrer.getJvmTypeReferenceBuilder().wildcard()));
		evaluateOperation.getParameters().add(jvmParam4);

		final var jvmParam2 = this.jvmTypesFactory.createJvmFormalParameter();
		jvmParam2.setName("event"); //$NON-NLS-1$
		jvmParam2.setParameterType(baseInferrer.getJvmTypeReferenceBuilder().typeRef(Object.class));
		evaluateOperation.getParameters().add(jvmParam2);

		final var jvmParam3 = this.jvmTypesFactory.createJvmFormalParameter();
		jvmParam3.setName("callbacks"); //$NON-NLS-1$
		jvmParam3.setParameterType(this.jvmTypeBuilder.cloneWithProxies(collectionType));
		evaluateOperation.getParameters().add(jvmParam3);

		container.getMembers().add(evaluateOperation);

		setBody(evaluateOperation, it -> {
			it.append("assert eventType != null;").newLine(); //$NON-NLS-1$
			it.append("assert event != null;").newLine(); //$NON-NLS-1$
			it.append("super.").append(SarlUtils.HIDDEN_MEMBER_CHARACTER); //$NON-NLS-1$
			it.append("evaluateBehaviorGuards(eventType, event, callbacks);"); //$NON-NLS-1$
			for (final var functions : guardDefs.getFunctions()) {
				it.newLine();
				it.append("if ("); //$NON-NLS-1$
				it.append(functions.getEventType());
				it.append(".class.equals(eventType)) {"); //$NON-NLS-1$
				it.increaseIndentation().newLine();
				it.append("final var occurrence = (").append(functions.getEventType()).append(") event;"); //$NON-NLS-1$ //$NON-NLS-2$
				for (final var methSpec : functions.getFunctions()) {
					it.newLine();
					var hasTypeParameters = !methSpec.bounds().isEmpty();
					if (hasTypeParameters) {
						it.append("if (").append(functions.getEventType()); //$NON-NLS-1$
						it.append(".").append(SarlUtils.HIDDEN_MEMBER_CHARACTER); //$NON-NLS-1$
						it.append("matchesTypeBounds(occurrence"); //$NON-NLS-1$
						for (final var typeParameter : methSpec.bounds) {
							it.append(", ").append(typeParameter.getType()).append(".class"); //$NON-NLS-1$ //$NON-NLS-2$
						}
						it.append(")) {").increaseIndentation().newLine(); //$NON-NLS-1$
					}
					it.append(methSpec.name());
					it.append("(occurrence, callbacks);"); //$NON-NLS-1$
					if (hasTypeParameters) {
						it.decreaseIndentation().newLine();
						it.append("}"); //$NON-NLS-1$
					}
				}
				it.decreaseIndentation().newLine();
				it.append("}"); //$NON-NLS-1$
			}
		});
	}

	/** Informations about the behavior units and the guards.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version compiler 0.15.0 20250909-115746
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid compiler
	 * @since 0.14
	 */
	protected static class BehaviorUnitDefinitions implements Serializable {

		private static final long serialVersionUID = 1382396220935311642L;

		private final Map<JvmTypeReference, BehaviorUnitFunctions> definitions = new TreeMap<>((a, b) -> {
			return geIdentifier(a).compareTo(geIdentifier(b));
		});

		private static String geIdentifier(JvmTypeReference type) {
			return type.getType().getQualifiedName();
		}
		
		/** Replies the types that have been used for definition behavior units.
		 *
		 * @return the types.
		 */
		public Iterable<JvmTypeReference> getEventTypes() {
			return this.definitions.keySet();
		}

		/** Replies the declarations of the functions that are associated to the given type.
		 *
		 * @param type the type.
		 * @return the definitions, never {@code null}.
		 */
		public BehaviorUnitFunctions getFunctionsFor(JvmTypeReference type) {
			return this.definitions.computeIfAbsent(type, key -> new BehaviorUnitFunctions(type.getType()));
		}

		/** Replies all the declarations of the functions.
		 *
		 * @return the definitions, never {@code null}.
		 */
		public Iterable<BehaviorUnitFunctions> getFunctions() {
			return this.definitions.values();
		}

	}

	/** Functions about an event for the behavior units.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version compiler 0.15.0 20250909-115746
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid compiler
	 * @since 0.14
	 */
	protected static class BehaviorUnitFunctions implements Serializable {

		private static final long serialVersionUID = 8379370500334485114L;

		private final JvmType eventType;
		
		private final Map<String, List<JvmTypeReference>> functions = new TreeMap<>();

		/** Construct a function declaration container.
		 *
		 * @param eventType the associated event type.
		 */
		BehaviorUnitFunctions(JvmType eventType) {
			this.eventType = eventType;
		}
		
		/** Add a function with the given name and the associated generic type parameters.
		 *
		 * @param name the name of the function.
		 * @param typeParameterBounds the bounds of the type parameters.
		 */
		public void registerFunction(String name, List<JvmTypeReference> typeParameterBounds) {
			this.functions.put(name, typeParameterBounds);
		}

		/** Replies the event type associated to the functions.
		 *
		 * @return the event type, never {@code null}.
		 */
		public JvmType getEventType() {
			return this.eventType;
		}
		
		/** Replies all the registered functions.
		 *
		 * @return the registered functions.
		 */
		public Iterable<BehaviorUnitFunction> getFunctions() {
			return this.functions.entrySet().stream().map(it -> {
				return new BehaviorUnitFunction(it.getKey(), it.getValue());
			}).toList();
		}
		
	}

	/** Functions about an event for the behavior units.
	 *
	 * @param name the name of the function.
	 * @param bounds the upper bounds for generic type parameters.
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version compiler 0.15.0 20250909-115746
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid compiler
	 * @since 0.14
	 */
	protected record BehaviorUnitFunction(String name, List<JvmTypeReference> bounds) {
		//
	}

}
