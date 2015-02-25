/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.jvmmodel;

import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.EarlyExit;
import io.sarl.lang.annotation.FiredEvent;
import io.sarl.lang.annotation.Generated;
import io.sarl.lang.controlflow.SARLExtendedEarlyExitComputer;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.signature.ActionKey;
import io.sarl.lang.signature.ActionNameKey;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.signature.InferredActionSignature;
import io.sarl.lang.signature.InferredStandardParameter;
import io.sarl.lang.signature.SignatureKey;
import io.sarl.lang.util.JvmIdentifiableComparator;
import io.sarl.lang.util.ModelUtil;

import java.lang.annotation.Annotation;
import java.text.MessageFormat;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.eclipse.xtend.core.jvmmodel.SyntheticNameClashResolver;
import org.eclipse.xtend.core.jvmmodel.XtendJvmModelInferrer;
import org.eclipse.xtend.core.xtend.CreateExtensionInfo;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociator;
import org.eclipse.xtext.xbase.jvmmodel.JvmAnnotationReferenceBuilder;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.validation.ReadAndWriteTracking;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.inject.Inject;

/** Infers a JVM model from the source model.
 *
 * The JVM model should contain all elements that would appear in the Java code
 * which is generated from the source model. Other models link against
 * the JVM model rather than the source model.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLJvmModelInferrer extends XtendJvmModelInferrer {

	@Inject
	private IJvmModelAssociator associator;

	@Inject
	private TypesFactory typesFactory;

	@Inject
	private JvmTypesBuilder jvmTypesBuilder;

	@Inject
	private SyntheticNameClashResolver nameClashResolver;

	@Inject
	private ActionSignatureProvider sarlSignatureProvider;

	@Inject
	private CommonTypeComputationServices services;

	@Inject
	private JvmTypeReferenceBuilder typeReferenceBuilder;

	@Inject
	private SARLExtendedEarlyExitComputer earlyExitComputer;

	@Inject
	private JvmAnnotationReferenceBuilder annotationTypesBuilder;

	@Inject
	private ISerializer sarlSerializer;

	@Inject
	private ReadAndWriteTracking readAndWriteTracking;

	private final Map<JvmIdentifiableElement, GenerationContext> generationContexts =
			new TreeMap<>(new JvmIdentifiableComparator());

	/** Regitering the initialization of the inferred elements.
	 *
	 * {@inheritDoc}
	 */
	@Override
	protected JvmDeclaredType doInferTypeSceleton(
			XtendTypeDeclaration declaration,
			IJvmDeclaredTypeAcceptor acceptor, boolean preIndexingPhase,
			XtendFile xtendFile, List<Runnable> doLater) {
		if (declaration instanceof SarlCapacity) {
			final SarlCapacity sarlCapacity = (SarlCapacity) declaration;
			final JvmGenericType javaType = this.typesFactory.createJvmGenericType();
			if (!preIndexingPhase) {
				doLater.add(new Runnable() {
					@Override
					public void run() {
						initialize(sarlCapacity, javaType);
					}
				});
			}
			return javaType;
		}
		if (declaration instanceof SarlSkill) {
			final SarlSkill sarlSkill = (SarlSkill) declaration;
			final JvmGenericType javaType = this.typesFactory.createJvmGenericType();
			if (!preIndexingPhase) {
				doLater.add(new Runnable() {
					@Override
					public void run() {
						initialize(sarlSkill, javaType);
					}
				});
			}
			return javaType;
		}
		return super.doInferTypeSceleton(declaration, acceptor, preIndexingPhase,
				xtendFile, doLater);
	}

	@Override
	protected void transform(XtendMember sourceMember, JvmGenericType container, boolean allowDispatch) {
		// TODO Auto-generated method stub
		super.transform(sourceMember, container, allowDispatch);
	}

	@Override
	protected void transform(XtendFunction source, JvmGenericType container,
			boolean allowDispatch) {
		if (source instanceof SarlAction) {
			transform((SarlAction) source, container, allowDispatch);
		} else {
			super.transform(source, container, allowDispatch);
		}
	}

	@Override
	protected void transform(XtendConstructor source, JvmGenericType container) {
		if (source instanceof SarlConstructor) {
			transform((SarlConstructor) source, container);
		} else {
			super.transform(source, container);
		}
	}

	/** Invoked for intializing the inferred SARL capacity.
	 *
	 * @param source - the SARL source.
	 * @param inferredJvmType - the Java inferred type.
	 */
	protected void initialize(SarlCapacity source, JvmGenericType inferredJvmType) {
		try {
			// Initialize the generator tools
			this.sarlSignatureProvider.resetSignatures(inferredJvmType);
			this.generationContexts.put(inferredJvmType,
					new GenerationContext(source, inferredJvmType));
			// Initialize the Java type
			inferredJvmType.setVisibility(source.getVisibility());
			inferredJvmType.setStatic(source.isStatic() && !isTopLevel(source));
			inferredJvmType.setInterface(true);
			inferredJvmType.setAbstract(true);
			// Generate the annotations
			translateAnnotationsTo(source.getAnnotations(), inferredJvmType);
			// Generate the super types
			translateSuperTypes(source, inferredJvmType, io.sarl.lang.core.Capacity.class,
					source.getExtends(), true);
			// Generate the members
			for (XtendMember member : source.getMembers()) {
				if (member instanceof SarlAction
						&& !Strings.isNullOrEmpty(((SarlAction) member).getName())) {
					transform(member, inferredJvmType, false);
				}
			}
			// Generate the documentation
			this.jvmTypesBuilder.copyDocumentationTo(source, inferredJvmType);
			// Resolve name classes for synthetic members
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			// Reset the generator tools
			this.generationContexts.remove(inferredJvmType);
		}
	}

	/** Invoked for intializing the inferred SARL skill.
	 *
	 * @param source - the SARL source.
	 * @param inferredJvmType - the Java inferred type.
	 */
	protected void initialize(SarlSkill source, JvmGenericType inferredJvmType) {
		try {
			// Initialize the generator tools
			this.sarlSignatureProvider.resetSignatures(inferredJvmType);
			this.generationContexts.put(inferredJvmType,
					new GenerationContext(source, inferredJvmType));
			// Initialize the Java type
			inferredJvmType.setVisibility(source.getVisibility());
			inferredJvmType.setStatic(source.isStatic() && !isTopLevel(source));
			inferredJvmType.setInterface(false);
			inferredJvmType.setAbstract(false);
			// Generate the annotations
			translateAnnotationsTo(source.getAnnotations(), inferredJvmType);
			// Generate the super types
			translateSuperTypes(source, inferredJvmType, io.sarl.lang.core.Skill.class,
					source.getExtends(), true);
			translateSuperTypes(source, inferredJvmType, io.sarl.lang.core.Capacity.class,
					source.getImplements(), true);
			// Generate the members
			translateClassMembers(source, inferredJvmType);
			// Generate the documentation
			this.jvmTypesBuilder.copyDocumentationTo(source, inferredJvmType);
			// Resolve name classes for synthetic members
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			// Reset the generator tools
			this.generationContexts.remove(inferredJvmType);
		}
	}

	//			GenerationInformation generationInformation = generateCodeForFeatures(it, this.element, this.element, false, null);
	//
	//			if (!generationInformation.hasConstructor) {
	//				final JvmTypeReference aType = SARLJvmModelInferrer.this._typeReferenceBuilder.typeRef(
	//						io.sarl.lang.core.Agent.class);
	//				JvmConstructor op = SARLJvmModelInferrer.this.typeBuilder.toConstructor(this.element,
	//						new Procedures.Procedure1<JvmConstructor>() {
	//					@Override
	//					public void apply(JvmConstructor it) {
	//						SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it, MessageFormat.format(
	//								Messages.SARLJvmModelInferrer_3, "owner")); //$NON-NLS-1$
	//						it.getParameters().add(SARLJvmModelInferrer.this.typeBuilder.toParameter(
	//								SkillGenerator.this.element, "owner", aType)); //$NON-NLS-1$
	//						SARLJvmModelInferrer.this.typeBuilder.setBody(it, toStringConcatenation("super(owner);")); //$NON-NLS-1$
	//					}
	//
	//				});
	//				op.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
	//				it.getMembers().add(op);
	//				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
	//				op = SARLJvmModelInferrer.this.typeBuilder.toConstructor(this.element,
	//						new Procedures.Procedure1<JvmConstructor>() {
	//					@Override
	//					public void apply(JvmConstructor it) {
	//						SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it, Messages.SARLJvmModelInferrer_4);
	//						SARLJvmModelInferrer.this.typeBuilder.setBody(it,  toStringConcatenation("super();")); //$NON-NLS-1$
	//					}
	//				});
	//
	//				op.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
	//				it.getMembers().add(op);
	//				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
	//			}
	//		}
	//
	//	}
	//

	/** Generate the members for the given features.
	 *
	 * This function generated the missed generated functions, e.g. the generated functions according to the default values
	 * of the formal parameters.
	 *
	 * @param sourceMember - the SARL element.
	 * @param inferredJvmType - the inferred type.
	 */
	@SuppressWarnings("unchecked")
	protected void translateClassMembers(XtendTypeDeclaration sourceMember, JvmGenericType inferredJvmType) {
		GenerationContext context = this.generationContexts.get(inferredJvmType);
		assert (context != null);

		final Map<ActionKey, JvmOperation> finalOperations = CollectionLiterals.newTreeMap(null);
		final Map<ActionKey, JvmOperation>  overridableOperations = CollectionLiterals.newTreeMap(null);
		Map<ActionKey, JvmOperation>  operationsToImplement = CollectionLiterals.newTreeMap(null);
		Map<SignatureKey, JvmConstructor>  implementedConstructors = CollectionLiterals.newTreeMap(null);
		ModelUtil.populateInheritanceContext(
				inferredJvmType,
				finalOperations,
				overridableOperations,
				null,
				operationsToImplement,
				null,
				this.sarlSignatureProvider);

		boolean hasConstructor = false;

		for (XtendMember feature : sourceMember.getMembers()) {
			if (feature instanceof SarlAction) {
				SarlAction action = (SarlAction) feature;
				JvmOperation operation = translateAction(
						action,
						inferredJvmType,
						action.getName(),
						action.getParameters(),
						action.getReturnType(),
						action.getTypeParameters(),
						action.getExceptions(),
						action.getFiredEvents(),
						action.getExpression(),
						action.getExpression() == null,
						false,
						false,
						false,
						false,
						action.getCreateExtensionInfo(),
						operationsToImplement,
						overridableOperations,
						new Functions.Function1<ActionKey, Boolean>() {
							@Override
							public Boolean apply(ActionKey it) {
								return !finalOperations.containsKey(it) && !overridableOperations.containsKey(it);
							}
						});
				if (operation != null) {
					context.incrementActionIndex();
				}
			} else if (feature instanceof SarlConstructor) {
				SarlConstructor constructor = (SarlConstructor) feature;
				JvmConstructor jvmConstructor = translateConstructor(
						constructor,
						inferredJvmType,
						constructor.getParameters(),
						constructor.getTypeParameters(),
						constructor.getExceptions(),
						constructor.getExpression(),
						implementedConstructors);
				if (jvmConstructor != null) {
					context.incrementActionIndex();
					hasConstructor = true;
				}
			} else if (feature instanceof SarlBehaviorUnit) {
				if (enableEventHandling) {
					BehaviorUnit behaviorUnit = (BehaviorUnit) feature;
					JvmOperation bMethod = generateBehaviorUnit(featureContainerType, behaviorUnit, behaviorUnitIndex);
					if (bMethod != null) {
						++behaviorUnitIndex;
						featureContainerType.getMembers().add(bMethod);
					}
				} else {
					throw new IllegalStateException(Messages.SARLJvmModelInferrer_12);
				}
			} else if (feature instanceof XtendField) {
				transform(feature, inferredJvmType, false);
			}
		}

		for (EObject feature : featureContainer.getFeatures()) {
			if (feature instanceof CapacityUses) {
				CapacityUses uses = (CapacityUses) feature;
				for (JvmParameterizedTypeReference used : uses.getCapacitiesUsed()) {
					actionIndex = generateCapacityDelegatorMethods(
							featureContainerType, topElement, used, actionIndex,
							operationsToImplement, overridableOperations);
				}
			}
		}

		actionIndex = generateMissedFunction(featureContainerType, featureContainer, actionIndex,
				operationsToImplement, overridableOperations);

		return new GenerationInformation(hasConstructor, actionIndex, behaviorUnitIndex);
	}


	/** Generate the given SARL signature into the given inferred type.
	 *
	 * @param sourceMember - the signature to generate.
	 * @param inferredJvmType - the target inferred type.
	 * @param allowDispatch - indicates if the 
	 */
	protected void transform(SarlAction sourceMember, JvmGenericType inferredJvmType, boolean allowDispatch) {
		GenerationContext context = this.generationContexts.get(inferredJvmType);
		assert (context != null);
		JvmOperation operation = translateAction(
				sourceMember,
				inferredJvmType,
				sourceMember.getName(),
				sourceMember.getParameters(),
				sourceMember.getReturnType(),
				sourceMember.getTypeParameters(),
				sourceMember.getExceptions(),
				sourceMember.getFiredEvents(),
				null,
				true,
				false,
				false,
				false,
				false,
				sourceMember.getCreateExtensionInfo(),
				null,
				null,
				null);
		if (operation != null) {
			context.incrementActionIndex();
		}
	}

	/** Generate the given SARL constructor into the given inferred type.
	 *
	 * @param sourceMember - the signature to generate.
	 * @param inferredJvmType - the target inferred type.
	 */
	protected void transform(SarlConstructor sourceMember, JvmGenericType inferredJvmType) {
		GenerationContext context = this.generationContexts.get(inferredJvmType);
		assert (context != null);
		JvmConstructor operation = translateConstructor(
				sourceMember,
				inferredJvmType,
				sourceMember.getParameters(),
				sourceMember.getTypeParameters(),
				sourceMember.getExceptions(),
				sourceMember.getExpression(),
				null);
		if (operation != null) {
			context.incrementActionIndex();
		}
	}

	/** Generate the super types for the given SARL statement.
	 *
	 * @param source - the original SARL statement.
	 * @param inferredJvmType - the JVM element to change.
	 * @param defaultType - the default type.
	 * @param types - the super types.
	 * @param addDefault - indicates if a default super type must be added is none was given by the SARL element.
	 */
	protected void translateSuperTypes(XtendTypeDeclaration source, JvmGenericType inferredJvmType,
			Class<?> defaultType, List<? extends JvmTypeReference> types, boolean addDefault) {
		GenerationContext context = this.generationContexts.get(inferredJvmType);
		long serial = context.getSerial();
		boolean isInterface = inferredJvmType.isInterface();
		for (JvmTypeReference superType : types) {
			if (superType.getType() instanceof JvmGenericType) {
				LightweightTypeReference reference = ModelUtil.toLightweightTypeReference(superType, this.services);
				if (reference.isInterfaceType() == isInterface && reference.isSubtypeOf(defaultType)) {
					inferredJvmType.getSuperTypes().add(this.jvmTypesBuilder.cloneWithProxies(superType));
					serial = serial + superType.getIdentifier().hashCode();
				}
			}
		}
		if (inferredJvmType.getSuperTypes().isEmpty() && addDefault) {
			JvmTypeReference type = this.typeReferenceBuilder.typeRef(defaultType);
			inferredJvmType.getSuperTypes().add(type);
			serial = serial + type.getIdentifier().hashCode();
		}
		context.setSerial(serial);
	}

	/** Create an annotation with classes as values.
	 *
	 * @param type - the type of the annotation.
	 * @param values - the values.
	 * @return the reference to the JVM annotation.
	 */
	protected JvmAnnotationReference translateAnnotationClassRef(Class<? extends Annotation> type,
			List<? extends JvmTypeReference> values) {
		JvmAnnotationReference annot = this.annotationTypesBuilder.annotationRef(type);
		JvmTypeAnnotationValue annotationValue = this.services.getTypesFactory().createJvmTypeAnnotationValue();
		for (JvmTypeReference value : values) {
			annotationValue.getValues().add(this.jvmTypesBuilder.cloneWithProxies(value));
		}
		annot.getExplicitValues().add(annotationValue);
		return annot;
	}

	/** Generate an action.
	 *
	 * @param source - the SARL element.
	 * @param inferredJvmType - the inferred type.
	 * @param name - the name of the action.
	 * @param params - the definition of the parameters.
	 * @param returnType - the return type, or <code>null</code>.
	 * @param typeParameters - the type parameters for the action.
	 * @param exceptions - the thrown exceptions.
	 * @param firedEvents - the fired events.
	 * @param operationBody - the body's or <code>null</code>.
	 * @param isAbstract - indicates if the operation is abstract.
	 * @param isNative - indicates if the operation is native.
	 * @param isSynchronized - indicates if the operation is synchronized.
	 * @param isStrictFloatingPoint - indicates if the operation uses strict floating points.
	 * @param isDispatch - indicates if the operation is a dispatch function.
	 * @param createExtensionInfo - describes the extension used for creation.
	 * @param operationsToImplement - if given, list of the operations that should be implemented in the container.
	 * 			This function remove the generated action from the list.
	 * @param implementedOperations - if given, the generated operation is added inside.
	 * @param inheritedOperation - test if an action is inherited or not.
	 * @return the main operation.
	 * @see #transform(XtendFunction, JvmGenericType, boolean)
	 */
	protected JvmOperation translateAction(
			XtendMember source,
			JvmGenericType inferredJvmType,
			String name,
			List<? extends XtendParameter> params,
			JvmTypeReference returnType,
			List<JvmTypeParameter> typeParameters,
			List<JvmTypeReference> exceptions,
			List<JvmTypeReference> firedEvents,
			XExpression operationBody,
			boolean isAbstract,
			boolean isNative,
			boolean isSynchronized,
			boolean isStrictFloatingPoint,
			boolean isDispatch,
			CreateExtensionInfo createExtensionInfo,
			Map<ActionKey, JvmOperation> operationsToImplement,
			Map<ActionKey, JvmOperation> implementedOperations,
			Functions.Function1<ActionKey, Boolean> inheritedOperation) {

		//**************************************
		// Main operation
		//
		final JvmOperation mainOperation = this.typesFactory.createJvmOperation();
		try {
			// name
			String sourceName = name;
			JvmVisibility visibility = source.getVisibility();
			if (isDispatch) {
				if (source.getDeclaredVisibility() == null) {
					visibility = JvmVisibility.PROTECTED;
				}
				sourceName = "_" + sourceName; //$NON-NLS-1$
			}
			mainOperation.setSimpleName(sourceName);
			// Initialize the context
			GenerationContext context = new GenerationContext(this.generationContexts.get(inferredJvmType));
			this.generationContexts.put(mainOperation, context);
			// Modifiers
			mainOperation.setAbstract(isAbstract);
			mainOperation.setNative(isNative);
			mainOperation.setSynchronized(isSynchronized);
			mainOperation.setStrictFloatingPoint(isStrictFloatingPoint);
			if (!isAbstract) {
				mainOperation.setFinal(source.isFinal());
			}
			mainOperation.setVisibility(visibility);
			mainOperation.setStatic(source.isStatic());
			// Parameters
			context.getLateArguments().clear();
			for (XtendParameter parameter : params) {
				translateParameter(mainOperation, parameter);
			}
			// Return type
			JvmTypeReference realReturnType = null;
			if (returnType != null) {
				realReturnType = this.jvmTypesBuilder.cloneWithProxies(returnType);
			} else if (createExtensionInfo != null) {
				realReturnType = this.jvmTypesBuilder.inferredType(createExtensionInfo.getCreateExpression());
			} else if (operationBody != null) {
				realReturnType = this.jvmTypesBuilder.inferredType(operationBody);
			} else {
				realReturnType = this.jvmTypesBuilder.inferredType();
			}
			mainOperation.setReturnType(returnType);
			// Type parameters
			copyAndFixTypeParameters(typeParameters, mainOperation);
			// Exceptions
			for (JvmTypeReference exception : exceptions) {
				mainOperation.getExceptions().add(this.jvmTypesBuilder.cloneWithProxies(exception));
			}
			// Fired events
			if (!firedEvents.isEmpty()) {
				mainOperation.getAnnotations().add(translateAnnotationClassRef(FiredEvent.class, firedEvents));
			}
			// Annotations
			translateAnnotationsTo(source.getAnnotations(), mainOperation);
			// Creation extension
			if (createExtensionInfo != null && source instanceof XtendFunction) {
				transformCreateExtension((XtendFunction) source, createExtensionInfo, inferredJvmType, mainOperation, returnType);
			} else {
				setBody(mainOperation, operationBody);
			}
			// Documentation
			this.jvmTypesBuilder.copyDocumentationTo(source, mainOperation);
			// Container
			inferredJvmType.getMembers().add(mainOperation);
			this.associator.associatePrimary(source, mainOperation);

			//**************************************
			// Early exit detection
			//
			//TODO: Generalize the detection of the EarlyExit
			boolean isEarlyExit = false;
			Iterator<JvmTypeReference> eventIterator = firedEvents.iterator();
			while (!isEarlyExit && eventIterator.hasNext()) {
				if (this.earlyExitComputer.isEarlyExitEvent(eventIterator.next())) {
					mainOperation.getAnnotations().add(this.annotationTypesBuilder.annotationRef(EarlyExit.class));
					isEarlyExit = true;
				}
			}

			//**************************************
			// Add operations with default values
			//	
			ActionNameKey actionKey = this.sarlSignatureProvider.createFunctionID(inferredJvmType, sourceName);

			InferredActionSignature otherSignatures = this.sarlSignatureProvider.createSignature(
					actionKey, params);

			ActionKey actSigKey = this.sarlSignatureProvider.createActionID(
					sourceName,
					otherSignatures.getFormalParameterKey());
			if (operationsToImplement != null && actSigKey != null) {
				JvmOperation removedOp = operationsToImplement.remove(actSigKey);
				if (removedOp != null && implementedOperations != null) {
					implementedOperations.put(actSigKey, removedOp);
				}
			}

			for (final Entry<SignatureKey, List<InferredStandardParameter>> otherSignature
					: otherSignatures.getInferredSignatures().entrySet()) {
				ActionKey ak = this.sarlSignatureProvider.createActionID(
						sourceName,
						otherSignature.getKey());
				if (ak != null
						&& (inheritedOperation == null
						|| inheritedOperation.apply(ak))) {
					JvmOperation additionalOperation = this.typesFactory.createJvmOperation();
					// name
					additionalOperation.setSimpleName(sourceName);
					// Modifiers
					additionalOperation.setAbstract(isAbstract);
					additionalOperation.setNative(isNative);
					additionalOperation.setSynchronized(isSynchronized);
					additionalOperation.setStrictFloatingPoint(isStrictFloatingPoint);
					if (!isAbstract) {
						additionalOperation.setFinal(source.isFinal());
					}
					additionalOperation.setVisibility(visibility);
					additionalOperation.setStatic(source.isStatic());
					// Parameters
					context.getLateArguments().clear();
					for (XtendParameter parameter : params) {
						translateParameter(additionalOperation, parameter);
					}
					// Return type
					final JvmTypeReference additionalReturnType = this.jvmTypesBuilder.cloneWithProxies(realReturnType);
					additionalOperation.setReturnType(additionalReturnType);
					// Type parameters
					copyAndFixTypeParameters(typeParameters, additionalOperation);
					// Exceptions
					for (JvmTypeReference exception : exceptions) {
						additionalOperation.getExceptions().add(this.jvmTypesBuilder.cloneWithProxies(exception));
					}
					// Fired events
					if (!firedEvents.isEmpty()) {
						additionalOperation.getAnnotations().add(translateAnnotationClassRef(FiredEvent.class, firedEvents));
					}
					// Annotations
					translateAnnotationsTo(source.getAnnotations(), additionalOperation);
					additionalOperation.getAnnotations().add(this.annotationTypesBuilder.annotationRef(
							DefaultValueUse.class,
							otherSignatures.getFormalParameterKey().toString()));
					additionalOperation.getAnnotations().add(
							this.annotationTypesBuilder.annotationRef(Generated.class));
					// Body
					if (!isAbstract) {
						final String finalSourceName = sourceName;
						this.jvmTypesBuilder.setBody(additionalOperation, new Procedures.Procedure1<ITreeAppendable>() {
							@SuppressWarnings("synthetic-access")
							@Override
							public void apply(ITreeAppendable it) {
								if (!Objects.equal("void", additionalReturnType.getIdentifier())) { //$NON-NLS-1$
									it.append("return "); //$NON-NLS-1$
								}
								it.append(finalSourceName);
								it.append("("); //$NON-NLS-1$
								it.append(IterableExtensions.join(
										SARLJvmModelInferrer.this.generationContexts.get(mainOperation)
										.getLateArguments(), ", ")); //$NON-NLS-1$
								it.append(");"); //$NON-NLS-1$
							}
						});
					}
					// Documentation
					this.jvmTypesBuilder.copyDocumentationTo(source, additionalOperation);
					// Container
					inferredJvmType.getMembers().add(additionalOperation);
					this.associator.associate(source, additionalOperation);
					// Early exit
					//TODO: Generalize the detection of the EarlyExit
					if (isEarlyExit) {
						additionalOperation.getAnnotations().add(this.annotationTypesBuilder.annotationRef(EarlyExit.class));
					}
					if (!firedEvents.isEmpty()) {
						additionalOperation.getAnnotations().add(translateAnnotationClassRef(FiredEvent.class, firedEvents));
					}

					// Update the output lists
					if (operationsToImplement != null) {
						JvmOperation removedOp = operationsToImplement.remove(ak);
						if (removedOp != null && implementedOperations != null) {
							implementedOperations.put(ak, removedOp);
						}
					}
				}
			}

			return mainOperation;
		} finally {
			this.generationContexts.remove(mainOperation);
		}
	}

	@Override
	protected void translateParameter(final JvmExecutable executable, XtendParameter parameter) {
		GenerationContext context = this.generationContexts.get(executable);
		assert (context != null);

		super.translateParameter(executable, parameter);

		if (parameter instanceof SarlFormalParameter 
				&& ((SarlFormalParameter) parameter).getDefaultValue() != null) {
			JvmFormalParameter jvmParameter = executable.getParameters().get(executable.getParameters().size());
			SarlFormalParameter sarlParameter = (SarlFormalParameter) parameter;
			XExpression defaultValue = sarlParameter.getDefaultValue();

			String namePostPart = context.getActionIndex() + "_" + context.getLateArguments().size(); //$NON-NLS-1$
			String name = ModelUtil.PREFIX_ATTRIBUTE_DEFAULT_VALUE + namePostPart;

			// FIXME: Hide these attributes into an inner interface.
			JvmField field = this.typesFactory.createJvmField();
			field.setSimpleName(name);
			context.getInferredTopContainer().getMembers().add(field);
			this.associator.associate(defaultValue, field);
			if (context.getInferredTopContainer().isInterface()) {
				field.setVisibility(JvmVisibility.PUBLIC);
			} else {
				field.setVisibility(JvmVisibility.PRIVATE);
			}
			field.setStatic(true);
			field.setTransient(false);
			field.setVolatile(false);
			field.setFinal(true);
			field.setType(this.jvmTypesBuilder.cloneWithProxies(parameter.getParameterType()));
			this.jvmTypesBuilder.setInitializer(field, defaultValue);

			this.jvmTypesBuilder.setDocumentation(field,
					MessageFormat.format(Messages.SARLJvmModelInferrer_11, sarlParameter.getName()));

			if (defaultValue != null) {
				field.getAnnotations().add(
						SARLJvmModelInferrer.this.annotationTypesBuilder.annotationRef(
								Generated.class,
								SARLJvmModelInferrer.this.sarlSerializer.serialize(defaultValue)));
			} else {
				field.getAnnotations().add(
						SARLJvmModelInferrer.this.annotationTypesBuilder.annotationRef(Generated.class));
			}

			if (executable instanceof JvmConstructor) {
				this.readAndWriteTracking.markInitialized(field, (JvmConstructor) executable);
			} else {
				this.readAndWriteTracking.markInitialized(field, null);
			}

			JvmAnnotationReference annot = this.annotationTypesBuilder.annotationRef(DefaultValue.class, namePostPart);
			jvmParameter.getAnnotations().add(annot);
		}

		context.getLateArguments().add(parameter.getParameterType().getIdentifier());
	}

	//	/** Generator of JVM elements.
	//	 */
	//	@Inject
	//	protected JvmTypesBuilder typeBuilder;
	//
	//	/** Provider of qualified names.
	//	 */
	//	@Inject
	//	protected IQualifiedNameProvider nameProvider;
	//
	//	/** On-fly Xbase compiler.
	//	 */
	//	@Inject
	//	protected XbaseCompiler xbaseCompiler;
	//
	//	/** Associator of the JVM elements and the SARL elements.
	//	 */
	//	@Inject
	//	protected JvmModelAssociator jvmModelAssociator;
	//
	//	/** Generator's logger.
	//	 */
	//	@Inject
	//	protected Logger log;
	//
	//	/** Manager of SARL action signatures.
	//	 */
	//	@Inject
	//	protected ActionSignatureProvider sarlSignatureProvider;
	//
	//	/** Tracker of field initialization.
	//	 */
	//	@Inject
	//	protected ReadAndWriteTracking readAndWriteTracking;
	//
	//	/** Several generation services.
	//	 */
	//	@Inject
	//	protected CommonTypeComputationServices services;
	//
	//	/** JVM type services.
	//	 */
	//	@Inject
	//	protected JvmTypeExtensions typeExtensions;
	//
	//	/** Computer of early-exits for SARL.
	//	 */
	//	@Inject
	//	protected SARLExtendedEarlyExitComputer earlyExitComputer;
	//
	//	/** SARL Serializer.
	//	 */
	//	@Inject
	//	protected ISerializer sarlSerializer;

	//
	//	/** Generate the missed functions.
	//	 *
	//	 * @param output - the output type.
	//	 * @param owner - the SARL owner.
	//	 * @param actionIndex - the index of the late generated action.
	//	 * @param operationsToImplement - if given, list of the operations that should be implemented in the container.
	//	 * 			This function remove the generated action from the list.
	//	 * @param overridableOperations - if given, the overrideable operations.
	//	 * @return the number of generated operations.
	//	 */
	//	protected int generateMissedFunction(
	//			JvmGenericType output,
	//			EObject owner,
	//			int actionIndex,
	//			Map<ActionKey, JvmOperation> operationsToImplement,
	//			Map<ActionKey, JvmOperation> overridableOperations) {
	//
	//		int actIndex = actionIndex;
	//		String currentKeyStr = null;
	//		JvmOperation originalOperation = null;
	//		SignatureKey sigKey = null;
	//
	//		for (Entry<ActionKey, JvmOperation> missedOperation : operationsToImplement.entrySet()) {
	//			String originalSignature = ModelUtil.annotationString(missedOperation.getValue(), DefaultValueUse.class);
	//			if (originalSignature != null) {
	//				if (!Objects.equal(originalSignature, currentKeyStr)) {
	//					currentKeyStr = originalSignature;
	//					sigKey = this.sarlSignatureProvider.createSignatureIDFromString(originalSignature);
	//					ActionKey key = this.sarlSignatureProvider.createActionID(
	//							missedOperation.getKey().getFunctionName(), sigKey);
	//					originalOperation = overridableOperations.get(key);
	//				}
	//				if (originalOperation != null) {
	//					JvmOperation op = this.typeBuilder.toMethod(owner, originalOperation.getSimpleName(),
	//							originalOperation.getReturnType(), null);
	//					op.setVarArgs(originalOperation.isVarArgs());
	//					op.setFinal(true);
	//					List<String> args = CollectionLiterals.newArrayList();
	//
	//					Iterator<JvmFormalParameter> it1 = missedOperation.getValue().getParameters().iterator();
	//					Iterator<JvmFormalParameter>  it2 = originalOperation.getParameters().iterator();
	//					JvmFormalParameter oparam = null;
	//
	//					while (it2.hasNext()) {
	//						JvmFormalParameter param = it2.next();
	//						String vId = ModelUtil.annotationString(param, DefaultValue.class);
	//						if (oparam == null && it1.hasNext()) {
	//							oparam = it1.next();
	//						}
	//						if (oparam != null && Objects.equal(oparam.getSimpleName(), param.getSimpleName())) {
	//							args.add(oparam.getSimpleName());
	//							op.getParameters().add(this.typeBuilder.toParameter(owner,
	//									oparam.getSimpleName(), oparam.getParameterType()));
	//							oparam = null;
	//						} else if (!Strings.isNullOrEmpty(vId)) {
	//							args.add(
	//									originalOperation.getDeclaringType().getQualifiedName()
	//									+ "." //$NON-NLS-1$
	//									+ ModelUtil.PREFIX_ATTRIBUTE_DEFAULT_VALUE
	//									+ vId);
	//						} else {
	//							throw new IllegalStateException(Messages.SARLJvmModelInferrer_8);
	//						}
	//					}
	//
	//					final String tmpName = originalOperation.getSimpleName();
	//					final List<String> tmpArgs = args;
	//					this.typeBuilder.setBody(op, new Procedures.Procedure1<ITreeAppendable>() {
	//						@Override
	//						public void apply(ITreeAppendable it) {
	//							it.append(tmpName);
	//							it.append("("); //$NON-NLS-1$
	//							it.append(IterableExtensions.join(tmpArgs, ", ")); //$NON-NLS-1$
	//							it.append(");"); //$NON-NLS-1$
	//						}
	//					});
	//					op.getAnnotations().add(this._annotationTypesBuilder.annotationRef(
	//							DefaultValueUse.class, originalSignature));
	//					op.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Generated.class));
	//					output.getMembers().add(op);
	//					++actIndex;
	//				}
	//			}
	//		}
	//		return actIndex;
	//	}
	//
	//	/** Create a string concatenation client from a set of Java code lines.
	//	 *
	//	 * @param javaCodeLines - the Java code lines.
	//	 * @return the client.
	//	 */
	//	protected static StringConcatenationClient toStringConcatenation(final String... javaCodeLines) {
	//		return new StringConcatenationClient() {
	//			@Override
	//			protected void appendTo(StringConcatenationClient.TargetStringConcatenation builder) {
	//				for (String line : javaCodeLines) {
	//					builder.append(line);
	//					builder.newLineIfNotEmpty();
	//				}
	//			}
	//		};
	//	}
	//
	//
	//	/** Generate an attribute.
	//	 *
	//	 * @param owner - the JVM element to change.
	//	 * @param attr - the attribute.
	//	 * @param attrVisibility - the attribute visibility.
	//	 * @return the JVM field.
	//	 */
	//	protected JvmField generateAttribute(JvmGenericType owner, final Attribute attr, final JvmVisibility attrVisibility) {
	//		if (!Strings.isNullOrEmpty(attr.getName())) {
	//			JvmField field = this.typeBuilder.toField(attr, attr.getName(), attr.getType(),
	//					new Procedures.Procedure1<JvmField>() {
	//				@Override
	//				public void apply(JvmField it) {
	//					it.setVisibility(attrVisibility);
	//					SARLJvmModelInferrer.this.typeBuilder.copyDocumentationTo(attr, it);
	//					it.setFinal(!attr.isWriteable());
	//					it.setStatic(false);
	//					SARLJvmModelInferrer.this.typeBuilder.setInitializer(it, attr.getInitialValue());
	//				}
	//			});
	//			assert (field != null) : "the JVM field was not created for the attribute:\n " + attr.getName(); //$NON-NLS-1$
	//			owner.getMembers().add(field);
	//			if (attr.getInitialValue() != null) {
	//				this.readAndWriteTracking.markInitialized(field, null);
	//			}
	//			return field;
	//		}
	//		return null;
	//	}
	//
	//	/** Genereate the implemented types/.
	//	 *
	//	 * @param owner - the JVM element to change.
	//	 * @param element - the SARL definition.
	//	 * @param mandatoryType - the root type of the implemented types, or <code>null</code>.
	//	 * @return the serial for the implemented type.
	//	 */
	//	protected long generateImplementedTypes(JvmGenericType owner, ImplementingElement element, Class<?> mandatoryType) {
	//		long serial = 0L;
	//		for (JvmTypeReference implementedType : element.getImplementedTypes()) {
	//			if (implementedType.getType() instanceof JvmGenericType) {
	//				LightweightTypeReference reference = ModelUtil.toLightweightTypeReference(implementedType, this.services);
	//				if (reference.isInterfaceType() && reference.isSubtypeOf(mandatoryType)) {
	//					owner.getSuperTypes().add(this.typeBuilder.cloneWithProxies(implementedType));
	//					serial = serial + implementedType.getIdentifier().hashCode();
	//				}
	//			}
	//		}
	//		return serial;
	//	}
	//
	//	/** Generate the delegators for capacity methods.
	//	 *
	//	 * @param owner - the JVM container.
	//	 * @param context - the SARL context.
	//	 * @param capacityType - the type of the capacity.
	//	 * @param index - index of the late generated action.
	//	 * @param operationsToImplement - if given, list of the operations that should be implemented in the container.
	//	 * 			This function remove the generated action from the list.
	//	 * @param implementedOperations - if given, the generated operation is added inside.
	//	 * @return the number of generated operations.
	//	 */
	//	@SuppressWarnings("unchecked")
	//	protected int generateCapacityDelegatorMethods(
	//			JvmGenericType owner,
	//			final InheritingElement context,
	//			final JvmParameterizedTypeReference capacityType,
	//			int index,
	//			Map<ActionKey, JvmOperation> operationsToImplement,
	//			Map<ActionKey, JvmOperation> implementedOperations) {
	//
	//		if (capacityType.getType() instanceof JvmGenericType) {
	//			LightweightTypeReference reference = ModelUtil.toLightweightTypeReference(capacityType, this.services);
	//			if (reference.isSubtypeOf(io.sarl.lang.core.Capacity.class)) {
	//				final Map<ActionKey, JvmOperation> capacityOperations = CollectionLiterals.newTreeMap(null);
	//
	//				ModelUtil.populateInterfaceElements(
	//						(JvmGenericType) capacityType.getType(),
	//						capacityOperations,
	//						null,
	//						this.sarlSignatureProvider);
	//
	//				int actionIndex = index;
	//				for (final Entry<ActionKey, JvmOperation> entry : capacityOperations.entrySet()) {
	//					if (implementedOperations == null || !implementedOperations.containsKey(entry.getKey())) {
	//						JvmOperation op = this.typeBuilder.toMethod(context, entry.getValue().getSimpleName(),
	//								entry.getValue().getReturnType(), new Procedures.Procedure1<JvmOperation>() {
	//							@Override
	//							public void apply(JvmOperation it) {
	//								it.setVisibility(JvmVisibility.PROTECTED);
	//								final List<String> args = CollectionLiterals.newArrayList();
	//								final List<String> argTypes = CollectionLiterals.newArrayList();
	//								for (JvmFormalParameter param : entry.getValue().getParameters()) {
	//									it.getParameters().add(
	//											SARLJvmModelInferrer.this.typeBuilder.toParameter(
	//													context,
	//													param.getSimpleName(),
	//													param.getParameterType()));
	//									args.add(param.getSimpleName());
	//									argTypes.add(
	//											param.getParameterType().getIdentifier());
	//								}
	//								String hyperrefLink = capacityType.getIdentifier() + "#" //$NON-NLS-1$
	//										+ entry.getValue().getSimpleName() + "(" //$NON-NLS-1$
	//										+ IterableExtensions.join(argTypes, ",") + ")"; //$NON-NLS-1$ //$NON-NLS-2$
	//								SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it,
	//										MessageFormat.format(
	//												Messages.SARLJvmModelInferrer_13,
	//												hyperrefLink));
	//								it.setVarArgs(entry.getValue().isVarArgs());
	//								SARLJvmModelInferrer.this.typeBuilder.setBody(it, new Procedures.Procedure1<ITreeAppendable>() {
	//									@Override
	//									public void apply(ITreeAppendable it) {
	//										if (!Objects.equal("void", //$NON-NLS-1$
	//												entry.getValue().getReturnType().getIdentifier())) {
	//											it.append("return "); //$NON-NLS-1$
	//										}
	//										it.append("getSkill("); //$NON-NLS-1$
	//										it.append(entry.getValue().getDeclaringType().getQualifiedName());
	//										it.append(".class)."); //$NON-NLS-1$
	//										it.append(entry.getValue().getSimpleName());
	//										it.append("("); //$NON-NLS-1$
	//										it.append(IterableExtensions.join(args, ", ")); //$NON-NLS-1$
	//										it.append(");"); //$NON-NLS-1$
	//									}
	//								});
	//							}
	//						});
	//
	//						// Copy the EarlyExit Annotation from the capacity
	//						if (ModelUtil.hasAnnotation(entry.getValue(), EarlyExit.class)) {
	//							op.getAnnotations().add(this._annotationTypesBuilder.annotationRef(EarlyExit.class));
	//						}
	//						// Copy the FiredEvent annotation from the capacity
	//						List<JvmTypeReference> firedEvents = ModelUtil.annotationClasses(entry.getValue(), FiredEvent.class);
	//						if (!firedEvents.isEmpty()) {
	//							op.getAnnotations().add(annotationClassRef(FiredEvent.class, firedEvents));
	//						}
	//						// Add the annotation dedicated to this particular method
	//						op.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Generated.class));
	//						// Add the imported feature marker
	//						op.getAnnotations().add(annotationClassRef(ImportedCapacityFeature.class,
	//								Collections.singletonList(capacityType)));
	//
	//						owner.getMembers().add(op);
	//						//
	//						if (operationsToImplement != null) {
	//							operationsToImplement.remove(entry.getKey());
	//						}
	//						if (implementedOperations != null) {
	//							implementedOperations.put(entry.getKey(), entry.getValue());
	//						}
	//						++actionIndex;
	//					}
	//				}
	//				return actionIndex;
	//			}
	//		}
	//		return index;
	//	}
	//
	//	/** Create an annotation with classes as values.
	//	 *
	//	 * @param type - the type of the annotation.
	//	 * @param values - the values.
	//	 * @return the reference to the JVM annotation.
	//	 */
	//	protected JvmAnnotationReference annotationClassRef(Class<? extends Annotation> type,
	//			List<? extends JvmTypeReference> values) {
	//		JvmAnnotationReference annot = this._annotationTypesBuilder.annotationRef(type);
	//		JvmTypeAnnotationValue annotationValue = this.services.getTypesFactory().createJvmTypeAnnotationValue();
	//		for (JvmTypeReference value : values) {
	//			annotationValue.getValues().add(this.typeBuilder.cloneWithProxies(value));
	//		}
	//		annot.getExplicitValues().add(annotationValue);
	//		return annot;
	//	}
	//
	//	/** Generate a abehavior unit.
	//	 *
	//	 * @param owner - the behavior unit container.
	//	 * @param unit - the SARL definition.
	//	 * @param index - index of the unit to generate.
	//	 * @return the JVM operation for the behavior unit.
	//	 */
	//	protected JvmOperation generateBehaviorUnit(JvmGenericType owner, final BehaviorUnit unit, int index) {
	//		if (unit.getName() != null) {
	//			final XExpression guard = unit.getGuard();
	//			final boolean isTrueGuard;
	//
	//			if (guard == null) {
	//				isTrueGuard = true;
	//			} else if (guard instanceof XBooleanLiteral) {
	//				XBooleanLiteral literal = (XBooleanLiteral) guard;
	//				if (literal.isIsTrue()) {
	//					isTrueGuard = true;
	//				} else {
	//					// The guard is always false => no need to generate the code
	//					return null;
	//				}
	//			} else {
	//				isTrueGuard = false;
	//			}
	//
	//			final JvmTypeReference voidType = this._typeReferenceBuilder.typeRef(Void.TYPE);
	//			final String behName = ModelUtil.PREFIX_ACTION_HANDLE + unit.getName().getSimpleName() + "_" + index; //$NON-NLS-1$
	//
	//			JvmOperation behaviorMethod = this.typeBuilder.toMethod(unit, behName, voidType,
	//					new Procedures.Procedure1<JvmOperation>() {
	//				@SuppressWarnings("synthetic-access")
	//				@Override
	//				public void apply(JvmOperation it) {
	//					SARLJvmModelInferrer.this.typeBuilder.copyDocumentationTo(unit, it);
	//					it.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Percept.class));
	//					it.getParameters().add(SARLJvmModelInferrer.this.typeBuilder.toParameter(
	//							unit, SARLKeywords.OCCURRENCE, unit.getName()));
	//				}
	//			});
	//			if (isTrueGuard) {
	//				this.typeBuilder.setBody(behaviorMethod, unit.getBody());
	//			} else {
	//				assert (guard != null);
	//
	//				final String guardMethodName = ModelUtil.PREFIX_HANDLE_GUARD + unit.getName().getSimpleName()
	//						+ "_" + index; //$NON-NLS-1$
	//				JvmOperation guardMethod = this.typeBuilder.toMethod(guard, guardMethodName,
	//						this._typeReferenceBuilder.typeRef(Boolean.TYPE), new Procedures.Procedure1<JvmOperation>() {
	//					@SuppressWarnings("synthetic-access")
	//					@Override
	//					public void apply(JvmOperation it) {
	//						it.setVisibility(JvmVisibility.PRIVATE);
	//						it.getParameters().add(SARLJvmModelInferrer.this.typeBuilder.toParameter(
	//								unit, SARLKeywords.OCCURRENCE, unit.getName()));
	//						SARLJvmModelInferrer.this.typeBuilder.setBody(it, guard);
	//						it.getAnnotations().add(
	//								SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
	//					}
	//				});
	//
	//				this.jvmModelAssociator.associateLogicalContainer(unit.getBody(), behaviorMethod);
	//
	//				this.typeBuilder.setBody(behaviorMethod, new Procedures.Procedure1<ITreeAppendable>() {
	//					@Override
	//					public void apply(ITreeAppendable it) {
	//						it.append("if (" + guardMethodName + "(" //$NON-NLS-1$//$NON-NLS-2$
	//								+ SARLKeywords.OCCURRENCE + ")) {").increaseIndentation(); //$NON-NLS-1$
	//						SARLJvmModelInferrer.this.xbaseCompiler.compile(unit.getBody(), it, voidType, null);
	//						it.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$
	//					}
	//				});
	//
	//				owner.getMembers().add(guardMethod);
	//			}
	//			return behaviorMethod;
	//		}
	//		this.log.fine(Messages.SARLJvmModelInferrer_10);
	//		return null;
	//	}
	//
	//	/** Generate a list of formal parameters with annotations for the default values.
	//	 *
	//	 * @param owner - the JVM element to change.
	//	 * @param actionContainer - the container of the action.
	//	 * @param varargs - indicates if the signature has variadic parameter.
	//	 * @param params - the parameters.
	//	 * @param isForInterface - indicates if the formal parameters are for an interface (<code>true</code>)
	//	 * 							or a class (<code>false</code>).
	//	 * @param actionIndex - the index of the generated action.
	//	 * @return the names of the formal parameters.
	//	 */
	//	protected List<String> generateFormalParametersAndDefaultValueFields(
	//			JvmExecutable owner,
	//			JvmGenericType actionContainer,
	//			boolean varargs,
	//			List<FormalParameter> params,
	//			final boolean isForInterface,
	//			int actionIndex) {
	//
	//		List<String> parameterTypes = CollectionLiterals.newArrayList();
	//		JvmFormalParameter lastParam = null;
	//		int paramIndex = 0;
	//		boolean hasDefaultValue = false;
	//		for (final FormalParameter param : params) {
	//			final String paramName = param.getName();
	//			JvmTypeReference paramType = param.getParameterType();
	//
	//			if (!Strings.isNullOrEmpty(paramName) && paramType != null) {
	//				lastParam = this.typeBuilder.toParameter(param, paramName, paramType);
	//
	//				if (param.getDefaultValue() != null) {
	//					hasDefaultValue = true;
	//					String namePostPart = actionIndex + "_" + paramIndex; //$NON-NLS-1$
	//					String name = ModelUtil.PREFIX_ATTRIBUTE_DEFAULT_VALUE + namePostPart;
	//					// FIXME: Hide these attributes into an inner interface.
	//					JvmField field = this.typeBuilder.toField(param.getDefaultValue(), name,
	//							paramType, new Procedures.Procedure1<JvmField>() {
	//						@SuppressWarnings("synthetic-access")
	//						@Override
	//						public void apply(JvmField it) {
	//							SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it,
	//									MessageFormat.format(Messages.SARLJvmModelInferrer_11, paramName));
	//							it.setStatic(true);
	//							it.setFinal(true);
	//							if (isForInterface) {
	//								it.setVisibility(JvmVisibility.PUBLIC);
	//							} else {
	//								it.setVisibility(JvmVisibility.PRIVATE);
	//							}
	//							SARLJvmModelInferrer.this.typeBuilder.setInitializer(it, param.getDefaultValue());
	//							if (param.getDefaultValue() != null) {
	//								it.getAnnotations().add(
	//										SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(
	//												Generated.class,
	//												SARLJvmModelInferrer.this.sarlSerializer.serialize(param.getDefaultValue())));
	//							}
	//						}
	//					});
	//					actionContainer.getMembers().add(field);
	//					if (owner instanceof JvmConstructor) {
	//						this.readAndWriteTracking.markInitialized(field, (JvmConstructor) owner);
	//					} else {
	//						this.readAndWriteTracking.markInitialized(field, null);
	//					}
	//					JvmAnnotationReference annot = this._annotationTypesBuilder.annotationRef(DefaultValue.class, namePostPart);
	//					lastParam.getAnnotations().add(annot);
	//				}
	//
	//				owner.getParameters().add(lastParam);
	//				parameterTypes.add(paramType.getIdentifier());
	//
	//				++paramIndex;
	//			}
	//		}
	//
	//		if (varargs && lastParam != null) {
	//			lastParam.setParameterType(this.typeBuilder.addArrayTypeDimension(lastParam.getParameterType()));
	//		}
	//		if (hasDefaultValue) {
	//			owner.getAnnotations().add(this._annotationTypesBuilder.annotationRef(DefaultValueSource.class));
	//		}
	//		return parameterTypes;
	//	}
	//
	//	/** Generate a list of formal parameters with annotations for the default values.
	//	 *
	//	 * @param owner - the JVM element to change.
	//	 * @param actionContainer - the container of the action.
	//	 * @param varargs - indicates if the signature has variadic parameter.
	//	 * @param signature - the description of the parameters.
	//	 * @param actionIndex - the index of the generated action.
	//	 * @return the names of the formal parameters.
	//	 */
	//	protected List<String> generateFormalParametersWithDefaultValue(JvmExecutable owner, JvmGenericType actionContainer,
	//			boolean varargs, List<InferredStandardParameter> signature, int actionIndex) {
	//		JvmFormalParameter lastParam = null;
	//		List<String> arguments = CollectionLiterals.newArrayList();
	//		int paramIndex = 0;
	//		for (InferredStandardParameter parameterSpec : signature) {
	//			if (parameterSpec instanceof InferredValuedParameter) {
	//				arguments.add(ModelUtil.PREFIX_ATTRIBUTE_DEFAULT_VALUE + actionIndex + "_" + paramIndex); //$NON-NLS-1$
	//			} else {
	//				FormalParameter param = parameterSpec.getParameter();
	//				String paramName = param.getName();
	//				JvmTypeReference paramType = param.getParameterType();
	//				if (!Strings.isNullOrEmpty(paramName) && paramType != null) {
	//					lastParam = this.typeBuilder.toParameter(param, paramName, paramType);
	//					owner.getParameters().add(lastParam);
	//					arguments.add(paramName);
	//				}
	//			}
	//			++paramIndex;
	//		}
	//		if (varargs && lastParam != null) {
	//			lastParam.setParameterType(this.typeBuilder.addArrayTypeDimension(lastParam.getParameterType()));
	//		}
	//		return arguments;
	//	}
	//
	//
	//	/** Generate a constructor.
	//	 *
	//	 * @param owner - the JVM element to change.
	//	 * @param context - the container of the SARL constructor.
	//	 * @param constructor - the constructor.
	//	 * @param index - the index of the constructor.
	//	 * @param generatedConstructors - a map to fill with the generated constructor.
	//	 * @return the signature of the constructor.
	//	 */
	//	protected SignatureKey generateConstructor(
	//			final JvmGenericType owner, TopElement context,
	//			final Constructor constructor, final int index,
	//			Map<SignatureKey, JvmConstructor> generatedConstructors) {
	//		ActionNameKey actionKey = this.sarlSignatureProvider.createConstructorID(owner);
	//		JvmConstructor cons = this.typeBuilder.toConstructor(constructor, new Procedures.Procedure1<JvmConstructor>() {
	//			@Override
	//			public void apply(JvmConstructor it) {
	//				SARLJvmModelInferrer.this.typeBuilder.copyDocumentationTo(constructor, it);
	//				it.setVarArgs(constructor.isVarargs());
	//				generateFormalParametersAndDefaultValueFields(
	//						it, owner, constructor.isVarargs(),
	//						constructor.getParams(), false, index);
	//				SARLJvmModelInferrer.this.typeBuilder.setBody(it, constructor.getBody());
	//			}
	//		});
	//		owner.getMembers().add(cons);
	//		if (generatedConstructors != null) {
	//			SignatureKey sigKey = this.sarlSignatureProvider.createSignatureIDFromJvmModel(
	//					cons.isVarArgs(), cons.getParameters());
	//			generatedConstructors.put(sigKey, cons);
	//		}
	//
	//		final InferredActionSignature otherSignatures = this.sarlSignatureProvider.createSignature(
	//				actionKey,
	//				constructor.isVarargs(), constructor.getParams());
	//
	//		for (final EList<InferredStandardParameter> otherSignature : otherSignatures) {
	//			JvmConstructor op = this.typeBuilder.toConstructor(constructor, new Procedures.Procedure1<JvmConstructor>() {
	//				@SuppressWarnings("synthetic-access")
	//				@Override
	//				public void apply(JvmConstructor it) {
	//					SARLJvmModelInferrer.this.typeBuilder.copyDocumentationTo(constructor, it);
	//					it.setVarArgs(constructor.isVarargs());
	//					final List<String> args = generateFormalParametersWithDefaultValue(
	//							it, owner, constructor.isVarargs(), otherSignature, index);
	//					SARLJvmModelInferrer.this.typeBuilder.setBody(it, new Procedures.Procedure1<ITreeAppendable>() {
	//						@Override
	//						public void apply(ITreeAppendable it2) {
	//							it2.append("this("); //$NON-NLS-1$
	//							it2.append(IterableExtensions.join(args, ", ")); //$NON-NLS-1$
	//							it2.append(");"); //$NON-NLS-1$
	//						}
	//					});
	//					it.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(
	//							DefaultValueUse.class,
	//							otherSignatures.getFormalParameterKey().toString()));
	//					it.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(
	//							Generated.class));
	//				}
	//			});
	//			owner.getMembers().add(op);
	//			if (generatedConstructors != null) {
	//				SignatureKey sigKey = this.sarlSignatureProvider.createSignatureIDFromJvmModel(
	//						op.isVarArgs(), op.getParameters());
	//				generatedConstructors.put(sigKey, op);
	//			}
	//		}
	//
	//		return otherSignatures.getFormalParameterKey();
	//	}
	//
	//	/** Generate the "equals()" operation.
	//	 * This function was deprecated in Xbase, and should be provided by DSL
	//	 * providers now.
	//	 *
	//	 * @param sarlElement - the SARL element for which the "equals function must be generated.
	//	 * @param declaredType - the declating type.
	//	 * @param jvmFields - the fields declared in the container.
	//	 * @return the "equals" function.
	//	 */
	//	protected JvmOperation toEqualsMethod(
	//			FeatureContainer sarlElement,
	//			final JvmDeclaredType declaredType,
	//			final JvmField... jvmFields) {
	//		if (sarlElement == null || declaredType == null) {
	//			return null;
	//		}
	//		JvmOperation result = this.typeBuilder.toMethod(sarlElement, "equals", //$NON-NLS-1$
	//				this._typeReferenceBuilder.typeRef(Boolean.TYPE), null);
	//		if (result == null) {
	//			return null;
	//		}
	//		result.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Override.class));
	//		JvmFormalParameter param = this.typeBuilder.toParameter(sarlElement, "obj", //$NON-NLS-1$
	//				this._typeReferenceBuilder.typeRef(Object.class));
	//		result.getParameters().add(param);
	//		this.typeBuilder.setBody(result, new Procedures.Procedure1<ITreeAppendable>() {
	//			@Override
	//			public void apply(ITreeAppendable it) {
	//				it.append("if (this == obj)").increaseIndentation(); //$NON-NLS-1$
	//				it.newLine().append("return true;").decreaseIndentation(); //$NON-NLS-1$
	//				it.newLine().append("if (obj == null)").increaseIndentation(); //$NON-NLS-1$
	//				it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
	//				it.newLine().append("if (getClass() != obj.getClass())").increaseIndentation(); //$NON-NLS-1$
	//				it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
	//				it.newLine().append("if (!super.equals(obj))").increaseIndentation(); //$NON-NLS-1$
	//				it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
	//				it.newLine().append(declaredType.getSimpleName() + " other = (" //$NON-NLS-1$
	//						+ declaredType.getSimpleName() + ") obj;"); //$NON-NLS-1$
	//				for (JvmField field : jvmFields) {
	//					generateToEqualForField(it, field);
	//				}
	//				it.newLine().append("return true;"); //$NON-NLS-1$
	//			}
	//
	//			private boolean arrayContains(String element, String... array) {
	//				for (String elt : array) {
	//					if (Objects.equal(elt, element)) {
	//						return true;
	//					}
	//				}
	//				return false;
	//			}
	//
	//			private void generateToEqualForField(ITreeAppendable it, JvmField field) {
	//				String typeName = field.getType().getIdentifier();
	//				if (arrayContains(typeName,
	//						Boolean.TYPE.getName(),
	//						Integer.TYPE.getName(),
	//						Long.TYPE.getName(),
	//						Character.TYPE.getName(),
	//						Byte.TYPE.getName(),
	//						Short.TYPE.getName())) {
	//					it.newLine().append("if (other." + field.getSimpleName() //$NON-NLS-1$
	//							+ " != this." + field.getSimpleName() + ")").increaseIndentation(); //$NON-NLS-1$ //$NON-NLS-2$
	//					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
	//				} else if (Objects.equal(Double.TYPE.getName(), typeName)) {
	//					it.newLine().append("if (Double.doubleToLongBits(other." + field.getSimpleName() //$NON-NLS-1$
	//							+ ") != Double.doubleToLongBits(this." + field.getSimpleName() //$NON-NLS-1$
	//							+ "))").increaseIndentation(); //$NON-NLS-1$
	//					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
	//				} else if (Objects.equal(Float.TYPE.getName(), typeName)) {
	//					it.newLine().append("if (Float.floatToIntBits(other." + field.getSimpleName() //$NON-NLS-1$
	//							+ ") != Float.floatToIntBits(this." + field.getSimpleName() //$NON-NLS-1$
	//							+ "))").increaseIndentation(); //$NON-NLS-1$
	//					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
	//				} else  {
	//					it.newLine().append("if (this." + field.getSimpleName() //$NON-NLS-1$
	//							+ " == null) {").increaseIndentation(); //$NON-NLS-1$
	//					it.newLine().append("if (other." + field.getSimpleName() //$NON-NLS-1$
	//							+ " != null)").increaseIndentation(); //$NON-NLS-1$
	//					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
	//					it.decreaseIndentation();
	//					it.newLine().append("} else if (!this." + field.getSimpleName() //$NON-NLS-1$
	//							+ ".equals(other." + field.getSimpleName() //$NON-NLS-1$
	//							+ "))").increaseIndentation(); //$NON-NLS-1$
	//					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
	//				}
	//			}
	//		});
	//		return result;
	//	}
	//
	//	/** Generate the "hashCode()" operation.
	//	 * This function was deprecated in Xbase, and should be provided by DSL
	//	 * providers now.
	//	 *
	//	 * @param sarlElement - the SARL element for which the "hashCode" msut be generated.
	//	 * @param jvmFields - the fields declared in the container.
	//	 * @return the "hashCode" function.
	//	 */
	//	protected JvmOperation toHashCodeMethod(
	//			FeatureContainer sarlElement,
	//			final JvmField... jvmFields) {
	//		if (sarlElement == null) {
	//			return null;
	//		}
	//		JvmOperation result = this.typeBuilder.toMethod(sarlElement, "hashCode", //$NON-NLS-1$
	//				this._typeReferenceBuilder.typeRef(Integer.TYPE), null);
	//		if (result == null) {
	//			return null;
	//		}
	//		result.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Override.class));
	//		this.typeBuilder.setBody(result, new Procedures.Procedure1<ITreeAppendable>() {
	//			@Override
	//			public void apply(ITreeAppendable it) {
	//				it.append("final int prime = 31;"); //$NON-NLS-1$
	//				it.newLine().append("int result = super.hashCode();"); //$NON-NLS-1$
	//				for (JvmField field : jvmFields) {
	//					String typeName = field.getType().getIdentifier();
	//					if (Objects.equal(Boolean.TYPE.getName(), typeName)) {
	//						it.newLine().append("result = prime * result + (this." //$NON-NLS-1$
	//								+ field.getSimpleName() + " ? 1231 : 1237);"); //$NON-NLS-1$
	//					} else if (Objects.equal(Integer.TYPE.getName(), typeName)
	//							|| Objects.equal(Character.TYPE.getName(), typeName)
	//							|| Objects.equal(Byte.TYPE.getName(), typeName)
	//							|| Objects.equal(Short.TYPE.getName(), typeName)) {
	//						it.newLine().append("result = prime * result + this." //$NON-NLS-1$
	//								+ field.getSimpleName() + ";"); //$NON-NLS-1$
	//					} else if (Objects.equal(Long.TYPE.getName(), typeName)) {
	//						it.newLine().append("result = prime * result + (int) (this." //$NON-NLS-1$
	//								+ field.getSimpleName() + " ^ (this." + field.getSimpleName() //$NON-NLS-1$
	//								+ " >>> 32));"); //$NON-NLS-1$
	//					} else if (Objects.equal(Float.TYPE.getName(), typeName)) {
	//						it.newLine().append("result = prime * result + Float.floatToIntBits(this." //$NON-NLS-1$
	//								+ field.getSimpleName() + ");"); //$NON-NLS-1$
	//					} else if (Objects.equal(Double.TYPE.getName(), typeName)) {
	//						it.newLine().append("result = prime * result + (int) (Double.doubleToLongBits(this." //$NON-NLS-1$
	//								+ field.getSimpleName() + ") ^ (Double.doubleToLongBits(this." //$NON-NLS-1$
	//								+ field.getSimpleName() + ") >>> 32));"); //$NON-NLS-1$
	//					} else {
	//						it.newLine().append("result = prime * result + ((this." //$NON-NLS-1$
	//								+ field.getSimpleName() + "== null) ? 0 : this." + field.getSimpleName() //$NON-NLS-1$
	//								+ ".hashCode());"); //$NON-NLS-1$
	//					}
	//				}
	//				it.newLine().append("return result;"); //$NON-NLS-1$
	//			}
	//		});
	//		return result;
	//	}
	//
	//	/** Abstract generator of the top elements.
	//	 *
	//	 * Provides shared statements for the generators.
	//	 *
	//	 * @param <T> - the type of the SARL element.
	//	 * @author $Author: sgalland$
	//	 * @version $FullVersion$
	//	 * @mavengroupid $GroupId$
	//	 * @mavenartifactid $ArtifactId$
	//	 */
	//	protected abstract class AbstractGenerator<T extends XtendTypeDeclaration> implements Procedures.Procedure1<JvmGenericType> {
	//
	//		/** SARL element.
	//		 */
	//		protected final T element;
	//
	//		/**
	//		 * @param element - the source element.
	//		 */
	//		public AbstractGenerator(T element) {
	//			assert (element != null);
	//			this.element = element;
	//		}
	//
	//		@Override
	//		public final void apply(JvmGenericType it) {
	//			// Reset the action registry
	//			SARLJvmModelInferrer.this.sarlSignatureProvider.resetSignatures(it);
	//			SARLJvmModelInferrer.this.typeBuilder.copyDocumentationTo(this.element, it);
	//			generate(it);
	//		}
	//
	//		/** Generate the JVM element from the SARL element.
	//		 *
	//		 * @param output - the output JVM element to complete.
	//		 */
	//		protected abstract void generate(JvmGenericType output);
	//
	//	}
	//
	//	/** Generator for the SARL Event statement.
	//	 *
	//	 * @author $Author: sgalland$
	//	 * @version $FullVersion$
	//	 * @mavengroupid $GroupId$
	//	 * @mavenartifactid $ArtifactId$
	//	 */
	//	protected class EventGenerator extends AbstractGenerator<SarlEvent> {
	//
	//		/**
	//		 * @param event - the event.
	//		 */
	//		public EventGenerator(SarlEvent event) {
	//			super(event);
	//		}
	//
	//		@Override
	//		public void generate(JvmGenericType it) {
	//			long serial = 1L;
	//			serial = serial + generateExtendedTypes(it, this.element, io.sarl.lang.core.Event.class);
	//			JvmField jvmField;
	//			List<JvmField> jvmFields = CollectionLiterals.newArrayList();
	//			int actionIndex = 0;
	//			boolean hasConstructor = false;
	//
	//			for (EObject feature : this.element.getFeatures()) {
	//				if (feature instanceof Attribute) {
	//					Attribute attribute = (Attribute) feature;
	//					jvmField = generateAttribute(it, attribute, JvmVisibility.PUBLIC);
	//					if (jvmField != null) {
	//						jvmFields.add(jvmField);
	//						it.getMembers().add(jvmField);
	//						serial = serial + attribute.getName().hashCode();
	//					}
	//				} else if (feature instanceof Constructor) {
	//					Constructor constructor = (Constructor) feature;
	//					if (generateConstructor(it, this.element, constructor, actionIndex, null) != null) {
	//						serial = serial + SARLJvmModelInferrer.this.nameProvider.getFullyQualifiedName(
	//								this.element).hashCode();
	//						++actionIndex;
	//						hasConstructor = true;
	//					}
	//				}
	//			}
	//
	//			if (!hasConstructor) {
	//				createDefaultConstructor(it);
	//			}
	//
	//			if (!jvmFields.isEmpty()) {
	//				generateAttributeFunctions(it, jvmFields);
	//			}
	//
	//			generateSerialID(it, serial);
	//		}
	//
	//		@SuppressWarnings("synthetic-access")
	//		private void generateSerialID(JvmGenericType it, final long serial) {
	//			JvmField serialField = SARLJvmModelInferrer.this.typeBuilder.toField(this.element, "serialVersionUID", //$NON-NLS-1$
	//					SARLJvmModelInferrer.this._typeReferenceBuilder.typeRef(long.class), new Procedures.Procedure1<JvmField>() {
	//				@Override
	//				public void apply(JvmField it2) {
	//					it2.setVisibility(JvmVisibility.PRIVATE);
	//					it2.setFinal(true);
	//					it2.setStatic(true);
	//					SARLJvmModelInferrer.this.typeBuilder.setInitializer(it2,
	//							toStringConcatenation(serial + "L")); //$NON-NLS-1$
	//				}
	//			});
	//			serialField.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(
	//					Generated.class));
	//			SARLJvmModelInferrer.this.typeExtensions.setSynthetic(serialField, true);
	//			it.getMembers().add(serialField);
	//			SARLJvmModelInferrer.this.readAndWriteTracking.markInitialized(serialField, null);
	//		}
	//
	//		@SuppressWarnings("synthetic-access")
	//		private void createDefaultConstructor(JvmGenericType it) {
	//			JvmConstructor cons1 = SARLJvmModelInferrer.this.typeBuilder.toConstructor(this.element,
	//					new Procedures.Procedure1<JvmConstructor>() {
	//				@Override
	//				public void apply(JvmConstructor it2) {
	//					SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it2, Messages.SARLJvmModelInferrer_0);
	//					SARLJvmModelInferrer.this.typeBuilder.setBody(it2, toStringConcatenation("super();")); //$NON-NLS-1$
	//				}
	//			});
	//			cons1.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
	//			SARLJvmModelInferrer.this.typeExtensions.setSynthetic(cons1, true);
	//			it.getMembers().add(cons1);
	//
	//			final JvmTypeReference addrType = SARLJvmModelInferrer.this._typeReferenceBuilder.typeRef(Address.class);
	//			JvmConstructor cons2 = SARLJvmModelInferrer.this.typeBuilder.toConstructor(this.element,
	//					new Procedures.Procedure1<JvmConstructor>() {
	//				@Override
	//				public void apply(JvmConstructor it2) {
	//					SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it2,
	//							MessageFormat.format(Messages.SARLJvmModelInferrer_1, "source")); //$NON-NLS-1$
	//					it2.getParameters().add(SARLJvmModelInferrer.this.typeBuilder.toParameter(
	//							EventGenerator.this.element, "source", addrType)); //$NON-NLS-1$
	//					SARLJvmModelInferrer.this.typeBuilder.setBody(it2,
	//							toStringConcatenation("super(source);")); //$NON-NLS-1$
	//				}
	//			});
	//			cons2.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
	//			SARLJvmModelInferrer.this.typeExtensions.setSynthetic(cons2, true);
	//			it.getMembers().add(cons2);
	//		}
	//
	//		@SuppressWarnings("synthetic-access")
	//		private void generateAttributeFunctions(JvmGenericType it, List<JvmField> jvmFields) {
	//			// single translation to the array
	//			JvmField[] tab = new JvmField[jvmFields.size()];
	//			jvmFields.toArray(tab);
	//			JvmGenericType elementType = SARLJvmModelInferrer.this.typeBuilder.toClass(this.element,
	//					SARLJvmModelInferrer.this.nameProvider.getFullyQualifiedName(this.element));
	//
	//			JvmOperation op = toEqualsMethod(this.element, elementType, tab);
	//			if (op != null) {
	//				op.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
	//				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
	//				it.getMembers().add(op);
	//			}
	//
	//			op = toHashCodeMethod(this.element, tab);
	//			if (op != null) {
	//				op.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
	//				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
	//				it.getMembers().add(op);
	//			}
	//
	//			op = SARLJvmModelInferrer.this.typeBuilder.toMethod(
	//					this.element,
	//					"attributesToString", //$NON-NLS-1$
	//					SARLJvmModelInferrer.this._typeReferenceBuilder.typeRef(String.class),
	//					new Procedures.Procedure1<JvmOperation>() {
	//						@Override
	//						public void apply(JvmOperation it2) {
	//							it2.setVisibility(JvmVisibility.PROTECTED);
	//							SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it2,
	//									MessageFormat.format(Messages.SARLJvmModelInferrer_2,
	//											EventGenerator.this.element.getName()));
	//							SARLJvmModelInferrer.this.typeBuilder.setBody(it2,
	//									new Procedures.Procedure1<ITreeAppendable>() {
	//								@Override
	//								public void apply(ITreeAppendable it3) {
	//									it3.append("StringBuilder result = new StringBuilder(" //$NON-NLS-1$
	//											+ "super.attributesToString());").newLine(); //$NON-NLS-1$
	//									for (Attribute attr : IterableExtensions.filter(
	//											EventGenerator.this.element.getFeatures(), Attribute.class)) {
	//										it3.append("result.append(\"" + attr.getName() //$NON-NLS-1$
	//												+ "  = \").append(this." //$NON-NLS-1$
	//												+ attr.getName() + ");").newLine(); //$NON-NLS-1$
	//									}
	//									it3.append("return result.toString();"); //$NON-NLS-1$
	//								}
	//							});
	//						}
	//					});
	//			if (op != null) {
	//				op.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
	//				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
	//				it.getMembers().add(op);
	//			}
	//		}
	//
	//	}
	//
	//	/** Generate the JVM element for a SARL Behavior.
	//	 *
	//	 * @author $Author: sgalland$
	//	 * @version $FullVersion$
	//	 * @mavengroupid $GroupId$
	//	 * @mavenartifactid $ArtifactId$
	//	 */
	//	protected class BehaviorGenerator extends AbstractGenerator<Behavior> {
	//
	//		/**
	//		 * @param behavior - the behavior.
	//		 */
	//		public BehaviorGenerator(Behavior behavior) {
	//			super(behavior);
	//		}
	//
	//		@SuppressWarnings("synthetic-access")
	//		@Override
	//		public void generate(JvmGenericType it) {
	//			generateExtendedTypes(it, this.element, io.sarl.lang.core.Behavior.class);
	//
	//			GenerationInformation genInfo = generateCodeForFeatures(it, this.element, this.element, true, null);
	//
	//			if (!genInfo.hasConstructor) {
	//				final JvmTypeReference aType = SARLJvmModelInferrer.this._typeReferenceBuilder.typeRef(
	//						io.sarl.lang.core.Agent.class);
	//				JvmConstructor cons = SARLJvmModelInferrer.this.typeBuilder.toConstructor(this.element,
	//						new Procedures.Procedure1<JvmConstructor>() {
	//					@Override
	//					public void apply(JvmConstructor it) {
	//						SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it, MessageFormat.format(
	//								Messages.SARLJvmModelInferrer_5, "owner")); //$NON-NLS-1$
	//						it.getParameters().add(SARLJvmModelInferrer.this.typeBuilder.toParameter(
	//								BehaviorGenerator.this.element, "owner", aType)); //$NON-NLS-1$
	//						SARLJvmModelInferrer.this.typeBuilder.setBody(it, toStringConcatenation("super(owner);")); //$NON-NLS-1$
	//					}
	//				});
	//
	//				cons.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
	//				it.getMembers().add(cons);
	//				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(cons, true);
	//			}
	//		}
	//
	//	}
	//
	//	/** Generator of the agent top element.
	//	 *
	//	 * @author $Author: sgalland$
	//	 * @version $FullVersion$
	//	 * @mavengroupid $GroupId$
	//	 * @mavenartifactid $ArtifactId$
	//	 */
	//	protected class AgentGenerator extends AbstractGenerator<SarlAgent> {
	//
	//		/**
	//		 * @param agent - the agent.
	//		 */
	//		public AgentGenerator(SarlAgent agent) {
	//			super(agent);
	//		}
	//
	//		@SuppressWarnings({ "synthetic-access", "unchecked" })
	//		@Override
	//		public void generate(JvmGenericType it) {
	//			generateExtendedTypes(it, this.element, io.sarl.lang.core.Agent.class);
	//
	//			Map<SignatureKey, JvmConstructor> generatedConstructors = CollectionLiterals.newTreeMap(null);
	//			generateCodeForFeatures(it, this.element, this.element, true, generatedConstructors);
	//
	//
	//			JvmConstructor cons1 = SARLJvmModelInferrer.this.typeBuilder.toConstructor(this.element,
	//					new Procedures.Procedure1<JvmConstructor>() {
	//				@Override
	//				public void apply(JvmConstructor it) {
	//					SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it, MessageFormat.format(
	//							Messages.SARLJvmModelInferrer_6, "parentID")); //$NON-NLS-1$
	//					it.getParameters().add(SARLJvmModelInferrer.this.typeBuilder.toParameter(
	//							AgentGenerator.this.element, "parentID", //$NON-NLS-1$
	//							SARLJvmModelInferrer.this._typeReferenceBuilder.typeRef(UUID.class)));
	//					SARLJvmModelInferrer.this.typeBuilder.setBody(it,
	//							toStringConcatenation("super(parentID, null);")); //$NON-NLS-1$
	//				}
	//			});
	//			SignatureKey sigCons1 = SARLJvmModelInferrer.this.sarlSignatureProvider.createSignatureIDFromJvmModel(
	//					cons1.isVarArgs(), cons1.getParameters());
	//			if (!generatedConstructors.containsKey(sigCons1)) {
	//				cons1.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
	//				it.getMembers().add(cons1);
	//				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(cons1, true);
	//			}
	//
	//			JvmConstructor cons2 = SARLJvmModelInferrer.this.typeBuilder.toConstructor(this.element,
	//					new Procedures.Procedure1<JvmConstructor>() {
	//				@Override
	//				public void apply(JvmConstructor it) {
	//					SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it, MessageFormat.format(
	//							Messages.SARLJvmModelInferrer_7,
	//							"parentID", "agentID")); //$NON-NLS-1$ //$NON-NLS-2$
	//					it.getParameters().add(SARLJvmModelInferrer.this.typeBuilder.toParameter(
	//							AgentGenerator.this.element, "parentID", //$NON-NLS-1$
	//							SARLJvmModelInferrer.this._typeReferenceBuilder.typeRef(UUID.class)));
	//					it.getParameters().add(SARLJvmModelInferrer.this.typeBuilder.toParameter(
	//							AgentGenerator.this.element, "agentID", //$NON-NLS-1$
	//							SARLJvmModelInferrer.this._typeReferenceBuilder.typeRef(UUID.class)));
	//					SARLJvmModelInferrer.this.typeBuilder.setBody(it,
	//							toStringConcatenation("super(parentID, agentID);")); //$NON-NLS-1$
	//				}
	//
	//			});
	//			SignatureKey sigCons2 = SARLJvmModelInferrer.this.sarlSignatureProvider.createSignatureIDFromJvmModel(
	//					cons2.isVarArgs(), cons2.getParameters());
	//			if (!generatedConstructors.containsKey(sigCons2)) {
	//				cons2.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
	//				it.getMembers().add(cons2);
	//				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(cons2, true);
	//			}
	//		}
	//
	//	}

	/** Generate a constructor.
	 *
	 * @param source - the SARL element.
	 * @param inferredJvmType - the inferred type.
	 * @param params - the definition of the parameters.
	 * @param typeParameters - the type parameters for the action.
	 * @param exceptions - the thrown exceptions.
	 * @param constructorBody - the body's or <code>null</code>.
	 * @param generatedConstructors - the list of constructor that is filled with the generated constructor.
	 * @return the main constructor.
	 * @see #transform(XtendFunction, JvmGenericType, boolean)
	 */
	protected JvmConstructor translateConstructor(
			XtendMember source,
			JvmGenericType inferredJvmType,
			List<? extends XtendParameter> params,
			List<JvmTypeParameter> typeParameters,
			List<JvmTypeReference> exceptions,
			XExpression constructorBody,
			Map<SignatureKey, JvmConstructor> generatedConstructors) {

		//**************************************
		// Main constructor
		//
		final JvmConstructor mainConstructor = this.typesFactory.createJvmConstructor();
		try {
			// Initialize the context
			GenerationContext context = new GenerationContext(this.generationContexts.get(inferredJvmType));
			this.generationContexts.put(mainConstructor, context);
			// Modifiers
			mainConstructor.setVisibility(source.getVisibility());
			// Parameters
			context.getLateArguments().clear();
			for (XtendParameter parameter : params) {
				translateParameter(mainConstructor, parameter);
			}
			// Type parameters
			copyAndFixTypeParameters(typeParameters, mainConstructor);
			// Exceptions
			for (JvmTypeReference exception : exceptions) {
				mainConstructor.getExceptions().add(this.jvmTypesBuilder.cloneWithProxies(exception));
			}
			// Annotations
			translateAnnotationsTo(source.getAnnotations(), mainConstructor);
			// Creation extension
			setBody(mainConstructor, constructorBody);
			// Documentation
			this.jvmTypesBuilder.copyDocumentationTo(source, mainConstructor);
			// Container
			inferredJvmType.getMembers().add(mainConstructor);
			this.associator.associatePrimary(source, mainConstructor);

			//**************************************
			// Add operations with default values
			//	
			ActionNameKey actionKey = this.sarlSignatureProvider.createConstructorID(inferredJvmType);

			InferredActionSignature otherSignatures = this.sarlSignatureProvider.createSignature(
					actionKey, params);

			// Update the output lists
			if (generatedConstructors != null) {
				SignatureKey sigKey = this.sarlSignatureProvider.createSignatureIDFromJvmModel(
						mainConstructor.isVarArgs(), mainConstructor.getParameters());
				generatedConstructors.put(sigKey, mainConstructor);
			}

			for (List<InferredStandardParameter> otherSignature : otherSignatures.getInferredSignatures().values()) {
				JvmConstructor additionalConstructor = this.typesFactory.createJvmConstructor();
				// Modifiers
				additionalConstructor.setVisibility(source.getVisibility());
				// Parameters
				context.getLateArguments().clear();
				for (InferredStandardParameter parameter : otherSignature) {
					translateParameter(additionalConstructor, parameter.getParameter());
				}
				// Type parameters
				copyAndFixTypeParameters(typeParameters, additionalConstructor);
				// Exceptions
				for (JvmTypeReference exception : exceptions) {
					additionalConstructor.getExceptions().add(this.jvmTypesBuilder.cloneWithProxies(exception));
				}
				// Annotations
				translateAnnotationsTo(source.getAnnotations(), additionalConstructor);
				additionalConstructor.getAnnotations().add(this.annotationTypesBuilder.annotationRef(
						DefaultValueUse.class,
						otherSignatures.getFormalParameterKey().toString()));
				additionalConstructor.getAnnotations().add(
						this.annotationTypesBuilder.annotationRef(Generated.class));
				// Body
				this.jvmTypesBuilder.setBody(additionalConstructor, new Procedures.Procedure1<ITreeAppendable>() {
					@SuppressWarnings("synthetic-access")
					@Override
					public void apply(ITreeAppendable it) {
						it.append("this("); //$NON-NLS-1$
						it.append(IterableExtensions.join(
								SARLJvmModelInferrer.this.generationContexts.get(mainConstructor)
								.getLateArguments(), ", ")); //$NON-NLS-1$
						it.append(");"); //$NON-NLS-1$
					}
				});
				// Documentation
				this.jvmTypesBuilder.copyDocumentationTo(source, additionalConstructor);
				// Container
				inferredJvmType.getMembers().add(additionalConstructor);
				this.associator.associate(source, additionalConstructor);

				// Update the output lists
				if (generatedConstructors != null) {
					SignatureKey sigKey = this.sarlSignatureProvider.createSignatureIDFromJvmModel(
							additionalConstructor.isVarArgs(), additionalConstructor.getParameters());
					generatedConstructors.put(sigKey, additionalConstructor);
				}
			}

			return mainConstructor;
		} finally {
			this.generationContexts.remove(mainConstructor);
		}
	}

	/** Context of the generation of an element.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected static class GenerationContext {

		private long serial;
		private int actionIndex;
		private int behaviorUnitIndex;
		private final GenerationContext parent;
		private List<String> latelyGenerateParameters = CollectionLiterals.newArrayList();
		private final XtendTypeDeclaration container;
		private final JvmGenericType inferredContainer;

		/** Construct a information about the generation.
		 * 
		 * @param container - the container that is the top context.
		 * @param inferredContainer - the inferred container that is the top context.
		 */
		public GenerationContext(XtendTypeDeclaration container, JvmGenericType inferredContainer) {
			this.parent = null;
			this.container = container;
			this.inferredContainer =  inferredContainer;
		}

		/** Construct a information about the generation.
		 * 
		 * @param context - the parent context.
		 */
		public GenerationContext(GenerationContext context) {
			this.parent = context;
			this.container = null;
			this.inferredContainer = null;
		}

		/** Replies the top container.
		 *
		 * @return the top container.
		 */
		public XtendTypeDeclaration getSourceTopContainer() {
			if (this.parent != null) {
				return this.parent.getSourceTopContainer();
			}
			return this.container;
		}

		/** Replies the top container.
		 *
		 * @return the top container.
		 */
		public JvmGenericType getInferredTopContainer() {
			if (this.parent != null) {
				return this.parent.getInferredTopContainer();
			}
			return this.inferredContainer;
		}

		/** Replies the lately generated arguments.
		 *
		 * @return the arguments.
		 */
		public List<String> getLateArguments() {
			return this.latelyGenerateParameters;
		}

		/** Set the lately generated arguments.
		 *
		 * @param arguments - the arguments.
		 */
		public void setLateArguments(List<String> arguments) {
			if (arguments == null) {
				this.latelyGenerateParameters = CollectionLiterals.newArrayList();
			} else {
				this.latelyGenerateParameters = arguments;
			}
		}

		/** Replies the serial associated to the generated element.
		 *
		 * @return the serial.
		 */
		public long getSerial() {
			if (this.parent != null) {
				return this.parent.getSerial();
			}
			return this.serial;
		}

		/** Set the serial associated to the generated element.
		 *
		 * @param serial - the new serial.
		 */
		public void setSerial(long serial) {
			if (this.parent != null) {
				this.parent.setSerial(serial);
			} else {
				this.serial = serial;
			}
		}

		/** Replies the index of the late created action.
		 *
		 * @return the index.
		 */
		public int getActionIndex() {
			if (this.parent != null) {
				return this.parent.getActionIndex();
			}
			return this.actionIndex;
		}

		/** Increments the index of the late created action.
		 *
		 * @return the new index.
		 */
		public int incrementActionIndex() {
			if (this.parent != null) {
				return this.parent.incrementActionIndex();
			}
			++this.actionIndex;
			return this.actionIndex;
		}

		/** Replies the index of the late created behavior unit.
		 *
		 * @return the index
		 */
		public int getBehaviorUnitIndex() {
			if (this.parent != null) {
				return this.parent.getBehaviorUnitIndex();
			}
			return this.behaviorUnitIndex;
		}

		/** Increments the index of the late created behavior unit.
		 *
		 * @return the new index.
		 */
		public int incrementBehaviorUnitIndex() {
			if (this.parent != null) {
				return this.parent.incrementBehaviorUnitIndex();
			}
			++this.behaviorUnitIndex;
			return this.behaviorUnitIndex;
		}

	}

}
