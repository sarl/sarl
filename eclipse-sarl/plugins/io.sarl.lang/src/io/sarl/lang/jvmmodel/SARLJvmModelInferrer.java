/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import java.lang.annotation.Annotation;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.UUID;
import java.util.logging.Logger;

import javax.annotation.Generated;

import com.google.common.base.Objects;
import com.google.common.base.Predicate;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import com.google.inject.MembersInjector;
import com.google.inject.Singleton;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.jvmmodel.SyntheticNameClashResolver;
import org.eclipse.xtend.core.jvmmodel.XtendJvmModelInferrer;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendEnum;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.LanguageInfo;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmAnnotationValue;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmEnumerationType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmFeature;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeConstraint;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmUpperBound;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.JvmWildcardTypeReference;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.serializer.sequencer.IContextFinder;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociator;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeExtensions;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.typesystem.InferredTypeIndicator;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.validation.ReadAndWriteTracking;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.actionprototype.ActionParameterTypes;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.actionprototype.InferredPrototype;
import io.sarl.lang.actionprototype.InferredStandardParameter;
import io.sarl.lang.actionprototype.InferredValuedParameter;
import io.sarl.lang.actionprototype.QualifiedActionName;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.EarlyExit;
import io.sarl.lang.annotation.FiredEvent;
import io.sarl.lang.annotation.ImportedCapacityFeature;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.compiler.IInlineExpressionCompiler;
import io.sarl.lang.controlflow.ISarlEarlyExitComputer;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.ClearableReference;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEnumLiteral;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.SarlSpace;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.typesystem.SARLAnnotationUtil;
import io.sarl.lang.typesystem.SARLExpressionHelper;
import io.sarl.lang.typesystem.SARLReentrantTypeResolver;
import io.sarl.lang.util.JvmVisibilityComparator;
import io.sarl.lang.util.Utils;

/** Infers a JVM model from the source model.
 *
 * <p>The JVM model should contain all elements that would appear in the Java code
 * which is generated from the source model. Other models link against
 * the JVM model rather than the source model.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings({"checkstyle:classfanoutcomplexity", "checkstyle:methodcount"})
@Singleton
public class SARLJvmModelInferrer extends XtendJvmModelInferrer {

	private static final String RUNNABLE_COLLECTION = Utils.createNameForHiddenLocalVariable("runnableCollection"); //$NON-NLS-1$

	/** See the filter in the super class.
	 */
	private static final Predicate<JvmAnnotationReference> ANNOTATION_TRANSLATION_FILTER = (annotation) -> {
		if (annotation == null || annotation.getAnnotation() == null) {
			return false;
		}
		//JvmType annotationType = annotation.getAnnotation();
		//if (annotationType instanceof JvmAnnotationType
		//		&& DisableCodeGenerationAdapter.isDisabled((JvmDeclaredType) annotationType)) {
		//	return false;
		//}
		return true;
	};

	/** The injector for generation contexts.
	 */
	@Inject
	private MembersInjector<GenerationContext> contextInjector;

	/** Generator of JVM elements.
	 */
	@Inject
	private JvmTypesBuilder typeBuilder;

	/** Generator's logger.
	 */
	@Inject
	private Logger log;

	/** Manager of SARL action signatures.
	 */
	@Inject
	private IActionPrototypeProvider sarlSignatureProvider;

	/** Tracker of field initialization.
	 */
	@Inject
	private ReadAndWriteTracking readAndWriteTracking;

	/** Several generation services.
	 */
	@Inject
	private CommonTypeComputationServices services;

	/** Extended helper for using XExpressions.
	 */
	@Inject
	private SARLExpressionHelper expressionHelper;

	/** JVM type services.
	 */
	@Inject
	private JvmTypeExtensions typeExtensions;

	/** Computer of early-exits for SARL.
	 */
	@Inject
	private ISarlEarlyExitComputer earlyExitComputer;

	/** Finder of annotations.
	 */
	@Inject
	private AnnotationLookup annotationFinder;

	/** Utilities for JVM annotations.
	 */
	@Inject
	private SARLAnnotationUtil annotationUtils;

	/** SARL Serializer.
	 */
	@Inject
	private ISerializer sarlSerializer;

	@Inject
	private TypesFactory typesFactory;

	@Inject
	private TypeReferences typeReferences;

	@Inject
	private SyntheticNameClashResolver nameClashResolver;

	@Inject
	private IJvmModelAssociator associator;

	@Inject
	private SarlJvmModelAssociations sarlAssociations;

	@Inject
	private JvmVisibilityComparator visibilityComparator;

	@Inject
	private LanguageInfo languageInfo;

	@Inject
	private IContextFinder contextFinder;

	@Inject
	private SARLGrammarKeywordAccess grammarKeywordAccess;

	@Inject
	private IInlineExpressionCompiler inlineExpressionCompiler;

	/** Generation contexts.
	 */
	private LinkedList<GenerationContext> bufferedContexes = new LinkedList<>();

	@Override
	protected void setBody(JvmExecutable executable, XExpression expression) {
		this.typeBuilder.removeExistingBody(executable);
		super.setBody(executable, expression);
	}

	/** Set the body of the executable.
	 *
	 * @param executable the executable.
	 * @param expression the body definition.
	 */
	protected void setBody(JvmExecutable executable, StringConcatenationClient expression) {
		this.typeBuilder.setBody(executable, expression);
	}

	/** Set the body of the executable.
	 *
	 * @param executable the executable.
	 * @param expression the body definition.
	 */
	protected void setBody(JvmExecutable executable, Procedure1<ITreeAppendable> expression) {
		this.typeBuilder.setBody(executable, expression);
	}

	/** Open the context for the generation of a SARL-specific element.
	 *
	 * @param sarlObject the SARL object that is the cause of the generation.
	 * @param type - the generated type.
	 * @param supportedMemberTypes the types of the supported members.
	 * @return the created context.
	 */
	protected final synchronized GenerationContext openContext(EObject sarlObject, JvmIdentifiableElement type,
			final Iterable<Class<? extends XtendMember>> supportedMemberTypes) {
		assert type != null;
		assert supportedMemberTypes != null;
		this.sarlSignatureProvider.clear(type);
		final GenerationContext context = new GenerationContext(sarlObject, type.getIdentifier()) {
			@Override
			public boolean isSupportedMember(XtendMember member) {
				for (final Class<? extends XtendMember> supportedMemberType : supportedMemberTypes) {
					if (supportedMemberType.isInstance(member)) {
						return true;
					}
				}
				return false;
			}
		};
		this.contextInjector.injectMembers(context);
		this.bufferedContexes.push(context);
		return context;
	}

	/** Close a generation context.
	 *
	 * @param context the context to be closed.
	 */
	protected final synchronized void closeContext(GenerationContext context) {
		final Iterator<GenerationContext> iterator = this.bufferedContexes.iterator();
		while (iterator.hasNext()) {
			final GenerationContext candidate = iterator.next();
			if (Objects.equal(candidate.getTypeIdentifier(), context.getTypeIdentifier())) {
				iterator.remove();
				return;
			}
		}
		throw new IllegalStateException("Not same contexts when closing"); //$NON-NLS-1$
	}

	/** Replies the SARL-specific generation context.
	 *
	 * @param type - the generated type.
	 * @return the SARL-specific generation context.
	 */
	protected final synchronized GenerationContext getContext(JvmIdentifiableElement type) {
		for (final GenerationContext candidate : this.bufferedContexes) {
			if (Objects.equal(candidate.getTypeIdentifier(), type.getIdentifier())) {
				return candidate;
			}
		}
		throw new IllegalStateException("generation context cannot be found for: " + type.getIdentifier()); //$NON-NLS-1$
	}

	@Override
	@SuppressWarnings("checkstyle:npathcomplexity")
	protected JvmDeclaredType doInferTypeSceleton(
			XtendTypeDeclaration declaration,
			IJvmDeclaredTypeAcceptor acceptor, boolean preIndexingPhase,
			XtendFile xtendFile, List<Runnable> doLater) {
		if (Strings.isNullOrEmpty(declaration.getName())) {
			return null;
		}

		final JvmDeclaredType type = super.doInferTypeSceleton(declaration, acceptor, preIndexingPhase,
				xtendFile, doLater);
		if (type != null) {
			return type;
		}
		if (declaration instanceof SarlAgent) {
			final SarlAgent sarlAgent = (SarlAgent) declaration;
			final JvmGenericType javaType = this.typesFactory.createJvmGenericType();
			if (!preIndexingPhase) {
				doLater.add(new Runnable() {
					@Override
					public void run() {
						initialize(sarlAgent, javaType);
					}
				});
			}
			return javaType;
		}
		if (declaration instanceof SarlBehavior) {
			final SarlBehavior sarlBehavior = (SarlBehavior) declaration;
			final JvmGenericType javaType = this.typesFactory.createJvmGenericType();
			if (!preIndexingPhase) {
				doLater.add(new Runnable() {
					@Override
					public void run() {
						initialize(sarlBehavior, javaType);
					}
				});
			}
			return javaType;
		}
		if (declaration instanceof SarlEvent) {
			final SarlEvent sarlEvent = (SarlEvent) declaration;
			final JvmGenericType javaType = this.typesFactory.createJvmGenericType();
			if (!preIndexingPhase) {
				doLater.add(new Runnable() {
					@Override
					public void run() {
						initialize(sarlEvent, javaType);
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
		if (declaration instanceof SarlSpace) {
			final SarlSpace sarlSpace = (SarlSpace) declaration;
			final JvmGenericType javaType = this.typesFactory.createJvmGenericType();
			if (!preIndexingPhase) {
				doLater.add(new Runnable() {
					@Override
					public void run() {
						initialize(sarlSpace, javaType);
					}
				});
			}
			return javaType;
		}
		return null;
	}

	@Override
	protected void initialize(XtendClass source, JvmGenericType inferredJvmType) {
		// Issue #356: do not generate if the class has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Issue #363: do not generate the class if the SARL library is incompatible.
		if (!Utils.isCompatibleSARLLibraryOnClasspath(this.typeReferences, source)) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final GenerationContext context = openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlConstructor.class, SarlAction.class,
				SarlBehaviorUnit.class, SarlCapacityUses.class, SarlRequiredCapacity.class));
		try {
			// Initialize the context with inheriting features
			Utils.populateInheritanceContext(
					inferredJvmType,
					context.getInheritedFinalOperations(),
					context.getInheritedOverridableOperations(),
					null,
					context.getInheritedOperationsToImplement(),
					null,
					this.sarlSignatureProvider);
			// Standard OOP generation
			super.initialize(source, inferredJvmType);
			// Add SARL synthetic functions
			appendSyntheticDefaultValuedParameterMethods(
					source,
					inferredJvmType,
					context);
			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);
		} finally {
			closeContext(context);
		}
	}

	@Override
	protected void initialize(XtendAnnotationType source, JvmAnnotationType inferredJvmType) {
		// Issue #356: do not generate if the annotation type has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Issue #363: do not generate the annotation if the SARL library is incompatible.
		if (!Utils.isCompatibleSARLLibraryOnClasspath(this.typeReferences, source)) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final GenerationContext context = openContext(source, inferredJvmType,
				Collections.singleton(SarlEnumLiteral.class));
		try {
			// Initialize the context with inheriting features
			Utils.populateInheritanceContext(
					inferredJvmType,
					context.getInheritedFinalOperations(),
					context.getInheritedOverridableOperations(),
					null,
					context.getInheritedOperationsToImplement(),
					null,
					this.sarlSignatureProvider);
			// Standard OOP generation
			super.initialize(source, inferredJvmType);
			// Add SARL synthetic functions
			appendSyntheticDefaultValuedParameterMethods(
					source,
					inferredJvmType,
					context);
			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);
		} finally {
			closeContext(context);
		}
	}

	@Override
	protected void initialize(XtendInterface source, JvmGenericType inferredJvmType) {
		// Issue #356: do not generate if the interface has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Issue #363: do not generate the interface if the SARL library is incompatible.
		if (!Utils.isCompatibleSARLLibraryOnClasspath(this.typeReferences, source)) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final GenerationContext context = openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlAction.class));
		try {
			// Initialize the context with inheriting features
			Utils.populateInheritanceContext(
					inferredJvmType,
					context.getInheritedFinalOperations(),
					context.getInheritedOverridableOperations(),
					null,
					context.getInheritedOperationsToImplement(),
					null,
					this.sarlSignatureProvider);
			// Standard OOP generation
			super.initialize(source, inferredJvmType);
			// Add SARL synthetic functions
			appendSyntheticDefaultValuedParameterMethods(
					source,
					inferredJvmType,
					context);
			// Add the @FunctionalInterface
			appendFunctionalInterfaceAnnotation(inferredJvmType);
			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);
		} finally {
			closeContext(context);
		}
	}

	@Override
	protected void initialize(XtendEnum source, JvmEnumerationType inferredJvmType) {
		// Issue #356: do not generate if the enumeration has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Issue #363: do not generate the enumeration if the SARL library is incompatible.
		if (!Utils.isCompatibleSARLLibraryOnClasspath(this.typeReferences, source)) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final GenerationContext context = openContext(source, inferredJvmType,
				Collections.singleton(SarlField.class));
		try {
			// Initialize the context with inheriting features
			Utils.populateInheritanceContext(
					inferredJvmType,
					context.getInheritedFinalOperations(),
					context.getInheritedOverridableOperations(),
					null,
					context.getInheritedOperationsToImplement(),
					null,
					this.sarlSignatureProvider);
			// Standard OOP generation
			super.initialize(source, inferredJvmType);
			// Add SARL synthetic functions
			appendSyntheticDefaultValuedParameterMethods(
					source,
					inferredJvmType,
					context);
			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);
		} finally {
			closeContext(context);
		}
	}

	/** Initialize the SARL agent type.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 */
	protected void initialize(SarlAgent source, JvmGenericType inferredJvmType) {
		// Issue #356: do not generate if the agent has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final GenerationContext context = openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlConstructor.class, SarlAction.class,
				SarlBehaviorUnit.class, SarlCapacityUses.class, SarlRequiredCapacity.class));
		try {
			// Copy the documentation
			this.typeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			inferredJvmType.setStatic(false);
			inferredJvmType.setStrictFloatingPoint(false);
			inferredJvmType.setVisibility(source.getVisibility());
			final boolean isAbstract = source.isAbstract() || Utils.hasAbstractMember(source);
			inferredJvmType.setAbstract(isAbstract);
			inferredJvmType.setFinal(!isAbstract && source.isFinal());

			// Generate the annotations.
			translateAnnotationsTo(source.getAnnotations(), inferredJvmType);

			// Generate the extended types.
			appendConstrainedExtends(context, inferredJvmType, Agent.class, SarlAgent.class, source.getExtends());

			// Issue #363: do not generate the agent if the SARL library is incompatible.
			if (Utils.isCompatibleSARLLibraryOnClasspath(this.typeReferences, source)) {
				// Generate the members of the generated type.
				appendAOPMembers(
						inferredJvmType,
						source,
						context);
			}

			// Add the default constructors for the agent, if not already added
			if (!context.hasConstructor()) {
				// new(builtinCapacityProvider : BuiltinCapacitiesProvider, parentID : UUID, agentID : UUID)
				final JvmConstructor constructor = this.typesFactory.createJvmConstructor();
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
				constructor.setSimpleName(source.getName());
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				this.typeBuilder.setDocumentation(constructor, MessageFormat.format(
						Messages.SARLJvmModelInferrer_7, "builtinCapacityProvider", //$NON-NLS-1$
						"parentID", "agentID")); //$NON-NLS-1$ //$NON-NLS-2$
				JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName("builtinCapacityProvider"); //$NON-NLS-1$
				jvmParam.setParameterType(this._typeReferenceBuilder.typeRef(BuiltinCapacitiesProvider.class));
				this.associator.associate(source, jvmParam);
				constructor.getParameters().add(jvmParam);
				jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName("parentID"); //$NON-NLS-1$
				jvmParam.setParameterType(this._typeReferenceBuilder.typeRef(UUID.class));
				this.associator.associate(source, jvmParam);
				constructor.getParameters().add(jvmParam);
				jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName("agentID"); //$NON-NLS-1$
				jvmParam.setParameterType(this._typeReferenceBuilder.typeRef(UUID.class));
				this.associator.associate(source, jvmParam);
				constructor.getParameters().add(jvmParam);
				setBody(constructor,
						toStringConcatenation("super(builtinCapacityProvider, parentID, agentID);")); //$NON-NLS-1$
				addAnnotationSafe(constructor, javax.inject.Inject.class);
				appendGeneratedAnnotation(constructor, context);
			}

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			closeContext(context);
		}
	}

	/** Initialize the SARL behavior type.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 */
	protected void initialize(SarlBehavior source, JvmGenericType inferredJvmType) {
		// Issue #356: do not generate if the behavior has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final GenerationContext context = openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlConstructor.class, SarlAction.class,
				SarlBehaviorUnit.class, SarlCapacityUses.class, SarlRequiredCapacity.class));
		try {
			// Copy the documentation
			this.typeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			inferredJvmType.setVisibility(source.getVisibility());
			inferredJvmType.setStatic(false);
			final boolean isAbstract = source.isAbstract() || Utils.hasAbstractMember(source);
			inferredJvmType.setAbstract(isAbstract);
			inferredJvmType.setStrictFloatingPoint(false);
			inferredJvmType.setFinal(!isAbstract && source.isFinal());

			// Generate the annotations.
			translateAnnotationsTo(source.getAnnotations(), inferredJvmType);

			// Generate the extended types.
			appendConstrainedExtends(context, inferredJvmType, Behavior.class, SarlBehavior.class, source.getExtends());

			// Issue #363: do not generate the behavior if the SARL library is incompatible.
			if (Utils.isCompatibleSARLLibraryOnClasspath(this.typeReferences, source)) {
				// Generate the members of the generated type.
				appendAOPMembers(
						inferredJvmType,
						source,
						context);
			}

			// Add the default constructors for the behavior, if not already added

			if (!context.hasConstructor()) {
				// new(owner: Agent)
				final JvmConstructor constructor = this.typesFactory.createJvmConstructor();
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
				constructor.setSimpleName(source.getName());
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				this.typeBuilder.setDocumentation(constructor, MessageFormat.format(
						Messages.SARLJvmModelInferrer_5, "owner")); //$NON-NLS-1$
				final JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName("owner"); //$NON-NLS-1$
				jvmParam.setParameterType(this._typeReferenceBuilder.typeRef(Agent.class));
				this.associator.associate(source, jvmParam);
				constructor.getParameters().add(jvmParam);
				setBody(constructor,
						toStringConcatenation("super(owner);")); //$NON-NLS-1$
				appendGeneratedAnnotation(constructor, context);
			}

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			closeContext(context);
		}
	}

	/** Initialize the SARL event type.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 */
	protected void initialize(SarlEvent source, JvmGenericType inferredJvmType) {
		// Issue #356: do not generate if the event has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final GenerationContext context = openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlConstructor.class));
		try {
			// Copy the documentation
			this.typeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			inferredJvmType.setVisibility(source.getVisibility());
			inferredJvmType.setStatic(false);
			inferredJvmType.setAbstract(false);
			inferredJvmType.setStrictFloatingPoint(false);
			if (!inferredJvmType.isAbstract()) {
				inferredJvmType.setFinal(source.isFinal());
			}

			// Generate the annotations.
			translateAnnotationsTo(source.getAnnotations(), inferredJvmType);

			// Generate the extended types.
			appendConstrainedExtends(context, inferredJvmType, Event.class, SarlEvent.class, source.getExtends());

			// Issue #363: do not generate the event if the SARL library is incompatible.
			if (Utils.isCompatibleSARLLibraryOnClasspath(this.typeReferences, source)) {
				// Generate the members of the generated type.
				appendAOPMembers(
						inferredJvmType,
						source,
						context);
			}

			if (!context.hasConstructor()) {
				// Add the default constructors for the behavior, if not already added

				// new()
				JvmConstructor constructor = this.typesFactory.createJvmConstructor();
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
				constructor.setSimpleName(source.getName());
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				this.typeBuilder.setDocumentation(constructor, Messages.SARLJvmModelInferrer_0);
				setBody(constructor, toStringConcatenation("super();")); //$NON-NLS-1$
				appendGeneratedAnnotation(constructor, context);

				// new(source: Address)
				constructor = this.typesFactory.createJvmConstructor();
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
				constructor.setSimpleName(source.getName());
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				this.typeBuilder.setDocumentation(constructor,
						MessageFormat.format(Messages.SARLJvmModelInferrer_1, "source")); //$NON-NLS-1$
				final JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName("source"); //$NON-NLS-1$
				jvmParam.setParameterType(this._typeReferenceBuilder.typeRef(Address.class));
				this.associator.associate(source, jvmParam);
				constructor.getParameters().add(jvmParam);
				setBody(constructor,
						toStringConcatenation("super(source);")); //$NON-NLS-1$
				appendGeneratedAnnotation(constructor, context);
			}

			// Add functions dedicated to comparisons (equals, hashCode, etc.)
			appendComparisonFunctions(context, source, inferredJvmType);

			// Add functions dedicated to String representation(toString, etc.)
			appendToStringFunctions(context, source, inferredJvmType);

			// Add the serial number
			appendSerialNumber(context, source, inferredJvmType);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			closeContext(context);
		}
	}

	/** Initialize the SARL skill type.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 */
	protected void initialize(SarlSkill source, JvmGenericType inferredJvmType) {
		// Issue #356: do not generate if the skill has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final GenerationContext context = openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlConstructor.class, SarlAction.class, SarlBehaviorUnit.class,
				SarlCapacityUses.class, SarlRequiredCapacity.class));
		try {
			// Copy the documentation
			this.typeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			inferredJvmType.setVisibility(source.getVisibility());
			inferredJvmType.setStatic(false);
			final boolean isAbstract = source.isAbstract() || Utils.hasAbstractMember(source);
			inferredJvmType.setAbstract(isAbstract);
			inferredJvmType.setStrictFloatingPoint(false);
			inferredJvmType.setFinal(!isAbstract && source.isFinal());

			// Generate the annotations.
			translateAnnotationsTo(source.getAnnotations(), inferredJvmType);

			// Generate the extended types.
			appendConstrainedExtends(context, inferredJvmType, Skill.class, SarlSkill.class, source.getExtends());
			appendConstrainedImplements(context, inferredJvmType, Capacity.class, SarlCapacity.class, source.getImplements());

			// Issue #363: do not generate the skill if the SARL library is incompatible.
			if (Utils.isCompatibleSARLLibraryOnClasspath(this.typeReferences, source)) {
				// Generate the members of the generated type.
				appendAOPMembers(
						inferredJvmType,
						source,
						context);
			}

			// Add the default constructors for the behavior, if not already added
			if (!context.hasConstructor()) {
				// new()
				JvmConstructor constructor = this.typesFactory.createJvmConstructor();
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
				constructor.setSimpleName(source.getName());
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				this.typeBuilder.setDocumentation(constructor, Messages.SARLJvmModelInferrer_4);
				setBody(constructor, toStringConcatenation("super();")); //$NON-NLS-1$
				appendGeneratedAnnotation(constructor, context);

				// new(owner: Agent)
				constructor = this.typesFactory.createJvmConstructor();
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
				constructor.setSimpleName(source.getName());
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				this.typeBuilder.setDocumentation(constructor,
						MessageFormat.format(Messages.SARLJvmModelInferrer_3, "owner")); //$NON-NLS-1$
				final JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName("owner"); //$NON-NLS-1$
				jvmParam.setParameterType(this._typeReferenceBuilder.typeRef(Agent.class));
				this.associator.associate(source, jvmParam);
				constructor.getParameters().add(jvmParam);
				setBody(constructor,
						toStringConcatenation("super(owner);")); //$NON-NLS-1$
				appendGeneratedAnnotation(constructor, context);
			}

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			closeContext(context);
		}
	}

	/** Initialize the SARL capacity type.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 */
	protected void initialize(SarlCapacity source, JvmGenericType inferredJvmType) {
		// Issue #356: do not generate if the capacity has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final GenerationContext context = openContext(source, inferredJvmType,
				Collections.singleton(SarlAction.class));
		try {
			// Copy the documentation
			this.typeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			inferredJvmType.setInterface(true);
			inferredJvmType.setAbstract(true);
			inferredJvmType.setVisibility(source.getVisibility());
			inferredJvmType.setStatic(false);
			inferredJvmType.setStrictFloatingPoint(false);
			inferredJvmType.setFinal(false);

			// Generate the annotations.
			translateAnnotationsTo(source.getAnnotations(), inferredJvmType);

			// Generate the extended types.
			appendConstrainedExtends(context, inferredJvmType, Capacity.class, SarlCapacity.class, source.getExtends());

			// Issue #363: do not generate the capacity if the SARL library is incompatible.
			if (Utils.isCompatibleSARLLibraryOnClasspath(this.typeReferences, source)) {
				// Generate the members of the generated type.
				appendAOPMembers(
						inferredJvmType,
						source,
						context);
			}

			// Add the @FunctionalInterface
			appendFunctionalInterfaceAnnotation(inferredJvmType);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			closeContext(context);
		}
	}

	/** Initialize the SARL space type.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 */
	@SuppressWarnings("static-method")
	protected void initialize(SarlSpace source, JvmGenericType inferredJvmType) {
		// Issue #356: do not generate if the space has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// TODO: Generate the space
	}

	@Override
	protected void transform(XtendMember sourceMember,
			JvmGenericType container, boolean allowDispatch) {
		if (sourceMember instanceof SarlBehaviorUnit) {
			transform((SarlBehaviorUnit) sourceMember, container);
		} else if (sourceMember instanceof SarlCapacityUses) {
			transform((SarlCapacityUses) sourceMember, container);
		} else if (sourceMember instanceof SarlRequiredCapacity) {
			transform((SarlRequiredCapacity) sourceMember, container);
		} else {
			super.transform(sourceMember, container, allowDispatch);
		}
	}

	/** Transform the constructor.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 */
	@Override
	protected void transform(final XtendConstructor source, final JvmGenericType container) {
		final GenerationContext context = getContext(container);
		final boolean isVarArgs = Utils.isVarArg(source.getParameters());

		// Generate the unique identifier of the constructor.
		final QualifiedActionName actionKey = this.sarlSignatureProvider.createConstructorQualifiedName(container);

		// Generate all the constructor signatures related to the constructor to create.
		final InferredPrototype constructorSignatures = this.sarlSignatureProvider.createPrototypeFromSarlModel(
				actionKey,
				Utils.isVarArg(source.getParameters()), source.getParameters());

		// Generate the main Java constructor.
		final JvmConstructor constructor = this.typesFactory.createJvmConstructor();
		container.getMembers().add(constructor);
		this.associator.associatePrimary(source, constructor);
		this.typeBuilder.copyDocumentationTo(source, constructor);
		final JvmVisibility visibility = source.getVisibility();
		constructor.setSimpleName(container.getSimpleName());
		constructor.setVisibility(visibility);
		constructor.setVarArgs(isVarArgs);

		// Generate the parameters
		final List<InferredStandardParameter> paramList = constructorSignatures.getOriginalParameterTypes();
		translateSarlFormalParameters(
				context,
				constructor, container, isVarArgs,
				source.getParameters(), false, paramList);

		// Generate additional information (type parameters, exceptions...)
		copyAndFixTypeParameters(source.getTypeParameters(), constructor);
		for (final JvmTypeReference exception : source.getExceptions()) {
			constructor.getExceptions().add(this.typeBuilder.cloneWithProxies(exception));
		}
		translateAnnotationsTo(source.getAnnotations(), constructor);

		// Set the body.
		setBody(constructor, source.getExpression());

		// The signature definition of the constructor.
		final ActionParameterTypes sigKey = this.sarlSignatureProvider.createParameterTypesFromJvmModel(
				isVarArgs, constructor.getParameters());

		// Update the list of generated constructors
		if (context != null) {
			context.getGeneratedConstructors().put(sigKey, constructor);
		}

		Runnable differedGeneration  = new Runnable() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void run() {
				// Generate the Java functions that correspond to the action with the parameter default values applied.
				for (final Entry<ActionParameterTypes, List<InferredStandardParameter>> entry
						: constructorSignatures.getInferredParameterTypes().entrySet()) {

					if (context == null || !context.getGeneratedConstructors().containsKey(entry.getKey())) {
						final List<InferredStandardParameter> otherSignature = entry.getValue();
						// Generate the additional constructor that is invoke the main constructor previously generated.
						final JvmConstructor constructor2 = SARLJvmModelInferrer.this.typesFactory.createJvmConstructor();
						container.getMembers().add(constructor2);
						SARLJvmModelInferrer.this.typeBuilder.copyDocumentationTo(source, constructor2);
						final JvmVisibility visibility = source.getVisibility();
						constructor2.setSimpleName(container.getSimpleName());
						constructor2.setVisibility(visibility);
						constructor2.setVarArgs(isVarArgs);

						final List<String> args = translateSarlFormalParametersForSyntheticOperation(
								constructor2, container, isVarArgs, otherSignature);

						addAnnotationSafe(constructor2, DefaultValueUse.class,
								constructorSignatures.getFormalParameterTypes().toString());
						appendGeneratedAnnotation(constructor2, context);

						setBody(constructor2, toStringConcatenation(
								"this(" //$NON-NLS-1$
								+ IterableExtensions.join(args, ", ") //$NON-NLS-1$
								+ ");")); //$NON-NLS-1$

						// Update the list of the generated constructors.
						if (context != null) {
							context.getGeneratedConstructors().put(entry.getKey(), constructor2);
						}
					}
				}
			}
		};

		if (context != null) {
			context.getDifferedGenerationElements().add(differedGeneration);
			context.setActionIndex(context.getActionIndex() + 1);
			context.incrementSerial(sigKey.hashCode());
		} else {
			differedGeneration.run();
		}
	}

	/** Transform the field.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 */
	@Override
	protected void transform(XtendField source, JvmGenericType container) {
		super.transform(source, container);
		final GenerationContext context = getContext(container);
		if (context != null) {
			final String name = source.getName();
			if (name != null) {
				context.incrementSerial(name.hashCode());
			}
			final JvmTypeReference type = source.getType();
			if (type != null) {
				context.incrementSerial(type.getIdentifier().hashCode());
			}
		}
	}

	/** Transform the function.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 */
	@Override
	@SuppressWarnings({"checkstyle:methodlength", "checkstyle:cyclomaticcomplexity",
			"checkstyle:npathcomplexity"})
	protected void transform(final XtendFunction source, final JvmGenericType container, boolean allowDispatch) {
		final GenerationContext context = getContext(container);

		// Compute the operation name
		// Issue #355: null or empty name is possible.
		final String originalFunctionName = source.getName();
		if (!Strings.isNullOrEmpty(originalFunctionName)) {
			final StringBuilder sourceNameBuffer = new StringBuilder(originalFunctionName);
			if (allowDispatch && source.isDispatch()) {
				sourceNameBuffer.insert(0, "_"); //$NON-NLS-1$
			}
			final String sourceName = sourceNameBuffer.toString();

			JvmVisibility visibility = source.getVisibility();
			if (visibility == null) {
				visibility = JvmVisibility.DEFAULT;
			}

			// Create the main function
			final JvmOperation operation = this.typesFactory.createJvmOperation();
			container.getMembers().add(operation);
			final boolean isAbstractOperation = source.isAbstract() || container.isInterface();
			operation.setAbstract(isAbstractOperation);
			operation.setNative(source.isNative());
			operation.setSynchronized(source.isSynchonized());
			operation.setStrictFloatingPoint(source.isStrictFloatingPoint());
			if (!operation.isAbstract()) {
				operation.setFinal(source.isFinal());
			}
			this.associator.associatePrimary(source, operation);
			operation.setSimpleName(sourceName);
			operation.setVisibility(visibility);
			operation.setStatic(source.isStatic());
			if (!operation.isAbstract() && !operation.isStatic() && container.isInterface()) {
				operation.setDefault(true);
			}
			this.typeBuilder.copyDocumentationTo(source, operation);

			// Type parameters
			copyAndFixTypeParameters(source.getTypeParameters(), operation);

			// Compute the identifier of the action.
			final QualifiedActionName actionKey = this.sarlSignatureProvider.createQualifiedActionName(
					container, sourceName);

			// Compute the different action prototypes associated to the action to create.
			final boolean isVarArgs = Utils.isVarArg(source.getParameters());
			final InferredPrototype actionSignatures = this.sarlSignatureProvider.createPrototypeFromSarlModel(
					actionKey,
					isVarArgs, source.getParameters());

			// Compute the action prototype of the action without optional parameter
			final ActionPrototype actSigKey = this.sarlSignatureProvider.createActionPrototype(
					sourceName,
					actionSignatures.getFormalParameterTypes());

			// Generate the parameters
			final List<InferredStandardParameter> paramList = actionSignatures.getOriginalParameterTypes();
			translateSarlFormalParameters(
					context,
					operation, container, isVarArgs,
					source.getParameters(),
					container.isInterface(), paramList);

			// Infer the return type
			final XExpression expression = source.getExpression();

			JvmTypeReference returnType = null;
			if (source.getReturnType() != null) {
				returnType = source.getReturnType();
			} else if (context != null) {
				JvmOperation inheritedOperation = context.getInheritedFinalOperations().get(actSigKey);
				if (inheritedOperation == null) {
					inheritedOperation = context.getInheritedOverridableOperations().get(actSigKey);
				}
				if (inheritedOperation == null) {
					inheritedOperation = context.getInheritedOperationsToImplement().get(actSigKey);
				}
				if (inheritedOperation != null) {
					returnType = inheritedOperation.getReturnType();
				}
				if (returnType == null
						&& expression != null
						&& ((!(expression instanceof XBlockExpression))
								|| (!((XBlockExpression) expression).getExpressions().isEmpty()))) {
					returnType = this.typeBuilder.inferredType(expression);
				}
			} else if (expression != null
					&& ((!(expression instanceof XBlockExpression))
							|| (!((XBlockExpression) expression).getExpressions().isEmpty()))) {
				returnType = this.typeBuilder.inferredType(expression);
			}
			final JvmTypeReference selectedReturnType;
			if (returnType == null) {
				selectedReturnType = this._typeReferenceBuilder.typeRef(Void.TYPE);
			} else if (InferredTypeIndicator.isInferred(returnType)) {
				selectedReturnType = returnType;
			} else {
				selectedReturnType = this.typeBuilder.cloneWithProxies(returnType);
			}
			operation.setReturnType(selectedReturnType);

			// Exceptions
			for (final JvmTypeReference exception : source.getExceptions()) {
				operation.getExceptions().add(this.typeBuilder.cloneWithProxies(exception));
			}

			// Add the body
			if (!operation.isAbstract() && !container.isInterface() && context != null) {
				setBody(operation, expression);
			}

			// User Annotations
			translateAnnotationsTo(source.getAnnotations(), operation);

			// Add @Inline annotation
			if (context != null
					&& context.isAtLeastJava8()
					&& context.getGeneratorConfig2().isGenerateInlineAnnotation()
					&& !source.isAbstract() && !container.isInterface()
					&& this.annotationFinder.findAnnotation(operation, Inline.class) == null) {
				SARLJvmModelInferrer.this.inlineExpressionCompiler.appendInlineAnnotation(operation, source);
			}

			// Standard annotations
			if (source.isOverride()
					&& this.annotationFinder.findAnnotation(operation, Override.class) == null
					&& this.typeReferences.findDeclaredType(Override.class, source) != null) {
				addAnnotationSafe(operation, Override.class);
			}
			if (this.expressionHelper.isPurableOperation(operation, expression)
					&& this.annotationFinder.findAnnotation(operation, Pure.class) == null
					&& this.typeReferences.findDeclaredType(Pure.class, source) != null) {
				// The function is pure
				addAnnotationSafe(operation, Pure.class);
			}

			final List<JvmTypeReference> firedEvents;
			if (source instanceof SarlAction) {
				firedEvents = ((SarlAction) source).getFiredEvents();
			} else {
				firedEvents = Collections.emptyList();
			}

			// Detecting if the action is an early-exit action.
			// If true, the Java code is annotated to be usable by the SARL validator.
			//TODO: Generalize the detection of the EarlyExit
			boolean isEarlyExitTmp = false;
			final Iterator<JvmTypeReference> eventIterator = firedEvents.iterator();
			while (!isEarlyExitTmp && eventIterator.hasNext()) {
				if (this.earlyExitComputer.isEarlyExitEvent(eventIterator.next())) {
					addAnnotationSafe(operation, EarlyExit.class);
					isEarlyExitTmp = true;
				}
			}
			final boolean isEarlyExit = isEarlyExitTmp;

			// Put the fired SARL events as Java annotations for beeing usable by the SARL validator.
			if (!firedEvents.isEmpty()) {
				operation.getAnnotations().add(annotationClassRef(FiredEvent.class, firedEvents));
			}

			// 1. Ensure that the Java annotations related to the default value are really present.
			//    They may be not present if the generated action is a specific version of an inherited
			//    action with default values for parameters.
			// 2. Update the two collections that describes the implemented and implementable operations.
			if (context != null) {
				final JvmOperation implementedOperation = context.getInheritedOperationsToImplement().remove(actSigKey);
				// Put the annotations that were defined in the implemented operation
				if (implementedOperation != null) {
					if (this.annotationFinder.findAnnotation(implementedOperation, DefaultValueSource.class) != null
							&& this.annotationFinder.findAnnotation(operation, DefaultValueSource.class) == null) {
						addAnnotationSafe(operation, DefaultValueSource.class);
					}
					// Reinject the @DefaultValue annotations
					final List<JvmFormalParameter> oparams = implementedOperation.getParameters();
					final List<JvmFormalParameter> cparams = operation.getParameters();
					assert oparams.size() == cparams.size();
					for (int i = 0; i < oparams.size(); ++i) {
						final JvmFormalParameter op = oparams.get(i);
						final JvmFormalParameter cp = cparams.get(i);
						final String ovalue = this.annotationUtils.findStringValue(op, DefaultValue.class);
						if (ovalue != null
								&& this.annotationFinder.findAnnotation(cp, DefaultValue.class) == null) {
							addAnnotationSafe(cp,
									DefaultValue.class,
									this.sarlSignatureProvider.qualifyDefaultValueID(
											implementedOperation.getDeclaringType().getIdentifier(),
											ovalue));
						}
					}
				}
				// Add the main operation into the list of overridable operations
				context.getInheritedOverridableOperations().put(actSigKey, operation);
			}

			@SuppressWarnings("checkstyle:anoninnerlength")
			Runnable differedGeneration = new Runnable() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void run() {
					// Generate the Java functions that correspond to the action with the parameter default values applied.
					for (final Entry<ActionParameterTypes, List<InferredStandardParameter>> otherSignature
							: actionSignatures.getInferredParameterTypes().entrySet()) {
						final ActionPrototype ak = SARLJvmModelInferrer.this.sarlSignatureProvider.createActionPrototype(
								sourceName,
								otherSignature.getKey());
						if (ak != null
								&& (context == null
								|| (!context.getInheritedFinalOperations().containsKey(ak)
										&& !context.getInheritedOverridableOperations().containsKey(ak)))) {

							// Generate the additional constructor that is invoke the main constructor previously generated.
							final JvmOperation operation2 = SARLJvmModelInferrer.this.typesFactory.createJvmOperation();
							container.getMembers().add(operation2);
							operation2.setSimpleName(operation.getSimpleName());
							operation2.setVisibility(operation.getVisibility());
							operation2.setVarArgs(operation.isVarArgs());
							operation2.setAbstract(operation.isAbstract());
							operation2.setDeprecated(operation.isDeprecated());
							operation2.setReturnType(
									SARLJvmModelInferrer.this.typeBuilder.cloneWithProxies(selectedReturnType));
							operation2.setFinal(!container.isInterface());
							operation2.setNative(false);
							operation2.setStrictFloatingPoint(false);
							operation2.setSynchronized(false);

							for (final JvmTypeReference exception : operation.getExceptions()) {
								operation2.getExceptions().add(SARLJvmModelInferrer.this.typeBuilder
										.cloneWithProxies(exception));
							}

							translateAnnotationsTo(source.getAnnotations(), operation2);
							if (source.isOverride()
									&& SARLJvmModelInferrer.this.annotationFinder.findAnnotation(operation,
									Override.class) == null
									&& SARLJvmModelInferrer.this.typeReferences.findDeclaredType(
											Override.class, source) != null) {
								addAnnotationSafe(operation, Override.class);
							}

							final List<String> args = translateSarlFormalParametersForSyntheticOperation(
									operation2, container, isVarArgs, otherSignature.getValue());

							if (!operation2.isAbstract()) {
								setBody(operation2, (it) -> {
									final JvmTypeReference type = operation2.getReturnType();
									if (!SARLJvmModelInferrer.this.typeReferences.is(type, void.class)) {
										it.append("return "); //$NON-NLS-1$
									}
									it.append(sourceName);
									it.append("("); //$NON-NLS-1$
									it.append(IterableExtensions.join(args, ", ")); //$NON-NLS-1$
									it.append(");"); //$NON-NLS-1$
								});
							}

							addAnnotationSafe(operation2, DefaultValueUse.class,
									actionSignatures.getFormalParameterTypes().toString());
							appendGeneratedAnnotation(operation2, context);

							// If the main action is an early-exit action, the additional operation
							// is also an early-exit operation.
							//TODO: Generalize the detection of the EarlyExit
							if (isEarlyExit) {
								addAnnotationSafe(operation2, EarlyExit.class);
							}

							// Put the fired SARL events as Java annotations for beeing usable by the SARL validator.
							if (!firedEvents.isEmpty()) {
								operation2.getAnnotations().add(annotationClassRef(FiredEvent.class, firedEvents));
							}

							// Copy the other annotations from the original operations
							for (final JvmAnnotationReference annotation : operation.getAnnotations()) {
								final String id = annotation.getAnnotation().getIdentifier();
								if ((!DefaultValueSource.class.getName().equals(id)
										&& (!EarlyExit.class.getName().equals(id)))
										&& (!FiredEvent.class.getName().equals(id))
										&& (!Inline.class.getName().equals(id))
										&& (!Generated.class.getName().equals(id))) {
									try {
										final JvmAnnotationReference clone = SARLJvmModelInferrer.this._annotationTypesBuilder
												.annotationRef(id);
										for (final JvmAnnotationValue annotationValue : annotation.getExplicitValues()) {
											clone.getExplicitValues().add(EcoreUtil.copy(annotationValue));
										}
										operation2.getAnnotations().add(clone);
									} catch (IllegalArgumentException exception) {
										// ignore
									}
								}
							}

							// Copy and clean the documentation
							copyAndCleanDocumentationTo(operation, operation2);

							// Update the two collections that describes the implemented and implementable operations.
							if (context != null) {
								context.getInheritedOperationsToImplement().remove(ak);
								context.getInheritedOverridableOperations().put(ak, operation2);
							}
						}
					}
				}
			};

			if (context != null) {
				context.getDifferedGenerationElements().add(differedGeneration);
				context.setActionIndex(context.getActionIndex() + 1);
				context.incrementSerial(actSigKey.hashCode());
			} else {
				differedGeneration.run();
			}
		}
	}

	/** Transform the given behavior unit.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 */
	protected void transform(final SarlBehaviorUnit source, JvmGenericType container) {
		final GenerationContext context = getContext(container);
		if (source.getName() != null && source.getName().getSimpleName() != null && context != null) {
			final XExpression guard = source.getGuard();

			final boolean isTrueGuard;

			// Check the guard value
			if (guard == null) {
				isTrueGuard = true;
			} else if (guard instanceof XBooleanLiteral) {
				final XBooleanLiteral literal = (XBooleanLiteral) guard;
				if (literal.isIsTrue()) {
					isTrueGuard = true;
				} else {
					// The guard is always false => no need to generate the code
					return;
				}
			} else {
				isTrueGuard = false;
			}

			final JvmTypeReference voidType = this._typeReferenceBuilder.typeRef(Void.TYPE);

			//----------------
			// Body function
			//----------------
			// Name
			final String bodyMethodName = Utils.createNameForHiddenEventHandlerMethod(source.getName().getSimpleName(),
					context.getBehaviorUnitIndex());
			// Operation
			final JvmOperation bodyOperation = this.typesFactory.createJvmOperation();
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
			JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
			jvmParam.setName(this.grammarKeywordAccess.getOccurrenceKeyword());
			jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(source.getName()));
			this.associator.associate(source, jvmParam);
			bodyOperation.getParameters().add(jvmParam);
			// Body
			setBody(bodyOperation, source.getExpression());
			// Annotations
			translateAnnotationsTo(source.getAnnotations(), bodyOperation);
			appendGeneratedAnnotation(bodyOperation, context);
			if (!this.services.getExpressionHelper().hasSideEffects(source.getExpression())) {
				addAnnotationSafe(bodyOperation, Pure.class);
			}
			// Synthetic flag
			this.typeExtensions.setSynthetic(bodyOperation, true);

			final Collection<Procedure1<ITreeAppendable>> evaluators = context.getGuardEvalationCodeFor(source);
			assert evaluators != null;

			if (isTrueGuard) {
				evaluators.add((it) -> {
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
				final String guardMethodName = Utils.createNameForHiddenGuardEvaluatorMethod(source.getName().getSimpleName(),
						context.getBehaviorUnitIndex());
				// Operation
				final JvmOperation guardOperation = this.typesFactory.createJvmOperation();
				guardOperation.setAbstract(false);
				guardOperation.setNative(false);
				guardOperation.setSynchronized(false);
				guardOperation.setStrictFloatingPoint(false);
				guardOperation.setFinal(false);
				guardOperation.setVisibility(JvmVisibility.PRIVATE);
				guardOperation.setStatic(false);
				guardOperation.setSimpleName(guardMethodName);
				guardOperation.setReturnType(this._typeReferenceBuilder.typeRef(Boolean.TYPE));
				// Add to container
				container.getMembers().add(guardOperation);
				this.associator.associate(source, guardOperation);
				this.associator.associatePrimary(guard, guardOperation);
				// First parameter: it
				jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName(this.grammarKeywordAccess.getItKeyword());
				jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(source.getName()));
				this.associator.associate(source, jvmParam);
				guardOperation.getParameters().add(jvmParam);
				// Second parameter: occurrence
				jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName(this.grammarKeywordAccess.getOccurrenceKeyword());
				jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(source.getName()));
				this.associator.associate(source, jvmParam);
				guardOperation.getParameters().add(jvmParam);
				// Body
				setBody(guardOperation, guard);
				// Annotations
				appendGeneratedAnnotation(guardOperation, context);
				addAnnotationSafe(guardOperation, Pure.class);
				// Synthetic flag
				this.typeExtensions.setSynthetic(guardOperation, true);

				//------------------
				// Handler function
				//------------------
				evaluators.add((it) -> {
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
			this.log.fine(Messages.SARLJvmModelInferrer_10);
		}
	}

	/** Transform the uses of SARL capacities.
	 *
	 * <p>Resolving the calls to the capacities' functions is done in {@link SARLReentrantTypeResolver}.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 */
	protected void transform(SarlCapacityUses source, JvmGenericType container) {
		final GenerationContext context = getContext(container);
		if (context == null) {
			return;
		}
		for (final JvmTypeReference capacityType : source.getCapacities()) {
			final JvmType type = capacityType.getType();
			if (type instanceof JvmGenericType
					/*&& this.inheritanceHelper.isSubTypeOf(capacityType, Capacity.class, SarlCapacity.class)*/
					&& !context.getGeneratedCapacityUseFields().contains(capacityType.getIdentifier())) {
				// Generate the buffer field
				final String fieldName = Utils.createNameForHiddenCapacityImplementationAttribute(capacityType.getIdentifier());
				final JvmField field = this.typesFactory.createJvmField();
				field.setVisibility(JvmVisibility.PRIVATE);
				field.setSimpleName(fieldName);
				field.setTransient(true);
				final JvmType clearableReferenceType = this.typeReferences.findDeclaredType(ClearableReference.class, container);
				final JvmTypeReference skillClearableReference = this.typeReferences.createTypeRef(
						clearableReferenceType,
						this.typeReferences.createTypeRef(this.typeReferences.findDeclaredType(Skill.class, container)));
				field.setType(skillClearableReference);

				this.associator.associatePrimary(source, field);

				addAnnotationSafe(field, Extension.class);
				field.getAnnotations().add(annotationClassRef(ImportedCapacityFeature.class,
						Collections.singletonList(capacityType)));
				appendGeneratedAnnotation(field, getContext(container));

				container.getMembers().add(field);

				// Generate the calling function
				final String methodName = Utils.createNameForHiddenCapacityImplementationCallingMethodFromFieldName(
						fieldName);
				final JvmOperation operation = this.typesFactory.createJvmOperation();
				operation.setVisibility(JvmVisibility.PRIVATE);
				operation.setReturnType(cloneWithTypeParametersAndProxies(capacityType, operation));
				operation.setSimpleName(methodName);

				this.associator.associatePrimary(source, operation);

				setBody(operation, (it) -> {
					it.append("if (this.").append(fieldName).append(" == null || this."); //$NON-NLS-1$ //$NON-NLS-2$
					it.append(fieldName).append(".get() == null) {"); //$NON-NLS-1$
					it.increaseIndentation();
					it.newLine();
					it.append("this.").append(fieldName).append(" = ") //$NON-NLS-1$ //$NON-NLS-2$
							.append(Utils.HIDDEN_MEMBER_CHARACTER).append("getSkill("); //$NON-NLS-1$
					it.append(capacityType.getType()).append(".class);"); //$NON-NLS-1$
					it.decreaseIndentation();
					it.newLine();
					it.append("}"); //$NON-NLS-1$
					it.newLine();
					it.append("return ").append(Utils.HIDDEN_MEMBER_CHARACTER) //$NON-NLS-1$
							.append("castSkill(").append(capacityType.getType()).append(".class, this.") //$NON-NLS-1$ //$NON-NLS-2$
							.append(fieldName).append(");"); //$NON-NLS-1$
				});

				// Add the annotation dedicated to this particular method
				if (context.isAtLeastJava8()) {
					final String inlineExpression = Utils.HIDDEN_MEMBER_CHARACTER
							+ "castSkill(" + capacityType.getSimpleName() //$NON-NLS-1$
							+ ".class, (this." + fieldName //$NON-NLS-1$
							+ " == null || this." + fieldName //$NON-NLS-1$
							+ ".get() == null) ? (this." + fieldName //$NON-NLS-1$
							+ " = " + Utils.HIDDEN_MEMBER_CHARACTER + "getSkill(" //$NON-NLS-1$ //$NON-NLS-2$
							+ capacityType.getSimpleName()
							+ ".class)) : this." + fieldName + ")"; //$NON-NLS-1$ //$NON-NLS-2$;
					this.inlineExpressionCompiler.appendInlineAnnotation(
							operation, source.eResource().getResourceSet(), inlineExpression, capacityType);
				}
				appendGeneratedAnnotation(operation, context);
				addAnnotationSafe(operation, Pure.class);

				container.getMembers().add(operation);

				context.addGeneratedCapacityUseField(capacityType.getIdentifier());
				context.incrementSerial(capacityType.getIdentifier().hashCode());
			}
		}
	}

	/** Transform the requirements of SARL capacities.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 */
	protected void transform(SarlRequiredCapacity source, JvmGenericType container) {
		//
	}

	/** Generate the code for the given SARL members in a agent-oriented container.
	 *
	 * @param featureContainerType - the feature container.
	 * @param container - the SARL container.
	 * @param context - description of the generation context in which the members must be considered.
	 */
	protected void appendAOPMembers(
			JvmGenericType featureContainerType,
			XtendTypeDeclaration container,
			GenerationContext context) {

		Utils.populateInheritanceContext(
				featureContainerType,
				context.getInheritedFinalOperations(),
				context.getInheritedOverridableOperations(),
				null,
				context.getInheritedOperationsToImplement(),
				null,
				this.sarlSignatureProvider);

		for (final XtendMember feature : container.getMembers()) {
			if (context.isSupportedMember(feature)
					&& (!(feature instanceof SarlCapacityUses))
					&& (!(feature instanceof SarlRequiredCapacity))) {
				transform(feature, featureContainerType, true);
			}
		}

		for (final XtendMember feature : container.getMembers()) {
			if (context.isSupportedMember(feature)
					&& ((feature instanceof SarlCapacityUses)
							|| (feature instanceof SarlRequiredCapacity))) {
				transform(feature, featureContainerType, false);
			}
		}

		// Add event handlers
		appendEventGuardEvaluators(featureContainerType);

		// Add dispatch methods
		appendSyntheticDispatchMethods(container, featureContainerType);

		// Add SARL synthetic functions
		appendSyntheticDefaultValuedParameterMethods(
				container,
				featureContainerType,
				context);
	}

	@Override
	protected JvmOperation deriveGenericDispatchOperationSignature(
			Iterable<JvmOperation> localOperations, JvmGenericType target) {
		final JvmOperation dispatcher = super.deriveGenericDispatchOperationSignature(localOperations, target);
		//
		// Fixing the behavior for determining the visibility of the dispatcher since
		// it does not fit the SARL requirements.
		// FIXME: Move to Xtend?
		//
		JvmVisibility higherVisibility = JvmVisibility.PRIVATE;
		for (final JvmOperation jvmOperation : localOperations) {
			final Iterable<XtendFunction> xtendFunctions = Iterables.filter(
					this.sarlAssociations.getSourceElements(jvmOperation), XtendFunction.class);
			for (final XtendFunction func : xtendFunctions) {
				final JvmVisibility visibility = func.getVisibility();
				assert visibility != null;
				if (this.visibilityComparator.compare(visibility, higherVisibility) > 0) {
					higherVisibility = visibility;
				}
			}
		}
		dispatcher.setVisibility(higherVisibility);

		return dispatcher;
	}

	/** Generate the missed operations that are the results from the generation of actions with default value parameters.
	 *
	 * @param source - the SARL container.
	 * @param target - the JVM feature container.
	 * @param context - description of the generation context in which the members must be considered.
	 */
	protected void appendSyntheticDefaultValuedParameterMethods(
			XtendTypeDeclaration source,
			JvmDeclaredType target,
			GenerationContext context) {

		// Generate the different operations.
		final Iterator<Runnable> differedGeneration = context.getDifferedGenerationElements().iterator();
		while (differedGeneration.hasNext()) {
			final Runnable runnable = differedGeneration.next();
			differedGeneration.remove();
			runnable.run();
		}

		// Generated the missed functions that are the result of the generation of operations
		// with default values.
		int actIndex = context.getActionIndex();

		for (final Entry<ActionPrototype, JvmOperation> missedOperation : context.getInheritedOperationsToImplement()
				.entrySet()) {

			final String originalSignature = this.annotationUtils.findStringValue(
					missedOperation.getValue(), DefaultValueUse.class);
			if (!Strings.isNullOrEmpty(originalSignature)) {

				// Find the definition of the operation from the inheritance context.
				final JvmOperation redefinedOperation = context.getInheritedOverridableOperations().get(
						this.sarlSignatureProvider.createActionPrototype(
								missedOperation.getKey().getActionName(),
								this.sarlSignatureProvider.createParameterTypesFromString(originalSignature)));
				if (redefinedOperation != null) {
					final ActionParameterTypes parameterTypes = this.sarlSignatureProvider.createParameterTypesFromJvmModel(
							redefinedOperation.isVarArgs(), redefinedOperation.getParameters());
					final QualifiedActionName qualifiedActionName = this.sarlSignatureProvider.createQualifiedActionName(
							missedOperation.getValue().getDeclaringType(),
							redefinedOperation.getSimpleName());

					// Retreive the inferred prototype (including the prototypes with optional arguments)
					InferredPrototype redefinedPrototype = this.sarlSignatureProvider.getPrototypes(
							qualifiedActionName, parameterTypes);
					if (redefinedPrototype == null) {
						// The original operation was not parsed by the SARL compiler in the current run-time context.
						redefinedPrototype = this.sarlSignatureProvider.createPrototypeFromJvmModel(
								qualifiedActionName, redefinedOperation.isVarArgs(),
								redefinedOperation.getParameters());
					}

					// Retreive the specification of the formal parameters that will be used for
					// determining the calling arguments.
					final List<InferredStandardParameter> argumentSpec = redefinedPrototype.getInferredParameterTypes().get(
							missedOperation.getKey().getParametersTypes());

					// Create the missed java operation.
					final JvmOperation op = this.typeBuilder.toMethod(
							source,
							missedOperation.getValue().getSimpleName(),
							missedOperation.getValue().getReturnType(),
							null);
					op.setVarArgs(missedOperation.getValue().isVarArgs());
					op.setFinal(true);

					final List<String> arguments = new ArrayList<>();

					// Create the formal parameters.
					for (final InferredStandardParameter parameter : argumentSpec) {
						if (parameter instanceof InferredValuedParameter) {
							final InferredValuedParameter inferredParameter = (InferredValuedParameter) parameter;
							arguments.add(this.sarlSignatureProvider.toJavaArgument(
									target.getIdentifier(), inferredParameter.getCallingArgument()));
						} else {
							arguments.add(parameter.getName());
							final JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
							jvmParam.setName(parameter.getName());
							jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(parameter.getType()));
							this.associator.associate(parameter.getParameter(), jvmParam);
							op.getParameters().add(jvmParam);
						}
					}

					// Create the body
					setBody(op, (it) -> {
						it.append(redefinedOperation.getSimpleName());
						it.append("("); //$NON-NLS-1$
						it.append(IterableExtensions.join(arguments, ", ")); //$NON-NLS-1$
						it.append(");"); //$NON-NLS-1$
					});

					// Add the annotations.
					addAnnotationSafe(op, DefaultValueUse.class, originalSignature);
					appendGeneratedAnnotation(op, context);

					// Add the operation in the container.
					target.getMembers().add(op);
					++actIndex;
				}
			}
		}
		context.setActionIndex(actIndex);
	}

	/** Create a string concatenation client from a set of Java code lines.
	 *
	 * @param javaCodeLines - the Java code lines.
	 * @return the client.
	 */
	private static StringConcatenationClient toStringConcatenation(final String... javaCodeLines) {
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(StringConcatenationClient.TargetStringConcatenation builder) {
				for (final String line : javaCodeLines) {
					builder.append(line);
					builder.newLineIfNotEmpty();
				}
			}
		};
	}

	/** Generate the extended types for the given SARL statement.
	 *
	 * @param context - the context of the generation.
	 * @param owner - the JVM element to change.
	 * @param defaultJvmType - the default JVM type.
	 * @param defaultSarlType - the default SARL type.
	 * @param supertype - the supertype.
	 */
	protected void appendConstrainedExtends(
			GenerationContext context,
			JvmGenericType owner, Class<?> defaultJvmType,
			Class<? extends XtendTypeDeclaration> defaultSarlType,
			JvmParameterizedTypeReference supertype) {
		final List<? extends JvmParameterizedTypeReference> supertypes;
		if (supertype == null) {
			supertypes = Collections.emptyList();
		} else {
			supertypes = Collections.singletonList(supertype);
		}
		appendConstrainedExtends(context, owner, defaultJvmType, defaultSarlType, supertypes);
	}

	/** Generate the extended types for the given SARL statement.
	 *
	 * @param context - the context of the generation.
	 * @param owner - the JVM element to change.
	 * @param defaultJvmType - the default JVM type.
	 * @param defaultSarlType - the default Sarl type.
	 * @param supertypes - the supertypes.
	 */
	protected void appendConstrainedExtends(
			GenerationContext context,
			JvmGenericType owner, Class<?> defaultJvmType, Class<? extends XtendTypeDeclaration> defaultSarlType,
			List<? extends JvmParameterizedTypeReference> supertypes) {
		boolean explicitType = false;
		for (final JvmParameterizedTypeReference superType : supertypes) {
			if (!Objects.equal(owner.getIdentifier(), superType.getIdentifier())
					&& superType.getType() instanceof JvmGenericType
					/*&& this.inheritanceHelper.isProxyOrSubTypeOf(superType, defaultJvmType, defaultSarlType, isInterface)*/) {
				owner.getSuperTypes().add(this.typeBuilder.cloneWithProxies(superType));
				context.incrementSerial(superType.getIdentifier().hashCode());
				explicitType = true;
			}
		}
		if (!explicitType) {
			final JvmTypeReference type = this._typeReferenceBuilder.typeRef(defaultJvmType);
			owner.getSuperTypes().add(type);
			context.incrementSerial(type.getIdentifier().hashCode());
		}
	}

	/** Generate the implemented types for the given SARL statement.
	 *
	 * @param context - the context of the generation.
	 * @param owner - the JVM element to change.
	 * @param defaultJvmType - the default JVM type.
	 * @param defaultSarlType - the default SARL type.
	 * @param implementedtypes - the implemented types.
	 */
	protected void appendConstrainedImplements(
			GenerationContext context,
			JvmGenericType owner, Class<?> defaultJvmType,
			Class<? extends XtendTypeDeclaration> defaultSarlType,
			List<? extends JvmParameterizedTypeReference> implementedtypes) {
		boolean explicitType = false;
		for (final JvmParameterizedTypeReference superType : implementedtypes) {
			if (!Objects.equal(owner.getIdentifier(), superType.getIdentifier())
					&& superType.getType() instanceof JvmGenericType
					/*&& this.inheritanceHelper.isProxyOrSubTypeOf(superType, defaultJvmType, defaultSarlType, true)*/) {
				owner.getSuperTypes().add(this.typeBuilder.cloneWithProxies(superType));
				context.incrementSerial(superType.getIdentifier().hashCode());
				explicitType = true;
			}
		}
		if (!explicitType) {
			final JvmTypeReference type = this._typeReferenceBuilder.typeRef(defaultJvmType);
			owner.getSuperTypes().add(type);
			context.incrementSerial(type.getIdentifier().hashCode());
		}
	}

	/** Add the @Generated annotation to the given target.
	 * The annotation will not have any generated SARL code associated to it.
	 *
	 * @param target - the target of the annotation.
	 * @param context the generation context.
	 */
	protected final void appendGeneratedAnnotation(JvmAnnotationTarget target, GenerationContext context) {
		appendGeneratedAnnotation(target, context, null);
	}

	/** Add the @Generated annotation to the given target.
	 *
	 * @param target - the target of the annotation.
	 * @param context the generation context.
	 * @param sarlCode - the code that is the cause of the generation.
	 */
	protected void appendGeneratedAnnotation(JvmAnnotationTarget target, GenerationContext context, String sarlCode) {
		final GeneratorConfig config = context.getGeneratorConfig();
		if (config.isGenerateGeneratedAnnotation()) {
			addAnnotationSafe(target, Generated.class, getClass().getName());
		}

		if (target instanceof JvmFeature) {
			addAnnotationSafe(target, SyntheticMember.class);
		}

		if (!Strings.isNullOrEmpty(sarlCode)) {
			addAnnotationSafe(target, SarlSourceCode.class, sarlCode);
		}
	}

	/** Add annotation safely.
	 *
	 * <p>This function creates an annotation reference. If the type for the annotation is not found;
	 * no annotation is added.
	 *
	 * @param target the receiver of the annotation.
	 * @param annotationType the type of the annotation.
	 * @param values the annotations values.
	 * @return the annotation reference or <code>null</code> if the annotation cannot be added.
	 */
	protected JvmAnnotationReference addAnnotationSafe(JvmAnnotationTarget target, Class<?> annotationType, String... values) {
		assert target != null;
		assert annotationType != null;
		try {
			final JvmAnnotationReference annotationRef = this._annotationTypesBuilder.annotationRef(annotationType, values);
			if (annotationRef != null) {
				target.getAnnotations().add(annotationRef);
			}
		} catch (IllegalArgumentException exception) {
			// Ignore
		}
		return null;
	}

	/** Create an annotation with classes as values.
	 *
	 * @param type - the type of the annotation.
	 * @param values - the values.
	 * @return the reference to the JVM annotation.
	 */
	private JvmAnnotationReference annotationClassRef(Class<? extends Annotation> type,
			List<? extends JvmTypeReference> values) {
		try {
			final JvmAnnotationReference annot = this._annotationTypesBuilder.annotationRef(type);
			final JvmTypeAnnotationValue annotationValue = this.services.getTypesFactory().createJvmTypeAnnotationValue();
			for (final JvmTypeReference value : values) {
				annotationValue.getValues().add(this.typeBuilder.cloneWithProxies(value));
			}
			annot.getExplicitValues().add(annotationValue);
			return annot;
		} catch (IllegalArgumentException exception) {
			// ignore
		}
		return null;
	}

	/** Append the guard evaluators.
	 *
	 * @param container the container type.
	 */
	protected void appendEventGuardEvaluators(JvmGenericType container) {
		final GenerationContext context = getContext(container);
		if (context != null) {
			final Collection<Pair<SarlBehaviorUnit, Collection<Procedure1<ITreeAppendable>>>> allEvaluators
					= context.getGuardEvaluationCodes();
			if (allEvaluators == null || allEvaluators.isEmpty()) {
				return;
			}

			final JvmTypeReference voidType = this._typeReferenceBuilder.typeRef(Void.TYPE);
			final JvmTypeReference runnableType = this._typeReferenceBuilder.typeRef(Runnable.class);
			final JvmTypeReference collectionType = this._typeReferenceBuilder.typeRef(Collection.class, runnableType);
			for (final Pair<SarlBehaviorUnit, Collection<Procedure1<ITreeAppendable>>> evaluators : allEvaluators) {
				final SarlBehaviorUnit source = evaluators.getKey();
				// Determine the name of the operation for the behavior output
				final String behName = Utils.createNameForHiddenGuardGeneralEvaluatorMethod(source.getName().getSimpleName());

				// Create the main function
				final JvmOperation operation = this.typesFactory.createJvmOperation();

				// Annotation for the event bus

				appendGeneratedAnnotation(operation, context);
				addAnnotationSafe(operation, PerceptGuardEvaluator.class);

				// Guard evaluator unit parameters
				// - Event occurrence
				JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName(this.grammarKeywordAccess.getOccurrenceKeyword());
				jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(source.getName()));
				this.associator.associate(source, jvmParam);
				operation.getParameters().add(jvmParam);
				// - List of runnables
				jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName(RUNNABLE_COLLECTION);
				jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(collectionType));
				operation.getParameters().add(jvmParam);

				operation.setAbstract(false);
				operation.setNative(false);
				operation.setSynchronized(false);
				operation.setStrictFloatingPoint(false);
				operation.setFinal(false);
				operation.setVisibility(JvmVisibility.PRIVATE);
				operation.setStatic(false);
				operation.setSimpleName(behName);
				operation.setReturnType(this.typeBuilder.cloneWithProxies(voidType));
				container.getMembers().add(operation);

				setBody(operation, (it) -> {
					it.append("assert "); //$NON-NLS-1$
					it.append(this.grammarKeywordAccess.getOccurrenceKeyword());
					it.append(" != null;"); //$NON-NLS-1$
					it.newLine();
					it.append("assert "); //$NON-NLS-1$
					it.append(RUNNABLE_COLLECTION);
					it.append(" != null;"); //$NON-NLS-1$
					for (final Procedure1<ITreeAppendable> code : evaluators.getValue()) {
						it.newLine();
						code.apply(it);
					}
				});

				this.associator.associatePrimary(source, operation);
				this.typeBuilder.copyDocumentationTo(source, operation);
				this.typeExtensions.setSynthetic(operation, true);
			}
		}
	}

	/** Append the @FunctionalInterface to the given type if it is a functional interface according
	 * to the Java 8 specification definition.
	 *
	 * @param type the type to update.
	 */
	protected void appendFunctionalInterfaceAnnotation(JvmGenericType type) {
		if (type != null && Utils.isFunctionalInterface(type, this.sarlSignatureProvider)
				&& this.annotationFinder.findAnnotation(type, FunctionalInterface.class) == null) {
			addAnnotationSafe(type, FunctionalInterface.class);
		}
	}

	/** Create the functions that permits to compare the object.
	 *
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	protected void appendComparisonFunctions(GenerationContext context, XtendTypeDeclaration source, JvmGenericType target) {
		// Create a list of the declared non-static fields.
		final List<JvmField> declaredInstanceFields = new ArrayList<>();
		for (final JvmField field : target.getDeclaredFields()) {
			if (!field.isStatic()) {
				declaredInstanceFields.add(field);
			}
		}

		if (!declaredInstanceFields.isEmpty()) {
			JvmOperation op = toEqualsMethod(source, target, declaredInstanceFields);
			if (op != null) {
				appendGeneratedAnnotation(op, context);
				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
				target.getMembers().add(op);
			}

			op = toHashCodeMethod(source, declaredInstanceFields);
			if (op != null) {
				appendGeneratedAnnotation(op, context);
				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
				target.getMembers().add(op);
			}
		}
	}

	/** Create the functions that are related to the <code>toString</code> function.
	 *
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	protected void appendToStringFunctions(GenerationContext context, XtendTypeDeclaration source,
			final JvmGenericType target) {
		// Create a list of the declared non-static fields.
		final List<JvmField> declaredInstanceFields = new ArrayList<>();
		for (final JvmField field : target.getDeclaredFields()) {
			if (!field.isStatic()) {
				declaredInstanceFields.add(field);
			}
		}

		if (!declaredInstanceFields.isEmpty()) {
			final JvmOperation op = SARLJvmModelInferrer.this.typeBuilder.toMethod(
					source,
					"attributesToString", //$NON-NLS-1$
					SARLJvmModelInferrer.this._typeReferenceBuilder.typeRef(String.class), (it2) -> {
					it2.setVisibility(JvmVisibility.PROTECTED);
					SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it2,
							MessageFormat.format(Messages.SARLJvmModelInferrer_2,
									target.getSimpleName()));
					setBody(it2, (it3) -> {
						it3.append("StringBuilder result = new StringBuilder(" //$NON-NLS-1$
								+ "super.attributesToString());").newLine(); //$NON-NLS-1$
						for (final JvmField attr : declaredInstanceFields) {
							it3.append("result.append(\"" + attr.getSimpleName() //$NON-NLS-1$
								+ "  = \").append(this." //$NON-NLS-1$
								+ attr.getSimpleName() + ");").newLine(); //$NON-NLS-1$
						}
						it3.append("return result.toString();"); //$NON-NLS-1$
					});
				});
			if (op != null) {
				appendGeneratedAnnotation(op, context);
				addAnnotationSafe(op, Pure.class);
				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
				target.getMembers().add(op);
			}
		}
	}

	/** Append the serial number field.
	 *
	 * <p>The serial number field is computed from the given context and from the generated fields.
	 *
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	protected void appendSerialNumber(GenerationContext context, XtendTypeDeclaration source, JvmGenericType target) {
		for (final JvmField field : Iterables.filter(target.getMembers(), JvmField.class)) {
			if ("serialVersionUID".equals(field.getSimpleName())) { //$NON-NLS-1$
				return;
			}
		}

		final JvmField field = this.typesFactory.createJvmField();
		field.setSimpleName("serialVersionUID"); //$NON-NLS-1$
		target.getMembers().add(field);
		this.associator.associatePrimary(source, field);
		field.setVisibility(JvmVisibility.PRIVATE);
		field.setStatic(true);
		field.setTransient(false);
		field.setVolatile(false);
		field.setFinal(true);
		field.setType(this.typeBuilder.cloneWithProxies(this._typeReferenceBuilder.typeRef(long.class)));
		final long serial = context.getSerial();
		this.typeBuilder.setInitializer(field, toStringConcatenation(serial + "L")); //$NON-NLS-1$
		appendGeneratedAnnotation(field, context);
		this.typeExtensions.setSynthetic(field, true);
		this.readAndWriteTracking.markInitialized(field, null);
	}

	/** Append the SARL specification version as a private field of the given container.
	 *
	 * <p>The added field may be used by any underground platform for determining what is
	 * the version of the SARL specification that was used for generating the container.
	 * The principle is inspired from the serialVersionUID from Java.
	 *
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	protected void appendSARLSpecificationVersion(GenerationContext context, XtendTypeDeclaration source,
			JvmDeclaredType target) {
		addAnnotationSafe(target, SarlSpecification.class, SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING);
	}

	/** Generate a list of formal parameters with annotations for the default values.
	 *
	 * @param context - the generation context.
	 * @param owner - the JVM element to change.
	 * @param actionContainer - the container of the action.
	 * @param varargs - indicates if the signature has variadic parameter.
	 * @param params - the parameters.
	 * @param isForInterface - indicates if the formal parameters are for an interface (<code>true</code>)
	 * 							or a class (<code>false</code>).
	 * @param paramSpec - the specification of the parameter as computed by a {@link IActionPrototypeProvider}.
	 */
	protected void translateSarlFormalParameters(
			GenerationContext context,
			JvmExecutable owner,
			JvmGenericType actionContainer,
			boolean varargs,
			List<? extends XtendParameter> params,
			final boolean isForInterface,
			List<InferredStandardParameter> paramSpec) {
		boolean hasDefaultValue = false;
		for (int i = 0; i < params.size(); ++i) {
			final XtendParameter param = params.get(i);
			final InferredStandardParameter inferredParam = paramSpec.get(i);
			final String paramName = param.getName();
			final JvmTypeReference paramType = param.getParameterType();

			if (!Strings.isNullOrEmpty(paramName) && paramType != null) {
				// "Standard" (Xtend) translation of the parameter
				translateParameter(owner, param);
				final JvmFormalParameter lastParam = owner.getParameters().get(owner.getParameters().size() - 1);
				// Treat the default value
				if (param instanceof SarlFormalParameter && ((SarlFormalParameter) param).getDefaultValue() != null) {
					final XExpression defaultValue = ((SarlFormalParameter) param).getDefaultValue();
					assert defaultValue != null;
					hasDefaultValue = true;
					final String namePostPart = inferredParam.getDefaultValueAnnotationValue();
					final String name = this.sarlSignatureProvider.createFieldNameForDefaultValueID(namePostPart);
					// FIXME: Hide these attributes into an inner interface.
					final JvmField field = this.typeBuilder.toField(defaultValue, name, paramType, (it) -> {
						SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it,
								MessageFormat.format(Messages.SARLJvmModelInferrer_11, paramName));
						it.setStatic(true);
						it.setFinal(true);
						if (isForInterface) {
							it.setVisibility(JvmVisibility.PUBLIC);
						} else {
							it.setVisibility(JvmVisibility.PRIVATE);
						}
						SARLJvmModelInferrer.this.typeBuilder.setInitializer(it, defaultValue);
					});
					actionContainer.getMembers().add(field);
					if (owner instanceof JvmConstructor) {
						this.readAndWriteTracking.markInitialized(field, (JvmConstructor) owner);
					} else {
						this.readAndWriteTracking.markInitialized(field, null);
					}
					addAnnotationSafe(lastParam, DefaultValue.class, namePostPart);

					final String rawCode = reentrantSerialize(defaultValue);
					appendGeneratedAnnotation(field, context, rawCode);
				}
			}
		}

		if (hasDefaultValue) {
			addAnnotationSafe(owner, DefaultValueSource.class);
		}
	}

	/** Generate a list of formal parameters with annotations for the default values.
	 *
	 * @param owner - the JVM element to change.
	 * @param actionContainer - the container of the action.
	 * @param varargs - indicates if the signature has variadic parameter.
	 * @param signature - the description of the parameters.
	 * @return the arguments to pass to the original function.
	 */
	protected List<String> translateSarlFormalParametersForSyntheticOperation(JvmExecutable owner, JvmGenericType actionContainer,
			boolean varargs, List<InferredStandardParameter> signature) {
		final List<String> arguments = CollectionLiterals.newArrayList();
		for (final InferredStandardParameter parameterSpec : signature) {
			if (parameterSpec instanceof InferredValuedParameter) {
				arguments.add(
						this.sarlSignatureProvider.toJavaArgument(
								actionContainer.getIdentifier(),
								((InferredValuedParameter) parameterSpec).getCallingArgument()));
			} else {
				final EObject param = parameterSpec.getParameter();
				final String paramName = parameterSpec.getName();
				final JvmTypeReference paramType = parameterSpec.getType();
				if (!Strings.isNullOrEmpty(paramName) && paramType != null) {
					final JvmFormalParameter lastParam = this.typesFactory.createJvmFormalParameter();
					owner.getParameters().add(lastParam);
					lastParam.setName(paramName);
					lastParam.setParameterType(this.typeBuilder.cloneWithProxies(paramType));
					this.associator.associate(param, lastParam);
					arguments.add(paramName);
				}
			}
		}
		return arguments;
	}

	/** Copy the annotations, except the ones given as parameters.
	 *
	 * @param annotations the annotations to copy.
	 * @param target the target.
	 * @param exceptions the annotations to skip.
	 */
	@SuppressWarnings("static-method")
	protected void translateAnnotationsTo(List<JvmAnnotationReference> annotations, JvmAnnotationTarget target,
			Class<?>... exceptions) {
		final Set<String> excepts = new HashSet<>();
		for (final Class<?> type : exceptions) {
			excepts.add(type.getName());
		}
		final List<JvmAnnotationReference> addition = new ArrayList<>();
		for (final JvmAnnotationReference annotation : Iterables.filter(annotations, (an) -> {
			if (!ANNOTATION_TRANSLATION_FILTER.apply(an)) {
				return false;
			}
			return !excepts.contains(an.getAnnotation().getIdentifier());
		})) {
			addition.add(annotation);
		}
		target.getAnnotations().addAll(addition);
	}

	/** Generate the "equals()" operation.
	 * This function was deprecated in Xbase, and should be provided by DSL
	 * providers now.
	 *
	 * @param sarlElement - the SARL element for which the "equals function must be generated.
	 * @param declaredType - the declating type.
	 * @param jvmFields - the fields declared in the container.
	 * @return the "equals" function.
	 */
	private JvmOperation toEqualsMethod(
			XtendTypeDeclaration sarlElement,
			final JvmDeclaredType declaredType,
			final Iterable<JvmField> jvmFields) {
		if (sarlElement == null || declaredType == null) {
			return null;
		}
		final JvmOperation result = this.typeBuilder.toMethod(sarlElement, "equals", //$NON-NLS-1$
				this._typeReferenceBuilder.typeRef(Boolean.TYPE), null);
		if (result == null) {
			return null;
		}
		addAnnotationSafe(result, Override.class);
		addAnnotationSafe(result, Pure.class);

		final JvmFormalParameter param = this.typesFactory.createJvmFormalParameter();
		param.setName("obj"); //$NON-NLS-1$
		param.setParameterType(this._typeReferenceBuilder.typeRef(Object.class));
		this.associator.associate(sarlElement, param);
		result.getParameters().add(param);
		setBody(result, new Procedures.Procedure1<ITreeAppendable>() {
			@Override
			public void apply(ITreeAppendable it) {
				it.append("if (this == obj)").increaseIndentation(); //$NON-NLS-1$
				it.newLine().append("return true;").decreaseIndentation(); //$NON-NLS-1$
				it.newLine().append("if (obj == null)").increaseIndentation(); //$NON-NLS-1$
				it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				it.newLine().append("if (getClass() != obj.getClass())").increaseIndentation(); //$NON-NLS-1$
				it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				it.newLine().append(declaredType.getSimpleName() + " other = (" //$NON-NLS-1$
						+ declaredType.getSimpleName() + ") obj;"); //$NON-NLS-1$
				for (final JvmField field : jvmFields) {
					generateToEqualForField(it, field);
				}
				it.newLine().append("return super.equals(obj);"); //$NON-NLS-1$
			}

			private boolean arrayContains(String element, String... array) {
				for (final String elt : array) {
					if (Objects.equal(elt, element)) {
						return true;
					}
				}
				return false;
			}

			private void generateToEqualForField(ITreeAppendable it, JvmField field) {
				final String typeName = field.getType().getIdentifier();
				if (arrayContains(typeName,
						Boolean.TYPE.getName(),
						Integer.TYPE.getName(),
						Long.TYPE.getName(),
						Character.TYPE.getName(),
						Byte.TYPE.getName(),
						Short.TYPE.getName())) {
					it.newLine().append("if (other." + field.getSimpleName() //$NON-NLS-1$
					+ " != this." + field.getSimpleName() + ")").increaseIndentation(); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				} else if (Objects.equal(Double.TYPE.getName(), typeName)) {
					it.newLine().append("if (Double.doubleToLongBits(other." + field.getSimpleName() //$NON-NLS-1$
					+ ") != Double.doubleToLongBits(this." + field.getSimpleName() //$NON-NLS-1$
					+ "))").increaseIndentation(); //$NON-NLS-1$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				} else if (Objects.equal(Float.TYPE.getName(), typeName)) {
					it.newLine().append("if (Float.floatToIntBits(other." + field.getSimpleName() //$NON-NLS-1$
					+ ") != Float.floatToIntBits(this." + field.getSimpleName() //$NON-NLS-1$
					+ "))").increaseIndentation(); //$NON-NLS-1$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				} else  {
					it.newLine().append("if (this." + field.getSimpleName() //$NON-NLS-1$
					+ " == null) {").increaseIndentation(); //$NON-NLS-1$
					it.newLine().append("if (other." + field.getSimpleName() //$NON-NLS-1$
					+ " != null)").increaseIndentation(); //$NON-NLS-1$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
					it.decreaseIndentation();
					it.newLine().append("} else if (!this." + field.getSimpleName() //$NON-NLS-1$
					+ ".equals(other." + field.getSimpleName() //$NON-NLS-1$
					+ "))").increaseIndentation(); //$NON-NLS-1$
					it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				}
			}
		});
		return result;
	}

	/** Generate the "hashCode()" operation.
	 * This function was deprecated in Xbase, and should be provided by DSL
	 * providers now.
	 *
	 * @param sarlElement - the SARL element for which the "hashCode" msut be generated.
	 * @param jvmFields - the fields declared in the container.
	 * @return the "hashCode" function.
	 */
	private JvmOperation toHashCodeMethod(
			XtendTypeDeclaration sarlElement,
			final Iterable<JvmField> jvmFields) {
		if (sarlElement == null) {
			return null;
		}
		final JvmOperation result = this.typeBuilder.toMethod(sarlElement, "hashCode", //$NON-NLS-1$
				this._typeReferenceBuilder.typeRef(Integer.TYPE), null);
		if (result == null) {
			return null;
		}
		addAnnotationSafe(result, Override.class);
		addAnnotationSafe(result, Pure.class);
		setBody(result, (it) -> {
			it.append("final int prime = 31;"); //$NON-NLS-1$
			it.newLine().append("int result = super.hashCode();"); //$NON-NLS-1$
			for (final JvmField field : jvmFields) {
				final String typeName = field.getType().getIdentifier();
				if (Objects.equal(Boolean.TYPE.getName(), typeName)) {
					it.newLine().append("result = prime * result + (this." //$NON-NLS-1$
							+ field.getSimpleName() + " ? 1231 : 1237);"); //$NON-NLS-1$
				} else if (Objects.equal(Integer.TYPE.getName(), typeName)
						|| Objects.equal(Character.TYPE.getName(), typeName)
						|| Objects.equal(Byte.TYPE.getName(), typeName)
						|| Objects.equal(Short.TYPE.getName(), typeName)) {
					it.newLine().append("result = prime * result + this." //$NON-NLS-1$
							+ field.getSimpleName() + ";"); //$NON-NLS-1$
				} else if (Objects.equal(Long.TYPE.getName(), typeName)) {
					it.newLine().append("result = prime * result + (int) (this." //$NON-NLS-1$
							+ field.getSimpleName() + " ^ (this." + field.getSimpleName() //$NON-NLS-1$
							+ " >>> 32));"); //$NON-NLS-1$
				} else if (Objects.equal(Float.TYPE.getName(), typeName)) {
					it.newLine().append("result = prime * result + Float.floatToIntBits(this." //$NON-NLS-1$
							+ field.getSimpleName() + ");"); //$NON-NLS-1$
				} else if (Objects.equal(Double.TYPE.getName(), typeName)) {
					it.newLine().append("result = prime * result + (int) (Double.doubleToLongBits(this." //$NON-NLS-1$
							+ field.getSimpleName() + ") ^ (Double.doubleToLongBits(this." //$NON-NLS-1$
							+ field.getSimpleName() + ") >>> 32));"); //$NON-NLS-1$
				} else {
					it.newLine().append("result = prime * result + ((this." //$NON-NLS-1$
							+ field.getSimpleName() + "== null) ? 0 : this." + field.getSimpleName() //$NON-NLS-1$
							+ ".hashCode());"); //$NON-NLS-1$
				}
			}
			it.newLine().append("return result;"); //$NON-NLS-1$
		});
		return result;
	}

	/** Clone the given type reference that for being link to the given operation.
	 *
	 * <p>The proxies are not resolved, and the type parameters are clone when they are
	 * related to the type parameter of the type container.
	 *
	 * @param type the source type.
	 * @param forOperation the operation that will contain the result type.
	 * @return the result type, i.e. a copy of the source type.
	 */
	protected JvmTypeReference cloneWithTypeParametersAndProxies(JvmTypeReference type, JvmOperation forOperation) {
		// TODO: Is similar function exist in Xtext?

		if (type == null) {
			return this._typeReferenceBuilder.typeRef(Object.class);
		}

		boolean cloneType = true;
		JvmTypeReference typeCandidate = type;

		// Use also cloneType as a flag that indicates if the type was already found in type parameters.
		if (cloneType && type instanceof JvmParameterizedTypeReference) {
			// Try to clone the type parameters.
			final List<JvmTypeParameter> typeParameters = forOperation.getTypeParameters();
			if (!typeParameters.isEmpty()) {
				cloneType = false;
				typeCandidate = cloneAndAssociate(type, typeParameters);
			}
		}

		// Do the clone according to the type of the entity.
		assert typeCandidate != null;
		final JvmTypeReference returnType;
		if (InferredTypeIndicator.isInferred(typeCandidate) || !cloneType) {
			returnType = typeCandidate;
		} else {
			returnType = this.typeBuilder.cloneWithProxies(typeCandidate);
		}
		return returnType;
	}

	private JvmTypeReference cloneAndAssociate(
			final JvmTypeReference type,
			final List<JvmTypeParameter> typeParameters) {
		final Map<String, JvmTypeParameter> typeParameterIdentifiers = new TreeMap<>();
		for (final JvmTypeParameter typeParameter : typeParameters) {
			typeParameterIdentifiers.put(typeParameter.getIdentifier(), typeParameter);
		}

		final boolean canAssociate = this.languageInfo.isLanguage(type.eResource());

		EcoreUtil.Copier copier = new EcoreUtil.Copier(false) {
			private static final long serialVersionUID = 698510355384773254L;

			@SuppressWarnings("synthetic-access")
			@Override
			protected EObject createCopy(EObject eobject) {
				final EObject result = super.createCopy(eobject);
				if (result != null && eobject != null && !eobject.eIsProxy()) {
					if (canAssociate) {
						SARLJvmModelInferrer.this.associator.associate(eobject, result);
					}
				}
				return result;
			}

			@SuppressWarnings("synthetic-access")
			@Override
			public EObject copy(EObject eobject) {
				final String id;
				if (eobject instanceof JvmTypeReference) {
					id = ((JvmTypeReference) eobject).getIdentifier();
				} else if (eobject instanceof JvmIdentifiableElement) {
					id = ((JvmIdentifiableElement) eobject).getIdentifier();
				} else {
					id = null;
				}
				if (id != null) {
					final JvmTypeParameter param = typeParameterIdentifiers.get(id);
					if (param != null) {
						return SARLJvmModelInferrer.this.typeReferences.createTypeRef(param);
					}
				}
				final EObject result = super.copy(eobject);
				if (result instanceof JvmWildcardTypeReference) {
					final JvmWildcardTypeReference wildcardType = (JvmWildcardTypeReference) result;
					boolean upperBoundSeen = false;
					for (final JvmTypeConstraint constraint : wildcardType.getConstraints()) {
						if (constraint instanceof JvmUpperBound) {
							upperBoundSeen = true;
							break;
						}
					}
					if (!upperBoundSeen) {
						// no upper bound found - seems to be an invalid - assume object as upper bound
						final JvmTypeReference object = SARLJvmModelInferrer.this._typeReferenceBuilder.typeRef(Object.class);
						final JvmUpperBound upperBound = SARLJvmModelInferrer.this.typesFactory.createJvmUpperBound();
						upperBound.setTypeReference(object);
						wildcardType.getConstraints().add(0, upperBound);
					}
				}
				return result;
			}

			@Override
			protected void copyReference(EReference ereference, EObject eobject, EObject copyEObject) {
				super.copyReference(ereference, eobject, copyEObject);
			}
		};
		final JvmTypeReference copy = (JvmTypeReference) copier.copy(type);
		copier.copyReferences();
		return copy;
	}

	/** Copy and clean the given documentation by removing any unecessary <code>@param</code>.
	 *
	 * @param sourceOperation the source for the documentation.
	 * @param targetOperation the target for the documentation.
	 * @return <code>true</code> if a documentation was added.
	 */
	protected boolean copyAndCleanDocumentationTo(JvmOperation sourceOperation, JvmOperation targetOperation) {
		assert sourceOperation != null;
		assert targetOperation != null;

		String comment = SARLJvmModelInferrer.this.typeBuilder.getDocumentation(sourceOperation);
		if (Strings.isNullOrEmpty(comment)) {
			return false;
		}

		final Set<String> targetParams = new TreeSet<>();
		for (final JvmFormalParameter param : targetOperation.getParameters()) {
			targetParams.add(param.getSimpleName());
		}

		for (final JvmFormalParameter param : sourceOperation.getParameters()) {
			final String id = param.getSimpleName();
			if (!targetParams.contains(id)) {
				comment = comment.replaceAll(
						"\\Q@param\\E\\s+\\Q" + id + "\\E", //$NON-NLS-1$//$NON-NLS-2$
						"@optionalparam " + id); //$NON-NLS-1$
			}
		}

		SARLJvmModelInferrer.this.typeBuilder.setDocumentation(targetOperation, comment);
		return true;
	}

	private String reentrantSerialize(EObject object) {
		Set<?> contexts = this.contextFinder.findByContentsAndContainer(object, null);
		// This is a bug fix for a bug that I cannot explain.
		// I some cases, the context of the given expression cannot be retreive directly.
		// A second call to the finding function solves the problem.
		while (contexts == null || contexts.isEmpty()) {
			Thread.yield();
			contexts = this.contextFinder.findByContentsAndContainer(object, null);
		}
		final String code = this.sarlSerializer.serialize(object);
		if (code != null) {
			return code.trim();
		}
		return code;
	}

}
