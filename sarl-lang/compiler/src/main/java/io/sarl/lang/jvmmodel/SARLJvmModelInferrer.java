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

package io.sarl.lang.jvmmodel;

import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.text.MessageFormat;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import javax.annotation.processing.Generated;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.jvmmodel.SyntheticNameClashResolver;
import org.eclipse.xtend.core.jvmmodel.XtendJvmModelInferrer;
import org.eclipse.xtend.core.xtend.AnonymousClass;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendEnum;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmEnumerationType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmFeature;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmUnknownTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociator;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.XbaseGenerated;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.eclipse.xtext.xbase.typesystem.InferredTypeIndicator;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.validation.ReadAndWriteTracking;
import org.eclipse.xtext.xtype.XComputedTypeReference;

import com.google.common.base.Objects;
import com.google.common.base.Predicate;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.collect.Sets;
import com.google.inject.Inject;
import com.google.inject.MembersInjector;
import com.google.inject.Singleton;

import io.sarl.lang.compiler.IInlineExpressionCompiler;
import io.sarl.lang.controlflow.ISarlEarlyExitComputer;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.AtomicSkillReference;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.annotation.DefaultValue;
import io.sarl.lang.core.annotation.DefaultValueSource;
import io.sarl.lang.core.annotation.DefaultValueUse;
import io.sarl.lang.core.annotation.EarlyExit;
import io.sarl.lang.core.annotation.FiredEvent;
import io.sarl.lang.core.annotation.ImportedCapacityFeature;
import io.sarl.lang.core.annotation.Injectable;
import io.sarl.lang.core.annotation.NoEqualityTestFunctionsGeneration;
import io.sarl.lang.core.annotation.PerceptGuardEvaluator;
import io.sarl.lang.core.annotation.SarlElementType;
import io.sarl.lang.core.annotation.SarlSourceCode;
import io.sarl.lang.core.annotation.SarlSpecification;
import io.sarl.lang.core.annotation.SyntheticMember;
import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.jvmmodel.GenerationContext.BehaviorUnitGuardEvaluators;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlArtifact;
import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlContinueExpression;
import io.sarl.lang.sarl.SarlEnumLiteral;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlProtocol;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.SarlSpace;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.sarl.actionprototype.InferredStandardParameter;
import io.sarl.lang.sarl.actionprototype.InferredValuedParameter;
import io.sarl.lang.services.ITypeDefaultValueProvider;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.typesystem.IOperationHelper;
import io.sarl.lang.typesystem.InheritanceHelper;
import io.sarl.lang.typesystem.SARLAnnotationUtil;
import io.sarl.lang.util.JvmVisibilityComparator;
import io.sarl.lang.util.Utils;
import io.sarl.lang.util.Utils.TypeParameterStatus;

/** Infers a JVM model from the source model.
 *
 * <p>The JVM model should contain all elements that would appear in the Java code
 * which is generated from the source model. Other models link against
 * the JVM model rather than the source model.
 *
 * <p>The roles of the different generation tools are:<ul>
 * <li>{@link io.sarl.lang.jvmmodel.SARLJvmModelInferrer}: Generating the expected Java Ecore model from the SARL Ecore model.</li>
 * <li>{@link org.eclipse.xtext.linking.ILinker}: Create links among the SARL Ecore models.<li>
 * <li>{@link io.sarl.lang.compiler.SARLJvmGenerator}: Generate the Java code from the Java Ecore model.</li>
 * <li>{@link io.sarl.lang.compiler.SarlCompiler}: Generate the Java code for the XExpression objects.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class SARLJvmModelInferrer extends XtendJvmModelInferrer {

	private static final String RUNNABLE_COLLECTION = Utils.createNameForHiddenLocalVariable("runnableCollection"); //$NON-NLS-1$

	private static final String HASHCODE_FUNCTION_NAME = "hashCode"; //$NON-NLS-1$

	private static final String EQUALS_FUNCTION_NAME = "equals"; //$NON-NLS-1$

	private static final String CLONE_FUNCTION_NAME = "clone"; //$NON-NLS-1$

	private static final String SERIAL_FIELD_NAME = "serialVersionUID"; //$NON-NLS-1$

	private static final String GENERATED_NAME = Generated.class.getName();

	/** See the filter in the super class.
	 */
	private static final Predicate<JvmAnnotationReference> ANNOTATION_TRANSLATION_FILTER = annotation -> {
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

	/** Helper for defining operations.
	 */
	@Inject
	private IOperationHelper operationHelper;

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
	private SARLGrammarKeywordAccess grammarKeywordAccess;

	@Inject
	private IInlineExpressionCompiler inlineExpressionCompiler;

	@Inject
	private InheritanceHelper inheritanceHelper;

	@Inject
	private IDefaultVisibilityProvider defaultVisibilityProvider;

	@Inject
	private ITypeDefaultValueProvider defaultValueProvider;

	/** Generation contexts.
	 */
	private LinkedList<GenerationContext> bufferedContexes = new LinkedList<>();

	/** Log an internal error but do not fail.
	 *
	 * @param exception the exception to log.
	 */
	protected void logInternalError(Throwable exception) {
		if (exception != null && this.log.isLoggable(Level.SEVERE)) {
			this.log.log(Level.SEVERE, Messages.SARLJvmModelInferrer_0, exception);
		}
	}

	/** Log an internal error but do not fail.
	 *
	 * @param message the internal message.
	 */
	protected void logInternalError(String message) {
		if (this.log.isLoggable(Level.SEVERE) && !Strings.isNullOrEmpty(message)) {
			this.log.log(Level.SEVERE,
					MessageFormat.format(Messages.SARLJvmModelInferrer_1,
							Messages.SARLJvmModelInferrer_0, message));
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
	 * @return the annotation reference or {@code null} if the annotation cannot be added.
	 */
	private JvmAnnotationReference addAnnotationSafe(JvmAnnotationTarget target, Class<?> annotationType, String... values) {
		assert target != null;
		assert annotationType != null;
		try {
			final var annotationRef = this._annotationTypesBuilder.annotationRef(annotationType, values);
			if (annotationRef != null) {
				if (target.getAnnotations().add(annotationRef)) {
					return annotationRef;
				}
			}
		} catch (IllegalArgumentException exception) {
			// Ignore
		}
		return null;
	}

	/** Add annotation safely.
	 *
	 * <p>This function creates an annotation reference. If the type for the annotation is not found;
	 * no annotation is added.
	 *
	 * @param target the receiver of the annotation.
	 * @param annotationType the type of the annotation.
	 * @param values the annotations values.
	 * @return the annotation reference or {@code null} if the annotation cannot be added.
	 * @since 0.12
	 */
	private JvmAnnotationReference addAnnotationSafe(JvmAnnotationTarget target, String annotationType, String... values) {
		assert target != null;
		assert annotationType != null;
		try {
			final var annotationRef = this._annotationTypesBuilder.annotationRef(annotationType, values);
			if (annotationRef != null) {
				if (target.getAnnotations().add(annotationRef)) {
					return annotationRef;
				}
			}
		} catch (IllegalArgumentException exception) {
			// Ignore
		}
		return null;
	}

	/** Add annotation safely.
	 *
	 * <p>This function creates an annotation reference. If the type for the annotation is not found;
	 * no annotation is added.
	 *
	 * @param target the receiver of the annotation.
	 * @param annotationType the type of the annotation.
	 * @param value the annotations value.
	 * @return the annotation reference or {@code null} if the annotation cannot be added.
	 */
	private JvmAnnotationReference addAnnotationSafe(JvmAnnotationTarget target, Class<?> annotationType, int value) {
		assert target != null;
		assert annotationType != null;
		try {
			final var result = this.typesFactory.createJvmAnnotationReference();
			final var jvmType = this.typeReferences.findDeclaredType(annotationType, target);
			if (jvmType == null) {
				return null;
			}
			if (jvmType instanceof JvmAnnotationType cvalue) {
				result.setAnnotation(cvalue);
				final var annotationValue = this.typesFactory.createJvmIntAnnotationValue();
				annotationValue.getValues().add(Integer.valueOf(value));
				result.getExplicitValues().add(annotationValue);
				if (target.getAnnotations().add(result)) {
					return result;
				}
			}
		} catch (IllegalArgumentException exception) {
			// Ignore
		}
		return null;
	}

	/** Create an annotation with classes as values.
	 *
	 * @param type the type of the annotation.
	 * @param values the values.
	 * @return the reference to the JVM annotation.
	 */
	private JvmAnnotationReference annotationClassRef(Class<? extends Annotation> type,
			List<? extends JvmTypeReference> values) {
		try {
			final var annot = this._annotationTypesBuilder.annotationRef(type);
			final var annotationValue = this.services.getTypesFactory().createJvmTypeAnnotationValue();
			for (final var value : values) {
				annotationValue.getValues().add(this.typeBuilder.cloneWithProxies(value));
			}
			annot.getExplicitValues().add(annotationValue);
			return annot;
		} catch (IllegalArgumentException exception) {
			// ignore
		}
		return null;
	}

	/** Initialize the local class to the given expression.
	 *
	 * @param context the generation context.
	 * @param feature the feature which contains the expression.
	 * @param expression the expression which contains the local class.
	 */
	protected void initializeLocalTypes(GenerationContext context, JvmFeature feature, XExpression expression) {
		if (expression != null) {
			var localTypeIndex = context.getLocalTypeIndex();
			final var iterator = EcoreUtil2.getAllNonDerivedContents(expression, true);
			final var nameStub = "__" + feature.getDeclaringType().getSimpleName() + "_"; //$NON-NLS-1$ //$NON-NLS-2$
			while (iterator.hasNext()) {
				final var next = iterator.next();
				if (next.eClass() == XtendPackage.Literals.ANONYMOUS_CLASS) {
					inferLocalClass((AnonymousClass) next, nameStub + localTypeIndex, feature);
					iterator.prune();
					++localTypeIndex;
				}
			}
			context.setLocalTypeIndex(localTypeIndex);
		}
	}

	/** {@inheritDoc}.
	 *
	 * <p>Overridden for: removing the existing associated body, and delaying the local type inference.
	 */
	@Override
	protected void setBody(JvmExecutable executable, XExpression expression) {
		final var context = getContext(
				EcoreUtil2.getContainerOfType(executable, JvmType.class));
		this.typeBuilder.removeExistingBody(executable);
		this.associator.associateLogicalContainer(expression, executable);
		if (expression != null) {
			if (context.getParentContext() == null) {
				context.getPostFinalizationElements().add(() -> {
					initializeLocalTypes(context, executable, expression);
				});
			} else {
				initializeLocalTypes(context, executable, expression);
			}
		} else {
			initializeLocalTypes(context, executable, expression);
		}
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
	 * @param type the generated type.
	 * @param supportedMemberTypes the types of the supported members.
	 * @return the created context.
	 */
	protected final synchronized GenerationContext openContext(EObject sarlObject, JvmDeclaredType type,
			final Iterable<Class<? extends XtendMember>> supportedMemberTypes) {
		assert type != null;
		assert supportedMemberTypes != null;
		final var context = new GenerationContext(sarlObject, type) {
			@Override
			public boolean isSupportedMember(XtendMember member) {
				for (final var supportedMemberType : supportedMemberTypes) {
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
	protected final void closeContext(GenerationContext context) {
		boolean runPostElements = false;
		GenerationContext selectedContext = null;
		synchronized (this) {
			final var iterator = this.bufferedContexes.iterator();
			while (selectedContext == null && iterator.hasNext()) {
				final var candidate = iterator.next();
				if (!candidate.isRelease() && Objects.equal(candidate.getTypeIdentifier(), context.getTypeIdentifier())) {
					runPostElements = candidate.getParentContext() == null;
					selectedContext = candidate;
				}
			}
		}
		if (selectedContext == null) {
			this.log.warning("Not same contexts when closing"); //$NON-NLS-1$
			return;
		}
		if (runPostElements) {
			for (final var handler : selectedContext.getPostFinalizationElements()) {
				handler.run();
			}
		}
		synchronized (this) {
			final var iterator = this.bufferedContexes.iterator();
			while (iterator.hasNext()) {
				final var candidate = iterator.next();
				if (selectedContext == candidate) {
					candidate.setParentContext(null);
					candidate.release();
					iterator.remove();
					return;
				}
			}
		}
	}

	/** Replies the SARL-specific generation context.
	 *
	 * @param type the generated type.
	 * @return the SARL-specific generation context.
	 */
	protected final synchronized GenerationContext getContext(JvmIdentifiableElement type) {
		for (final var candidate : this.bufferedContexes) {
			if (Objects.equal(candidate.getTypeIdentifier(), type.getIdentifier())) {
				return candidate;
			}
		}
		throw new GenerationContextNotFoundInternalError(type);
	}

	@Override
	protected void inferTypeSceleton(final XtendTypeDeclaration declaration, final IJvmDeclaredTypeAcceptor acceptor, 
			boolean preIndexingPhase, XtendFile xtendFile, List<Runnable> doLater, JvmDeclaredType containerSceleton) {
		// Generate the elements for a 1-to-many mapping between SARL and JVM
		Stream<? extends JvmDeclaredType> inferredSkeletons = null;
		try {
			inferredSkeletons = doInferTypeSkeletons(declaration, acceptor, preIndexingPhase, xtendFile, doLater);
		} catch (InternalError internalError) {
			throw internalError;
		} catch (Exception exception) {
			logInternalError(exception);
			inferredSkeletons = null;
		}
		if (inferredSkeletons != null) {
			this.associator.removeAllAssociation(declaration);
			final var iterator = inferredSkeletons.iterator();
			boolean first = true;
			while (iterator.hasNext()) {
				final var inferredSkeleton = iterator.next();
				if (!Strings.isNullOrEmpty(inferredSkeleton.getSimpleName())) {
					inferredSkeleton.setPackageName(xtendFile.getPackage());
					inferredSkeleton.setVisibility(JvmVisibility.PUBLIC);
					setFileHeader(xtendFile, inferredSkeleton);
					if (first) {
						first = false;
						this.associator.associatePrimary(declaration, inferredSkeleton);
					} else {
						this.associator.associate(declaration, inferredSkeleton);
					}
					if (containerSceleton != null) {
						containerSceleton.getMembers().add(inferredSkeleton);
					}
					acceptor.accept(inferredSkeleton);
				}
			}
		} else {
			// Generate the elements for a 1-to-1 mapping between SARL and JVM
			super.inferTypeSceleton(declaration, acceptor, preIndexingPhase, xtendFile, doLater, containerSceleton);
		}
	}

	/** Autowrap the provided runnable elements in order to avoid the internal exceptions
	 * to stop the JVM generation too early.
	 *
	 * @param doLater the list to wrap.
	 * @return the wrapping list.
	 */
	private List<Runnable> wrapDoLaterList(List<Runnable> doLater) {
		return new AbstractList<>() {
			@Override
			public void add(int index, Runnable element) {
				doLater.add(index, wrap(element));
			}

			@Override
			public Runnable set(int index, Runnable element) {
				return unwrap(doLater.set(index, wrap(element)));
			}

			@Override
			public Runnable remove(int index) {
				return unwrap(doLater.remove(index));
			}

			@Override
			public Runnable get(int index) {
				return unwrap(doLater.get(index));
			}

			@Override
			public int size() {
				return doLater.size();
			}

			private Runnable wrap(Runnable runnable) {
				return new SafeRunnable(runnable);
			}

			private Runnable unwrap(Runnable runnable) {
				final var wrapper = (SafeRunnable) runnable;
				return wrapper == null ? null : wrapper.wrapped;
			}

			/** Safe runnable.
			 *
			 * @author $Author: sgalland$
			 * @version $FullVersion$
			 * @mavengroupid $GroupId$
			 * @mavenartifactid $ArtifactId$
			 */
			final class SafeRunnable implements Runnable {

				public final Runnable wrapped;

				SafeRunnable(Runnable wrapped) {
					this.wrapped = wrapped;
				}

				@Override
				public void run() {
					try {
						this.wrapped.run();
					} catch (InternalError internalError) {
						throw internalError;
					} catch (Exception exception) {
						logInternalError(exception);
					}
				}
			}
		};
	}

	/** Generate many JVM types from a single SARL type.
	 *
	 * @param declaration the SARL declaration.
	 * @param acceptor the acceptor of JVM types.
	 * @param preIndexingPhase indicates if the current phase is pre-indexing or generation.
	 * @param sarlFile the SARL file from which the SARL element was extracted.
	 * @param doLater a receiver of tasks that should be run later in the process.
	 * @return the list of created JVM types.
	 * @since 0.14
	 */
	protected Stream<? extends JvmDeclaredType> doInferTypeSkeletons(
			XtendTypeDeclaration declaration,
			IJvmDeclaredTypeAcceptor acceptor, boolean preIndexingPhase,
			XtendFile sarlFile, List<Runnable> doLater) {
		if (Strings.isNullOrEmpty(declaration.getName())) {
			return null;
		}

		// Autowrap the provided runnable elements in order to avoid the internal exceptions
		// to stop the JVM generation too early.
		final var doLaterExceptionSafe = wrapDoLaterList(doLater);

		if (declaration instanceof SarlProtocol sarlProtocol) {
			final var javaTypes = new DefaultJvmGenericTypeProvider();
			initialize(sarlProtocol, (JvmGenericTypeFactory) javaTypes);
			if (!preIndexingPhase) {
				doLaterExceptionSafe.add(() -> initialize(sarlProtocol, (JvmGenericTypeProvider) javaTypes));
			}
			return javaTypes.stream();
		}

		if (declaration instanceof SarlSpace sarlSpace) {
			final var javaTypes = new DefaultJvmGenericTypeProvider();
			initialize(sarlSpace, (JvmGenericTypeFactory) javaTypes);
			if (!preIndexingPhase) {
				doLaterExceptionSafe.add(() -> initialize(sarlSpace, (JvmGenericTypeProvider) javaTypes));
			}
			return javaTypes.stream();
		}

		return null;
	}

	@Override
	protected JvmDeclaredType doInferTypeSceleton(
			XtendTypeDeclaration declaration,
			IJvmDeclaredTypeAcceptor acceptor, boolean preIndexingPhase,
			XtendFile xtendFile, List<Runnable> doLater) {
		if (Strings.isNullOrEmpty(declaration.getName())) {
			return null;
		}
		try {
			// Autowrap the provided runnable elements in order to avoid the internal exceptions
			// to stop the JVM generation too early.
			final var doLaterExceptionSafe = wrapDoLaterList(doLater);
			if (declaration instanceof SarlAgent sarlAgent) {
				final var javaType = this.typesFactory.createJvmGenericType();
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> initialize(sarlAgent, javaType));
				}
				return javaType;
			}
			if (declaration instanceof SarlBehavior sarlBehavior) {
				final var javaType = this.typesFactory.createJvmGenericType();
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> initialize(sarlBehavior, javaType));
				}
				return javaType;
			}
			if (declaration instanceof SarlEvent sarlEvent) {
				final var javaType = this.typesFactory.createJvmGenericType();
				copyTypeParameters(sarlEvent.getTypeParameters(), javaType);
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> initialize(sarlEvent, javaType));
				}
				return javaType;
			}
			if (declaration instanceof SarlSkill sarlSkill) {
				final var javaType = this.typesFactory.createJvmGenericType();
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> initialize(sarlSkill, javaType));
				}
				return javaType;
			}
			if (declaration instanceof SarlCapacity sarlCapacity) {
				final var javaType = this.typesFactory.createJvmGenericType();
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> initialize(sarlCapacity, javaType));
				}
				return javaType;
			}
			if (declaration instanceof SarlArtifact sarlArtifact) {
				final var javaType = this.typesFactory.createJvmGenericType();
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> initialize(sarlArtifact, javaType));
				}
				return javaType;
			}

			return super.doInferTypeSceleton(declaration, acceptor, preIndexingPhase,
					xtendFile, doLaterExceptionSafe);
		} catch (InternalError internalError) {
			throw internalError;
		} catch (Exception exception) {
			logInternalError(exception);
			return null;
		}
	}

	@Override
	protected final void addDefaultConstructor(XtendClass source, JvmGenericType target) {
		// This function is called for adding the default constructor in a class.
		// This function does nothing, and should not be call from the SARL model inferrer.
	}

	/** Add the default constructors.
	 *
	 * <p>The default constructors have the same signature as the constructors of the super class.
	 *
	 * <p>This function adds the default constructors if no constructor was already added. This condition
	 * is determined with a call to {@link GenerationContext#hasConstructor()}.
	 *
	 * @param source the SARL element in which no constructor was specified. This SARL element should be
	 *     associated to the {@code target} element.
	 * @param target the JVM type that is receiving the default constructor.
	 * @see GenerationContext#hasConstructor()
	 */
	protected void appendDefaultConstructors(XtendTypeDeclaration source, JvmGenericType target) {
		final var context = getContext(target);
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
								context,
								cvalue,
								target, source, Sets.newTreeSet(),
								JvmVisibility.PUBLIC);
					}
				}
			}
		}
	}

	private void setVisibility(JvmMember jvmMember, XtendMember member) {
		var visibility = member.getVisibility();
		if (visibility == null) {
			visibility = this.defaultVisibilityProvider.getDefaultJvmVisibility(member);
		}
		jvmMember.setVisibility(visibility);
	}

	private static boolean hasAdditionalMembers(AnonymousClass anonymousClass) {
		for (final var member: anonymousClass.getMembers()) {
			if (member instanceof XtendField || (member instanceof XtendFunction && !((XtendFunction) member).isOverride())) {
				return true;
			}
		}
		return false;
	}

	/** {@inheritDoc}.
	 *
	 * <p>The function is overridden in order to interleave the instructions from Xtend and the ones needed
	 * for SARL.
	 */
	@Override
	public void inferLocalClass(AnonymousClass anonymousClass, String localClassName, JvmFeature container) {
		// Issue #356: do not generate if the class has no name.
		assert anonymousClass != null;
		assert container != null;
		if (Strings.isNullOrEmpty(localClassName)) {
			return;
		}
		// Issue #363: do not generate the class if the SARL library is incompatible.
		if (!Utils.isCompatibleSARLLibraryOnClasspath(this.typeReferences, anonymousClass)) {
			return;
		}

		// Create the inner type
		// --- Begin Xtend Part
		try {
			final var inferredJvmType = this.typesFactory.createJvmGenericType();
			inferredJvmType.setSimpleName(localClassName);
			// --- End Xtend Part

			// Issue #1028: Force the "isAnonymous" flag because the capabilities of the Java compiler enables.
			final var isAnonymous = container instanceof JvmConstructor || !hasAdditionalMembers(anonymousClass);
			inferredJvmType.setAnonymous(isAnonymous);

			// --- Begin Xtend Part
			inferredJvmType.setFinal(true);
			setVisibility(inferredJvmType, anonymousClass);
			inferredJvmType.getSuperTypes().add(this.typeBuilder.inferredType(anonymousClass));
			container.getLocalClasses().add(inferredJvmType);
			this.associator.associatePrimary(anonymousClass, inferredJvmType);
			// --- End Xtend Part

			// Create the generation context that is used by the other transformation functions.
			final var parentContext = getContext(
					EcoreUtil2.getContainerOfType(container, JvmType.class));
			final var context = openContext(anonymousClass, inferredJvmType, Arrays.asList(
					SarlField.class, SarlConstructor.class, SarlAction.class));
			context.setParentContext(parentContext);
			try {
				// --- Begin Xtend Part
				for (final var member : anonymousClass.getMembers()) {
					if (context.isSupportedMember(member)) {
						transform(member, inferredJvmType, true);
					}
				}

				appendSyntheticDispatchMethods(anonymousClass, inferredJvmType);
				this.nameClashResolver.resolveNameClashes(inferredJvmType);
				// --- End Xtend Part

				// Add SARL synthetic functions
				appendSyntheticDefaultValuedParameterMethods(
						anonymousClass,
						inferredJvmType,
						true,
						context);

				// --- Begin Xtend Part
				final var constructorCall = anonymousClass.getConstructorCall();
				for (final var actualParameter : constructorCall.getArguments()) {
					this.associator.associateLogicalContainer(actualParameter, container);
				}
				// --- End Xtend Part
			} finally {
				closeContext(context);
			}
		} catch (AssertionError | InternalError internalError) {
			throw internalError;
		} catch (Exception exception) {
			logInternalError(exception);
		}
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
		final var context = openContext(source, inferredJvmType, Arrays.asList(
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
			super.initialize(source, inferredJvmType);
			// Override the visibility
			setVisibility(inferredJvmType, source);

			// Change the injectable flag
			context.setInjectable(inferredJvmType.getExtendedClass());

			// Add SARL synthetic functions
			appendSyntheticDefaultValuedParameterMethods(
					source,
					inferredJvmType,
					true,
					context);

			// Add functions dedicated to comparisons (equals, hashCode, etc.)
			appendComparisonFunctions(context, source, inferredJvmType);

			// Add clone functions if the generated type is cloneable
			appendCloneFunctionIfCloneable(context, source, inferredJvmType);

			// Add the default constructors for the behavior, if not already added
			appendDefaultConstructors(source, inferredJvmType);

			// Add serialVersionUID field if the generated type is serializable
			appendSerialNumberIfSerializable(context, source, inferredJvmType);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Add the type of SARL Element
			appendSARLElementType(source, inferredJvmType);

			// Add the type of SARL Element
			appendInjectableAnnotationIfInjectable(inferredJvmType, context);

			// Add the Xbase annotation that avoid JaCoCo to analyze the generate code
			appendXbaseGeneratedAnnotation(inferredJvmType);
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
		final var context = openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlAction.class));
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

			// Standard OOP generation
			super.initialize(source, inferredJvmType);
			// Override the visibility
			setVisibility(inferredJvmType, source);

			// Add SARL synthetic functions
			appendSyntheticDefaultValuedParameterMethods(
					source,
					inferredJvmType,
					true,
					context);

			// Add the @FunctionalInterface
			appendFunctionalInterfaceAnnotation(inferredJvmType);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Add the type of SARL Element
			appendSARLElementType(source, inferredJvmType);

			// Add the type of SARL Element
			appendInjectableAnnotationIfInjectable(inferredJvmType, context);

			// Add the Xbase annotation that avoid JaCoCo to analyze the generate code
			appendXbaseGeneratedAnnotation(inferredJvmType);
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
		final var context = openContext(source, inferredJvmType,
				Collections.singleton(SarlEnumLiteral.class));
		try {
			// Standard OOP generation
			super.initialize(source, inferredJvmType);
			// Override the visibility
			setVisibility(inferredJvmType, source);

			// Add SARL synthetic functions
			appendSyntheticDefaultValuedParameterMethods(
					source,
					inferredJvmType,
					true,
					context);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Add the type of SARL Element
			appendSARLElementType(source, inferredJvmType);

			// Add the type of SARL Element
			appendInjectableAnnotationIfInjectable(inferredJvmType, context);

			// Add the Xbase annotation that avoid JaCoCo to analyze the generate code
			appendXbaseGeneratedAnnotation(inferredJvmType);
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
		final var context = openContext(source, inferredJvmType,
				Collections.singleton(SarlField.class));
		try {
			// Standard OOP generation
			super.initialize(source, inferredJvmType);
			// Override the visibility
			setVisibility(inferredJvmType, source);

			// Add SARL synthetic functions
			appendSyntheticDefaultValuedParameterMethods(
					source,
					inferredJvmType,
					true,
					context);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Add the type of SARL Element
			appendSARLElementType(source, inferredJvmType);

			// Add the type of SARL Element
			appendInjectableAnnotationIfInjectable(inferredJvmType, context);

			// Add the Xbase annotation that avoid JaCoCo to analyze the generate code
			appendXbaseGeneratedAnnotation(inferredJvmType);
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
		final var context = openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlConstructor.class, SarlAction.class,
				SarlBehaviorUnit.class, SarlCapacityUses.class, SarlRequiredCapacity.class));
		try {
			// Initialize the context with inheriting features
			Utils.populateInheritanceContext(
					inferredJvmType,
					source.getExtends(),
					null,
					context.getInheritedFinalOperations(),
					context.getInheritedOverridableOperations(),
					null,
					context.getInheritedOperationsToImplement(),
					null,
					this.sarlSignatureProvider);

			// Copy the documentation
			this.typeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			inferredJvmType.setStatic(false);
			inferredJvmType.setStrictFloatingPoint(false);
			setVisibility(inferredJvmType, source);
			final var isAbstract = source.isAbstract() || Utils.hasAbstractMember(source);
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

			// Change the injectable flag
			context.setInjectable(inferredJvmType.getExtendedClass());

			// Add functions dedicated to comparisons (equals, hashCode, etc.)
			appendComparisonFunctions(context, source, inferredJvmType);

			// Add clone functions if the generated type is cloneable
			appendCloneFunctionIfCloneable(context, source, inferredJvmType);

			// Add the default constructors for the behavior, if not already added
			appendDefaultConstructors(source, inferredJvmType);

			// Add serialVersionUID field if the generated type is serializable
			appendSerialNumberIfSerializable(context, source, inferredJvmType);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Add the type of SARL Element
			appendSARLElementType(source, inferredJvmType);

			// Add the type of SARL Element
			appendInjectableAnnotationIfInjectable(inferredJvmType, context);

			// Add the Xbase annotation that avoid JaCoCo to analyze the generate code
			appendXbaseGeneratedAnnotation(inferredJvmType);

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
		final var context = openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlConstructor.class, SarlAction.class,
				SarlBehaviorUnit.class, SarlCapacityUses.class, SarlRequiredCapacity.class));
		try {
			// Initialize the context with inheriting features
			Utils.populateInheritanceContext(
					inferredJvmType,
					source.getExtends(),
					null,
					context.getInheritedFinalOperations(),
					context.getInheritedOverridableOperations(),
					null,
					context.getInheritedOperationsToImplement(),
					null,
					this.sarlSignatureProvider);

			// Copy the documentation
			this.typeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			setVisibility(inferredJvmType, source);
			inferredJvmType.setStatic(false);
			final var isAbstract = source.isAbstract() || Utils.hasAbstractMember(source);
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

			// Change the injectable flag
			context.setInjectable(inferredJvmType.getExtendedClass());

			// Add functions dedicated to comparisons (equals, hashCode, etc.)
			appendComparisonFunctions(context, source, inferredJvmType);

			// Add clone functions if the generated type is cloneable
			appendCloneFunctionIfCloneable(context, source, inferredJvmType);

			// Add the default constructors for the behavior, if not already added
			appendDefaultConstructors(source, inferredJvmType);

			// Add serialVersionUID field if the generated type is serializable
			appendSerialNumberIfSerializable(context, source, inferredJvmType);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Add the type of SARL Element
			appendSARLElementType(source, inferredJvmType);

			// Add the type of SARL Element
			appendInjectableAnnotationIfInjectable(inferredJvmType, context);

			// Add the Xbase annotation that avoid JaCoCo to analyze the generate code
			appendXbaseGeneratedAnnotation(inferredJvmType);

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
		final var context = openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlConstructor.class));
		try {
			// Initialize the context with inheriting features
			Utils.populateInheritanceContext(
					inferredJvmType,
					source.getExtends(),
					null,
					context.getInheritedFinalOperations(),
					context.getInheritedOverridableOperations(),
					null,
					context.getInheritedOperationsToImplement(),
					null,
					this.sarlSignatureProvider);

			// Copy the documentation
			this.typeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			setVisibility(inferredJvmType, source);
			inferredJvmType.setStatic(false);
			final var isAbstract = source.isAbstract() || Utils.hasAbstractMember(source);
			inferredJvmType.setAbstract(isAbstract);
			inferredJvmType.setStrictFloatingPoint(false);
			inferredJvmType.setFinal(!isAbstract && source.isFinal());

			// Generate the annotations.
			translateAnnotationsTo(source.getAnnotations(), inferredJvmType);

			// Generate the extended types.
			appendConstrainedExtends(context, inferredJvmType, Event.class, SarlEvent.class, source.getExtends());

			// Fixing the bounds of the type parameters
			fixTypeParameters(inferredJvmType);
			
			// Issue #363: do not generate the event if the SARL library is incompatible.
			if (Utils.isCompatibleSARLLibraryOnClasspath(this.typeReferences, source)) {
				// Generate the members of the generated type.
				appendAOPMembers(
						inferredJvmType,
						source,
						context);
			}
		
			// Issue #902: Hidden function "matchesTypeBounds"
			if (!source.getTypeParameters().isEmpty()) {
				final var typeBoundMatchOperation = this.typesFactory.createJvmOperation();
				typeBoundMatchOperation.setAbstract(false);
				typeBoundMatchOperation.setNative(false);
				typeBoundMatchOperation.setSynchronized(false);
				typeBoundMatchOperation.setStrictFloatingPoint(false);
				typeBoundMatchOperation.setFinal(false);
				typeBoundMatchOperation.setVisibility(JvmVisibility.PUBLIC);
				typeBoundMatchOperation.setStatic(true);
				typeBoundMatchOperation.setSimpleName(SarlUtils.HIDDEN_MEMBER_CHARACTER + "matchesTypeBounds"); //$NON-NLS-1$
				typeBoundMatchOperation.setReturnType(this._typeReferenceBuilder.typeRef(Boolean.TYPE));
				// Add to container
				inferredJvmType.getMembers().add(typeBoundMatchOperation);
				this.associator.associate(source, typeBoundMatchOperation);
				// First parameter: it
				var jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName(this.grammarKeywordAccess.getItKeyword());
				jvmParam.setParameterType(this.typeReferences.createTypeRef(inferredJvmType));
				this.associator.associate(source, jvmParam);
				typeBoundMatchOperation.getParameters().add(jvmParam);
				// Rest of parameters: bounds
				jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName("bounds"); //$NON-NLS-1$
				jvmParam.setParameterType(this.typeReferences.createArrayType(
						this.typeReferences.getTypeForName(Class.class, source)));
				this.associator.associate(source, jvmParam);
				typeBoundMatchOperation.getParameters().add(jvmParam);
				typeBoundMatchOperation.setVarArgs(true);
				// Body
				final var declaredFields = source.getMembers().stream().filter(it0 -> it0 instanceof XtendField).map(it0 -> (XtendField) it0).toList();
				setBody(typeBoundMatchOperation, it -> {
					it.append("if (bounds != null && bounds.length == "); //$NON-NLS-1$
					it.append(Integer.toString(source.getTypeParameters().size()));
					it.append(") {").increaseIndentation(); //$NON-NLS-1$
					var i = 0;
					for (final var parameter : source.getTypeParameters()) {
						final var matchableFields = declaredFields.stream().filter(it0 -> {
							return it0.getType().getIdentifier().equals(parameter.getIdentifier());
						}).toList();
						if (!matchableFields.isEmpty()) {
							it.newLine().append("if ("); //$NON-NLS-1$
							var first = true;
							for (final var matchField : matchableFields) {
								if (first) {
									first = false;
								} else {
									it.append(" || "); //$NON-NLS-1$
								}
								it.append("(it.").append(matchField.getName()).append(" != null && !bounds["); //$NON-NLS-1$ //$NON-NLS-2$
								it.append(Integer.toString(i)).append("].isInstance(it."); //$NON-NLS-1$
								it.append(matchField.getName()).append("))"); //$NON-NLS-1$
							}
							it.append(") {").increaseIndentation().newLine(); //$NON-NLS-1$
							it.append("return false;").decreaseIndentation().newLine(); //$NON-NLS-1$
							it.append("}"); //$NON-NLS-1$
						}
						++i;
					}
					it.newLine().append("return true;"); //$NON-NLS-1$
					it.decreaseIndentation().newLine().append("}").newLine(); //$NON-NLS-1$
					it.append("return false;"); //$NON-NLS-1$
				});
				// Annotations
				appendGeneratedAnnotation(typeBoundMatchOperation, context);
				addAnnotationSafe(typeBoundMatchOperation, Pure.class);
			}

			// Change the injectable flag
			context.setInjectable(inferredJvmType.getExtendedClass());

			// Add the default constructors for the behavior, if not already added
			appendDefaultConstructors(source, inferredJvmType);

			// Add functions dedicated to comparisons (equals, hashCode, etc.)
			appendComparisonFunctions(context, source, inferredJvmType);

			// Add functions dedicated to String representation(toString, etc.)
			appendToStringFunctions(context, source, inferredJvmType);

			// Add clone functions if the generated type is cloneable
			appendCloneFunctionIfCloneable(context, source, inferredJvmType);

			// Add serialVersionUID field if the generated type is serializable
			appendSerialNumberIfSerializable(context, source, inferredJvmType);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Add the type of SARL Element
			appendSARLElementType(source, inferredJvmType);

			// Add the type of SARL Element
			appendInjectableAnnotationIfInjectable(inferredJvmType, context);

			// Add the Xbase annotation that avoid JaCoCo to analyze the generate code
			appendXbaseGeneratedAnnotation(inferredJvmType);

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
		final var context = openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlConstructor.class, SarlAction.class, SarlBehaviorUnit.class,
				SarlCapacityUses.class, SarlRequiredCapacity.class));
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

			// Copy the documentation
			this.typeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			setVisibility(inferredJvmType, source);
			inferredJvmType.setStatic(false);
			final var isAbstract = source.isAbstract() || Utils.hasAbstractMember(source);
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

			// Change the injectable flag
			context.setInjectable(inferredJvmType.getExtendedClass());

			// Add functions dedicated to comparisons (equals, hashCode, etc.)
			appendComparisonFunctions(context, source, inferredJvmType);

			// Add clone functions if the generated type is cloneable
			appendCloneFunctionIfCloneable(context, source, inferredJvmType);

			// Add the default constructors for the behavior, if not already added
			appendDefaultConstructors(source, inferredJvmType);

			// Add serialVersionUID field if the generated type is serializable
			appendSerialNumberIfSerializable(context, source, inferredJvmType);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(context, source, inferredJvmType);

			// Add the type of SARL Element
			appendSARLElementType(source, inferredJvmType);

			// Add the type of SARL Element
			appendInjectableAnnotationIfInjectable(inferredJvmType, context);

			// Add the Xbase annotation that avoid JaCoCo to analyze the generate code
			appendXbaseGeneratedAnnotation(inferredJvmType);

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
		final var context = openContext(source, inferredJvmType,
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
			this.typeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			inferredJvmType.setInterface(true);
			inferredJvmType.setAbstract(true);
			setVisibility(inferredJvmType, source);
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

			// Add the type of SARL Element
			appendSARLElementType(source, inferredJvmType);

			// Add the type of SARL Element
			appendInjectableAnnotationIfInjectable(inferredJvmType, context);

			// Add the Xbase annotation that avoid JaCoCo to analyze the generate code
			appendXbaseGeneratedAnnotation(inferredJvmType);

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			closeContext(context);
		}

		// Generate the context aware wrapper
		appendCapacityContextAwareWrapper(source, inferredJvmType);
	}

	/** Prepare the initialization of the space types.
	 *
	 * @param source the source.
	 * @param inferredJvmTypes the factory for the JVM types.
	 * @since 0.15
	 */
	protected void initialize(SarlSpace source, JvmGenericTypeFactory inferredJvmTypes) {
		//
	}

	/** Initialize the SARL space type.
	 *
	 * @param source the source.
	 * @param inferredJvmTypes the created JVM types.
	 */
	@SuppressWarnings("static-method")
	protected void initialize(SarlSpace source, JvmGenericTypeProvider inferredJvmTypes) {
		// Issue #356: do not generate if the space has no name.
		assert source != null;
		assert inferredJvmTypes != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
	}

	/** Initialize the SARL artifact type.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 */
	@SuppressWarnings("static-method")
	protected void initialize(SarlArtifact source, JvmGenericType inferredJvmType) {
		// Issue #356: do not generate if the space has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
	}
	
	/** Prepare the initialization of the protocol types.
	 *
	 * @param source the source.
	 * @param inferredJvmTypes the factory for the JVM types.
	 * @since 0.15
	 */
	@SuppressWarnings("static-method")
	protected void initialize(SarlProtocol source, JvmGenericTypeFactory inferredJvmTypes) {
		inferredJvmTypes
			.createReceiver(0, source.getName())
			.createReceiver(1, source.getName() + "Impl"); //$NON-NLS-1$
	}

	/** Initialize the SARL protocol types.
	 *
	 * @param source the source.
	 * @param inferredJvmTypes the accessor to the created JVM types.
	 * @since 0.14
	 */
	protected void initialize(SarlProtocol source, JvmGenericTypeProvider inferredJvmTypes) {
		// Issue #356: do not generate if the space has no name.
		assert source != null;
		assert inferredJvmTypes != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}

		// First type
		
		// Create the generation context that is used by the other transformation functions.
		final var type0 = inferredJvmTypes.getGenericType(0);
		final var context0 = openContext(source, type0, Collections.singleton(SarlField.class));
		try {
			type0.setInterface(true);
			type0.setAbstract(false);
			setVisibility(type0, source);
			type0.setStatic(false);
			type0.setStrictFloatingPoint(false);
			type0.setFinal(false);
			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(type0);
		} finally {
			closeContext(context0);
		}

		// Second type
		
		// Create the generation context that is used by the other transformation functions.
		final var type1 = inferredJvmTypes.getGenericType(1);
		final var context1 = openContext(source, type1, Collections.singleton(SarlField.class));
		try {
			type1.setInterface(false);
			type1.setAbstract(false);
			setVisibility(type1, source);
			type1.setStatic(false);
			type1.setStrictFloatingPoint(false);
			type1.setFinal(false);
			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(type0);
		} finally {
			closeContext(context1);
		}
	}

	@Override
	protected void transform(XtendMember sourceMember,
			JvmGenericType container, boolean allowDispatch) {
		try {
			if (sourceMember instanceof SarlBehaviorUnit cvalue) {
				transform(cvalue, container);
			} else if (sourceMember instanceof SarlCapacityUses cvalue) {
				transform(cvalue, container);
			} else if (sourceMember instanceof SarlRequiredCapacity cvalue) {
				transform(cvalue, container);
			} else {
				super.transform(sourceMember, container, allowDispatch);
			}
		} catch (InternalError internalError) {
			throw internalError;
		} catch (Exception exception) {
			logInternalError(exception);
		}
	}

	/** Transform the constructor.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 */
	@Override
	protected void transform(final XtendConstructor source, final JvmGenericType container) {
		final var constructorName = container.getSimpleName();

		// Special case: static constructor
		if (source.isStatic()) {
			final var staticConstructor = this.typesFactory.createJvmOperation();
			container.getMembers().add(staticConstructor);
			this.associator.associatePrimary(source, staticConstructor);
			staticConstructor.setSimpleName(Utils.getStaticConstructorName());
			staticConstructor.setVisibility(JvmVisibility.PRIVATE);
			staticConstructor.setStatic(true);
			staticConstructor.setReturnType(this._typeReferenceBuilder.typeRef(Void.TYPE));
			setBody(staticConstructor, source.getExpression());
			copyAndCleanDocumentationTo(source, staticConstructor);
			return;
		}

		final var context = getContext(container);

		assert context != null;

		final var isVarArgs = Utils.isVarArg(source.getParameters());

		// Generate the unique identifier of the constructor.
		final var actionKey = this.sarlSignatureProvider.createConstructorQualifiedName(container);

		// Generate all the constructor signatures related to the constructor to create.
		final var constructorSignatures = this.sarlSignatureProvider.createPrototypeFromSarlModel(
				context.getActionPrototypeContext(this.sarlSignatureProvider),
				actionKey,
				Utils.isVarArg(source.getParameters()), source.getParameters());

		// Generate the main Java constructor.
		final var constructor = this.typesFactory.createJvmConstructor();
		container.getMembers().add(constructor);
		this.associator.associatePrimary(source, constructor);
		constructor.setSimpleName(constructorName);
		setVisibility(constructor, source);
		constructor.setVarArgs(isVarArgs);

		// Generate the parameters
		final var paramList = constructorSignatures.getOriginalParameterTypes();
		translateSarlFormalParameters(
				context,
				constructor, container, isVarArgs,
				source.getParameters(), false, paramList, false);

		// Generate additional information (type parameters, exceptions...)
		copyAndFixTypeParameters(source.getTypeParameters(), constructor);
		for (final var exception : source.getExceptions()) {
			constructor.getExceptions().add(this.typeBuilder.cloneWithProxies(exception));
		}
		translateAnnotationsTo(source.getAnnotations(), constructor);

		// Set the body.
		setBody(constructor, source.getExpression());

		// The signature definition of the constructor.
		final var sigKey = this.sarlSignatureProvider.createParameterTypesFromJvmModel(
				isVarArgs, constructor.getParameters());

		// Update the list of generated constructors
		context.getGeneratedConstructors().put(sigKey, constructor);

		copyAndCleanDocumentationTo(source, constructor);

		final Runnable differedGeneration  = () -> {
			// Generate the Java functions that correspond to the action with the parameter default values applied.
			for (final var entry : constructorSignatures.getInferredParameterTypes().entrySet()) {
				if (!context.getGeneratedConstructors().containsKey(entry.getKey())) {
					final var otherSignature = entry.getValue();
					// Generate the additional constructor that is invoke the main constructor previously generated.
					translateSarlFormalParametersForLocalHiddenDefaultValues(context, null);
					final var constructor2 = SARLJvmModelInferrer.this.typesFactory.createJvmConstructor();
					container.getMembers().add(constructor2);
					copyAndCleanDocumentationTo(source, constructor2);
					constructor2.setSimpleName(container.getSimpleName());
					constructor2.setVisibility(constructor.getVisibility());
					constructor2.setVarArgs(isVarArgs);

					final var args = translateSarlFormalParametersForSyntheticOperation(
							constructor2, container, isVarArgs, otherSignature);

					addAnnotationSafe(
							constructor2, DefaultValueUse.class,
							constructorSignatures.getFormalParameterTypes().toString());
					appendGeneratedAnnotation(constructor2, context);

					setBody(constructor2, toStringConcatenation(
							"this(" //$NON-NLS-1$
							+ IterableExtensions.join(args, ", ") //$NON-NLS-1$
							+ ");")); //$NON-NLS-1$

					// Update the list of the generated constructors.
					context.getGeneratedConstructors().put(entry.getKey(), constructor2);
				}
			}
		};

		context.getPreFinalizationElements().add(differedGeneration);
		context.setActionIndex(context.getActionIndex() + 1);
		context.incrementSerial(sigKey.hashCode());
		context.setInjectable(constructor);
	}

	/** Transform the field.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 */
	@Override
	protected void transform(XtendField source, JvmGenericType container) {
		super.transform(source, container);
		// Override the visibility
		final var field = (JvmField) this.sarlAssociations.getPrimaryJvmElement(source);
		if (field != null) {
			setVisibility(field, source);
	
			final var context = getContext(container);
			if (context != null) {
				final var name = source.getName();
				if (!Strings.isNullOrEmpty(name)) {
					context.incrementSerial(name.hashCode());
				}
				final var type = source.getType();
				if (type != null) {
					context.incrementSerial(type.getIdentifier().hashCode());
				}
				context.setInjectable(field);
			}
		}
	}

	/** Transform the function.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 * @param allowDispatch indicates if dispatch function is allowed in the context.
	 */
	@Override
	protected void transform(final XtendFunction source, final JvmGenericType container, boolean allowDispatch) {
		final var context = getContext(container);
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
				translateMainFunction(context, source, container);
			} else {
				translateRegularFunction(context, source, container, allowDispatch);
			}			
		}
	}

	/** Transform a regular function.
	 *
	 * @param context the generation context.
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 * @since 0.15
	 */
	@SuppressWarnings("null")
	protected void translateMainFunction(final GenerationContext context, final XtendFunction source, final JvmGenericType container) {
		final var functionName = Utils.getNameForJavaMainFunction();

		// Create the main function
		final var operation = this.typesFactory.createJvmOperation();
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
		operation.setReturnType(this.typeReferences.getTypeForName(void.class, container));

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
		final var jvmParamArgs = this.typesFactory.createJvmFormalParameter();
		final String jvmParamArgsName;
		if (isValidParameter) {
			jvmParamArgsName = source.getParameters().get(0).getName();
		} else {
			jvmParamArgsName = this.grammarKeywordAccess.getItKeyword();
		}
		jvmParamArgs.setName(jvmParamArgsName);
		final var stringType = this.typeReferences.getTypeForName(String.class, container);
		jvmParamArgs.setParameterType(this.typeReferences.createArrayType(stringType));
		operation.getParameters().add(jvmParamArgs);

		// Exceptions
		for (final var exception : source.getExceptions()) {
			operation.getExceptions().add(this.typeBuilder.cloneWithProxies(exception));
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
			setBody(operation, source.getExpression());
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
				final var operation2 = SARLJvmModelInferrer.this.typesFactory.createJvmOperation();
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
					operation2.getExceptions().add(SARLJvmModelInferrer.this.typeBuilder
							.cloneWithProxies(exception));
				}

				final JvmTypeReference returnType2;
				if (operationReturnType instanceof XComputedTypeReference) {
					returnType2 = this.typeReferences.createDelegateTypeReference(operationReturnType);
				} else {
					returnType2 = cloneWithTypeParametersAndProxies(operationReturnType, operation2);
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
						oparams);

				setBody(operation2, source.getExpression());

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
							final var clone = SARLJvmModelInferrer.this._annotationTypesBuilder
									.annotationRef(id);
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
		translateAnnotationsTo(source.getAnnotations(), operation);
		if (!isValidParameter || !isValidReturnType) {
			appendGeneratedAnnotation(operation, context);
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
	 * @since 0.15
	 */
	protected void translateRegularFunction(final GenerationContext context, final XtendFunction source, final JvmGenericType container, boolean allowDispatch) {
		final var originalFunctionName = source.getName();
		final var sourceNameBuffer = new StringBuilder(originalFunctionName);
		if (allowDispatch && source.isDispatch()) {
			sourceNameBuffer.insert(0, "_"); //$NON-NLS-1$
		}
		final var sourceName = sourceNameBuffer.toString();

		// Create the principal function
		final var operation = this.typesFactory.createJvmOperation();
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
		copyAndFixTypeParameters(source.getTypeParameters(), operation);

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
				true);

		// Get the super function
		final var inheritedOperation = context.getInheritedOperation(actSigKey);

		// Infer the return type
		final var selectedReturnType = inferFunctionReturnType(source, operation, inheritedOperation);
		operation.setReturnType(selectedReturnType);

		// Exceptions
		for (final var exception : source.getExceptions()) {
			operation.getExceptions().add(this.typeBuilder.cloneWithProxies(exception));
		}

		// Add the body
		if (enableFunctionBody) {
			setBody(operation, source.getExpression());
		}

		// User Annotations
		translateAnnotationsTo(source.getAnnotations(), operation);

		// Add @Inline annotation
		if (context.getGeneratorConfig2().isGenerateInlineAnnotation()
				&& !source.isAbstract() && !container.isInterface()
				&& (source.isStatic() || source.isFinal() || container.isFinal())
				&& context.getParentContext() == null
				&& this.annotationFinder.findAnnotation(operation, Inline.class) == null) {
			context.getPostFinalizationElements().add(
					() -> this.inlineExpressionCompiler.appendInlineAnnotation(operation, source));
		}

		// Add @Override annotation
		if (source.isOverride()
				&& this.annotationFinder.findAnnotation(operation, Override.class) == null
				&& this.typeReferences.findDeclaredType(Override.class, source) != null) {
			addAnnotationSafe(operation, Override.class);
		}

		// Add @Pure annotation
		final boolean addDynamicPureAnnotationGenerator;
		final var hasExplicitPureAnnotation = this.annotationFinder.findAnnotation(operation, Pure.class) != null;
		if (!hasExplicitPureAnnotation 
				&& context.getGeneratorConfig2().isGeneratePureAnnotation()
				&& this.typeReferences.findDeclaredType(Pure.class, source) != null) {
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
				addAnnotationSafe(operation, EarlyExit.class);
			}
		} else {
			firedEvents = Collections.emptyList();
			isEarlyExit = false;
		}

		// Put the fired SARL events as Java annotations for being usable by the SARL validator.
		if (!firedEvents.isEmpty()) {
			operation.getAnnotations().add(annotationClassRef(FiredEvent.class, firedEvents));
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
			if (this.annotationFinder.findAnnotation(implementedOperation, DefaultValueSource.class) != null
					&& this.annotationFinder.findAnnotation(operation, DefaultValueSource.class) == null) {
				addAnnotationSafe(operation, DefaultValueSource.class);
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
						&& this.annotationFinder.findAnnotation(cp, DefaultValue.class) == null) {
					addAnnotationSafe(cp,
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
				final var ak = SARLJvmModelInferrer.this.sarlSignatureProvider.createActionPrototype(
						sourceName,
						otherSignature.getKey());
				if (ak != null) {
					final var inheritedOp = context.getInheritedImplementedOperation(ak);
					translateSarlFormalParametersForLocalHiddenDefaultValues(context, inheritedOp);
					final var localOp = context.getLocalOperation(ak);
					if (inheritedOp == null && localOp == null) {
						// Generate the additional constructor that is invoke the main constructor previously generated.
						final var operation2 = SARLJvmModelInferrer.this.typesFactory.createJvmOperation();
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

						copyTypeParametersFromJvmOperation(operation, operation2);

						for (final var exception : operation.getExceptions()) {
							operation2.getExceptions().add(SARLJvmModelInferrer.this.typeBuilder
									.cloneWithProxies(exception));
						}

						final JvmTypeReference returnType2;
						if (selectedReturnType instanceof XComputedTypeReference) {
							returnType2 = this.typeReferences.createDelegateTypeReference(selectedReturnType);
						} else {
							returnType2 = cloneWithTypeParametersAndProxies(selectedReturnType, operation2);
						}
						operation2.setReturnType(returnType2);

						final var args = translateSarlFormalParametersForSyntheticOperation(
								operation2, container, isVarArgs,
								otherSignature.getValue());

						operation2.setDefault(container.isInterface());
						operation2.setAbstract(false);
						setBody(operation2, it -> {
							final var type = operation2.getReturnType();
							if (!SARLJvmModelInferrer.this.typeReferences.is(type, void.class)) {
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
								&& SARLJvmModelInferrer.this.annotationFinder.findAnnotation(operation,
										Override.class) == null
										&& SARLJvmModelInferrer.this.typeReferences.findDeclaredType(
												Override.class, source) != null) {
							addAnnotationSafe(
									operation, Override.class);
						}

						// @DefaultValueUse annotation
						addAnnotationSafe(
								operation2, DefaultValueUse.class,
								actionSignatures.getFormalParameterTypes().toString());
						appendGeneratedAnnotation(operation2, context);

						// @EarlyExit annotation
						// If the main action is an early-exit action, the additional operation
						// is also an early-exit operation.
						if (isEarlyExit) {
							addAnnotationSafe(
									operation2, EarlyExit.class);
						}

						// @FiredEvent annotation
						// Put the fired SARL events as Java annotations for being usable by the SARL validator.
						if (!firedEvents.isEmpty()) {
							operation2.getAnnotations().add(
									annotationClassRef(FiredEvent.class, firedEvents));
						}

						// @Pure annotation
						if (addDynamicPureAnnotationGenerator) {
							this.operationHelper.attachPureAnnotationAdapter(operation2, (op, helper) -> {
								return Boolean.valueOf(helper.isPureOperation(operation));
							});
						} else if (hasExplicitPureAnnotation) {
							addAnnotationSafe(
									operation2, Pure.class);
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
									final var clone = SARLJvmModelInferrer.this._annotationTypesBuilder
											.annotationRef(id);
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

	/** Transform the given behavior unit.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 */
	protected void transform(final SarlBehaviorUnit source, JvmGenericType container) {
		final var context = getContext(container);
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

			final var voidType = this._typeReferenceBuilder.typeRef(Void.TYPE);

			//----------------
			// Body function
			//----------------
			// Name
			final var bodyMethodName = Utils.createNameForHiddenEventHandlerMethod(source.getName(), context.getBehaviorUnitIndex());
			// Operation
			final var bodyOperation = this.typesFactory.createJvmOperation();
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
			var jvmParam = this.typesFactory.createJvmFormalParameter();
			jvmParam.setName(this.grammarKeywordAccess.getOccurrenceKeyword());
			jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(source.getName()));
			this.associator.associate(source, jvmParam);
			bodyOperation.getParameters().add(jvmParam);
			// Body
			setBody(bodyOperation, source.getExpression());
			// Annotations
			translateAnnotationsTo(source.getAnnotations(), bodyOperation);
			if (context.getGeneratorConfig2().isGeneratePureAnnotation()
					&& !this.services.getExpressionHelper().hasSideEffects(source.getExpression())) {
				addAnnotationSafe(bodyOperation, Pure.class);
			}

			final var evaluators = context.ensureGuardEvaluationCodeFor(source, this.typeReferences);
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
				final var guardOperation = this.typesFactory.createJvmOperation();
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
				if (context.getGeneratorConfig2().isGeneratePureAnnotation()) {
					addAnnotationSafe(guardOperation, Pure.class);
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
			logInternalError(Messages.SARLJvmModelInferrer_10);
		}
	}

	/** Transform the uses of SARL capacities.
	 *
	 * <p>Resolving the calls to the capacities' functions is done in {@link io.sarl.lang.typesystem.SARLReentrantTypeResolver}.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 */
	protected void transform(SarlCapacityUses source, JvmGenericType container) {
		final var context = getContext(container);
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
				final var field = this.typesFactory.createJvmField();
				container.getMembers().add(field);
				field.setVisibility(JvmVisibility.PRIVATE);
				field.setSimpleName(fieldName);
				field.setTransient(true);
				final var skillClearableReference = this.typeReferences.getTypeForName(AtomicSkillReference.class, container);
				field.setType(skillClearableReference);

				this.associator.associatePrimary(source, field);

				addAnnotationSafe(field, Extension.class);
				try {
					field.getAnnotations().add(annotationClassRef(ImportedCapacityFeature.class,
							Collections.singletonList(capacityType)));
				} catch (Throwable ex) {
					// Ignore this error
				}
				appendGeneratedAnnotation(field, getContext(container));

				// Generate the calling function
				final var methodName = Utils.createNameForHiddenCapacityImplementationCallingMethodFromFieldName(
						fieldName);
				final var operation = this.typesFactory.createJvmOperation();
				container.getMembers().add(operation);
				operation.setVisibility(JvmVisibility.PRIVATE);
				operation.setReturnType(cloneWithTypeParametersAndProxies(capacityType, operation));
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
				appendGeneratedAnnotation(operation, context);
				if (context.getGeneratorConfig2().isGeneratePureAnnotation()) {
					addAnnotationSafe(operation, Pure.class);
				}

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
	 * @param featureContainerType the feature container.
	 * @param container the SARL container.
	 * @param context description of the generation context in which the members must be considered.
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

		final var delayedMembers = new LinkedList<XtendMember>();

		for (final var feature : container.getMembers()) {
			if (context.isSupportedMember(feature)) {
				if ((feature instanceof SarlCapacityUses)
						|| (feature instanceof SarlRequiredCapacity)) {
					delayedMembers.add(feature);
				} else {
					transform(feature, featureContainerType, true);
				}
			}
		}

		for (final var feature : delayedMembers) {
			transform(feature, featureContainerType, false);
		}

		// Add event handlers
		appendEventGuardEvaluators(featureContainerType);

		// Add dispatch methods
		appendSyntheticDispatchMethods(container, featureContainerType);

		// Add SARL synthetic functions
		appendSyntheticDefaultValuedParameterMethods(
				container,
				featureContainerType,
				true,
				context);
	}

	@Override
	protected JvmOperation deriveGenericDispatchOperationSignature(
			Iterable<JvmOperation> localOperations, JvmGenericType target) {
		final var dispatcher = super.deriveGenericDispatchOperationSignature(localOperations, target);
		//
		// Fixing the behavior for determining the visibility of the dispatcher since
		// it does not fit the SARL requirements.
		//
		var higherVisibility = JvmVisibility.PRIVATE;
		for (final var jvmOperation : localOperations) {
			final var xtendFunctions = Iterables.filter(
					this.sarlAssociations.getSourceElements(jvmOperation), XtendFunction.class);
			for (final var func : xtendFunctions) {
				var visibility = func.getVisibility();
				if (visibility == null) {
					visibility = this.defaultVisibilityProvider.getDefaultJvmVisibility(func);
				}
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
	 * @param source the SARL container.
	 * @param target the JVM feature container.
	 * @param ignoreOverridableOperations indicates if the operations must not be added if they are marked has
	 *     overridable.
	 * @param context description of the generation context in which the members must be considered.
	 */
	protected void appendSyntheticDefaultValuedParameterMethods(
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
					final var op = this.typeBuilder.toMethod(
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
							final var jvmParam = this.typesFactory.createJvmFormalParameter();
							jvmParam.setName(parameter.getName());
							jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(parameter.getType().toTypeReference()));
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
	 * @param javaCodeLines the Java code lines.
	 * @return the client.
	 */
	private static StringConcatenationClient toStringConcatenation(final String... javaCodeLines) {
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(StringConcatenationClient.TargetStringConcatenation builder) {
				for (final var line : javaCodeLines) {
					builder.append(line);
					builder.newLineIfNotEmpty();
				}
			}
		};
	}

	/** Generate the extended types for the given SARL statement.
	 *
	 * @param context the context of the generation.
	 * @param owner the JVM element to change.
	 * @param defaultJvmType the default JVM type.
	 * @param defaultSarlType the default SARL type.
	 * @param supertype the supertype.
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
	 * @param context the context of the generation.
	 * @param owner the JVM element to change.
	 * @param defaultJvmType the default JVM type.
	 * @param defaultSarlType the default Sarl type.
	 * @param supertypes the supertypes.
	 */
	protected void appendConstrainedExtends(
			GenerationContext context,
			JvmGenericType owner, Class<?> defaultJvmType, Class<? extends XtendTypeDeclaration> defaultSarlType,
			List<? extends JvmParameterizedTypeReference> supertypes) {
		boolean explicitType = false;
		final var ownerId = owner.getIdentifier();
		for (final var superType : supertypes) {
			String superTypeId;
			try {
				superTypeId = superType.getIdentifier();
			} catch (Exception ex) {
				logInternalError(ex);
				superTypeId = null;
			}
			if (!Objects.equal(ownerId, superTypeId)
					&& superType.getType() instanceof JvmGenericType
					/*&& this.inheritanceHelper.isProxyOrSubTypeOf(superType, defaultJvmType, defaultSarlType, isInterface)*/) {
				owner.getSuperTypes().add(this.typeBuilder.cloneWithProxies(superType));
				context.incrementSerial(superType.getIdentifier().hashCode());
				explicitType = true;
			}
		}
		if (!explicitType) {
			final var type = this._typeReferenceBuilder.typeRef(defaultJvmType);
			if (!(type instanceof JvmUnknownTypeReference)) {
				owner.getSuperTypes().add(type);
			}
			context.incrementSerial(type.getIdentifier().hashCode());
		}
	}

	/** Generate the implemented types for the given SARL statement.
	 *
	 * @param context the context of the generation.
	 * @param owner the JVM element to change.
	 * @param defaultJvmType the default JVM type.
	 * @param defaultSarlType the default SARL type.
	 * @param implementedtypes the implemented types.
	 */
	protected void appendConstrainedImplements(
			GenerationContext context,
			JvmGenericType owner, Class<?> defaultJvmType,
			Class<? extends XtendTypeDeclaration> defaultSarlType,
			List<? extends JvmParameterizedTypeReference> implementedtypes) {
		var explicitType = false;
		for (final var superType : implementedtypes) {
			if (!Objects.equal(owner.getIdentifier(), superType.getIdentifier())
					&& superType.getType() instanceof JvmGenericType
					/*&& this.inheritanceHelper.isProxyOrSubTypeOf(superType, defaultJvmType, defaultSarlType, true)*/) {
				owner.getSuperTypes().add(this.typeBuilder.cloneWithProxies(superType));
				context.incrementSerial(superType.getIdentifier().hashCode());
				explicitType = true;
			}
		}
		if (!explicitType) {
			final var type = this._typeReferenceBuilder.typeRef(defaultJvmType);
			owner.getSuperTypes().add(type);
			context.incrementSerial(type.getIdentifier().hashCode());
		}
	}

	/** Add the @Generated annotation to the given target.
	 * The annotation will not have any generated SARL code associated to it.
	 *
	 * @param target the target of the annotation.
	 * @param context the generation context.
	 */
	protected final void appendGeneratedAnnotation(JvmAnnotationTarget target, GenerationContext context) {
		appendGeneratedAnnotation(target, context, null);
	}

	/** Add the @Generated annotation to the given target.
	 *
	 * @param target the target of the annotation.
	 * @param context the generation context.
	 * @param sarlCode the code that is the cause of the generation.
	 */
	protected void appendGeneratedAnnotation(JvmAnnotationTarget target, GenerationContext context, String sarlCode) {
		final var config = context.getGeneratorConfig();
		if (config.isGenerateGeneratedAnnotation()) {
			addAnnotationSafe(target, GENERATED_NAME, getClass().getName());
		}

		if (target instanceof JvmFeature) {
			addAnnotationSafe(target, SyntheticMember.class);
		}

		if (!Strings.isNullOrEmpty(sarlCode)) {
			addAnnotationSafe(target, SarlSourceCode.class, sarlCode);
		}
	}

	/** Add the @XbaseGenerated annotation to the given target.
	 *
	 * @param target the target of the annotation.
	 */
	protected void appendXbaseGeneratedAnnotation(JvmAnnotationTarget target) {
		if (target instanceof JvmExecutable || target instanceof JvmDeclaredType) {
			addAnnotationSafe(target, XbaseGenerated.class);
		}
	}

	/** Append the guard evaluators.
	 *
	 * @param container the container type.
	 */
	protected void appendEventGuardEvaluators(JvmGenericType container) {
		final var context = getContext(container);
		if (context != null) {
			final var allEvaluators = context.getGuardEvaluationCodes();
			if (allEvaluators == null || allEvaluators.isEmpty()) {
				return;
			}

			final var guardDefs = new BehaviorUnitDefinitions();

			for (final var evaluators : allEvaluators) {
				final var behName = appendEventGuardEvaluatorForReflectMethod(evaluators, container, context);
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

			appendEventGuardEvaluatorsForPolymorphicMethod(guardDefs, isRootType, container, context);
		}
	}

	/** Append the guard evaluators for the polymorphic method.
	 *
	 * @param guardDefs the definition of the guards.
	 * @param isRootType indicates if the containing type is considered as a root type from the polymorphic method
	 *     point of view.
	 * @param container the receiver of the generated components.
	 * @param context the generation context.
	 * @since 0.12
	 */
	protected void appendEventGuardEvaluatorsForPolymorphicMethod(BehaviorUnitDefinitions guardDefs,
			boolean isRootType, JvmGenericType container, GenerationContext context) {
		final var voidType = this._typeReferenceBuilder.typeRef(Void.TYPE);

		// Function "$getSupportedEvents"
		final var eventTypeOperation = this.typesFactory.createJvmOperation();
		appendGeneratedAnnotation(eventTypeOperation, context);
		addAnnotationSafe(eventTypeOperation, Override.class);

		eventTypeOperation.setAbstract(false);
		eventTypeOperation.setNative(false);
		eventTypeOperation.setSynchronized(false);
		eventTypeOperation.setStrictFloatingPoint(false);
		eventTypeOperation.setFinal(false);
		eventTypeOperation.setVisibility(JvmVisibility.PUBLIC);
		eventTypeOperation.setStatic(false);
		eventTypeOperation.setSimpleName(SarlUtils.HIDDEN_MEMBER_CHARACTER + "getSupportedEvents"); //$NON-NLS-1$
		eventTypeOperation.setReturnType(this.typeBuilder.cloneWithProxies(voidType));

		final var jvmParam0 = this.typesFactory.createJvmFormalParameter();
		jvmParam0.setName("toBeFilled"); //$NON-NLS-1$
		jvmParam0.setParameterType(this._typeReferenceBuilder.typeRef(Set.class,
				this._typeReferenceBuilder.typeRef(Class.class,
						this._typeReferenceBuilder.wildcardExtends(
								this._typeReferenceBuilder.typeRef(Event.class)))));
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
		final var eventSupportOperation = this.typesFactory.createJvmOperation();
		appendGeneratedAnnotation(eventSupportOperation, context);
		addAnnotationSafe(eventSupportOperation, Override.class);

		eventSupportOperation.setAbstract(false);
		eventSupportOperation.setNative(false);
		eventSupportOperation.setSynchronized(false);
		eventSupportOperation.setStrictFloatingPoint(false);
		eventSupportOperation.setFinal(false);
		eventSupportOperation.setVisibility(JvmVisibility.PUBLIC);
		eventSupportOperation.setStatic(false);
		eventSupportOperation.setSimpleName(SarlUtils.HIDDEN_MEMBER_CHARACTER + "isSupportedEvent"); //$NON-NLS-1$
		eventSupportOperation.setReturnType(this._typeReferenceBuilder.typeRef(boolean.class));

		final var jvmParam1 = this.typesFactory.createJvmFormalParameter();
		jvmParam1.setName("event"); //$NON-NLS-1$
		jvmParam1.setParameterType(this._typeReferenceBuilder.typeRef(Class.class,
				this._typeReferenceBuilder.wildcardExtends(
						this._typeReferenceBuilder.typeRef(Event.class))));
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
		final var runnableType = this._typeReferenceBuilder.typeRef(Runnable.class);
		final var collectionType = this._typeReferenceBuilder.typeRef(Collection.class, runnableType);
		final var evaluateOperation = this.typesFactory.createJvmOperation();
		appendGeneratedAnnotation(evaluateOperation, context);
		addAnnotationSafe(evaluateOperation, Override.class);

		evaluateOperation.setAbstract(false);
		evaluateOperation.setNative(false);
		evaluateOperation.setSynchronized(false);
		evaluateOperation.setStrictFloatingPoint(false);
		evaluateOperation.setFinal(false);
		evaluateOperation.setVisibility(JvmVisibility.PUBLIC);
		evaluateOperation.setStatic(false);
		evaluateOperation.setSimpleName(SarlUtils.HIDDEN_MEMBER_CHARACTER + "evaluateBehaviorGuards"); //$NON-NLS-1$
		evaluateOperation.setReturnType(this.typeBuilder.cloneWithProxies(voidType));

		final var jvmParam4 = this.typesFactory.createJvmFormalParameter();
		jvmParam4.setName("eventType"); //$NON-NLS-1$
		jvmParam4.setParameterType(this._typeReferenceBuilder.typeRef(Class.class, this._typeReferenceBuilder.wildcard()));
		evaluateOperation.getParameters().add(jvmParam4);

		final var jvmParam2 = this.typesFactory.createJvmFormalParameter();
		jvmParam2.setName("event"); //$NON-NLS-1$
		jvmParam2.setParameterType(this._typeReferenceBuilder.typeRef(Object.class));
		evaluateOperation.getParameters().add(jvmParam2);

		final var jvmParam3 = this.typesFactory.createJvmFormalParameter();
		jvmParam3.setName("callbacks"); //$NON-NLS-1$
		jvmParam3.setParameterType(this.typeBuilder.cloneWithProxies(collectionType));
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

	/** Append the guard evaluators for the reflection-based method.
	 *
	 * @param evaluators the guard evaluators to generate.
	 * @param container the receiver of the generated components.
	 * @param context the generation context.
	 * @return the name of the generated function.
	 * @since 0.12
	 */
	protected String appendEventGuardEvaluatorForReflectMethod(
			BehaviorUnitGuardEvaluators evaluators,
			JvmGenericType container, GenerationContext context) {
		final var source = evaluators.source();
		final var sourceId = evaluators.eventType();
		final var voidType = this._typeReferenceBuilder.typeRef(Void.TYPE);
		final var runnableType = this._typeReferenceBuilder.typeRef(Runnable.class);
		final var collectionType = this._typeReferenceBuilder.typeRef(Collection.class, runnableType);

		// Force the event type to be expressed in its raw form in the guard function's prototype
		final var rawEventId = sourceId.getType();
		final var rawEventReference = this.typeReferences.createTypeRef(rawEventId);
		rawEventReference.getArguments().clear();

		// Determine the name of the operation for the behavior output
		final var behaviorUnitName = Utils.createNameForHiddenGuardGeneralEvaluatorMethod(sourceId);

		// Create the main function
		final var operation = this.typesFactory.createJvmOperation();

		// Annotation for the event bus

		appendGeneratedAnnotation(operation, context);
		final var guardAnnotation = addAnnotationSafe(operation, PerceptGuardEvaluator.class);
		if (guardAnnotation != null) {
			final var bounds  = Utils.getTypeParameterBoundsFor(sourceId, this.typeReferences);
			if (bounds != null && !bounds.isEmpty()) {
				JvmOperation boundOperation = null;
				for (final var guardAnnotationOperation : guardAnnotation.getAnnotation().getDeclaredOperations()) {
					if ("typeParameters".equals(guardAnnotationOperation.getSimpleName())) { //$NON-NLS-1$
						boundOperation = guardAnnotationOperation;
						break;
					}
				}
				final var generics = this.typesFactory.createJvmTypeAnnotationValue();
				for (final var boundType : bounds) {
					generics.getValues().add(this.typeBuilder.cloneWithProxies(boundType));
				}
				generics.setOperation(boundOperation);
				guardAnnotation.getExplicitValues().add(generics);
			}
		}

		// Guard evaluator unit parameters
		// - Event occurrence
		var jvmParam = this.typesFactory.createJvmFormalParameter();
		jvmParam.setName(this.grammarKeywordAccess.getOccurrenceKeyword());
		jvmParam.setParameterType(rawEventReference);
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
		operation.setSimpleName(behaviorUnitName);
		operation.setReturnType(this.typeBuilder.cloneWithProxies(voidType));
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
		this.typeBuilder.copyDocumentationTo(source, operation);

		return behaviorUnitName;
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

	private boolean isAppendComparisonFunctionsEnable(GenerationContext context, JvmGenericType target) {
		if (context.getGeneratorConfig2().isGenerateEqualityTestFunctions()) {
			var current = target;
			final var encounteredTypes = new TreeSet<String>();
			do {
				encounteredTypes.add(current.getIdentifier());
				if (this.annotationFinder.findAnnotation(current, NoEqualityTestFunctionsGeneration.class) != null) {
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

	private static boolean isAppendToStringFunctionsEnable(GenerationContext context) {
		return context.getGeneratorConfig2().isGenerateToStringFunctions();
	}

	private static boolean isAppendCloneFunctionsEnable(GenerationContext context) {
		return context.getGeneratorConfig2().isGenerateCloneFunctions();
	}

	private static boolean isAppendSerialNumbersEnable(GenerationContext context) {
		return context.getGeneratorConfig2().isGenerateSerialNumberFields();
	}

	/** Create the functions that permits to compare the object.
	 * The comparaison functions are {@link #equals(Object)} and {@link #hashCode()}.
	 *
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	protected void appendComparisonFunctions(GenerationContext context, XtendTypeDeclaration source,
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
							final var op = toEqualsMethod(source, target, declaredInstanceFields,
									context.getGeneratorConfig2().isGeneratePureAnnotation());
							if (op != null) {
								appendGeneratedAnnotation(op, context);
								target.getMembers().add(op);
							}
						}

						if (!isHashCodeUserDefined) {
							final var op = toHashCodeMethod(source, declaredInstanceFields,
									context.getGeneratorConfig2().isGeneratePureAnnotation());
							if (op != null) {
								appendGeneratedAnnotation(op, context);
								target.getMembers().add(op);
							}
						}
					}
				}
			}
		}
	}

	/** Create the functions that are related to the {@code toString} function.
	 *
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	protected void appendToStringFunctions(GenerationContext context, XtendTypeDeclaration source,
			final JvmGenericType target) {
		if (!isAppendToStringFunctionsEnable(context)) {
			return;
		}
		// Create a list of the declared non-static fields.
		final var declaredInstanceFields = new ArrayList<JvmField>();
		for (final var field : target.getDeclaredFields()) {
			if (!field.isStatic()) {
				declaredInstanceFields.add(field);
			}
		}

		if (!declaredInstanceFields.isEmpty()) {
			final var voidType = this._typeReferenceBuilder.typeRef(Void.TYPE);
			final var op = SARLJvmModelInferrer.this.typeBuilder.toMethod(
					source,
					"toString", //$NON-NLS-1$
					voidType, it2 -> {
						it2.setVisibility(JvmVisibility.PROTECTED);
						SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it2,
								MessageFormat.format(Messages.SARLJvmModelInferrer_2,
										target.getSimpleName()));
						final var param = this.typesFactory.createJvmFormalParameter();
						param.setName("builder"); //$NON-NLS-1$
						param.setParameterType(SARLJvmModelInferrer.this._typeReferenceBuilder.typeRef(ToStringBuilder.class));
						it2.getParameters().add(param);
						setBody(it2, it3 -> {
							it3.append("super.toString(builder);"); //$NON-NLS-1$
							for (final var attr : declaredInstanceFields) {
								it3.newLine();
								it3.append("builder.add(\"" + attr.getSimpleName() //$NON-NLS-1$
								+ "\", this." //$NON-NLS-1$
								+ attr.getSimpleName() + ");"); //$NON-NLS-1$
							}
						});
					});
			if (op != null) {
				appendGeneratedAnnotation(op, context);
				if (context.getGeneratorConfig2().isGeneratePureAnnotation()) {
					addAnnotationSafe(op, Pure.class);
				}
				target.getMembers().add(op);
			}
		}
	}

	/** Append the serial number field.
	 *
	 * <p>The serial number field is computed from the given context and from the generated fields.
	 * The field is added if no field with name "serialVersionUID" was defined.
	 *
	 * <p>This function does not test if the field container is serializable.
	 *
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 * @see #appendSerialNumberIfSerializable(GenerationContext, XtendTypeDeclaration, JvmGenericType)
	 */
	protected void appendSerialNumber(GenerationContext context, XtendTypeDeclaration source, JvmGenericType target) {
		if (!isAppendSerialNumbersEnable(context)) {
			return;
		}
		for (final var field : target.getDeclaredFields()) {
			if (SERIAL_FIELD_NAME.equals(field.getSimpleName())) {
				return;
			}
		}

		final var field = this.typesFactory.createJvmField();
		field.setSimpleName(SERIAL_FIELD_NAME);
		field.setVisibility(JvmVisibility.PRIVATE);
		field.setStatic(true);
		field.setTransient(false);
		field.setVolatile(false);
		field.setFinal(true);
		target.getMembers().add(field);
		field.setType(this.typeBuilder.cloneWithProxies(this._typeReferenceBuilder.typeRef(long.class)));
		final var serial = context.getSerial();
		this.typeBuilder.setInitializer(field, toStringConcatenation(serial + "L")); //$NON-NLS-1$
		appendGeneratedAnnotation(field, context);
		this.readAndWriteTracking.markInitialized(field, null);
	}

	/** Append the serial number field if and only if the container type is serializable.
	 *
	 * <p>The serial number field is computed from the given context and from the generated fields.
	 *
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 * @see #appendSerialNumber(GenerationContext, XtendTypeDeclaration, JvmGenericType)
	 */
	protected void appendSerialNumberIfSerializable(GenerationContext context, XtendTypeDeclaration source, JvmGenericType target) {
		if (!target.isInterface() && this.inheritanceHelper.isSubTypeOf(target, Serializable.class, null)) {
			appendSerialNumber(context, source, target);
		}
	}

	/** Append the clone function.
	 *
	 * <p>The clone function replies a value of the current type, not {@code Object}.
	 *
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 * @since 0.6
	 * @see #appendCloneFunctionIfCloneable(GenerationContext, XtendTypeDeclaration, JvmGenericType)
	 */
	protected void appendCloneFunction(GenerationContext context, XtendTypeDeclaration source, JvmGenericType target) {
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
				genericParameters[i] = this._typeReferenceBuilder.typeRef(typeParameter);
			}
			final var myselfReference = this._typeReferenceBuilder.typeRef(target, genericParameters);
			final var operation = this.typeBuilder.toMethod(
					source, CLONE_FUNCTION_NAME, myselfReference, null);
			target.getMembers().add(operation);
			operation.setVisibility(JvmVisibility.PUBLIC);
			addAnnotationSafe(operation, Override.class);
			if (context.getGeneratorConfig2().isGeneratePureAnnotation()) {
				addAnnotationSafe(operation, Pure.class);
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
			appendGeneratedAnnotation(operation, context);
		}
	}

	/** Append the clone function only if the type is a subtype of {@link Cloneable}.
	 *
	 * <p>The clone function replies a value of the current type, not {@code Object}.
	 *
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 * @since 0.6
	 * @see #appendCloneFunction(GenerationContext, XtendTypeDeclaration, JvmGenericType)
	 */
	protected void appendCloneFunctionIfCloneable(GenerationContext context, XtendTypeDeclaration source, JvmGenericType target) {
		if (!target.isInterface() && this.inheritanceHelper.isSubTypeOf(target, Cloneable.class, null)) {
			appendCloneFunction(context, source, target);
		}
	}

	/** Append the SARL specification version as an annotation to the given container.
	 *
	 * <p>The added annotation may be used by any underground platform for determining what is
	 * the version of the SARL specification that was used for generating the container.
	 *
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	protected void appendSARLSpecificationVersion(GenerationContext context, XtendTypeDeclaration source,
			JvmDeclaredType target) {
		addAnnotationSafe(target, SarlSpecification.class, SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING);
	}

	/** Append the SARL element type as an annotation to the given container.
	 *
	 * <p>The added annotation may be used by any underground platform for determining what is
	 * the type of the SARL element without invoking the costly "instanceof" operations.
	 *
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	protected void appendSARLElementType(XtendTypeDeclaration source, JvmDeclaredType target) {
		addAnnotationSafe(target, SarlElementType.class, source.eClass().getClassifierID());
	}

	/** Append the injectable annotation to the given container.
	 *
	 * @param target the inferred JVM object.
	 * @param context the generation context.
	 * @since 0.12
	 */
	protected void appendInjectableAnnotationIfInjectable(JvmDeclaredType target, GenerationContext context) {
		if (context.isInjectable()) {
			addAnnotationSafe(target, Injectable.class);
		}
	}

	/** Remove the type parameters from the given type.
	 *
	 * <table>
	 * <caption>Criteria for removal</caption>
	 * <thead><tr><th>Referenced type</th><th>Input</th><th>Replied referenced type</th><th>Output</th></tr></thead>
	 * <tbody>
	 * <tr><td>Type with generic type parameter</td><td>{@code T<G>}</td><td>the type itself</td><td>{@code T}</td></tr>
	 * <tr><td>Type without generic type parameter</td><td>{@code T}</td><td>the type itself</td><td>{@code T}</td></tr>
	 * <tr><td>Type parameter without bound</td><td>{@code <S>}</td><td>{@code Object}</td><td>{@code Object}</td></tr>
	 * <tr><td>Type parameter with lower bound</td><td>{@code <S super B>}</td><td>{@code Object}</td><td>{@code Object}</td></tr>
	 * <tr><td>Type parameter with upper bound</td><td>{@code <S extends B>}</td><td>the bound type</td><td>{@code B}</td></tr>
	 * </tbody>
	 * </table>
	 *
	 * @param type the type.
	 * @param context the context in which the reference is located.
	 * @return the same type without the type parameters.
	 */
	protected JvmTypeReference skipTypeParameters(JvmTypeReference type, Notifier context) {
		final var ltr = Utils.toLightweightTypeReference(type, this.services);
		return ltr.getRawTypeReference().toJavaCompliantTypeReference();
	}

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
	 */
	protected void translateSarlFormalParameters(
			GenerationContext context,
			JvmExecutable owner,
			JvmGenericType actionContainer,
			boolean varargs,
			List<? extends XtendParameter> params,
			final boolean isForInterface,
			List<InferredStandardParameter> paramSpec,
			boolean ignoreOverridableOperations) {
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
				translateParameter(owner, param);
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
								final var method = this.typeBuilder.toMethod(defaultValue, functionName, inferredType, it -> {
									SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it,
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

								addAnnotationSafe(method, Pure.class);

								final var rawCode = Utils.getSarlCodeFor(defaultValue);
								appendGeneratedAnnotation(method, context, rawCode);

								setBody(method, defaultValue);

								addAnnotationSafe(createdParam, DefaultValue.class, namePostPart);

								this.associator.associate(defaultValue, method);
							}
						};
						context.addUserObject("translateSarlFormalParameters", creationCode1); //$NON-NLS-1$
					}
				}
			}
		}

		if (hasDefaultValue) {
			addAnnotationSafe(owner, DefaultValueSource.class);
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
	 * @return the arguments to pass to the original function.
	 */
	protected List<String> translateSarlFormalParametersForSyntheticOperation(JvmExecutable owner, JvmGenericType actionContainer,
			boolean varargs, List<InferredStandardParameter> signature) {
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
					final var lastParam = this.typesFactory.createJvmFormalParameter();
					owner.getParameters().add(lastParam);
					lastParam.setName(paramName);
					if (owner instanceof JvmOperation) {
						lastParam.setParameterType(cloneWithTypeParametersAndProxies(paramType.toTypeReference(), owner));
					} else {
						lastParam.setParameterType(this.typeBuilder.cloneWithProxies(paramType.toTypeReference()));
					}
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
		final var excepts = new HashSet<String>();
		for (final var type : exceptions) {
			excepts.add(type.getName());
		}
		final var addition = new ArrayList<JvmAnnotationReference>();
		for (final var annotation : Iterables.filter(annotations, an -> {
			if (!ANNOTATION_TRANSLATION_FILTER.apply(an)) {
				return false;
			}
			return !excepts.contains(an.getAnnotation().getIdentifier());
		})) {
			addition.add(annotation);
		}
		target.getAnnotations().addAll(addition);
	}

	/** Replies the type parameters for the given type.
	 *
	 * @param type the type.
	 * @return the type parameters for the given type.
	 */
	@SuppressWarnings("static-method")
	protected List<JvmTypeParameter> getTypeParametersFor(XtendTypeDeclaration type) {
		if (type instanceof XtendClass cvalue) {
			return cvalue.getTypeParameters();
		}
		if (type instanceof XtendInterface cvalue) {
			return cvalue.getTypeParameters();
		}
		return Collections.emptyList();
	}

	private boolean isEqualityTestValidField(JvmField field) {
		return !field.isStatic() && !SarlUtils.isHiddenMember(field.getSimpleName())
				&& this.annotationFinder.findAnnotation(field, NoEqualityTestFunctionsGeneration.class) == null;
	}

	private boolean isEqualityTestValidField(JvmTypeReference reference) {
		for (final var type : EQUALITY_TEST_TYPES) {
			if (this.typeReferences.is(reference, type)) {
				return true;
			}
		}
		return false;
	}

	/** Generate the "equals()" operation.
	 * This function was deprecated in Xbase, and should be provided by DSL
	 * providers now.
	 *
	 * @param sarlElement the SARL element for which the "equals function must be generated.
	 * @param declaredType the declating type.
	 * @param jvmFields the fields declared in the container.
	 * @param generatePureAnnotation indicates if the {@code @Pure} annotation should be generated.
	 * @return the "equals" function.
	 */
	private JvmOperation toEqualsMethod(
			XtendTypeDeclaration sarlElement,
			final JvmDeclaredType declaredType,
			final Iterable<JvmField> jvmFields,
			boolean generatePureAnnotation) {
		if (sarlElement == null || declaredType == null) {
			return null;
		}

		final var result = this.typeBuilder.toMethod(sarlElement, EQUALS_FUNCTION_NAME,
				this._typeReferenceBuilder.typeRef(boolean.class), null);
		if (result == null) {
			return null;
		}
		addAnnotationSafe(result, Override.class);
		if (generatePureAnnotation) {
			addAnnotationSafe(result, Pure.class);
		}

		final var param = this.typesFactory.createJvmFormalParameter();
		param.setName("obj"); //$NON-NLS-1$
		param.setParameterType(this._typeReferenceBuilder.typeRef(Object.class));
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
				final var refs = SARLJvmModelInferrer.this.typeReferences;
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
					it.append("if (other.").append(field.getSimpleName()); //$NON-NLS-1$
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

	/** Generate the "hashCode()" operation.
	 * This function was deprecated in Xbase, and should be provided by DSL
	 * providers now.
	 *
	 * @param sarlElement the SARL element for which the "hashCode" msut be generated.
	 * @param jvmFields the fields declared in the container.
	 * @return the "hashCode" function.
	 */
	private JvmOperation toHashCodeMethod(
			XtendTypeDeclaration sarlElement,
			final Iterable<JvmField> jvmFields,
			boolean generatePureAnnotation) {
		if (sarlElement == null) {
			return null;
		}
		final var result = this.typeBuilder.toMethod(sarlElement, HASHCODE_FUNCTION_NAME,
				this._typeReferenceBuilder.typeRef(int.class), null);
		if (result == null) {
			return null;
		}
		addAnnotationSafe(result, Override.class);
		if (generatePureAnnotation) {
			addAnnotationSafe(result, Pure.class);
		}
		setBody(result, it -> {
			final var refs = SARLJvmModelInferrer.this.typeReferences;
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

	/** Clone the given type reference that for being link to the given executable component.
	 *
	 * <p>The proxies are not resolved, and the type parameters are clone when they are
	 * related to the type parameter of the executable or the type container.
	 *
	 * @param type the source type.
	 * @param forExecutable the executable component that will contain the result type.
	 * @return the result type, i.e. a copy of the source type.
	 */
	protected JvmTypeReference cloneWithTypeParametersAndProxies(JvmTypeReference type, JvmExecutable forExecutable) {
		// Ensure that the executable is inside a container. Otherwise, the cloning function will fail.
		assert forExecutable.getDeclaringType() != null;
		// Get the type parameter mapping that is a consequence of the super type extension within the container.
		final var superTypeParameterMapping = new HashMap<String, JvmTypeReference>();
		Utils.getSuperTypeParameterMap(forExecutable.getDeclaringType(), superTypeParameterMapping);
		// Do the cloning
		return Utils.cloneWithTypeParametersAndProxies(
				type,
				forExecutable.getTypeParameters(),
				superTypeParameterMapping,
				this._typeReferenceBuilder,
				this.typeBuilder, this.typeReferences, this.typesFactory);
	}

	/** Clone the given type reference that is associated to another Xtext resource.
	 *
	 * <p>This function ensures that the resource of the reference clone is not pointing
	 * to the resource of the original reference.
	 *
	 * <p>This function calls {@link org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder#cloneWithProxies(JvmTypeReference)} or
	 * {@link #cloneWithTypeParametersAndProxies(JvmTypeReference, JvmExecutable)} if the
	 * {@code target} is {@code null} for the first, and not {@code null} for the second.
	 *
	 * @param type the source type.
	 * @param target the operation for which the type is clone, or {@code null} if not relevant.
	 * @return the result type, i.e. a copy of the source type.
	 */
	protected JvmTypeReference cloneWithProxiesFromOtherResource(JvmTypeReference type, JvmOperation target) {
		if (type == null) {
			return this._typeReferenceBuilder.typeRef(Void.TYPE);
		}
		// Do not clone inferred types because they are not yet resolved and it is located within the current resource.
		if (InferredTypeIndicator.isInferred(type)) {
			return type;
		}
		// Do not clone primitive types because the associated resource to the type reference will not be correct.
		final var id = type.getIdentifier();
		if (Objects.equal(id, Void.TYPE.getName())) {
			return this._typeReferenceBuilder.typeRef(Void.TYPE);
		}
		if (this.services.getPrimitives().isPrimitive(type)) {
			return this._typeReferenceBuilder.typeRef(id);
		}
		// Clone the type
		if (target != null) {
			return cloneWithTypeParametersAndProxies(type, target);
		}
		return this.typeBuilder.cloneWithProxies(type);
	}

	/** Copy and clean the given documentation by removing any unnecessary {@code @param}.
	 *
	 * @param sourceOperation the source for the documentation.
	 * @param targetOperation the target for the documentation.
	 * @return {@code true} if a documentation was added.
	 */
	protected boolean copyAndCleanDocumentationTo(JvmExecutable sourceOperation, JvmExecutable targetOperation) {
		assert sourceOperation != null;
		assert targetOperation != null;

		var comment = SARLJvmModelInferrer.this.typeBuilder.getDocumentation(sourceOperation);
		if (Strings.isNullOrEmpty(comment)) {
			return false;
		}

		comment = cleanDocumentation(comment,
				Iterables.transform(sourceOperation.getParameters(), it -> it.getSimpleName()),
				Iterables.transform(targetOperation.getParameters(), it -> it.getSimpleName()));

		SARLJvmModelInferrer.this.typeBuilder.setDocumentation(targetOperation, comment);
		return true;
	}

	/** Copy and clean the given documentation by removing any unecessary {@code @param}.
	 *
	 * @param sourceOperation the source for the documentation.
	 * @param targetOperation the target for the documentation.
	 * @return {@code true} if a documentation was added.
	 */
	protected boolean copyAndCleanDocumentationTo(XtendExecutable sourceOperation, JvmExecutable targetOperation) {
		assert sourceOperation != null;
		assert targetOperation != null;

		var comment = SARLJvmModelInferrer.this.typeBuilder.getDocumentation(sourceOperation);
		if (Strings.isNullOrEmpty(comment)) {
			return false;
		}

		comment = cleanDocumentation(comment,
				Iterables.transform(sourceOperation.getParameters(), it -> it.getName()),
				Iterables.transform(targetOperation.getParameters(), it -> it.getSimpleName()));

		SARLJvmModelInferrer.this.typeBuilder.setDocumentation(targetOperation, comment);
		return true;
	}

	/** Copy and clean the given documentation by removing any unnecessary {@code @param}.
	 *
	 * @param sourceParameters the parameters of the source.
	 * @param targetParameters the parameters of the target.
	 * @return {@code true} if a documentation was added.
	 */
	private static String cleanDocumentation(String comment, Iterable<String> sourceParameters, Iterable<String> targetParameters) {
		var clean = comment;
		if (!Strings.isNullOrEmpty(clean)) {
			final var parameterNames = new TreeSet<String>();
			if (sourceParameters != null) {
				for (final var param : sourceParameters) {
					if (!Strings.isNullOrEmpty(param)) {
						parameterNames.add(param);
					}
				}
			}
			if (targetParameters != null) {
				for (final var param : targetParameters) {
					if (!Strings.isNullOrEmpty(param)) {
						parameterNames.remove(param);
					}
				}
			}
			for (final var parameterName : parameterNames) {
				clean = clean.replaceFirst(
						"\\Q@param\\E\\s+\\Q" + parameterName + "\\E\\s*", //$NON-NLS-1$//$NON-NLS-2$
						"@optionalparam " + parameterName + " "); //$NON-NLS-1$//$NON-NLS-2$
			}
		}
		return clean;
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
	 */
	protected void copyTypeParametersFromJvmOperation(JvmOperation fromOperation, JvmOperation toOperation) {
		Utils.copyTypeParametersFromJvmOperation(fromOperation, toOperation,
				this._typeReferenceBuilder, this.typeBuilder, this.typeReferences, this.typesFactory);
	}

	/** Copy the JVM operations from the source to the destination.
	 *
	 * @param source the source.
	 * @param target the destination.
	 * @param createdActions the set of actions that are created before (input) or during (output) the invocation.
	 * @param copyHiddenNames indicates if the operations with hidden name are copied.
	 * @param bodyBuilder the builder of the target's operations.
	 * @since 0.12
	 */
	protected void copyNonStaticPublicJvmOperations(JvmGenericType source, JvmGenericType target,
			Set<ActionPrototype> createdActions, boolean copyHiddenNames,
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
				final var newOp = this.typesFactory.createJvmOperation();
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

				copyTypeParametersFromJvmOperation(operation, newOp);

				for (final var exception : operation.getExceptions()) {
					newOp.getExceptions().add(cloneWithTypeParametersAndProxies(exception, newOp));
				}

				for (final var parameter : operation.getParameters()) {
					final var newParam = this.typesFactory.createJvmFormalParameter();
					newOp.getParameters().add(newParam);
					newParam.setName(parameter.getSimpleName());
					newParam.setParameterType(cloneWithTypeParametersAndProxies(parameter.getParameterType(), newOp));
				}

				newOp.setVarArgs(operation.isVarArgs());

				newOp.setReturnType(cloneWithTypeParametersAndProxies(operation.getReturnType(), newOp));

				setBody(newOp, it -> bodyBuilder.apply(operation, it));
			}
		}
	}

	/** Copy the JVM constructors from the source to the destination.
	 *
	 * @param context the current generation context.
	 * @param source the source.
	 * @param target the destination.
	 * @param sarlSource the SARL source element. If {@code null}, the generated constructors will not be associated to the SARL element.
	 * @param createdConstructors the set of constructors that are created before (input) or during (output) the invocation.
	 * @param minimalVisibility the minimal visibility to apply to the created constructors.
	 * @since 0.10
	 */
	protected void copyVisibleJvmConstructors(
			GenerationContext context,
			JvmGenericType source, JvmGenericType target,
			XtendTypeDeclaration sarlSource, Set<ActionParameterTypes> createdConstructors,
			JvmVisibility minimalVisibility) {
		final var samePackage = Objects.equal(source.getPackageName(), target.getPackageName());
		final var constructors = Iterables.transform(Iterables.filter(source.getMembers(), it -> {
			if (it instanceof JvmConstructor op) {
				return op.getVisibility() != JvmVisibility.PRIVATE
						&& (op.getVisibility() != JvmVisibility.DEFAULT || samePackage);
			}
			return false;
		}), it -> (JvmConstructor) it);

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
				final var newCons = this.typesFactory.createJvmConstructor();
				newCons.setDeprecated(constructor.isDeprecated());
				newCons.setSimpleName(target.getSimpleName());
				target.getMembers().add(newCons);

				for (final var parameter : constructor.getParameters()) {
					final var newParam = this.typesFactory.createJvmFormalParameter();
					newParam.setName(parameter.getSimpleName());
					newCons.getParameters().add(newParam);

					final var originalParamTypeReference = parameter.getParameterType();
					final var paramType = cloneWithTypeParametersAndProxies(originalParamTypeReference, newCons);
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

				appendGeneratedAnnotation(newCons, getContext(target));

				for (final var annotationReference : constructor.getAnnotations()) {
					final var annotationType = annotationReference.getAnnotation();
					if (isAccessibleTypeAccordingToJavaSpecifications(context, annotationType)
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
	 * @param context the generation context.
	 * @param type the type to test.
	 * @return {@code true} if the type is accessible.
	 */
	@SuppressWarnings("static-method")
	protected boolean isAccessibleTypeAccordingToJavaSpecifications(GenerationContext context, JvmDeclaredType type) {
		// TODO find and use an API-oriented way to have access to the module access definitions.
		final var packageName = type.getPackageName();
		if (!Strings.isNullOrEmpty(packageName)) {
			return !packageName.contains(".internal"); //$NON-NLS-1$
		}
		return true;
	}

	/** Infer the function's return type.
	 *
	 * @param body the body of the function.
	 * @return the return type.
	 */
	protected JvmTypeReference inferFunctionReturnType(XExpression body) {
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
			return this._typeReferenceBuilder.typeRef(Void.TYPE);
		}
		return this.typeBuilder.inferredType(body);
	}

	/** Infer the return type for the given source function.
	 *
	 * @param source the source function.
	 * @param target the target operation.
	 * @param overriddenOperation reference to the overridden operation.
	 * @return the inferred return type.
	 * @since 0.7
	 */
	protected JvmTypeReference inferFunctionReturnType(XtendFunction source, JvmOperation target, JvmOperation overriddenOperation) {
		// The return type is explicitly given
		if (source.getReturnType() != null) {
			return ensureValidType(source.eResource(), source.getReturnType());
		}

		// An super operation was detected => reuse its return type.
		if (overriddenOperation != null) {
			final var type = overriddenOperation.getReturnType();
			//return cloneWithProxiesFromOtherResource(type, target);
			return this.typeReferences.createDelegateTypeReference(type);
		}

		// Return type is inferred from the operation's expression.
		final var expression = source.getExpression();
		JvmTypeReference returnType = null;
		if (expression != null
				&& ((!(expression instanceof XBlockExpression))
						|| (!((XBlockExpression) expression).getExpressions().isEmpty()))) {
			returnType = inferFunctionReturnType(expression);
		}
		return ensureValidType(source.eResource(), returnType);
	}

	private JvmTypeReference ensureValidType(Resource targetResource, JvmTypeReference returnType) {
		// No return type could be inferred => assume "void"
		if (returnType == null) {
			return this._typeReferenceBuilder.typeRef(Void.TYPE);
		}
		// The given type is not associated to the target resource => force relocation.
		final var returnTypeResource = returnType.eResource();
		if (returnTypeResource != null && !Objects.equal(returnType.eResource(), targetResource)) {
			return this.typeBuilder.cloneWithProxies(returnType);
		}
		// A return type was inferred => use it as-is because it is not yet resolved to the concrete type.
		if (InferredTypeIndicator.isInferred(returnType)) {
			return returnType;
		}
		// A return was inferred and resolved => use it.
		return this.typeBuilder.cloneWithProxies(returnType);
	}

	/** Initialize the SARL capacity context-aware wrapper.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 * @since 0.6
	 */
	protected void appendCapacityContextAwareWrapper(SarlCapacity source, JvmGenericType inferredJvmType) {
		final var innerType = this.typesFactory.createJvmGenericType();
		innerType.setInterface(false);
		innerType.setAbstract(false);
		innerType.setVisibility(JvmVisibility.PUBLIC);
		innerType.setStatic(true);
		innerType.setStrictFloatingPoint(false);
		innerType.setFinal(false);
		final var innerTypeName = Capacity.ContextAwareCapacityWrapper.class.getSimpleName();
		innerType.setSimpleName(innerTypeName);

		inferredJvmType.getMembers().add(innerType);

		this.typeBuilder.setDocumentation(innerType, "@ExcludeFromApidoc"); //$NON-NLS-1$

		final var typeParameter = this.typesFactory.createJvmTypeParameter();
		typeParameter.setName("C"); //$NON-NLS-1$
		final var constraint = this.typesFactory.createJvmUpperBound();
		constraint.setTypeReference(this._typeReferenceBuilder.typeRef(inferredJvmType));
		typeParameter.getConstraints().add(constraint);
		innerType.getTypeParameters().add(typeParameter);

		final var extendedTypeIterator = inferredJvmType.getExtendedInterfaces().iterator();
		if (extendedTypeIterator.hasNext()) {
			final var extendedType = extendedTypeIterator.next();
			final var superType = this._typeReferenceBuilder.typeRef(
					extendedType.getQualifiedName() + "$" + innerTypeName, //$NON-NLS-1$
					this._typeReferenceBuilder.typeRef(typeParameter));
			innerType.getSuperTypes().add(superType);
		}

		innerType.getSuperTypes().add(this._typeReferenceBuilder.typeRef(inferredJvmType));

		final var constructor = this.typesFactory.createJvmConstructor();
		constructor.setVisibility(JvmVisibility.PUBLIC);
		innerType.getMembers().add(constructor);
		final var parameter1 = this.typesFactory.createJvmFormalParameter();
		parameter1.setName("capacity"); //$NON-NLS-1$
		parameter1.setParameterType(this._typeReferenceBuilder.typeRef(typeParameter));
		constructor.getParameters().add(parameter1);
		final var parameter2 = this.typesFactory.createJvmFormalParameter();
		parameter2.setName("caller"); //$NON-NLS-1$
		parameter2.setParameterType(this._typeReferenceBuilder.typeRef(AgentTrait.class));
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
			copyNonStaticPublicJvmOperations(sourceType, innerType, createdActions, false, (operation, it) -> {
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

	/** Informations about the behavior units and the guards.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	private static class BehaviorUnitDefinitions implements Serializable {

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
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	private static class BehaviorUnitFunctions implements Serializable {

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
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	private record BehaviorUnitFunction(String name, List<JvmTypeReference> bounds) {
		//
	}

	/** Internal error.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private abstract static class InternalError extends RuntimeException {

		private static final long serialVersionUID = 4637115741105214351L;

		InternalError(String message) {
			super(message);
		}

	}

	/** Internal error.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class GenerationContextNotFoundInternalError extends InternalError {

		private static final long serialVersionUID = 2275793506661573859L;

		GenerationContextNotFoundInternalError(JvmIdentifiableElement type) {
			super("generation context cannot be found for: " + type.getIdentifier()); //$NON-NLS-1$
		}

	}

	/** Factory of JVM type for the 1-to-many generation.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 * @see JvmGenericTypeFactory
	 */
	@FunctionalInterface
	protected interface JvmGenericTypeProvider {
		
		/** Replies the generic type that was created at the given slot.
		 *
		 * @param index the slot index of the generic type.
		 * @return the generic type, never {@code null}.
		 */
		JvmGenericType getGenericType(int index);
		
	}

	/** Factory of JVM type for the 1-to-many generation.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 * @see JvmGenericTypeProvider
	 */
	protected interface JvmGenericTypeFactory {
		
		/** Create a generic type that was created at the given slot.
		 *
		 * @param index the slot index of the generic type.
		 * @param name the name of the generic type. It must not be empty.
		 * @return {@code this}.
		 */
		JvmGenericTypeFactory createReceiver(int index, String name);

	}

	/** Factory of JVM type for the 1-to-many generation.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	protected class DefaultJvmGenericTypeProvider implements JvmGenericTypeFactory, JvmGenericTypeProvider {

		private final Map<Integer, JvmGenericType> createdTypes = new HashMap<>();
		
		/** Constructor.
		 */
		public DefaultJvmGenericTypeProvider() {
			//
		}
		
		@Override
		public JvmGenericTypeFactory createReceiver(int index, String name) {
			assert index >= 0 && !Strings.isNullOrEmpty(name);
			this.createdTypes.computeIfAbsent(Integer.valueOf(index), it -> {
				final var type = SARLJvmModelInferrer.this.typesFactory.createJvmGenericType();
				type.setSimpleName(name);
				return type;
			});
			return this;
		}
		
		@Override
		public JvmGenericType getGenericType(int index) {
			assert index >= 0;
			final var type = this.createdTypes.get(Integer.valueOf(index));
			assert type != null;
			return type;
		}

		/** Replies the stream on the created JVM types.
		 *
		 * @return the stream, never {@code null}.
		 */
		public Stream<JvmGenericType> stream() {
			return this.createdTypes.values().stream();
		}
		
	}

}
