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

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.TreeSet;

import javax.annotation.processing.Generated;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.xtend.core.jvmmodel.SyntheticNameClashResolver;
import org.eclipse.xtend.core.xtend.AnonymousClass;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmFeature;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociator;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.typesystem.InferredTypeIndicator;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import com.google.common.base.Objects;
import com.google.common.base.Predicate;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.inject.Inject;

import io.sarl.lang.core.annotation.DefaultValueUse;
import io.sarl.lang.core.annotation.SarlSourceCode;
import io.sarl.lang.core.annotation.SyntheticMember;
import io.sarl.lang.jvmmodel.GenerationContext;
import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.IDefaultVisibilityProvider;
import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.sarl.actionprototype.InferredValuedParameter;
import io.sarl.lang.services.ITypeDefaultValueProvider;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.typesystem.SARLAnnotationUtil;
import io.sarl.lang.util.Utils;

/** Abstract implementation of a fragment that may be used for inferring the JVM model.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public abstract class AbstractJvmModelInferrerFragment {

	/** Name of the annotation used for marking the constructs as generated.
	 */
	protected static final String GENERATED_NAME = Generated.class.getName();

	/** Name for the collection of runnable objects for the behavior units.
	 */
	protected static final String RUNNABLE_COLLECTION = Utils.createNameForHiddenLocalVariable("runnableCollection"); //$NON-NLS-1$

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

	/** Access to the associations between SARL objects and JVM objects.
	 */
	@Inject
	protected SarlJvmModelAssociations sarlAssociations;

	/** Factory of JVM types.
	 *
	 * @see #jvmTypeBuilder
	 */
	@Inject
	protected TypesFactory jvmTypesFactory;

	/** Tools for adding constructs to the JVM elements.
	 *
	 * @see #jvmTypesFactory
	 */
	@Inject
	protected JvmTypesBuilder jvmTypeBuilder;

	/** Associator of the SARL element to the generated JVM element.
	 */
	@Inject
	protected IJvmModelAssociator associator;

	/** Builder of and accessor to JVM type references.
	 */
	@Inject
	protected TypeReferences jvmTypeReferences;

	/** Finder of JVM annotations attached to JVM elements.
	 */
	@Inject
	protected AnnotationLookup jvmAnnotationFinder;

	/** Utilities for JVM annotations.
	 */
	@Inject
	protected SARLAnnotationUtil annotationUtils;

	/** Several generation services.
	 */
	@Inject
	protected CommonTypeComputationServices services;

	/** Accessor to the SARL keywords defined in the grammar.
	 */
	@Inject
	protected SARLGrammarKeywordAccess grammarKeywordAccess;

	/** Manager and provider of SARL action signatures.
	 */
	@Inject
	protected IActionPrototypeProvider sarlSignatureProvider;

	/** Provider the default value for a specific type.
	 */
	@Inject
	protected ITypeDefaultValueProvider defaultValueProvider;

	@Inject
	private IDefaultVisibilityProvider defaultVisibilityProvider;

	/** Resolver on name incompatibility.
	 */
	@Inject
	protected SyntheticNameClashResolver nameClashResolver;

	/** Change the visibility modifier of the given member.
	 *
	 * @param jvmMember the JVM version of the member.
	 * @param member the SARL version of the member
	 */
	protected void setVisibility(JvmMember jvmMember, XtendMember member) {
		var visibility = member.getVisibility();
		if (visibility == null) {
			visibility = this.defaultVisibilityProvider.getDefaultJvmVisibility(member);
		}
		jvmMember.setVisibility(visibility);
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
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @return the result type, i.e. a copy of the source type.
	 */
	protected JvmTypeReference cloneWithProxiesFromOtherResource(JvmTypeReference type, JvmOperation target,
			IBaseJvmModelInferrer baseInferrer) {
		if (type == null) {
			return baseInferrer.getJvmTypeReferenceBuilder().typeRef(Void.TYPE);
		}
		// Do not clone inferred types because they are not yet resolved and it is located within the current resource.
		if (InferredTypeIndicator.isInferred(type)) {
			return type;
		}
		// Do not clone primitive types because the associated resource to the type reference will not be correct.
		final var id = type.getIdentifier();
		if (Objects.equal(id, Void.TYPE.getName())) {
			return baseInferrer.getJvmTypeReferenceBuilder().typeRef(Void.TYPE);
		}
		if (this.services.getPrimitives().isPrimitive(type)) {
			return baseInferrer.getJvmTypeReferenceBuilder().typeRef(id);
		}
		// Clone the type
		if (target != null) {
			return cloneWithTypeParametersAndProxies(type, target, baseInferrer);
		}
		return this.jvmTypeBuilder.cloneWithProxies(type);
	}


	/** Clone the given type reference that for being link to the given executable component.
	 *
	 * <p>The proxies are not resolved, and the type parameters are clone when they are
	 * related to the type parameter of the executable or the type container.
	 *
	 * @param type the source type.
	 * @param forExecutable the executable component that will contain the result type.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @return the result type, i.e. a copy of the source type.
	 */
	protected JvmTypeReference cloneWithTypeParametersAndProxies(JvmTypeReference type, JvmExecutable forExecutable,
			IBaseJvmModelInferrer baseInferrer) {
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
				baseInferrer.getJvmTypeReferenceBuilder(),
				this.jvmTypeBuilder, this.jvmTypeReferences, this.jvmTypesFactory);
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

		var comment = this.jvmTypeBuilder.getDocumentation(sourceOperation);
		if (Strings.isNullOrEmpty(comment)) {
			return false;
		}

		comment = cleanDocumentation(comment,
				Iterables.transform(sourceOperation.getParameters(), it -> it.getSimpleName()),
				Iterables.transform(targetOperation.getParameters(), it -> it.getSimpleName()));

		this.jvmTypeBuilder.setDocumentation(targetOperation, comment);
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

		var comment = this.jvmTypeBuilder.getDocumentation(sourceOperation);
		if (Strings.isNullOrEmpty(comment)) {
			return false;
		}

		comment = cleanDocumentation(comment,
				Iterables.transform(sourceOperation.getParameters(), it -> it.getName()),
				Iterables.transform(targetOperation.getParameters(), it -> it.getSimpleName()));

		this.jvmTypeBuilder.setDocumentation(targetOperation, comment);
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


	/** Add the @Generated annotation to the given target.
	 * The annotation will not have any generated SARL code associated to it.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param target the target of the annotation.
	 * @param context the generation context.
	 */
	protected final void appendGeneratedAnnotation(IBaseJvmModelInferrer baseInferrer, JvmAnnotationTarget target, GenerationContext context) {
		appendGeneratedAnnotation(baseInferrer, target, context, null);
	}

	/** Add the @Generated annotation to the given target.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param target the target of the annotation.
	 * @param context the generation context.
	 * @param sarlCode the code that is the cause of the generation.
	 */
	protected void appendGeneratedAnnotation(IBaseJvmModelInferrer baseInferrer, JvmAnnotationTarget target, GenerationContext context, String sarlCode) {
		final var config = context.getGeneratorConfig();
		if (config.isGenerateGeneratedAnnotation()) {
			addAnnotationSafe(baseInferrer, target, GENERATED_NAME, getClass().getName());
		}

		if (target instanceof JvmFeature) {
			addAnnotationSafe(baseInferrer, target, SyntheticMember.class);
		}

		if (!Strings.isNullOrEmpty(sarlCode)) {
			addAnnotationSafe(baseInferrer, target, SarlSourceCode.class, sarlCode);
		}
	}

	/** Add annotation safely.
	 *
	 * <p>This function creates an annotation reference. If the type for the annotation is not found;
	 * no annotation is added.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param target the receiver of the annotation.
	 * @param annotationType the type of the annotation.
	 * @param values the annotations values.
	 * @return the annotation reference or {@code null} if the annotation cannot be added.
	 */
	@SuppressWarnings("static-method")
	protected JvmAnnotationReference addAnnotationSafe(IBaseJvmModelInferrer baseInferrer, JvmAnnotationTarget target,
			Class<?> annotationType, String... values) {
		assert target != null;
		assert annotationType != null;
		try {
			final var annotationRef = baseInferrer.getJvmAnnotationReferenceBuilder().annotationRef(annotationType, values);
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
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param target the receiver of the annotation.
	 * @param annotationType the type of the annotation.
	 * @param values the annotations values.
	 * @return the annotation reference or {@code null} if the annotation cannot be added.
	 * @since 0.12
	 */
	@SuppressWarnings("static-method")
	protected JvmAnnotationReference addAnnotationSafe(IBaseJvmModelInferrer baseInferrer, JvmAnnotationTarget target, String annotationType, String... values) {
		assert target != null;
		assert annotationType != null;
		try {
			final var annotationRef = baseInferrer.getJvmAnnotationReferenceBuilder().annotationRef(annotationType, values);
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
	protected JvmAnnotationReference addAnnotationSafe(JvmAnnotationTarget target, Class<?> annotationType, int value) {
		assert target != null;
		assert annotationType != null;
		try {
			final var result = this.jvmTypesFactory.createJvmAnnotationReference();
			final var jvmType = this.jvmTypeReferences.findDeclaredType(annotationType, target);
			if (jvmType == null) {
				return null;
			}
			if (jvmType instanceof JvmAnnotationType cvalue) {
				result.setAnnotation(cvalue);
				final var annotationValue = this.jvmTypesFactory.createJvmIntAnnotationValue();
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


	/** Create a JVM annotation with classes as values.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param type the type of the annotation.
	 * @param values the values.
	 * @return the reference to the JVM annotation.
	 */
	protected JvmAnnotationReference annotationClassRef(
			IBaseJvmModelInferrer baseInferrer, Class<? extends Annotation> type,
			List<? extends JvmTypeReference> values) {
		try {
			final var annot = baseInferrer.getJvmAnnotationReferenceBuilder().annotationRef(type);
			final var annotationValue = this.services.getTypesFactory().createJvmTypeAnnotationValue();
			for (final var value : values) {
				annotationValue.getValues().add(this.jvmTypeBuilder.cloneWithProxies(value));
			}
			annot.getExplicitValues().add(annotationValue);
			return annot;
		} catch (IllegalArgumentException exception) {
			// ignore
		}
		return null;
	}

	/** Create a string concatenation client from a set of Java code lines.
	 *
	 * @param javaCodeLines the Java code lines.
	 * @return the client.
	 */
	protected static StringConcatenationClient toStringConcatenation(final String... javaCodeLines) {
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
	
	/** Set the body of the executable.
	 *
	 * @param executable the executable.
	 * @param expression the body definition.
	 */
	protected void setBody(JvmExecutable executable, Procedure1<ITreeAppendable> expression) {
		this.jvmTypeBuilder.setBody(executable, expression);
	}

	/** Set the body of the executable.
	 *
	 * @param executable the executable.
	 * @param expression the body definition.
	 */
	protected void setBody(JvmExecutable executable, StringConcatenationClient expression) {
		this.jvmTypeBuilder.setBody(executable, expression);
	}

	/** Set the body of the executable.
	 *
	 * <p>Overridden from Xtend for: removing the existing associated body, and delaying the local type inference.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param executable the executable to set the body to.
	 * @param expression the expression that is representing the body of the {@code executable}.
	 */
	public void setBody(IBaseJvmModelInferrer baseInferrer, JvmExecutable executable, XExpression expression) {
		final var context = baseInferrer.getContext(EcoreUtil2.getContainerOfType(executable, JvmType.class));
		this.jvmTypeBuilder.removeExistingBody(executable);
		this.associator.associateLogicalContainer(expression, executable);
		if (expression != null) {
			if (context.getParentContext() == null) {
				context.getPostFinalizationElements().add(() -> {
					initializeLocalTypes(baseInferrer, context, executable, expression);
				});
			} else {
				initializeLocalTypes(baseInferrer, context, executable, expression);
			}
		} else {
			initializeLocalTypes(baseInferrer, context, executable, expression);
		}
	}

	/** Initialize the local class to the given expression.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the generation context.
	 * @param feature the feature which contains the expression.
	 * @param expression the expression which contains the local class.
	 */
	protected void initializeLocalTypes(IBaseJvmModelInferrer baseInferrer, GenerationContext context, JvmFeature feature, XExpression expression) {
		if (expression != null) {
			var localTypeIndex = context.getLocalTypeIndex();
			final var iterator = EcoreUtil2.getAllNonDerivedContents(expression, true);
			final var nameStub = "__" + feature.getDeclaringType().getSimpleName() + "_"; //$NON-NLS-1$ //$NON-NLS-2$
			while (iterator.hasNext()) {
				final var next = iterator.next();
				if (next.eClass() == XtendPackage.Literals.ANONYMOUS_CLASS) {
					inferLocalClass(baseInferrer, (AnonymousClass) next, nameStub + localTypeIndex, feature);
					iterator.prune();
					++localTypeIndex;
				}
			}
			context.setLocalTypeIndex(localTypeIndex);
		}
	}

	/** Infer and transform the local (anonymous) class.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param anonymousClass the anonymous class to be inferred.
	 * @param localClassName the name of the class.
	 * @param container the container feature of the anonymous class.
	 */
	public void inferLocalClass(IBaseJvmModelInferrer baseInferrer, AnonymousClass anonymousClass, String localClassName, JvmFeature container) {
		// Issue #356: do not generate if the class has no name.
		assert anonymousClass != null;
		assert container != null;
		if (Strings.isNullOrEmpty(localClassName)) {
			return;
		}
		// Issue #363: do not generate the class if the SARL library is incompatible.
		if (!Utils.isCompatibleSARLLibraryOnClasspath(this.jvmTypeReferences, anonymousClass)) {
			return;
		}

		// Create the inner type
		// --- Begin Xtend Part
		try {
			final var inferredJvmType = this.jvmTypesFactory.createJvmGenericType();
			inferredJvmType.setSimpleName(localClassName);
			// --- End Xtend Part

			// Issue #1028: Force the "isAnonymous" flag because the capabilities of the Java compiler enables.
			final var isAnonymous = container instanceof JvmConstructor || !hasAdditionalMembers(anonymousClass);
			inferredJvmType.setAnonymous(isAnonymous);

			// --- Begin Xtend Part
			inferredJvmType.setFinal(true);
			setVisibility(inferredJvmType, anonymousClass);
			inferredJvmType.getSuperTypes().add(this.jvmTypeBuilder.inferredType(anonymousClass));
			container.getLocalClasses().add(inferredJvmType);
			this.associator.associatePrimary(anonymousClass, inferredJvmType);
			// --- End Xtend Part

			// Create the generation context that is used by the other transformation functions.
			final var parentContext = baseInferrer.getContext(EcoreUtil2.getContainerOfType(container, JvmType.class));
			final var context = baseInferrer.openContext(anonymousClass, inferredJvmType, Arrays.asList(
					SarlField.class, SarlConstructor.class, SarlAction.class));
			context.setParentContext(parentContext);
			try {
				// --- Begin Xtend Part
				for (final var member : anonymousClass.getMembers()) {
					if (context.isSupportedMember(member)) {
						baseInferrer.transform(member, inferredJvmType, true);
					}
				}

				baseInferrer.appendSyntheticDispatchMethods(anonymousClass, inferredJvmType);
				this.nameClashResolver.resolveNameClashes(inferredJvmType);
				// --- End Xtend Part

				// Add SARL synthetic functions
				appendSyntheticDefaultValuedParameterMethods(
						baseInferrer,
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
				baseInferrer.closeContext(context);
			}
		} catch (AssertionError | InternalError internalError) {
			throw internalError;
		} catch (Exception exception) {
			baseInferrer.logInternalError(exception);
		}
	}

	private static boolean hasAdditionalMembers(AnonymousClass anonymousClass) {
		for (final var member: anonymousClass.getMembers()) {
			if (member instanceof XtendField || (member instanceof XtendFunction && !((XtendFunction) member).isOverride())) {
				return true;
			}
		}
		return false;
	}

	/** Generate the missed operations that are the results from the generation of actions with default value parameters.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param source the SARL container.
	 * @param target the JVM feature container.
	 * @param ignoreOverridableOperations indicates if the operations must not be added if they are marked has
	 *     overridable.
	 * @param context description of the generation context in which the members must be considered.
	 */
	private void appendSyntheticDefaultValuedParameterMethods(
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

	/** Abstract implementation of a fragment that may be used for inferring the JVM model.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	public static class Impl extends AbstractJvmModelInferrerFragment {

		/** Constructor.
		 */
		public Impl() {
			//
		}
		
	}

}
