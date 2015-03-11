/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import io.sarl.lang.SARLKeywords;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.EarlyExit;
import io.sarl.lang.annotation.FiredEvent;
import io.sarl.lang.annotation.Generated;
import io.sarl.lang.annotation.ImportedCapacityFeature;
import io.sarl.lang.controlflow.SARLExtendedEarlyExitComputer;
import io.sarl.lang.core.Percept;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlRequiredCapacity;
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
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.UUID;
import java.util.logging.Logger;

import org.eclipse.xtend.core.jvmmodel.SyntheticNameClashResolver;
import org.eclipse.xtend.core.jvmmodel.XtendJvmModelInferrer;
import org.eclipse.xtend.core.xtend.CreateExtensionInfo;
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
import org.eclipse.xtend.lib.macro.declaration.Modifier;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.XbaseCompiler;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociator;
import org.eclipse.xtext.xbase.jvmmodel.JvmAnnotationReferenceBuilder;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeExtensions;
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
	private Logger log;

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

	@Inject
	private JvmTypeExtensions typeExtensions;

	@Inject
	private XbaseCompiler xbaseCompiler;

	private final Map<JvmIdentifiableElement, GenerationContext> generationContexts =
			new TreeMap<>(new JvmIdentifiableComparator());

	private static StringConcatenationClient toStringConcatenation(final String... javaCodeLines) {
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(StringConcatenationClient.TargetStringConcatenation builder) {
				for (String line : javaCodeLines) {
					builder.append(line);
					builder.newLineIfNotEmpty();
				}
			}
		};
	}

	private static boolean isInstance(Object o, Class<?>[] types) {
		if (o != null) {
			for (Class<?> type : types) {
				if (type.isInstance(o)) {
					return true;
				}
			}
		}
		return false;
	}

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
		return super.doInferTypeSceleton(declaration, acceptor, preIndexingPhase,
				xtendFile, doLater);
	}

	@Override
	protected void transform(XtendMember sourceMember, JvmGenericType container, boolean allowDispatch) {
		if (sourceMember instanceof SarlBehaviorUnit) {
			transform((SarlBehaviorUnit) sourceMember, container);
		} else if (sourceMember instanceof SarlCapacityUses) {
			transform((SarlCapacityUses) sourceMember, container);
		} else if (sourceMember instanceof SarlRequiredCapacity) {
			transform((SarlRequiredCapacity) sourceMember, container);
		} else {
			super.transform(sourceMember, container, allowDispatch);
		}
		GenerationContext context = this.generationContexts.get(container);
		assert (context != null);
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
	protected void transform(XtendField source, JvmGenericType container) {
		super.transform(source, container);
		GenerationContext context = this.generationContexts.get(container);
		assert (context != null);
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
			this.generationContexts.put(inferredJvmType, new GenerationContext(inferredJvmType));
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
			translateMembers(source, inferredJvmType, SarlAction.class);
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
			GenerationContext context = new GenerationContext(inferredJvmType);
			this.generationContexts.put(inferredJvmType, context);
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
			translateMembers(source, inferredJvmType,
					SarlCapacityUses.class,
					SarlRequiredCapacity.class,
					XtendField.class,
					XtendConstructor.class,
					SarlAction.class,
					SarlBehaviorUnit.class,
					XtendClass.class,
					XtendInterface.class,
					XtendEnum.class,
					XtendAnnotationType.class);
			// Add default constructors
			if (!context.getGeneratedConstructors().isEmpty()) {
				JvmTypeReference aType = this.typeReferenceBuilder.typeRef(io.sarl.lang.core.Agent.class);
				JvmConstructor constructor = this.typesFactory.createJvmConstructor();
				constructor.setVarArgs(false);
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				constructor.getAnnotations().add(this.annotationTypesBuilder.annotationRef(Generated.class));
				this.jvmTypesBuilder.setDocumentation(constructor, MessageFormat.format(
						Messages.SARLJvmModelInferrer_3, "owner")); //$NON-NLS-1$
				JvmFormalParameter parameter = this.typesFactory.createJvmFormalParameter();
				parameter.setName("owner"); //$NON-NLS-1$
				parameter.setParameterType(this.jvmTypesBuilder.cloneWithProxies(aType));
				constructor.getParameters().add(parameter);
				this.jvmTypesBuilder.setBody(constructor, toStringConcatenation("super(owner);")); //$NON-NLS-1$
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);

				constructor = this.typesFactory.createJvmConstructor();
				constructor.setVarArgs(false);
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				constructor.getAnnotations().add(this.annotationTypesBuilder.annotationRef(Generated.class));
				this.jvmTypesBuilder.setDocumentation(constructor, Messages.SARLJvmModelInferrer_4);
				this.jvmTypesBuilder.setBody(constructor, toStringConcatenation("super();")); //$NON-NLS-1$
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
			}
			// Add missed functions with default value parameters.
			addMissedDefaultValueBasedFunctionsTo(source, inferredJvmType);
			// Add standard functions
			addStandardFunctionsTo(source, inferredJvmType);
			// Add serial ID
			addSerialIDTo(source, inferredJvmType);
			// Generate the documentation
			this.jvmTypesBuilder.copyDocumentationTo(source, inferredJvmType);
			// Resolve name classes for synthetic members
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			// Reset the generator tools
			this.generationContexts.remove(inferredJvmType);
		}
	}

	/** Invoked for intializing the inferred SARL behavior.
	 *
	 * @param source - the SARL source.
	 * @param inferredJvmType - the Java inferred type.
	 */
	protected void initialize(SarlBehavior source, JvmGenericType inferredJvmType) {
		try {
			// Initialize the generator tools
			this.sarlSignatureProvider.resetSignatures(inferredJvmType);
			GenerationContext context = new GenerationContext(inferredJvmType);
			this.generationContexts.put(inferredJvmType, context);
			// Initialize the Java type
			inferredJvmType.setVisibility(source.getVisibility());
			inferredJvmType.setStatic(source.isStatic() && !isTopLevel(source));
			inferredJvmType.setInterface(false);
			inferredJvmType.setAbstract(false);
			// Generate the annotations
			translateAnnotationsTo(source.getAnnotations(), inferredJvmType);
			// Generate the super types
			translateSuperTypes(source, inferredJvmType, io.sarl.lang.core.Behavior.class,
					source.getExtends(), true);
			// Generate the members
			translateMembers(source, inferredJvmType,
					SarlCapacityUses.class,
					SarlRequiredCapacity.class,
					XtendField.class,
					XtendConstructor.class,
					SarlAction.class,
					SarlBehaviorUnit.class,
					XtendClass.class,
					XtendInterface.class,
					XtendEnum.class,
					XtendAnnotationType.class);
			// Add default constructors
			if (!context.getGeneratedConstructors().isEmpty()) {
				JvmTypeReference aType = this.typeReferenceBuilder.typeRef(io.sarl.lang.core.Agent.class);
				JvmConstructor constructor = this.typesFactory.createJvmConstructor();
				constructor.setVarArgs(false);
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				constructor.getAnnotations().add(this.annotationTypesBuilder.annotationRef(Generated.class));
				this.jvmTypesBuilder.setDocumentation(constructor, MessageFormat.format(
						Messages.SARLJvmModelInferrer_5, "owner")); //$NON-NLS-1$
				JvmFormalParameter parameter = this.typesFactory.createJvmFormalParameter();
				parameter.setName("owner"); //$NON-NLS-1$
				parameter.setParameterType(this.jvmTypesBuilder.cloneWithProxies(aType));
				constructor.getParameters().add(parameter);
				this.jvmTypesBuilder.setBody(constructor, toStringConcatenation("super(owner);")); //$NON-NLS-1$
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
			}
			// Add missed functions with default value parameters.
			addMissedDefaultValueBasedFunctionsTo(source, inferredJvmType);
			// Add standard functions
			addStandardFunctionsTo(source, inferredJvmType);
			// Add serial ID
			addSerialIDTo(source, inferredJvmType);
			// Generate the documentation
			this.jvmTypesBuilder.copyDocumentationTo(source, inferredJvmType);
			// Resolve name classes for synthetic members
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			// Reset the generator tools
			this.generationContexts.remove(inferredJvmType);
		}
	}

	/** Invoked for intializing the inferred SARL event.
	 *
	 * @param source - the SARL source.
	 * @param inferredJvmType - the Java inferred type.
	 */
	protected void initialize(SarlEvent source, JvmGenericType inferredJvmType) {
		try {
			// Initialize the generator tools
			this.sarlSignatureProvider.resetSignatures(inferredJvmType);
			GenerationContext context = new GenerationContext(inferredJvmType);
			this.generationContexts.put(inferredJvmType, context);
			// Initialize the Java type
			inferredJvmType.setVisibility(source.getVisibility());
			inferredJvmType.setStatic(source.isStatic() && !isTopLevel(source));
			inferredJvmType.setInterface(false);
			inferredJvmType.setAbstract(false);
			// Generate the annotations
			translateAnnotationsTo(source.getAnnotations(), inferredJvmType);
			// Generate the super types
			translateSuperTypes(source, inferredJvmType, io.sarl.lang.core.Event.class,
					source.getExtends(), true);
			// Generate the members
			translateMembers(source, inferredJvmType,
					XtendField.class,
					XtendConstructor.class);
			// Add default constructors
			if (!context.getGeneratedConstructors().isEmpty()) {
				JvmConstructor constructor = this.typesFactory.createJvmConstructor();
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
				constructor.setSimpleName(source.getName());
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
			}
			// Add standard functions
			addStandardFunctionsTo(source, inferredJvmType);
			// Add serial ID
			addSerialIDTo(source, inferredJvmType);
			// Generate the documentation
			this.jvmTypesBuilder.copyDocumentationTo(source, inferredJvmType);
			// Resolve name classes for synthetic members
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			// Reset the generator tools
			this.generationContexts.remove(inferredJvmType);
		}
	}

	/** Invoked for intializing the inferred SARL agent.
	 *
	 * @param source - the SARL source.
	 * @param inferredJvmType - the Java inferred type.
	 */
	protected void initialize(SarlAgent source, JvmGenericType inferredJvmType) {
		try {
			// Initialize the generator tools
			this.sarlSignatureProvider.resetSignatures(inferredJvmType);
			GenerationContext context = new GenerationContext(inferredJvmType);
			this.generationContexts.put(inferredJvmType, context);
			// Initialize the Java type
			inferredJvmType.setVisibility(source.getVisibility());
			inferredJvmType.setStatic(source.isStatic() && !isTopLevel(source));
			inferredJvmType.setInterface(false);
			inferredJvmType.setAbstract(false);
			// Generate the annotations
			translateAnnotationsTo(source.getAnnotations(), inferredJvmType);
			// Generate the super types
			translateSuperTypes(source, inferredJvmType, io.sarl.lang.core.Agent.class,
					source.getExtends(), true);
			// Generate the members
			translateMembers(source, inferredJvmType,
					XtendField.class,
					XtendConstructor.class);
			// Add default constructors
			JvmConstructor constructor1 = this.typesFactory.createJvmConstructor();
			constructor1.setVisibility(JvmVisibility.PUBLIC);
			this.typeExtensions.setSynthetic(constructor1, true);
			this.jvmTypesBuilder.setDocumentation(constructor1, MessageFormat.format(
					Messages.SARLJvmModelInferrer_6, "parentID")); //$NON-NLS-1$
			JvmFormalParameter parameter1 = this.typesFactory.createJvmFormalParameter();
			parameter1.setName("parentID"); //$NON-NLS-1$
			parameter1.setParameterType(this.typeReferenceBuilder.typeRef(UUID.class));
			constructor1.getParameters().add(parameter1);
			constructor1.getAnnotations().add(this.annotationTypesBuilder.annotationRef(Generated.class));
			this.jvmTypesBuilder.setBody(constructor1, toStringConcatenation("super(parentID, null);")); //$NON-NLS-1$
			SignatureKey sigConstructor1 = this.sarlSignatureProvider.createSignatureIDFromJvmModel(
					constructor1.isVarArgs(), constructor1.getParameters());
			if (!context.getGeneratedConstructors().containsKey(sigConstructor1)) {
				inferredJvmType.getMembers().add(constructor1);
				this.associator.associate(source, constructor1);
			}
			JvmConstructor constructor2 = this.typesFactory.createJvmConstructor();
			constructor1.setVisibility(JvmVisibility.PUBLIC);
			this.typeExtensions.setSynthetic(constructor2, true);
			this.jvmTypesBuilder.setDocumentation(constructor2, MessageFormat.format(
					Messages.SARLJvmModelInferrer_7,
					"parentID", "agentID")); //$NON-NLS-1$ //$NON-NLS-2$
			JvmFormalParameter parameter2 = this.typesFactory.createJvmFormalParameter();
			parameter2.setName("parentID"); //$NON-NLS-1$
			parameter2.setParameterType(this.typeReferenceBuilder.typeRef(UUID.class));
			constructor2.getParameters().add(parameter2);
			JvmFormalParameter parameter3 = this.typesFactory.createJvmFormalParameter();
			parameter3.setName("agentID"); //$NON-NLS-1$
			parameter3.setParameterType(this.typeReferenceBuilder.typeRef(UUID.class));
			constructor2.getParameters().add(parameter3);
			constructor2.getAnnotations().add(this.annotationTypesBuilder.annotationRef(Generated.class));
			this.jvmTypesBuilder.setBody(constructor2, toStringConcatenation("super(parentID, agentID);")); //$NON-NLS-1$
			SignatureKey sigConstructor2 = this.sarlSignatureProvider.createSignatureIDFromJvmModel(
					constructor2.isVarArgs(), constructor2.getParameters());
			if (!context.getGeneratedConstructors().containsKey(sigConstructor2)) {
				inferredJvmType.getMembers().add(constructor2);
				this.associator.associate(source, constructor2);
			}
			// Add missed functions with default value parameters.
			addMissedDefaultValueBasedFunctionsTo(source, inferredJvmType);
			// Add standard functions
			addStandardFunctionsTo(source, inferredJvmType);
			// Add serial ID
			addSerialIDTo(source, inferredJvmType);
			// Generate the documentation
			this.jvmTypesBuilder.copyDocumentationTo(source, inferredJvmType);
			// Resolve name classes for synthetic members
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			// Reset the generator tools
			this.generationContexts.remove(inferredJvmType);
		}
	}

	/** Generate the members for the given features.
	 *
	 * This function generated the missed generated functions, e.g. the generated functions according to the default values
	 * of the formal parameters.
	 *
	 * @param sourceMember - the SARL element.
	 * @param inferredJvmType - the inferred type.
	 * @param validTypes - the list of the member types that are allowed.
	 */
	protected void translateMembers(XtendTypeDeclaration sourceMember, JvmGenericType inferredJvmType,
			Class<?>... validTypes) {
		GenerationContext context = this.generationContexts.get(inferredJvmType);
		assert (context != null);

		context.populateInheritanceContext(inferredJvmType, this.sarlSignatureProvider);

		List<SarlCapacityUses> capacityUses = CollectionLiterals.newArrayList();
		List<SarlRequiredCapacity> requiredCapacities = CollectionLiterals.newArrayList();

		for (XtendMember feature : sourceMember.getMembers()) {
			if (isInstance(feature, validTypes)) {
				if (feature instanceof SarlCapacityUses) {
					capacityUses.add((SarlCapacityUses) feature);
				} else if (feature instanceof SarlRequiredCapacity) {
					requiredCapacities.add((SarlRequiredCapacity) feature);
				} else {
					transform(feature, inferredJvmType, false);
				}
			}
		}

		for (SarlCapacityUses feature : capacityUses) {
			transform(feature, inferredJvmType, false);
		}

		for (SarlRequiredCapacity feature : requiredCapacities) {
			transform(feature, inferredJvmType, false);
		}

		addMissedDefaultValueBasedFunctionsTo(sourceMember, inferredJvmType);
	}

	/** Generate a SARL capacity use statement.
	 *
	 * @param source - the SARL unit.
	 * @param inferredJvmType - the JVM container.
	 */
	protected void transform(SarlCapacityUses source, JvmGenericType inferredJvmType) {
		for (JvmTypeReference used : source.getCapacities()) {
			addCapacityDeletatorOperationsTo(source, used, inferredJvmType);
		}
	}

	/** Generate a SARL capacity requirement statement.
	 *
	 * @param source - the SARL unit.
	 * @param inferredJvmType - the JVM container.
	 */
	protected void transform(SarlRequiredCapacity source, JvmGenericType inferredJvmType) {
		//
	}

	/** Generate a SARL behavior unit.
	 *
	 * @param source - the SARL unit.
	 * @param inferredJvmType - the JVM container.
	 */
	protected void transform(final SarlBehaviorUnit source, JvmGenericType inferredJvmType) {
		GenerationContext context = this.generationContexts.get(inferredJvmType);
		assert (context != null);
		if (source.getName() != null) {
			XExpression guard = source.getGuard();
			boolean isTrueGuard;

			if (guard == null) {
				isTrueGuard = true;
			} else if (guard instanceof XBooleanLiteral) {
				XBooleanLiteral literal = (XBooleanLiteral) guard;
				if (literal.isIsTrue()) {
					isTrueGuard = true;
				} else {
					// The guard is always false => no need to generate the code
					return;
				}
			} else {
				isTrueGuard = false;
			}

			final JvmTypeReference voidType = this.typeReferenceBuilder.typeRef(Void.TYPE);
			String behName = ModelUtil.PREFIX_ACTION_HANDLE + source.getName().getSimpleName() + "_" //$NON-NLS-1$
					+ context.getBehaviorUnitIndex();

			JvmOperation behaviorMethod = this.typesFactory.createJvmOperation();
			behaviorMethod.setSimpleName(behName);
			// Modifiers
			behaviorMethod.setAbstract(false);
			behaviorMethod.setNative(false);
			behaviorMethod.setSynchronized(false);
			behaviorMethod.setStrictFloatingPoint(false);
			behaviorMethod.setFinal(false);
			behaviorMethod.setVisibility(JvmVisibility.PUBLIC);
			behaviorMethod.setStatic(false);
			// Parameters
			JvmFormalParameter methodParameter = this.typesFactory.createJvmFormalParameter();
			methodParameter.setName(SARLKeywords.OCCURRENCE);
			methodParameter.setParameterType(this.jvmTypesBuilder.cloneWithProxies(source.getName()));
			behaviorMethod.getParameters().add(methodParameter);
			// Return type
			behaviorMethod.setReturnType(voidType);
			// Annotations
			translateAnnotationsTo(source.getAnnotations(), behaviorMethod);
			behaviorMethod.getAnnotations().add(SARLJvmModelInferrer.this.annotationTypesBuilder.annotationRef(Percept.class));
			// Documentation
			this.jvmTypesBuilder.copyDocumentationTo(source, behaviorMethod);
			// Body and guard
			if (isTrueGuard) {
				setBody(behaviorMethod, source.getExpression());
			} else {
				assert (guard != null);

				final String guardMethodName = ModelUtil.PREFIX_HANDLE_GUARD + source.getName().getSimpleName()
						+ "_" + context.getBehaviorUnitIndex(); //$NON-NLS-1$

				JvmOperation guardMethod = this.typesFactory.createJvmOperation();
				guardMethod.setSimpleName(guardMethodName);
				// Modifiers
				guardMethod.setAbstract(false);
				guardMethod.setNative(false);
				guardMethod.setSynchronized(false);
				guardMethod.setStrictFloatingPoint(false);
				guardMethod.setFinal(false);
				guardMethod.setVisibility(JvmVisibility.PRIVATE);
				guardMethod.setStatic(false);
				// Parameters
				JvmFormalParameter guardParameter = this.typesFactory.createJvmFormalParameter();
				guardParameter.setName(SARLKeywords.OCCURRENCE);
				guardParameter.setParameterType(this.jvmTypesBuilder.cloneWithProxies(source.getName()));
				guardMethod.getParameters().add(guardParameter);
				// Return type
				guardMethod.setReturnType(this.typeReferenceBuilder.typeRef(Boolean.TYPE));
				// Annotations
				translateAnnotationsTo(source.getAnnotations(), guardMethod);
				guardMethod.getAnnotations().add(
						SARLJvmModelInferrer.this.annotationTypesBuilder.annotationRef(Generated.class));
				// Documentation
				this.jvmTypesBuilder.copyDocumentationTo(source, guardMethod);
				// Body and guard
				setBody(guardMethod, guard);


				this.associator.associateLogicalContainer(source.getExpression(), behaviorMethod);

				this.jvmTypesBuilder.setBody(behaviorMethod, new Procedures.Procedure1<ITreeAppendable>() {
					@SuppressWarnings("synthetic-access")
					@Override
					public void apply(ITreeAppendable it) {
						it.append("if (" + guardMethodName + "(" //$NON-NLS-1$//$NON-NLS-2$
								+ SARLKeywords.OCCURRENCE + ")) {").increaseIndentation(); //$NON-NLS-1$
						SARLJvmModelInferrer.this.xbaseCompiler.compile(source.getExpression(), it, voidType, null);
						it.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$
					}
				});

				inferredJvmType.getMembers().add(guardMethod);
			}
			// Container
			inferredJvmType.getMembers().add(behaviorMethod);
			this.associator.associatePrimary(source, behaviorMethod);

			context.incrementBehaviorUnitIndex();
		} else {
			this.log.fine(Messages.SARLJvmModelInferrer_10);
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

	/** Generate the given SARL signature into the given inferred type.
	 *
	 * @param sourceMember - the signature to generate.
	 * @param inferredJvmType - the target inferred type.
	 * @param allowDispatch - indicates if the 
	 */
	protected void transform(SarlAction sourceMember, JvmGenericType inferredJvmType, boolean allowDispatch) {
		final GenerationContext context = this.generationContexts.get(inferredJvmType);
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
				sourceMember.getExpression(),
				inferredJvmType.isInterface() || sourceMember.getExpression() == null,
				sourceMember.getModifiers().contains(Modifier.NATIVE.name().toLowerCase()),
				sourceMember.getModifiers().contains(Modifier.SYNCHRONIZED.name().toLowerCase()),
				sourceMember.getModifiers().contains(Modifier.STRICTFP.name().toLowerCase()),
				sourceMember.getModifiers().contains(Modifier.DISPATCH.name().toLowerCase()),
				sourceMember.getCreateExtensionInfo(),
				context.getInheritedOperationsToImplement(),
				context.getGeneratedOperations(),
				new Functions.Function1<ActionKey, Boolean>() {
					@Override
					public Boolean apply(ActionKey it) {
						return !context.getInheritedFinalOperations().containsKey(it)
								&& !context.getInheritedOverridableOperations().containsKey(it);
					}
				});
		if (operation != null) {
			context.incrementActionIndex();
		}
	}

	/** Generate the given SARL constructor into the given inferred type.
	 *
	 * @param sourceMember - the signature to generate.
	 * @param inferredJvmType - the target inferred type.
	 */
	@Override
	protected void transform(XtendConstructor sourceMember, JvmGenericType inferredJvmType) {
		GenerationContext context = this.generationContexts.get(inferredJvmType);
		assert (context != null);
		JvmConstructor operation = translateConstructor(
				sourceMember,
				inferredJvmType,
				sourceMember.getParameters(),
				sourceMember.getTypeParameters(),
				sourceMember.getExceptions(),
				sourceMember.getExpression(),
				context.getGeneratedConstructors());
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
		boolean isInterface = inferredJvmType.isInterface();
		for (JvmTypeReference superType : types) {
			if (superType.getType() instanceof JvmGenericType) {
				LightweightTypeReference reference = ModelUtil.toLightweightTypeReference(superType, this.services);
				if (reference.isInterfaceType() == isInterface && reference.isSubtypeOf(defaultType)) {
					inferredJvmType.getSuperTypes().add(this.jvmTypesBuilder.cloneWithProxies(superType));
				}
			}
		}
		if (inferredJvmType.getSuperTypes().isEmpty() && addDefault) {
			JvmTypeReference type = this.typeReferenceBuilder.typeRef(defaultType);
			inferredJvmType.getSuperTypes().add(type);
		}
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
			mainOperation.setReturnType(realReturnType);
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

	/** Generate the delegators for capacity methods.
	 *
	 * @param source - the cause of the addition.
	 * @param capacityType - the type of the capacity.
	 * @param inferredJvmType - the JVM container.
	 */
	@SuppressWarnings("unchecked")
	protected void addCapacityDeletatorOperationsTo(
			XtendMember source,
			JvmTypeReference capacityType,
			JvmGenericType inferredJvmType) {
		GenerationContext context = this.generationContexts.get(inferredJvmType);
		assert (context != null);
		if (capacityType.getType() instanceof JvmGenericType) {
			LightweightTypeReference reference = ModelUtil.toLightweightTypeReference(capacityType, this.services);
			if (reference.isSubtypeOf(io.sarl.lang.core.Capacity.class)) {
				final Map<ActionKey, JvmOperation> capacityOperations = CollectionLiterals.newTreeMap(null);

				ModelUtil.populateInterfaceElements(
						(JvmGenericType) capacityType.getType(),
						capacityOperations,
						null,
						this.sarlSignatureProvider);

				for (final Entry<ActionKey, JvmOperation> entry : capacityOperations.entrySet()) {
					if (!context.getGeneratedOperations().containsKey(entry.getKey())) {
						final JvmOperation sourceOperation = entry.getValue();
						JvmOperation operation = this.typesFactory.createJvmOperation();
						// name
						operation.setSimpleName(sourceOperation.getSimpleName());
						// Modifiers
						operation.setAbstract(false);
						operation.setNative(sourceOperation.isNative());
						operation.setSynchronized(sourceOperation.isSynchronized());
						operation.setStrictFloatingPoint(sourceOperation.isStrictFloatingPoint());
						operation.setFinal(false);
						operation.setVisibility(JvmVisibility.PROTECTED);
						operation.setStatic(false);
						operation.setVarArgs(sourceOperation.isVarArgs());
						// Parameters
						final List<String> argumentNames = CollectionLiterals.newArrayList();
						List<String> argumentTypes = CollectionLiterals.newArrayList();
						for (JvmFormalParameter sourceParameter : sourceOperation.getParameters()) {
							//FIXME: Copy annotations
							JvmFormalParameter parameter = this.typesFactory.createJvmFormalParameter();
							parameter.setName(sourceParameter.getSimpleName());
							parameter.setParameterType(
									this.jvmTypesBuilder.cloneWithProxies(sourceParameter.getParameterType()));
							operation.getParameters().add(parameter);
							argumentNames.add(parameter.getSimpleName());
							argumentTypes.add(parameter.getParameterType().getIdentifier());
						}
						// Return type
						JvmTypeReference returnType = sourceOperation.getReturnType();
						JvmTypeReference realReturnType = null;
						if (returnType != null) {
							realReturnType = this.jvmTypesBuilder.cloneWithProxies(returnType);
						} else {
							realReturnType = this.jvmTypesBuilder.inferredType();
						}
						operation.setReturnType(realReturnType);
						// Type parameters
						copyAndFixTypeParameters(sourceOperation.getTypeParameters(), operation);
						// Exceptions
						for (JvmTypeReference exception : sourceOperation.getExceptions()) {
							operation.getExceptions().add(this.jvmTypesBuilder.cloneWithProxies(exception));
						}
						// Annotations
						if (ModelUtil.hasAnnotation(sourceOperation, EarlyExit.class)) {
							operation.getAnnotations().add(this.annotationTypesBuilder.annotationRef(EarlyExit.class));
						}
						List<JvmTypeReference> firedEvents = ModelUtil.annotationClasses(sourceOperation, FiredEvent.class);
						if (!firedEvents.isEmpty()) {
							operation.getAnnotations().add(translateAnnotationClassRef(FiredEvent.class, firedEvents));
						}
						operation.getAnnotations().add(this.annotationTypesBuilder.annotationRef(Generated.class));
						operation.getAnnotations().add(translateAnnotationClassRef(ImportedCapacityFeature.class,
								Collections.singletonList(capacityType)));
						// Documentation
						String hyperrefLink = capacityType.getIdentifier() + "#" //$NON-NLS-1$
								+ entry.getValue().getSimpleName() + "(" //$NON-NLS-1$
								+ IterableExtensions.join(argumentTypes, ",") + ")"; //$NON-NLS-1$ //$NON-NLS-2$
						this.jvmTypesBuilder.setDocumentation(operation,
								MessageFormat.format(
										Messages.SARLJvmModelInferrer_13,
										hyperrefLink));
						// Body
						this.jvmTypesBuilder.setBody(operation, new Procedures.Procedure1<ITreeAppendable>() {
							@Override
							public void apply(ITreeAppendable it) {
								if (!Objects.equal("void", //$NON-NLS-1$
										sourceOperation.getReturnType().getIdentifier())) {
									it.append("return "); //$NON-NLS-1$
								}
								it.append("getSkill("); //$NON-NLS-1$
								it.append(sourceOperation.getDeclaringType().getQualifiedName());
								it.append(".class)."); //$NON-NLS-1$
								it.append(sourceOperation.getSimpleName());
								it.append("("); //$NON-NLS-1$
								it.append(IterableExtensions.join(argumentNames, ", ")); //$NON-NLS-1$
								it.append(");"); //$NON-NLS-1$
							}
						});
						// Container
						inferredJvmType.getMembers().add(operation);
						this.associator.associate(source, operation);

						context.getInheritedOperationsToImplement().remove(entry.getKey());
						context.getGeneratedOperations().put(entry.getKey(), sourceOperation);
						context.incrementActionIndex();
					}
				}
			}
		}
	}

	/** Add the serial field.
	 *
	 * @param source - the SARL type.
	 * @param inferredJvmType - the JVM container.
	 * @see "http://docs.oracle.com/javase/8/docs/platform/serialization/spec/class.html#a4100"
	 */
	protected void addSerialIDTo(XtendTypeDeclaration source, JvmGenericType inferredJvmType) {
		JvmField serialField = this.typesFactory.createJvmField();
		serialField.setSimpleName("serialVersionUID"); //$NON-NLS-1$
		serialField.setFinal(true);
		serialField.setStatic(true);
		serialField.setVisibility(JvmVisibility.PRIVATE);
		serialField.setType(this.typeReferenceBuilder.typeRef(long.class));

		this.jvmTypesBuilder.setInitializer(serialField, toStringConcatenation(
				ModelUtil.computeSerialVersionUID(inferredJvmType) + "L")); //$NON-NLS-1$

		serialField.getAnnotations().add(this.annotationTypesBuilder.annotationRef(Generated.class));
		SARLJvmModelInferrer.this.typeExtensions.setSynthetic(serialField, true);
		inferredJvmType.getMembers().add(serialField);
		this.associator.associate(source, serialField);

		SARLJvmModelInferrer.this.readAndWriteTracking.markInitialized(serialField, null);
	}

	/** Generate the missed functions with default value parameters in the given type declaration.
	 *
	 * @param source - the SARL type.
	 * @param inferredJvmType - the JVM container.
	 */
	protected void addMissedDefaultValueBasedFunctionsTo(
			XtendTypeDeclaration source,
			JvmGenericType inferredJvmType) {
		GenerationContext context = this.generationContexts.get(inferredJvmType);
		assert (context != null);

		String currentKeyStr = null;
		JvmOperation originalOperation = null;
		SignatureKey sigKey = null;

		for (Entry<ActionKey, JvmOperation> missedOperation : context.getInheritedOperationsToImplement().entrySet()) {
			String originalSignature = ModelUtil.annotationString(missedOperation.getValue(), DefaultValueUse.class);
			if (originalSignature != null) {
				if (!Objects.equal(originalSignature, currentKeyStr)) {
					currentKeyStr = originalSignature;
					sigKey = this.sarlSignatureProvider.createSignatureIDFromString(originalSignature);
					ActionKey key = this.sarlSignatureProvider.createActionID(
							missedOperation.getKey().getFunctionName(), sigKey);
					originalOperation = context.getInheritedOverridableOperations().get(key);
				}
				if (originalOperation != null) {
					JvmOperation operation = this.typesFactory.createJvmOperation();
					// name
					operation.setSimpleName(originalOperation.getSimpleName());
					// Modifiers
					operation.setAbstract(false);
					operation.setNative(originalOperation.isNative());
					operation.setSynchronized(originalOperation.isSynchronized());
					operation.setStrictFloatingPoint(originalOperation.isStrictFloatingPoint());
					operation.setFinal(true);
					operation.setVisibility(originalOperation.getVisibility());
					operation.setStatic(false);
					operation.setVarArgs(originalOperation.isVarArgs());
					// Parameters
					final List<String> arguments = CollectionLiterals.newArrayList();
					Iterator<JvmFormalParameter> it1 = missedOperation.getValue().getParameters().iterator();
					Iterator<JvmFormalParameter>  it2 = originalOperation.getParameters().iterator();
					JvmFormalParameter oparam = null;

					while (it2.hasNext()) {
						JvmFormalParameter param = it2.next();
						String vId = ModelUtil.annotationString(param, DefaultValue.class);
						if (oparam == null && it1.hasNext()) {
							oparam = it1.next();
						}
						if (oparam != null && Objects.equal(oparam.getSimpleName(), param.getSimpleName())) {
							arguments.add(oparam.getSimpleName());
							JvmFormalParameter parameter = this.typesFactory.createJvmFormalParameter();
							parameter.setName(oparam.getSimpleName());
							parameter.setParameterType(
									this.jvmTypesBuilder.cloneWithProxies(oparam.getParameterType()));
							operation.getParameters().add(parameter);
							oparam = null;
						} else if (!Strings.isNullOrEmpty(vId)) {
							arguments.add(
									originalOperation.getDeclaringType().getQualifiedName()
									+ "." //$NON-NLS-1$
									+ ModelUtil.PREFIX_ATTRIBUTE_DEFAULT_VALUE
									+ vId);
						} else {
							throw new IllegalStateException(Messages.SARLJvmModelInferrer_8);
						}
					}
					// Return type
					JvmTypeReference returnType = originalOperation.getReturnType();
					JvmTypeReference realReturnType = null;
					if (returnType != null) {
						realReturnType = this.jvmTypesBuilder.cloneWithProxies(returnType);
					} else {
						realReturnType = this.jvmTypesBuilder.inferredType();
					}
					operation.setReturnType(realReturnType);
					// Type parameters
					copyAndFixTypeParameters(originalOperation.getTypeParameters(), operation);
					// Exceptions
					for (JvmTypeReference exception : originalOperation.getExceptions()) {
						operation.getExceptions().add(this.jvmTypesBuilder.cloneWithProxies(exception));
					}
					// Annotations
					if (ModelUtil.hasAnnotation(operation, EarlyExit.class)) {
						operation.getAnnotations().add(this.annotationTypesBuilder.annotationRef(EarlyExit.class));
					}
					List<JvmTypeReference> firedEvents = ModelUtil.annotationClasses(originalOperation, FiredEvent.class);
					if (!firedEvents.isEmpty()) {
						operation.getAnnotations().add(translateAnnotationClassRef(FiredEvent.class, firedEvents));
					}
					operation.getAnnotations().add(this.annotationTypesBuilder.annotationRef(DefaultValueUse.class,
							originalSignature));
					operation.getAnnotations().add(this.annotationTypesBuilder.annotationRef(Generated.class));
					// Documentation
					this.jvmTypesBuilder.copyDocumentationTo(originalOperation, operation);
					// Body
					final String tmpName = originalOperation.getSimpleName();
					this.jvmTypesBuilder.setBody(operation, new Procedures.Procedure1<ITreeAppendable>() {
						@Override
						public void apply(ITreeAppendable it) {
							it.append(tmpName);
							it.append("("); //$NON-NLS-1$
							it.append(IterableExtensions.join(arguments, ", ")); //$NON-NLS-1$
							it.append(");"); //$NON-NLS-1$
						}
					});
					// Container
					inferredJvmType.getMembers().add(operation);
					this.associator.associate(source, operation);

					context.incrementActionIndex();
				}
			}
		}
	}

	/** Add the standard functions related to the fields.
	 * 
	 * The typical functions are: {@link #toString()}, {@link #hashCode()}, and {@link #equals(Object)}.
	 * 
	 * @param sourceMember - the SARL source.
	 * @param inferredJvmType - the target type.
	 */
	protected void addStandardFunctionsTo(XtendTypeDeclaration sourceMember, JvmGenericType inferredJvmType) {
		List<JvmField> fields = CollectionLiterals.newArrayList();
		for (JvmMember member : inferredJvmType.getMembers()) {
			if (member instanceof JvmField) {
				fields.add((JvmField) member);
			}
		}

		JvmOperation op = getEqualsMethod(sourceMember, inferredJvmType, fields);
		if (op != null) {
			op.getAnnotations().add(SARLJvmModelInferrer.this.annotationTypesBuilder.annotationRef(Generated.class));
			SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
			inferredJvmType.getMembers().add(op);
			this.associator.associate(sourceMember, inferredJvmType);
		}

		op = getHashCodeMethod(sourceMember, inferredJvmType, fields);
		if (op != null) {
			op.getAnnotations().add(SARLJvmModelInferrer.this.annotationTypesBuilder.annotationRef(Generated.class));
			SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
			inferredJvmType.getMembers().add(op);
			this.associator.associate(sourceMember, inferredJvmType);
		}

		op = getAttributeToStringMethod(sourceMember, inferredJvmType, fields);
		if (op != null) {
			op.getAnnotations().add(SARLJvmModelInferrer.this.annotationTypesBuilder.annotationRef(Generated.class));
			SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
			inferredJvmType.getMembers().add(op);
			this.associator.associate(sourceMember, inferredJvmType);
		}
	}

	/** Generate the "equals()" operation.
	 * This function was deprecated in Xbase, and should be provided by DSL
	 * providers now.
	 *
	 * @param sourceMember - the SARL source.
	 * @param inferredJvmType - the target type.
	 * @param fields - the fields declared in the container.
	 * @return the "equals" function.
	 */
	protected JvmOperation getAttributeToStringMethod(XtendTypeDeclaration sourceMember, final JvmGenericType inferredJvmType,
			final List<JvmField> fields) {
		JvmOperation operation = this.typesFactory.createJvmOperation();
		// name
		operation.setSimpleName("attributesToString"); //$NON-NLS-1$
		// Modifiers
		operation.setAbstract(false);
		operation.setNative(false);
		operation.setSynchronized(true);
		operation.setStrictFloatingPoint(false);
		operation.setFinal(false);
		operation.setVisibility(JvmVisibility.PROTECTED);
		operation.setStatic(false);
		operation.setVarArgs(false);
		// Parameters
		JvmFormalParameter parameter = this.typesFactory.createJvmFormalParameter();
		parameter.setName("obj"); //$NON-NLS-1$
		parameter.setParameterType(this.typeReferenceBuilder.typeRef(Object.class));
		operation.getParameters().add(parameter);
		// Return type
		JvmTypeReference returnType = this.typeReferenceBuilder.typeRef(String.class);
		JvmTypeReference realReturnType = null;
		if (returnType != null) {
			realReturnType = this.jvmTypesBuilder.cloneWithProxies(returnType);
		} else {
			realReturnType = this.jvmTypesBuilder.inferredType();
		}
		operation.setReturnType(realReturnType);
		// Annotations
		operation.getAnnotations().add(this.annotationTypesBuilder.annotationRef(Override.class));
		// Documentation
		this.jvmTypesBuilder.setDocumentation(operation, MessageFormat.format(Messages.SARLJvmModelInferrer_2,
				sourceMember.getName()));
		// Body
		this.jvmTypesBuilder.setBody(operation, new Procedures.Procedure1<ITreeAppendable>() {
			@Override
			public void apply(ITreeAppendable it) {
				it.append("StringBuilder result = new StringBuilder(" //$NON-NLS-1$
						+ "super.attributesToString());").newLine(); //$NON-NLS-1$
				for (JvmField attr : fields) {
					it.append("result.append(\"" + attr.getSimpleName() //$NON-NLS-1$
							+ "  = \").append(this." //$NON-NLS-1$
							+ attr.getSimpleName() + ");").newLine(); //$NON-NLS-1$
				}
				it.append("return result.toString();"); //$NON-NLS-1$
			}
		});
		return operation;
	}

	/** Generate the "equals()" operation.
	 * This function was deprecated in Xbase, and should be provided by DSL
	 * providers now.
	 *
	 * @param sourceMember - the SARL source.
	 * @param inferredJvmType - the target type.
	 * @param fields - the fields declared in the container.
	 * @return the "equals" function.
	 */
	protected JvmOperation getEqualsMethod(XtendTypeDeclaration sourceMember, final JvmGenericType inferredJvmType,
			final List<JvmField> fields) {
		JvmOperation operation = this.typesFactory.createJvmOperation();
		// name
		operation.setSimpleName("equals"); //$NON-NLS-1$
		// Modifiers
		operation.setAbstract(false);
		operation.setNative(false);
		operation.setSynchronized(true);
		operation.setStrictFloatingPoint(false);
		operation.setFinal(false);
		operation.setVisibility(JvmVisibility.PUBLIC);
		operation.setStatic(false);
		operation.setVarArgs(false);
		// Parameters
		JvmFormalParameter parameter = this.typesFactory.createJvmFormalParameter();
		parameter.setName("obj"); //$NON-NLS-1$
		parameter.setParameterType(this.typeReferenceBuilder.typeRef(Object.class));
		operation.getParameters().add(parameter);
		// Return type
		JvmTypeReference returnType = this.typeReferenceBuilder.typeRef(Boolean.TYPE);
		JvmTypeReference realReturnType = null;
		if (returnType != null) {
			realReturnType = this.jvmTypesBuilder.cloneWithProxies(returnType);
		} else {
			realReturnType = this.jvmTypesBuilder.inferredType();
		}
		operation.setReturnType(realReturnType);
		// Annotations
		operation.getAnnotations().add(this.annotationTypesBuilder.annotationRef(Override.class));
		// Body
		this.jvmTypesBuilder.setBody(operation, new Procedures.Procedure1<ITreeAppendable>() {
			@Override
			public void apply(ITreeAppendable it) {
				it.append("if (this == obj)").increaseIndentation(); //$NON-NLS-1$
				it.newLine().append("return true;").decreaseIndentation(); //$NON-NLS-1$
				it.newLine().append("if (obj == null)").increaseIndentation(); //$NON-NLS-1$
				it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				it.newLine().append("if (getClass() != obj.getClass())").increaseIndentation(); //$NON-NLS-1$
				it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				it.newLine().append("if (!super.equals(obj))").increaseIndentation(); //$NON-NLS-1$
				it.newLine().append("return false;").decreaseIndentation(); //$NON-NLS-1$
				it.newLine().append(inferredJvmType.getSimpleName() + " other = (" //$NON-NLS-1$
						+ inferredJvmType.getSimpleName() + ") obj;"); //$NON-NLS-1$
				for (JvmField field : fields) {
					generateToEqualForField(it, field);
				}
				it.newLine().append("return true;"); //$NON-NLS-1$
			}

			private boolean arrayContains(String element, String... array) {
				for (String elt : array) {
					if (Objects.equal(elt, element)) {
						return true;
					}
				}
				return false;
			}

			private void generateToEqualForField(ITreeAppendable it, JvmField field) {
				String typeName = field.getType().getIdentifier();
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
		return operation;
	}

	/** Generate the "hashCode()" operation.
	 * This function was deprecated in Xbase, and should be provided by DSL
	 * providers now.
	 *
	 * @param sourceMember - the SARL source.
	 * @param inferredJvmType - the target type.
	 * @param fields - the fields declared in the container.
	 * @return the "hashCode" function.
	 */
	protected JvmOperation getHashCodeMethod(XtendTypeDeclaration sourceMember, JvmGenericType inferredJvmType,
			final List<JvmField> fields) {
		JvmOperation operation = this.typesFactory.createJvmOperation();
		// name
		operation.setSimpleName("hashCode"); //$NON-NLS-1$
		// Modifiers
		operation.setAbstract(false);
		operation.setNative(false);
		operation.setSynchronized(true);
		operation.setStrictFloatingPoint(false);
		operation.setFinal(false);
		operation.setVisibility(JvmVisibility.PUBLIC);
		operation.setStatic(false);
		operation.setVarArgs(false);
		// Parameters
		JvmFormalParameter parameter = this.typesFactory.createJvmFormalParameter();
		parameter.setName("obj"); //$NON-NLS-1$
		parameter.setParameterType(this.typeReferenceBuilder.typeRef(Object.class));
		operation.getParameters().add(parameter);
		// Return type
		JvmTypeReference returnType = this.typeReferenceBuilder.typeRef(Integer.TYPE);
		JvmTypeReference realReturnType = null;
		if (returnType != null) {
			realReturnType = this.jvmTypesBuilder.cloneWithProxies(returnType);
		} else {
			realReturnType = this.jvmTypesBuilder.inferredType();
		}
		operation.setReturnType(realReturnType);
		// Annotations
		operation.getAnnotations().add(this.annotationTypesBuilder.annotationRef(Override.class));
		// Body
		this.jvmTypesBuilder.setBody(operation, new Procedures.Procedure1<ITreeAppendable>() {
			@Override
			public void apply(ITreeAppendable it) {
				it.append("final int prime = 31;"); //$NON-NLS-1$
				it.newLine().append("int result = super.hashCode();"); //$NON-NLS-1$
				for (JvmField field : fields) {
					String typeName = field.getType().getIdentifier();
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
			}
		});
		return operation;
	}

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
	private static class GenerationContext {

		private final JvmGenericType inferredContainer;
		private final GenerationContext parent;
		private int actionIndex;
		private int behaviorUnitIndex;
		private Map<ActionKey, JvmOperation> finalOperations;
		private Map<ActionKey, JvmOperation>  overridableOperations;
		private Map<ActionKey, JvmOperation>  operationsToImplement;
		private Map<String, JvmField> inheritedFields;
		private Map<SignatureKey, JvmConstructor> generatedConstructors;
		private Map<ActionKey, JvmOperation> implementedOperations;
		private List<String> latelyGenerateParameters = CollectionLiterals.newArrayList();

		/** Construct a information about the generation.
		 * 
		 * @param inferredContainer - the inferred container that is the top context.
		 */
		@SuppressWarnings("unchecked")
		public GenerationContext(JvmGenericType inferredContainer) {
			this.parent = null;
			this.inferredContainer =  inferredContainer;
			this.finalOperations = CollectionLiterals.newTreeMap(null);
			this.overridableOperations = CollectionLiterals.newTreeMap(null);
			this.operationsToImplement = CollectionLiterals.newTreeMap(null);
			this.inheritedFields = CollectionLiterals.newTreeMap(null);
			this.generatedConstructors = CollectionLiterals.newTreeMap(null);
			this.implementedOperations = CollectionLiterals.newTreeMap(null);
		}

		/** Construct a information about the generation.
		 * 
		 * @param context - the parent context.
		 */
		@SuppressWarnings("unchecked")
		public GenerationContext(GenerationContext context) {
			this.parent = context;
			this.inferredContainer = null;
			this.finalOperations = CollectionLiterals.newTreeMap(null);
			this.overridableOperations = CollectionLiterals.newTreeMap(null);
			this.operationsToImplement = CollectionLiterals.newTreeMap(null);
			this.inheritedFields = CollectionLiterals.newTreeMap(null);
			this.generatedConstructors = CollectionLiterals.newTreeMap(null);
			this.implementedOperations = CollectionLiterals.newTreeMap(null);
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

		/** Populate the inheritance context from the given type.
		 *
		 * @param inferredJvmType - the type to populate from.
		 * @param sarlSignatureProvider - the provider of SARL signatures.
		 * @see #getInheritedFinalOperations()
		 * @see #getInheritedOperationsToImplement()
		 * @see #getInheritedOverridableOperations()
		 * @see #getGeneratedConstructors()
		 * @see #getGeneratedOperations()
		 */
		@SuppressWarnings("unchecked")
		public void populateInheritanceContext(JvmGenericType inferredJvmType, ActionSignatureProvider sarlSignatureProvider) {
			this.finalOperations = CollectionLiterals.newTreeMap(null);
			this.overridableOperations = CollectionLiterals.newTreeMap(null);
			this.operationsToImplement = CollectionLiterals.newTreeMap(null);
			this.generatedConstructors = CollectionLiterals.newTreeMap(null);
			this.implementedOperations = CollectionLiterals.newTreeMap(null);
			ModelUtil.populateInheritanceContext(
					inferredJvmType,
					this.finalOperations,
					this.overridableOperations,
					this.inheritedFields,
					this.operationsToImplement,
					null,
					sarlSignatureProvider);
		}

		/** Replies the list of the inherited final operations.
		 *
		 * @return the list of the inherited final operations.
		 * @see #populateInheritanceContext(JvmGenericType, ActionSignatureProvider)
		 */
		public Map<ActionKey, JvmOperation> getInheritedFinalOperations() {
			return this.finalOperations;
		}

		/** Replies the list of the overridable operations.
		 *
		 * @return the list of the overridable operations.
		 * @see #populateInheritanceContext(JvmGenericType, ActionSignatureProvider)
		 */
		public Map<ActionKey, JvmOperation>  getInheritedOverridableOperations() {
			return this.overridableOperations;
		}

		/** Replies the list of the super-type operations to implement.
		 *
		 * @return the list of the super-type operations to implement.
		 * @see #populateInheritanceContext(JvmGenericType, ActionSignatureProvider)
		 */
		public Map<ActionKey, JvmOperation>  getInheritedOperationsToImplement() {
			return this.operationsToImplement;
		}

		/** Replies the list of the generated constructors.
		 *
		 * @return the list of the generated constructors.
		 * @see #populateInheritanceContext(JvmGenericType, ActionSignatureProvider)
		 */
		public Map<SignatureKey, JvmConstructor> getGeneratedConstructors() {
			return this.generatedConstructors;
		}

		/** Replies the list of the generated operations.
		 *
		 * @return the list of the generated operations.
		 * @see #populateInheritanceContext(JvmGenericType, ActionSignatureProvider)
		 */
		public Map<ActionKey, JvmOperation> getGeneratedOperations() {
			return this.implementedOperations;
		}

	}

}
