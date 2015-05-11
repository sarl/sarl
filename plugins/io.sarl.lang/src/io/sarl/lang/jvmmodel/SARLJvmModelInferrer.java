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
import io.sarl.lang.actionprototype.ActionParameterTypes;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.actionprototype.ActionPrototypeProvider;
import io.sarl.lang.actionprototype.InferredPrototype;
import io.sarl.lang.actionprototype.InferredStandardParameter;
import io.sarl.lang.actionprototype.InferredValuedParameter;
import io.sarl.lang.actionprototype.QualifiedActionName;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.EarlyExit;
import io.sarl.lang.annotation.FiredEvent;
import io.sarl.lang.annotation.Generated;
import io.sarl.lang.annotation.ImportedCapacityFeature;
import io.sarl.lang.controlflow.SARLExtendedEarlyExitComputer;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Percept;
import io.sarl.lang.core.Skill;
import io.sarl.lang.jvmmodel.JvmModelInferrerProber.Step;
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
import io.sarl.lang.util.Utils;

import java.lang.annotation.Annotation;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.UUID;
import java.util.logging.Logger;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.jvmmodel.SyntheticNameClashResolver;
import org.eclipse.xtend.core.jvmmodel.XtendJvmModelInferrer;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociator;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeExtensions;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.validation.ReadAndWriteTracking;

import com.google.common.base.Objects;
import com.google.common.base.Optional;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import com.google.inject.Singleton;

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
@Singleton
public class SARLJvmModelInferrer extends XtendJvmModelInferrer {

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
	private ActionPrototypeProvider sarlSignatureProvider;

	/** Tracker of field initialization.
	 */
	@Inject
	private ReadAndWriteTracking readAndWriteTracking;

	/** Several generation services.
	 */
	@Inject
	private CommonTypeComputationServices services;

	/** JVM type services.
	 */
	@Inject
	private JvmTypeExtensions typeExtensions;

	/** Computer of early-exits for SARL.
	 */
	@Inject
	private SARLExtendedEarlyExitComputer earlyExitComputer;

	/** SARL Serializer.
	 */
	@Inject
	private ISerializer sarlSerializer;

	/** Prober for the internal behavior on this inferrer.
	 */
	@Inject
	private Optional<JvmModelInferrerProber> inferrerProber;

	@Inject
	private TypesFactory typesFactory;

	@Inject
	private TypeReferences typeReferences;

	@Inject
	private SyntheticNameClashResolver nameClashResolver;

	@Inject
	private IJvmModelAssociator associator;

	/** Generation contexts.
	 */
	private Map<String, GenerationContext> ctx = new TreeMap<>();

	private static String contextKey(JvmGenericType type) {
		return type.eResource().getURI() + "/" + type.getQualifiedName(); //$NON-NLS-1$
	}

	private void openContext(JvmGenericType type, GenerationContext context) {
		this.ctx.put(contextKey(type), context);
	}

	private void closeContext(JvmGenericType type) {
		this.ctx.remove(contextKey(type));
	}

	private GenerationContext getContext(JvmGenericType type) {
		return this.ctx.get(contextKey(type));
	}

	@Override
	protected JvmDeclaredType doInferTypeSceleton(
			XtendTypeDeclaration declaration,
			IJvmDeclaredTypeAcceptor acceptor, boolean preIndexingPhase,
			XtendFile xtendFile, List<Runnable> doLater) {
		JvmDeclaredType type = super.doInferTypeSceleton(declaration, acceptor, preIndexingPhase,
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
		return null;
	}

	/** Initialize the SARL agent type.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 */
	protected void initialize(SarlAgent source, JvmGenericType inferredJvmType) {
		try {
			// Create the generation context that is used by the other transformation functions.
			GenerationContext context = new GenerationContext() {
				@Override
				public boolean isSupportedMember(XtendMember member) {
					if ((member instanceof XtendField)
						|| (member instanceof XtendConstructor)
						|| (member instanceof SarlBehaviorUnit)
						|| (member instanceof SarlCapacityUses)
						|| (member instanceof SarlRequiredCapacity)) {
						return true;
					}
					if (member instanceof XtendFunction
						&& ((XtendFunction) member).getExpression() != null) {
						return true;
					}
					return false;
				}

			};
			openContext(inferredJvmType, context);

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
			appendConstrainedExtends(context, inferredJvmType, Agent.class, source.getExtends());

			// Generate the members of the generated type.
			appendSarlMembers(
					inferredJvmType,
					source,
					context);

			// Add the default constructors for the agent, if not already added
			if (!context.hasConstructor()) {
				// new(parentID: UUID)
				JvmConstructor constructor = this.typesFactory.createJvmConstructor();
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
				constructor.setSimpleName(source.getName());
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				this.typeBuilder.setDocumentation(constructor, MessageFormat.format(
						Messages.SARLJvmModelInferrer_6, "parentID")); //$NON-NLS-1$
				JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName("parentID"); //$NON-NLS-1$
				jvmParam.setParameterType(this._typeReferenceBuilder.typeRef(UUID.class));
				this.associator.associate(source, jvmParam);
				constructor.getParameters().add(jvmParam);
				this.typeBuilder.setBody(constructor,
						toStringConcatenation("super(parentID, null);")); //$NON-NLS-1$
				constructor.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Generated.class));

				// new(parentID : UUID, agentID : UUID)
				constructor = this.typesFactory.createJvmConstructor();
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
				constructor.setSimpleName(source.getName());
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				this.typeBuilder.setDocumentation(constructor, MessageFormat.format(
						Messages.SARLJvmModelInferrer_7,
						"parentID", "agentID")); //$NON-NLS-1$ //$NON-NLS-2$
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
				this.typeBuilder.setBody(constructor,
						toStringConcatenation("super(parentID, agentID);")); //$NON-NLS-1$
				constructor.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Generated.class));
			}

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			closeContext(inferredJvmType);
		}
	}

	/** Initialize the SARL behavior type.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 */
	protected void initialize(SarlBehavior source, JvmGenericType inferredJvmType) {
		try {
			// Create the generation context that is used by the other transformation functions.
			GenerationContext context = new GenerationContext() {
				@Override
				public boolean isSupportedMember(XtendMember member) {
					if ((member instanceof XtendField)
						|| (member instanceof XtendConstructor)
						|| (member instanceof SarlBehaviorUnit)
						|| (member instanceof SarlCapacityUses)
						|| (member instanceof SarlRequiredCapacity)) {
						return true;
					}
					if (member instanceof XtendFunction
						&& ((XtendFunction) member).getExpression() != null) {
						return true;
					}
					return false;
				}
			};
			openContext(inferredJvmType, context);

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
			appendConstrainedExtends(context, inferredJvmType, Behavior.class, source.getExtends());

			// Generate the members of the generated type.
			appendSarlMembers(
					inferredJvmType,
					source,
					context);

			// Add the default constructors for the behavior, if not already added

			if (!context.hasConstructor()) {
				// new(owner: Agent)
				JvmConstructor constructor = this.typesFactory.createJvmConstructor();
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
				constructor.setSimpleName(source.getName());
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				this.typeBuilder.setDocumentation(constructor, MessageFormat.format(
						Messages.SARLJvmModelInferrer_5, "owner")); //$NON-NLS-1$
				JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName("owner"); //$NON-NLS-1$
				jvmParam.setParameterType(this._typeReferenceBuilder.typeRef(Agent.class));
				this.associator.associate(source, jvmParam);
				constructor.getParameters().add(jvmParam);
				this.typeBuilder.setBody(constructor,
						toStringConcatenation("super(owner);")); //$NON-NLS-1$
				constructor.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Generated.class));
			}

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			closeContext(inferredJvmType);
		}
	}

	/** Initialize the SARL event type.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 */
	protected void initialize(SarlEvent source, JvmGenericType inferredJvmType) {
		try {
			// Create the generation context that is used by the other transformation functions.
			GenerationContext context = new GenerationContext() {
				@Override
				public boolean isSupportedMember(XtendMember member) {
					return ((member instanceof XtendField)
							|| (member instanceof XtendConstructor));
				}
			};
			openContext(inferredJvmType, context);

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
			appendConstrainedExtends(context, inferredJvmType, Event.class, source.getExtends());

			// Generate the members of the generated type.
			appendSarlMembers(
					inferredJvmType,
					source,
					context);

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
				this.typeBuilder.setBody(constructor, toStringConcatenation("super();")); //$NON-NLS-1$
				constructor.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Generated.class));

				// new(source: Address)
				constructor = this.typesFactory.createJvmConstructor();
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
				constructor.setSimpleName(source.getName());
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				this.typeBuilder.setDocumentation(constructor,
						MessageFormat.format(Messages.SARLJvmModelInferrer_1, "source")); //$NON-NLS-1$
				JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName("source"); //$NON-NLS-1$
				jvmParam.setParameterType(this._typeReferenceBuilder.typeRef(Address.class));
				this.associator.associate(source, jvmParam);
				constructor.getParameters().add(jvmParam);
				this.typeBuilder.setBody(constructor,
						toStringConcatenation("super(source);")); //$NON-NLS-1$
				constructor.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Generated.class));
			}

			// Add functions dedicated to comparisons (equals, hashCode, etc.)
			appendComparisonFunctions(context, source, inferredJvmType);

			// Add functions dedicated to String representation(toString, etc.)
			appendToStringFunctions(context, source, inferredJvmType);

			// Add the serial number
			appendSerialNumber(context, source, inferredJvmType);

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			closeContext(inferredJvmType);
		}
	}

	/** Initialize the SARL skill type.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 */
	protected void initialize(SarlSkill source, JvmGenericType inferredJvmType) {
		try {
			// Create the generation context that is used by the other transformation functions.
			GenerationContext context = new GenerationContext() {
				@Override
				public boolean isSupportedMember(XtendMember member) {
					if ((member instanceof XtendField)
						|| (member instanceof XtendConstructor)
						|| (member instanceof SarlBehaviorUnit)
						|| (member instanceof SarlCapacityUses)
						|| (member instanceof SarlRequiredCapacity)) {
						return true;
					}
					if (member instanceof XtendFunction
						&& ((XtendFunction) member).getExpression() != null) {
						return true;
					}
					return false;
				}
			};
			openContext(inferredJvmType, context);

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
			appendConstrainedExtends(context, inferredJvmType, Skill.class, source.getExtends());
			appendConstrainedImplements(context, inferredJvmType, Capacity.class, source.getImplements());

			// Generate the members of the generated type.
			appendSarlMembers(
					inferredJvmType,
					source,
					context);

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
				this.typeBuilder.setBody(constructor, toStringConcatenation("super();")); //$NON-NLS-1$
				constructor.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Generated.class));

				// new(owner: Agent)
				constructor = this.typesFactory.createJvmConstructor();
				inferredJvmType.getMembers().add(constructor);
				this.associator.associate(source, constructor);
				constructor.setSimpleName(source.getName());
				constructor.setVisibility(JvmVisibility.PUBLIC);
				this.typeExtensions.setSynthetic(constructor, true);
				this.typeBuilder.setDocumentation(constructor,
						MessageFormat.format(Messages.SARLJvmModelInferrer_3, "owner")); //$NON-NLS-1$
				JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName("owner"); //$NON-NLS-1$
				jvmParam.setParameterType(this._typeReferenceBuilder.typeRef(Agent.class));
				this.associator.associate(source, jvmParam);
				constructor.getParameters().add(jvmParam);
				this.typeBuilder.setBody(constructor,
						toStringConcatenation("super(owner);")); //$NON-NLS-1$
				constructor.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Generated.class));
			}

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			closeContext(inferredJvmType);
		}
	}

	/** Initialize the SARL capacity type.
	 *
	 * @param source the source.
	 * @param inferredJvmType the JVM type.
	 */
	protected void initialize(SarlCapacity source, JvmGenericType inferredJvmType) {
		try {
			// Create the generation context that is used by the other transformation functions.
			GenerationContext context = new GenerationContext() {
				@Override
				public boolean isSupportedMember(XtendMember member) {
					return (member instanceof XtendFunction
							&& ((XtendFunction) member).getExpression() == null);
				}
			};
			openContext(inferredJvmType, context);

			// Copy the documentation
			this.typeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			inferredJvmType.setInterface(true);
			inferredJvmType.setAbstract(true);
			inferredJvmType.setVisibility(source.getVisibility());
			inferredJvmType.setStatic(false);
			inferredJvmType.setStrictFloatingPoint(false);
			if (!inferredJvmType.isAbstract()) {
				inferredJvmType.setFinal(source.isFinal());
			}

			// Generate the annotations.
			translateAnnotationsTo(source.getAnnotations(), inferredJvmType);

			// Generate the extended types.
			appendConstrainedExtends(context, inferredJvmType, Capacity.class, source.getExtends());

			// Generate the members of the generated type.
			appendSarlMembers(
					inferredJvmType,
					source,
					context);

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			closeContext(inferredJvmType);
		}
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
		QualifiedActionName actionKey = this.sarlSignatureProvider.createConstructorQualifiedName(container);

		// Generate all the constructor signatures related to the constructor to create.
		final InferredPrototype constructorSignatures = this.sarlSignatureProvider.createPrototypeFromSarlModel(
				actionKey,
				Utils.isVarArg(source.getParameters()), source.getParameters());

		// Generate the main Java constructor.
		final JvmConstructor constructor = this.typesFactory.createJvmConstructor();
		container.getMembers().add(constructor);
		this.associator.associatePrimary(source, constructor);
		this.typeBuilder.copyDocumentationTo(source, constructor);
		JvmVisibility visibility = source.getVisibility();
		constructor.setSimpleName(container.getSimpleName());
		constructor.setVisibility(visibility);
		constructor.setVarArgs(isVarArgs);

		// Generate the parameters
		List<InferredStandardParameter> paramList = constructorSignatures.getOriginalParameterTypes();
		translateSarlFormalParameters(
				context,
				constructor, container, isVarArgs,
				source.getParameters(), false, paramList);

		// Generate additional information (type parameters, exceptions...)
		copyAndFixTypeParameters(source.getTypeParameters(), constructor);
		for (JvmTypeReference exception : source.getExceptions()) {
			constructor.getExceptions().add(this.typeBuilder.cloneWithProxies(exception));
		}
		translateAnnotationsTo(source.getAnnotations(), constructor);

		// Set the body.
		setBody(constructor, source.getExpression());

		// The signature definition of the constructor.
		ActionParameterTypes sigKey = this.sarlSignatureProvider.createParameterTypesFromJvmModel(
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
				for (Entry<ActionParameterTypes, List<InferredStandardParameter>> entry
						: constructorSignatures.getInferredParameterTypes().entrySet()) {

					if (context == null || !context.getGeneratedConstructors().containsKey(entry.getKey())) {
						final List<InferredStandardParameter> otherSignature = entry.getValue();
						// Generate the additional constructor that is invoke the main constructor previously generated.
						JvmConstructor constructor2 = SARLJvmModelInferrer.this.typesFactory.createJvmConstructor();
						container.getMembers().add(constructor2);
						SARLJvmModelInferrer.this.typeBuilder.copyDocumentationTo(source, constructor2);
						JvmVisibility visibility = source.getVisibility();
						constructor2.setSimpleName(container.getSimpleName());
						constructor2.setVisibility(visibility);
						constructor2.setVarArgs(isVarArgs);

						List<String> args = translateSarlFormalParametersForSyntheticOperation(
								constructor2, container, isVarArgs, otherSignature);

						constructor2.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(
								DefaultValueUse.class,
								constructorSignatures.getFormalParameterTypes().toString()));
						constructor2.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(
								Generated.class));

						SARLJvmModelInferrer.this.typeBuilder.setBody(constructor2, toStringConcatenation(
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
		GenerationContext context = getContext(container);
		if (context != null) {
			String name = source.getName();
			if (name != null) {
				context.incrementSerial(name.hashCode());
			}
			JvmTypeReference type = source.getType();
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
	protected void transform(final XtendFunction source, final JvmGenericType container, boolean allowDispatch) {
		final GenerationContext context = getContext(container);
		final boolean isVarArgs = Utils.isVarArg(source.getParameters());
		final List<JvmTypeReference> firedEvents;
		if (source instanceof SarlAction) {
			firedEvents = ((SarlAction) source).getFiredEvents();
		} else {
			firedEvents = Collections.emptyList();
		}

		// Compute the operation name
		StringBuilder sourceNameBuffer = new StringBuilder(source.getName());
		JvmVisibility visibility = source.getVisibility();
		if (allowDispatch && source.isDispatch()) {
			if (source.getDeclaredVisibility() == null) {
				visibility = JvmVisibility.PROTECTED;
			}
			sourceNameBuffer.insert(0, "_"); //$NON-NLS-1$
		}
		final String sourceName = sourceNameBuffer.toString();

		// Compute the identifier of the action.
		QualifiedActionName actionKey = this.sarlSignatureProvider.createQualifiedActionName(
				container, sourceName);

		// Compute the different action prototypes associated to the action to create.
		final InferredPrototype actionSignatures = this.sarlSignatureProvider.createPrototypeFromSarlModel(
				actionKey,
				isVarArgs, source.getParameters());

		// Compute the action prototype of the action without optional parameter
		final ActionPrototype actSigKey = this.sarlSignatureProvider.createActionPrototype(
				sourceName,
				actionSignatures.getFormalParameterTypes());

		// Create the main function
		final JvmOperation operation = this.typesFactory.createJvmOperation();
		container.getMembers().add(operation);
		operation.setAbstract(source.isAbstract() || container.isInterface());
		operation.setNative(source.isNative());
		operation.setSynchronized(source.isSynchonized());
		operation.setStrictFloatingPoint(source.isStrictFloatingPoint());
		if (!source.isAbstract()) {
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

		// Generate the parameters
		List<InferredStandardParameter> paramList = actionSignatures.getOriginalParameterTypes();
		translateSarlFormalParameters(
				context,
				operation, container, isVarArgs,
				source.getParameters(),
				container.isInterface(), paramList);

		// Infer the return type
		XExpression expression = source.getExpression();

		JvmTypeReference returnTypeCandidate = null;
		if (source.getReturnType() != null) {
			returnTypeCandidate = source.getReturnType();
		} else if (context != null) {
			JvmOperation inheritedOperation = context.getInheritedFinalOperations().get(actSigKey);
			if (inheritedOperation == null) {
				inheritedOperation = context.getInheritedOverridableOperations().get(actSigKey);
			}
			if (inheritedOperation == null) {
				inheritedOperation = context.getInheritedOperationsToImplement().get(actSigKey);
			}
			if (inheritedOperation != null) {
				returnTypeCandidate = inheritedOperation.getReturnType();
			} else if (expression != null
					&& ((!(expression instanceof XBlockExpression))
						|| (!((XBlockExpression) expression).getExpressions().isEmpty()))) {
				returnTypeCandidate = this.typeBuilder.inferredType(expression);
			}
		} else if (expression != null
				&& ((!(expression instanceof XBlockExpression))
					|| (!((XBlockExpression) expression).getExpressions().isEmpty()))) {
			returnTypeCandidate = this.typeBuilder.inferredType(expression);
		}
		final JvmTypeReference returnType;
		if (returnTypeCandidate == null) {
			returnType = this._typeReferenceBuilder.typeRef(Void.TYPE);
		} else {
			returnType = returnTypeCandidate;
		}
		operation.setReturnType(this.typeBuilder.cloneWithProxies(returnType));

		// Type parameters
		copyAndFixTypeParameters(source.getTypeParameters(), operation);

		// Exceptions
		for (JvmTypeReference exception : source.getExceptions()) {
			operation.getExceptions().add(this.typeBuilder.cloneWithProxies(exception));
		}

		// Annotations
		translateAnnotationsTo(source.getAnnotations(), operation);
		if (source.isOverride()
				&& !Utils.hasAnnotation(operation, Override.class)
				&& this.typeReferences.findDeclaredType(Override.class, source) != null) {
			operation.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Override.class));
		}
		if (!source.isAbstract() && (expression == null || !this.services.getExpressionHelper().hasSideEffects(expression))) {
			// The function is pure
			if (!Utils.hasAnnotation(operation, Pure.class)
					&& this.typeReferences.findDeclaredType(Pure.class, source) != null) {
				operation.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Pure.class));
			}
		}

		// Detecting if the action is an early-exit action.
		// If true, the Java code is annotated to be usable by the SARL validator.
		//TODO: Generalize the detection of the EarlyExit
		boolean isEarlyExitTmp = false;
		Iterator<JvmTypeReference> eventIterator = firedEvents.iterator();
		while (!isEarlyExitTmp && eventIterator.hasNext()) {
			if (this.earlyExitComputer.isEarlyExitEvent(eventIterator.next())) {
				operation.getAnnotations().add(this._annotationTypesBuilder.annotationRef(EarlyExit.class));
				isEarlyExitTmp = true;
			}
		}
		final boolean isEarlyExit = isEarlyExitTmp;

		// Put the fired SARL events as Java annotations for beeing usable by the SARL validator.
		if (!firedEvents.isEmpty()) {
			operation.getAnnotations().add(annotationClassRef(FiredEvent.class, firedEvents));
		}

		// Create extension / Body
		if (!container.isInterface()) {
			setBody(operation, expression);
		}

		// 1. Ensure that the Java annotations related to the default value are really present.
		//    They may be not present if the generated action is a specific version of an inherited
		//    action with default values for parameters.
		// 2. Update the two collections that describes the implemented and implementable operations.
		if (context != null) {
			JvmOperation implementedOperation = context.getInheritedOperationsToImplement().remove(actSigKey);
			// Put the annotations that were defined in the implemented operation
			if (implementedOperation != null) {
				if (Utils.hasAnnotation(implementedOperation, DefaultValueSource.class)
						&& !Utils.hasAnnotation(operation, DefaultValueSource.class)) {
					operation.getAnnotations().add(this._annotationTypesBuilder.annotationRef(
							DefaultValueSource.class));
				}
				// Reinject the @DefaultValue annotations
				List<JvmFormalParameter> oParams = implementedOperation.getParameters();
				List<JvmFormalParameter> cParams = operation.getParameters();
				assert (oParams.size() == cParams.size());
				for (int i = 0; i < oParams.size(); ++i) {
					JvmFormalParameter op = oParams.get(i);
					JvmFormalParameter cp = cParams.get(i);
					String ovalue = Utils.annotationString(op, DefaultValue.class);
					if (ovalue != null
							&& !Utils.hasAnnotation(cp, DefaultValue.class)) {
						cp.getAnnotations().add(this._annotationTypesBuilder.annotationRef(
								DefaultValue.class,
								this.sarlSignatureProvider.qualifyDefaultValueID(
										implementedOperation.getDeclaringType().getIdentifier(),
										ovalue)));
					}
				}
			}
			// Add the main operation into the list of overridable operations
			context.getInheritedOverridableOperations().put(actSigKey, operation);
		}

		Runnable differedGeneration = new Runnable() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void run() {
				// Generate the Java functions that correspond to the action with the parameter default values applied.
				for (final Entry<ActionParameterTypes, List<InferredStandardParameter>> otherSignature
						: actionSignatures.getInferredParameterTypes().entrySet()) {
					ActionPrototype ak = SARLJvmModelInferrer.this.sarlSignatureProvider.createActionPrototype(
							sourceName,
							otherSignature.getKey());
					if (ak != null
							&& (context == null
							|| (!context.getInheritedFinalOperations().containsKey(ak)
									&& !context.getInheritedOverridableOperations().containsKey(ak)))) {

						// Generate the additional constructor that is invoke the main constructor previously generated.
						final JvmOperation operation2 = SARLJvmModelInferrer.this.typesFactory.createJvmOperation();
						container.getMembers().add(operation2);
						SARLJvmModelInferrer.this.typeBuilder.copyDocumentationTo(source, operation2);
						operation2.setSimpleName(operation.getSimpleName());
						operation2.setVisibility(operation.getVisibility());
						operation2.setVarArgs(operation.isVarArgs());
						operation2.setAbstract(operation.isAbstract());
						operation2.setDeprecated(operation.isDeprecated());
						operation2.setReturnType(
								SARLJvmModelInferrer.this.typeBuilder.cloneWithProxies(returnType));
						operation2.setFinal(!container.isInterface());
						operation2.setNative(false);
						operation2.setStrictFloatingPoint(false);
						operation2.setSynchronized(false);

						for (JvmTypeReference exception : operation.getExceptions()) {
							operation2.getExceptions().add(SARLJvmModelInferrer.this.typeBuilder.cloneWithProxies(exception));
						}

						translateAnnotationsTo(source.getAnnotations(), operation2);
						if (source.isOverride()
								&& !Utils.hasAnnotation(operation, Override.class)
								&& SARLJvmModelInferrer.this.typeReferences.findDeclaredType(Override.class, source) != null) {
							operation.getAnnotations().add(
									SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Override.class));
						}

						final List<String> args = translateSarlFormalParametersForSyntheticOperation(
								operation2, container, isVarArgs, otherSignature.getValue());

						if (!operation2.isAbstract()) {
							SARLJvmModelInferrer.this.typeBuilder.setBody(operation2,
									new Procedures.Procedure1<ITreeAppendable>() {
								@Override
								public void apply(ITreeAppendable it) {
									JvmTypeReference type = operation2.getReturnType();
									if (!SARLJvmModelInferrer.this.typeReferences.is(type, void.class)) {
										it.append("return "); //$NON-NLS-1$
									}
									it.append(sourceName);
									it.append("("); //$NON-NLS-1$
									it.append(IterableExtensions.join(args, ", ")); //$NON-NLS-1$
									it.append(");"); //$NON-NLS-1$
								}
							});
						}

						operation2.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(
								DefaultValueUse.class,
								actionSignatures.getFormalParameterTypes().toString()));
						operation2.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(
								Generated.class));

						// If the main action is an early-exit action, the additional operation is also an early-exit operation.
						//TODO: Generalize the detection of the EarlyExit
						if (isEarlyExit) {
							operation2.getAnnotations().add(
									SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(EarlyExit.class));
						}

						// Put the fired SARL events as Java annotations for beeing usable by the SARL validator.
						if (!firedEvents.isEmpty()) {
							operation2.getAnnotations().add(annotationClassRef(FiredEvent.class, firedEvents));
						}

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

	/** Transform the given behavior unit.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 */
	protected void transform(final SarlBehaviorUnit source, JvmGenericType container) {
		final GenerationContext context = getContext(container);
		if (source.getName() != null && context != null) {
			final XExpression guard = source.getGuard();
			final boolean isTrueGuard;

			// Check the guard value
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

			// Determine the name of the operation for the behavior output
			final JvmTypeReference voidType = this._typeReferenceBuilder.typeRef(Void.TYPE);
			final String behName = Utils.PREFIX_ACTION_HANDLE + source.getName().getSimpleName() + "_"  //$NON-NLS-1$
					+ context.getBehaviorUnitIndex();

			// Create the main function
			JvmOperation operation = this.typesFactory.createJvmOperation();

			// Annotations from the code
			translateAnnotationsTo(source.getAnnotations(), operation);

			// Annotation for the event bus
			operation.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Percept.class));

			// Behavior unit parameters
			JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
			jvmParam.setName(SARLKeywords.OCCURRENCE);
			jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(source.getName()));
			this.associator.associate(source, jvmParam);
			operation.getParameters().add(jvmParam);

			// Body of the behavior unit
			if (isTrueGuard) {
				this.typeBuilder.setBody(operation, source.getExpression());
			} else {
				assert (guard != null);

				final String guardMethodName = Utils.PREFIX_HANDLE_GUARD + source.getName().getSimpleName()
						+ "_" + context.getBehaviorUnitIndex(); //$NON-NLS-1$
				JvmOperation guardOperation = this.typesFactory.createJvmOperation();
				guardOperation.setAbstract(false);
				guardOperation.setNative(false);
				guardOperation.setSynchronized(false);
				guardOperation.setStrictFloatingPoint(false);
				guardOperation.setFinal(false);
				guardOperation.setVisibility(JvmVisibility.PRIVATE);
				guardOperation.setStatic(false);
				guardOperation.setSimpleName(guardMethodName);
				guardOperation.setReturnType(this._typeReferenceBuilder.typeRef(Boolean.TYPE));
				container.getMembers().add(guardOperation);
				this.associator.associatePrimary(source, guardOperation);
				jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName(SARLKeywords.OCCURRENCE);
				jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(source.getName()));
				this.associator.associate(source, jvmParam);
				guardOperation.getParameters().add(jvmParam);
				this.typeBuilder.setBody(guardOperation, guard);
				guardOperation.getAnnotations().add(
						this._annotationTypesBuilder.annotationRef(Generated.class));
				this.typeExtensions.setSynthetic(guardOperation, true);

				final String bodyMethodName = Utils.PREFIX_HANDLE_BODY + source.getName().getSimpleName()
						+ "_" + context.getBehaviorUnitIndex(); //$NON-NLS-1$
				JvmOperation bodyOperation = this.typesFactory.createJvmOperation();
				bodyOperation.setAbstract(false);
				bodyOperation.setNative(false);
				bodyOperation.setSynchronized(false);
				bodyOperation.setStrictFloatingPoint(false);
				bodyOperation.setFinal(false);
				bodyOperation.setVisibility(JvmVisibility.PRIVATE);
				bodyOperation.setStatic(false);
				bodyOperation.setSimpleName(bodyMethodName);
				bodyOperation.setReturnType(voidType);
				container.getMembers().add(bodyOperation);
				this.associator.associatePrimary(source, bodyOperation);
				jvmParam = this.typesFactory.createJvmFormalParameter();
				jvmParam.setName(SARLKeywords.OCCURRENCE);
				jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(source.getName()));
				this.associator.associate(source, jvmParam);
				bodyOperation.getParameters().add(jvmParam);
				this.typeBuilder.setBody(bodyOperation, source.getExpression());
				bodyOperation.getAnnotations().add(
						this._annotationTypesBuilder.annotationRef(Generated.class));
				this.typeExtensions.setSynthetic(bodyOperation, true);

				this.typeBuilder.setBody(operation, toStringConcatenation(
						"if (" + guardMethodName + "(" + SARLKeywords.OCCURRENCE + ")) {", //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
						bodyMethodName + "(" + SARLKeywords.OCCURRENCE + ");", //$NON-NLS-1$//$NON-NLS-2$
						"}")); //$NON-NLS-1$
			}

			operation.setAbstract(false);
			operation.setNative(false);
			operation.setSynchronized(false);
			operation.setStrictFloatingPoint(false);
			operation.setFinal(false);
			operation.setVisibility(JvmVisibility.PUBLIC);
			operation.setStatic(false);
			operation.setSimpleName(behName);
			operation.setReturnType(voidType);
			container.getMembers().add(operation);
			this.associator.associatePrimary(source, operation);
			this.typeBuilder.copyDocumentationTo(source, operation);

			context.setBehaviorUnitIndex(context.getBehaviorUnitIndex() + 1);
			context.incrementSerial(behName.hashCode());
		}
		this.log.fine(Messages.SARLJvmModelInferrer_10);
	}

	/** Transform the uses of SARL capacities.
	 *
	 * @param source the feature to transform.
	 * @param container the target container of the transformation result.
	 */
	@SuppressWarnings("unchecked")
	protected void transform(SarlCapacityUses source, JvmGenericType container) {
		final GenerationContext context = getContext(container);
		if (context == null) {
			return;
		}
		for (JvmTypeReference capacityType : source.getCapacities()) {
			if (capacityType.getType() instanceof JvmGenericType) {
				LightweightTypeReference reference = Utils.toLightweightTypeReference(capacityType, this.services);
				if (reference.isSubtypeOf(Capacity.class)) {
					final Map<ActionPrototype, JvmOperation> capacityOperations = CollectionLiterals.newTreeMap(null);

					Utils.populateInterfaceElements(
							(JvmGenericType) capacityType.getType(),
							capacityOperations,
							null,
							this.sarlSignatureProvider);

					for (final Entry<ActionPrototype, JvmOperation> entry : capacityOperations.entrySet()) {
						if (!context.getInheritedOverridableOperations().containsKey(entry.getKey())) {

							// Create the main function
							JvmOperation operation = this.typesFactory.createJvmOperation();
							operation.setAbstract(false);
							operation.setNative(false);
							operation.setSynchronized(false);
							operation.setStrictFloatingPoint(false);
							operation.setFinal(true);
							operation.setVisibility(JvmVisibility.PROTECTED);
							operation.setStatic(false);
							operation.setSimpleName(entry.getValue().getSimpleName());
							operation.setReturnType(
									this.typeBuilder.cloneWithProxies(entry.getValue().getReturnType()));
							container.getMembers().add(operation);
							this.associator.associatePrimary(source, operation);

							final List<String> args = CollectionLiterals.newArrayList();
							List<String> argTypes = CollectionLiterals.newArrayList();
							for (JvmFormalParameter param : entry.getValue().getParameters()) {
								JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
								jvmParam.setName(param.getSimpleName());
								jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(param.getParameterType()));
								this.associator.associate(source, jvmParam);
								operation.getParameters().add(jvmParam);
								args.add(param.getSimpleName());
								argTypes.add(
										param.getParameterType().getIdentifier());
							}
							String hyperrefLink = capacityType.getIdentifier() + "#" //$NON-NLS-1$
									+ entry.getValue().getSimpleName() + "(" //$NON-NLS-1$
									+ IterableExtensions.join(argTypes, ",") + ")"; //$NON-NLS-1$ //$NON-NLS-2$
							this.typeBuilder.setDocumentation(operation,
									MessageFormat.format(
											Messages.SARLJvmModelInferrer_13,
											hyperrefLink));
							operation.setVarArgs(entry.getValue().isVarArgs());

							// Body
							this.typeBuilder.setBody(operation, new Procedures.Procedure1<ITreeAppendable>() {
								@Override
								public void apply(ITreeAppendable it) {
									if (!Objects.equal("void", //$NON-NLS-1$
											entry.getValue().getReturnType().getIdentifier())) {
										it.append("return "); //$NON-NLS-1$
									}
									it.append("getSkill("); //$NON-NLS-1$
									it.append(entry.getValue().getDeclaringType().getQualifiedName());
									it.append(".class)."); //$NON-NLS-1$
									it.append(entry.getValue().getSimpleName());
									it.append("("); //$NON-NLS-1$
									it.append(IterableExtensions.join(args, ", ")); //$NON-NLS-1$
									it.append(");"); //$NON-NLS-1$
								}
							});

							// Copy the EarlyExit Annotation from the capacity
							if (Utils.hasAnnotation(entry.getValue(), EarlyExit.class)) {
								operation.getAnnotations().add(this._annotationTypesBuilder.annotationRef(EarlyExit.class));
							}
							// Copy the FiredEvent annotation from the capacity
							List<JvmTypeReference> firedEvents = Utils.annotationClasses(entry.getValue(), FiredEvent.class);
							if (!firedEvents.isEmpty()) {
								operation.getAnnotations().add(annotationClassRef(FiredEvent.class, firedEvents));
							}
							// Add the annotation dedicated to this particular method
							operation.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Generated.class));
							// Add the imported feature marker
							operation.getAnnotations().add(annotationClassRef(ImportedCapacityFeature.class,
									Collections.singletonList(capacityType)));
							//
							context.getInheritedOperationsToImplement().remove(entry.getKey());
							context.getInheritedOverridableOperations().put(entry.getKey(), entry.getValue());
							context.setActionIndex(context.getActionIndex() + 1);
						}
					}

					context.incrementSerial(capacityType.getIdentifier().hashCode());
				}
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

	/** Generate the code for the given SARL members.
	 *
	 * @param featureContainerType - the feature container.
	 * @param container - the SARL container.
	 * @param context - description of the generation context in which the members must be considered.
	 */
	protected void appendSarlMembers(
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

		//*****************
		// For Unit Tests
		if (this.inferrerProber.isPresent()) {
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_0,
					featureContainerType.getQualifiedName() + "#finalOperations", //$NON-NLS-1$
					new TreeMap<>(context.getInheritedFinalOperations()));
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_0,
					featureContainerType.getQualifiedName() + "#overridableOperations", //$NON-NLS-1$
					new TreeMap<>(context.getInheritedOverridableOperations()));
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_0,
					featureContainerType.getQualifiedName() + "#operationsToImplement", //$NON-NLS-1$
					new TreeMap<>(context.getInheritedOperationsToImplement()));
		}
		//*****************

		for (XtendMember feature : container.getMembers()) {
			if (context.isSupportedMember(feature)
					&& (!(feature instanceof SarlCapacityUses))
					&& (!(feature instanceof SarlRequiredCapacity))) {
				transform(feature, featureContainerType, true);
			}
		}

		//*****************
		// For Unit Tests
		if (this.inferrerProber.isPresent()) {
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_1,
					featureContainerType.getQualifiedName() + "#finalOperations", //$NON-NLS-1$
					new TreeMap<>(context.getInheritedFinalOperations()));
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_1,
					featureContainerType.getQualifiedName() + "#overridableOperations", //$NON-NLS-1$
					new TreeMap<>(context.getInheritedOverridableOperations()));
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_1,
					featureContainerType.getQualifiedName() + "#operationsToImplement", //$NON-NLS-1$
					new TreeMap<>(context.getInheritedOperationsToImplement()));
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_1,
					featureContainerType.getQualifiedName() + "#differedCodeGeneration", //$NON-NLS-1$
					new ArrayList<>(context.getDifferedGenerationElements()));
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_1,
					featureContainerType.getQualifiedName() + "#actionIndex", context.getActionIndex()); //$NON-NLS-1$
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_1,
					featureContainerType.getQualifiedName() + "#behaviorUnitIndex", context.getBehaviorUnitIndex()); //$NON-NLS-1$
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_1,
					featureContainerType.getQualifiedName() + "#hasConstructor", context.hasConstructor()); //$NON-NLS-1$
		}
		//*****************

		for (XtendMember feature : container.getMembers()) {
			if (context.isSupportedMember(feature)
					&& ((feature instanceof SarlCapacityUses)
						|| (feature instanceof SarlRequiredCapacity))) {
				transform(feature, featureContainerType, false);
			}
		}

		//*****************
		// For Unit Tests
		if (this.inferrerProber.isPresent()) {
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_2,
					featureContainerType.getQualifiedName() + "#finalOperations", //$NON-NLS-1$
					new TreeMap<>(context.getInheritedFinalOperations()));
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_2,
					featureContainerType.getQualifiedName() + "#overridableOperations", //$NON-NLS-1$
					new TreeMap<>(context.getInheritedOverridableOperations()));
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_2,
					featureContainerType.getQualifiedName() + "#operationsToImplement", //$NON-NLS-1$
					new TreeMap<>(context.getInheritedOperationsToImplement()));
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_2,
					featureContainerType.getQualifiedName() + "#actionIndex", context.getActionIndex()); //$NON-NLS-1$
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_2,
					featureContainerType.getQualifiedName() + "#behaviorUnitIndex", context.getBehaviorUnitIndex()); //$NON-NLS-1$
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_2,
					featureContainerType.getQualifiedName() + "#hasConstructor", context.hasConstructor()); //$NON-NLS-1$
		}
		//*****************

		// Add dispatch methods
		appendSyntheticDispatchMethods(container, featureContainerType);

		// Add SARL synthetic functions
		appendSyntheticDefaultValuedParameterMethods(
				container,
				featureContainerType,
				context);

		//*****************
		// For Unit Tests
		if (this.inferrerProber.isPresent()) {
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_3,
					featureContainerType.getQualifiedName() + "#finalOperations", //$NON-NLS-1$
					new TreeMap<>(context.getInheritedFinalOperations()));
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_3,
					featureContainerType.getQualifiedName() + "#overridableOperations", //$NON-NLS-1$
					new TreeMap<>(context.getInheritedOverridableOperations()));
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_3,
					featureContainerType.getQualifiedName() + "#operationsToImplement", //$NON-NLS-1$
					new TreeMap<>(context.getInheritedOperationsToImplement()));
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_3,
					featureContainerType.getQualifiedName() + "#actionIndex", context.getActionIndex()); //$NON-NLS-1$
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_3,
					featureContainerType.getQualifiedName() + "#behaviorUnitIndex", context.getBehaviorUnitIndex()); //$NON-NLS-1$
			this.inferrerProber.get().register(Step.GENERATE_CODE_FOR_FEATURES_3,
					featureContainerType.getQualifiedName() + "#hasConstructor", context.hasConstructor()); //$NON-NLS-1$
		}
		//*****************
	}

	/** Generate the missed operations that are the results from the generation of actions with default value parameters.
	 *
	 * @param source - the SARL container.
	 * @param target - the JVM feature container.
	 * @param context - description of the generation context in which the members must be considered.
	 */
	protected void appendSyntheticDefaultValuedParameterMethods(
			XtendTypeDeclaration source,
			JvmGenericType target,
			GenerationContext context) {

		// Generate the different operations.
		Iterator<Runnable> differedGeneration = context.getDifferedGenerationElements().iterator();
		while (differedGeneration.hasNext()) {
			Runnable r = differedGeneration.next();
			differedGeneration.remove();
			r.run();
		}

		// Generated the missed functions that are the result of the generation of operations
		// with default values.
		int actIndex = context.getActionIndex();

		for (Entry<ActionPrototype, JvmOperation> missedOperation : context.getInheritedOperationsToImplement().entrySet()) {

			String originalSignature = Utils.annotationString(missedOperation.getValue(), DefaultValueUse.class);
			if (!Strings.isNullOrEmpty(originalSignature)) {

				// Find the definition of the operation from the inheritance context.
				final JvmOperation redefinedOperation = context.getInheritedOverridableOperations().get(
						this.sarlSignatureProvider.createActionPrototype(
								missedOperation.getKey().getActionName(),
								this.sarlSignatureProvider.createParameterTypesFromString(originalSignature)));
				if (redefinedOperation != null) {
					ActionParameterTypes parameterTypes = this.sarlSignatureProvider.createParameterTypesFromJvmModel(
							redefinedOperation.isVarArgs(), redefinedOperation.getParameters());
					QualifiedActionName qualifiedActionName = this.sarlSignatureProvider.createQualifiedActionName(
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
					List<InferredStandardParameter> argumentSpec = redefinedPrototype.getInferredParameterTypes().get(
							missedOperation.getKey().getParametersTypes());

					// Create the missed java operation.
					JvmOperation op = this.typeBuilder.toMethod(
							source,
							missedOperation.getValue().getSimpleName(),
							missedOperation.getValue().getReturnType(),
							null);
					op.setVarArgs(missedOperation.getValue().isVarArgs());
					op.setFinal(true);

					final List<String> arguments = new ArrayList<>();

					// Create the formal parameters.
					for (InferredStandardParameter parameter : argumentSpec) {
						if (parameter instanceof InferredValuedParameter) {
							InferredValuedParameter p = (InferredValuedParameter) parameter;
							arguments.add(
									this.sarlSignatureProvider.toJavaArgument(
											target.getIdentifier(),
											p.getCallingArgument()));
						} else {
							arguments.add(parameter.getName());
							JvmFormalParameter jvmParam = this.typesFactory.createJvmFormalParameter();
							jvmParam.setName(parameter.getName());
							jvmParam.setParameterType(this.typeBuilder.cloneWithProxies(parameter.getType()));
							this.associator.associate(source, jvmParam);
							op.getParameters().add(jvmParam);
						}
					}

					// Create the body
					this.typeBuilder.setBody(op, new Procedures.Procedure1<ITreeAppendable>() {
						@Override
						public void apply(ITreeAppendable it) {
							it.append(redefinedOperation.getSimpleName());
							it.append("("); //$NON-NLS-1$
							it.append(IterableExtensions.join(arguments, ", ")); //$NON-NLS-1$
							it.append(");"); //$NON-NLS-1$
						}
					});

					// Add the annotations.
					op.getAnnotations().add(this._annotationTypesBuilder.annotationRef(
							DefaultValueUse.class, originalSignature));
					op.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Generated.class));

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
				for (String line : javaCodeLines) {
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
	 * @param defaultType - the default type.
	 * @param supertype - the supertype.
	 */
	protected void appendConstrainedExtends(
			GenerationContext context,
			JvmGenericType owner, Class<?> defaultType,
			JvmParameterizedTypeReference supertype) {
		List<? extends JvmParameterizedTypeReference> supertypes;
		if (supertype == null) {
			supertypes = Collections.emptyList();
		} else {
			supertypes = Collections.singletonList(supertype);
		}
		appendConstrainedExtends(context, owner, defaultType, supertypes);
	}

	/** Generate the extended types for the given SARL statement.
	 *
	 * @param context - the context of the generation.
	 * @param owner - the JVM element to change.
	 * @param defaultType - the default type.
	 * @param supertypes - the supertypes.
	 */
	protected void appendConstrainedExtends(
			GenerationContext context,
			JvmGenericType owner, Class<?> defaultType,
			List<? extends JvmParameterizedTypeReference> supertypes) {
		boolean isInterface = owner.isInterface();
		boolean explicitType = false;
		for (JvmParameterizedTypeReference superType : supertypes) {
			if (superType.getType() instanceof JvmGenericType) {
				LightweightTypeReference reference = Utils.toLightweightTypeReference(superType, this.services);
				if (reference.isInterfaceType() == isInterface && reference.isSubtypeOf(defaultType)) {
					owner.getSuperTypes().add(this.typeBuilder.cloneWithProxies(superType));
					context.incrementSerial(superType.getIdentifier().hashCode());
					explicitType = true;
				}
			}
		}
		if (!explicitType) {
			JvmTypeReference type = this._typeReferenceBuilder.typeRef(defaultType);
			owner.getSuperTypes().add(type);
			context.incrementSerial(type.getIdentifier().hashCode());
		}
	}

	/** Generate the implemented types for the given SARL statement.
	 *
	 * @param context - the context of the generation.
	 * @param owner - the JVM element to change.
	 * @param defaultType - the default type.
	 * @param implementedtypes - the implemented types.
	 */
	protected void appendConstrainedImplements(
			GenerationContext context,
			JvmGenericType owner, Class<?> defaultType,
			List<? extends JvmParameterizedTypeReference> implementedtypes) {
		boolean explicitType = false;
		for (JvmParameterizedTypeReference superType : implementedtypes) {
			if (superType.getType() instanceof JvmGenericType) {
				LightweightTypeReference reference = Utils.toLightweightTypeReference(superType, this.services);
				if (reference.isInterfaceType() && reference.isSubtypeOf(defaultType)) {
					owner.getSuperTypes().add(this.typeBuilder.cloneWithProxies(superType));
					context.incrementSerial(superType.getIdentifier().hashCode());
					explicitType = true;
				}
			}
		}
		if (!explicitType) {
			JvmTypeReference type = this._typeReferenceBuilder.typeRef(defaultType);
			owner.getSuperTypes().add(type);
			context.incrementSerial(type.getIdentifier().hashCode());
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
		List<JvmField> declaredInstanceFields = new ArrayList<>();
		for (JvmField field : target.getDeclaredFields()) {
			if (!field.isStatic()) {
				declaredInstanceFields.add(field);
			}
		}

		if (!declaredInstanceFields.isEmpty()) {
			JvmOperation op = toEqualsMethod(source, target, declaredInstanceFields);
			if (op != null) {
				op.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
				target.getMembers().add(op);
			}

			op = toHashCodeMethod(source, declaredInstanceFields);
			if (op != null) {
				op.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
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
		for (JvmField field : target.getDeclaredFields()) {
			if (!field.isStatic()) {
				declaredInstanceFields.add(field);
			}
		}

		if (!declaredInstanceFields.isEmpty()) {
			JvmOperation op = SARLJvmModelInferrer.this.typeBuilder.toMethod(
					source,
					"attributesToString", //$NON-NLS-1$
					SARLJvmModelInferrer.this._typeReferenceBuilder.typeRef(String.class),
					new Procedures.Procedure1<JvmOperation>() {
						@SuppressWarnings("synthetic-access")
						@Override
						public void apply(JvmOperation it2) {
							it2.setVisibility(JvmVisibility.PROTECTED);
							SARLJvmModelInferrer.this.typeBuilder.setDocumentation(it2,
									MessageFormat.format(Messages.SARLJvmModelInferrer_2,
											target.getSimpleName()));
							SARLJvmModelInferrer.this.typeBuilder.setBody(it2,
									new Procedures.Procedure1<ITreeAppendable>() {
								@Override
								public void apply(ITreeAppendable it3) {
									it3.append("StringBuilder result = new StringBuilder(" //$NON-NLS-1$
											+ "super.attributesToString());").newLine(); //$NON-NLS-1$
									for (JvmField attr : declaredInstanceFields) {
										it3.append("result.append(\"" + attr.getSimpleName() //$NON-NLS-1$
												+ "  = \").append(this." //$NON-NLS-1$
												+ attr.getSimpleName() + ");").newLine(); //$NON-NLS-1$
									}
									it3.append("return result.toString();"); //$NON-NLS-1$
								}
							});
						}
					});
			if (op != null) {
				op.getAnnotations().add(SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(Generated.class));
				SARLJvmModelInferrer.this.typeExtensions.setSynthetic(op, true);
				target.getMembers().add(op);
			}
		}
	}

	/** Append the serial number field.
	 *
	 * The serial number field is computed from the given context and from the generated fields.
	 *
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	protected void appendSerialNumber(GenerationContext context, XtendTypeDeclaration source, JvmGenericType target) {
		long serial = context.getSerial();

		for (JvmField field : Iterables.filter(target.getMembers(), JvmField.class)) {
			if ("serialVersionUID".equals(field.getSimpleName())) { //$NON-NLS-1$
				return;
			}
		}

		JvmField field = this.typesFactory.createJvmField();
		field.setSimpleName("serialVersionUID"); //$NON-NLS-1$
		target.getMembers().add(field);
		this.associator.associatePrimary(source, field);
		field.setVisibility(JvmVisibility.PRIVATE);
		field.setStatic(true);
		field.setTransient(false);
		field.setVolatile(false);
		field.setFinal(true);
		field.setType(this.typeBuilder.cloneWithProxies(this._typeReferenceBuilder.typeRef(long.class)));
		this.typeBuilder.setInitializer(field, toStringConcatenation(serial + "L")); //$NON-NLS-1$
		field.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Generated.class));
		this.typeExtensions.setSynthetic(field, true);
		this.readAndWriteTracking.markInitialized(field, null);
	}

	/** Create an annotation with classes as values.
	 *
	 * @param type - the type of the annotation.
	 * @param values - the values.
	 * @return the reference to the JVM annotation.
	 */
	private JvmAnnotationReference annotationClassRef(Class<? extends Annotation> type,
			List<? extends JvmTypeReference> values) {
		JvmAnnotationReference annot = this._annotationTypesBuilder.annotationRef(type);
		JvmTypeAnnotationValue annotationValue = this.services.getTypesFactory().createJvmTypeAnnotationValue();
		for (JvmTypeReference value : values) {
			annotationValue.getValues().add(this.typeBuilder.cloneWithProxies(value));
		}
		annot.getExplicitValues().add(annotationValue);
		return annot;
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
	 * @param paramSpec - the specification of the parameter as computed by a {@link ActionPrototypeProvider}.
	 */
	protected void translateSarlFormalParameters(
			GenerationContext context,
			JvmExecutable owner,
			JvmGenericType actionContainer,
			boolean varargs,
			List<? extends XtendParameter> params,
			final boolean isForInterface,
			List<InferredStandardParameter> paramSpec) {
		JvmFormalParameter lastParam = null;
		boolean hasDefaultValue = false;
		for (int i = 0; i < params.size(); ++i) {
			final XtendParameter param = params.get(i);
			final InferredStandardParameter inferredParam = paramSpec.get(i);
			final String paramName = param.getName();
			JvmTypeReference paramType = param.getParameterType();

			if (!Strings.isNullOrEmpty(paramName) && paramType != null) {
				// "Standard" (Xtend) translation of the parameter
				translateParameter(owner, param);
				lastParam = owner.getParameters().get(owner.getParameters().size() - 1);

				// Treat the default value
				if (param instanceof SarlFormalParameter && ((SarlFormalParameter) param).getDefaultValue() != null) {
					final XExpression defaultValue = ((SarlFormalParameter) param).getDefaultValue();
					hasDefaultValue = true;
					String namePostPart = inferredParam.getDefaultValueAnnotationValue();
					String name = this.sarlSignatureProvider.createFieldNameForDefaultValueID(namePostPart);
					// FIXME: Hide these attributes into an inner interface.
					JvmField field = this.typeBuilder.toField(defaultValue, name,
							paramType, new Procedures.Procedure1<JvmField>() {
						@SuppressWarnings("synthetic-access")
						@Override
						public void apply(JvmField it) {
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
							if (defaultValue != null) {
								it.getAnnotations().add(
										SARLJvmModelInferrer.this._annotationTypesBuilder.annotationRef(
												Generated.class,
												SARLJvmModelInferrer.this.sarlSerializer.serialize(defaultValue)));
							}
						}
					});
					actionContainer.getMembers().add(field);
					if (owner instanceof JvmConstructor) {
						this.readAndWriteTracking.markInitialized(field, (JvmConstructor) owner);
					} else {
						this.readAndWriteTracking.markInitialized(field, null);
					}
					JvmAnnotationReference annot = this._annotationTypesBuilder.annotationRef(DefaultValue.class, namePostPart);
					lastParam.getAnnotations().add(annot);
				}
			}
		}

		if (hasDefaultValue) {
			owner.getAnnotations().add(this._annotationTypesBuilder.annotationRef(DefaultValueSource.class));
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
		JvmFormalParameter lastParam = null;
		List<String> arguments = CollectionLiterals.newArrayList();
		for (InferredStandardParameter parameterSpec : signature) {
			if (parameterSpec instanceof InferredValuedParameter) {
				arguments.add(
						this.sarlSignatureProvider.toJavaArgument(
								actionContainer.getIdentifier(),
								((InferredValuedParameter) parameterSpec).getCallingArgument()));
			} else {
				EObject param = parameterSpec.getParameter();
				String paramName = parameterSpec.getName();
				JvmTypeReference paramType = parameterSpec.getType();
				if (!Strings.isNullOrEmpty(paramName) && paramType != null) {
					lastParam = this.typesFactory.createJvmFormalParameter();
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
		JvmOperation result = this.typeBuilder.toMethod(sarlElement, "equals", //$NON-NLS-1$
				this._typeReferenceBuilder.typeRef(Boolean.TYPE), null);
		if (result == null) {
			return null;
		}
		result.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Override.class));

		JvmFormalParameter param = this.typesFactory.createJvmFormalParameter();
		param.setName("obj"); //$NON-NLS-1$
		param.setParameterType(this._typeReferenceBuilder.typeRef(Object.class));
		this.associator.associate(sarlElement, param);
		result.getParameters().add(param);
		this.typeBuilder.setBody(result, new Procedures.Procedure1<ITreeAppendable>() {
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
				for (JvmField field : jvmFields) {
					generateToEqualForField(it, field);
				}
				it.newLine().append("return super.equals(obj);"); //$NON-NLS-1$
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
		JvmOperation result = this.typeBuilder.toMethod(sarlElement, "hashCode", //$NON-NLS-1$
				this._typeReferenceBuilder.typeRef(Integer.TYPE), null);
		if (result == null) {
			return null;
		}
		result.getAnnotations().add(this._annotationTypesBuilder.annotationRef(Override.class));
		this.typeBuilder.setBody(result, new Procedures.Procedure1<ITreeAppendable>() {
			@Override
			public void apply(ITreeAppendable it) {
				it.append("final int prime = 31;"); //$NON-NLS-1$
				it.newLine().append("int result = super.hashCode();"); //$NON-NLS-1$
				for (JvmField field : jvmFields) {
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
		return result;
	}

	/** Describe generation context.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected abstract static class GenerationContext {

		/** Compute serial number for serializable objects.
		 */
		private long serial = 1L;

		/** Index of the late generated action.
		 */
		private int actionIndex;

		/** Index of the late generated behavior unit.
		 */
		private int behaviorUnitIndex;

		/** collection of the generated constructors.
		 */
		@SuppressWarnings("unchecked")
		private final Map<ActionParameterTypes, JvmConstructor> generatedConstructors = CollectionLiterals.newTreeMap(null);

		/** Collection of the inherited final operations.
		 */
		@SuppressWarnings("unchecked")
		private final Map<ActionPrototype, JvmOperation> finalOperations = CollectionLiterals.newTreeMap(null);

		/** Collection of the inherited overridable operations.
		 */
		@SuppressWarnings("unchecked")
		private final Map<ActionPrototype, JvmOperation>  overridableOperations = CollectionLiterals.newTreeMap(null);

		/** Collection of the inherited operations that have not been implemented.
		 */
		@SuppressWarnings("unchecked")
		private final Map<ActionPrototype, JvmOperation>  operationsToImplement = CollectionLiterals.newTreeMap(null);

		/** List of elements that must be generated at the end of the generation process.
		 */
		private final List<Runnable> differedCodeGeneration = CollectionLiterals.newLinkedList();

		/** Construct a information about the generation.
		 */
		public GenerationContext() {
			//
		}

		/** Replies the computed serial number.
		 *
		 * @return the serial number.
		 */
		public long getSerial() {
			return this.serial;
		}

		/** Increment the serial number by the given ammount.
		 *
		 * @param value the value to add to the serial number.
		 */
		public void incrementSerial(long value) {
			this.serial += value;
		}

		/** Replies if a constructor is generated.
		 *
		 * @return <code>true</code> if the constructor is generated; <code>false</code> if created.
		 */
		public boolean hasConstructor() {
			return !this.generatedConstructors.isEmpty();
		}

		/** Add a generated constructor into the context.
		 *
		 * @param parameters the specification of the parameters of the constructor.
		 * @param jvmElement the generated element.
		 */
		public void addGeneratedConstructor(ActionParameterTypes parameters, JvmConstructor jvmElement) {
			this.generatedConstructors.put(parameters, jvmElement);
		}

		/** Replies the collection of the generated constructor.
		 *
		 * @return the original collection of constructors.
		 */
		public Map<ActionParameterTypes, JvmConstructor> getGeneratedConstructors() {
			return this.generatedConstructors;
		}

		/** Replies the collection of the inherited final operations.
		 *
		 * @return the original collection of operations.
		 */
		public Map<ActionPrototype, JvmOperation> getInheritedFinalOperations() {
			return this.finalOperations;
		}

		/** Replies the collection of the inherited overridable operations.
		 *
		 * @return the original collection of operations.
		 */
		public Map<ActionPrototype, JvmOperation> getInheritedOverridableOperations() {
			return this.overridableOperations;
		}

		/** Replies the collection of the inherited operations that are not yet implemented.
		 *
		 * @return the original collection of operations.
		 */
		public Map<ActionPrototype, JvmOperation> getInheritedOperationsToImplement() {
			return this.operationsToImplement;
		}

		/** Add an element that must be generated at the end of the generation process.
		 *
		 * @param element the element to generate at the end.
		 */
		public void addDifferedGenerationElement(Runnable element) {
			this.differedCodeGeneration.add(element);
		}

		/** Replies the collection of the elements that must be generated at the end of
		 * the generation process.
		 *
		 * @return the original collection of elements.
		 */
		public List<Runnable> getDifferedGenerationElements() {
			return this.differedCodeGeneration;
		}

		/** Replies the index of the late created action.
		 *
		 * @return the index.
		 */
		public int getActionIndex() {
			return this.actionIndex;
		}

		/** Set the index of the late created action.
		 *
		 * @param index the index.
		 */
		public void setActionIndex(int index) {
			this.actionIndex = index;
		}

		/** Replies the index of the last created behavior unit.
		 *
		 * @return the index
		 */
		public int getBehaviorUnitIndex() {
			return this.behaviorUnitIndex;
		}

		/** Replies the index of the last created behavior unit.
		 *
		 * @param index the index.
		 */
		public void setBehaviorUnitIndex(int index) {
			this.behaviorUnitIndex = index;
		}

		/** Replies if the given member is supported in the current context.
		 *
		 * @param member the member to test.
		 * @return <code>true</code> if the member is supported, <code>false</code> for ignoring it.
		 */
		public abstract boolean isSupportedMember(XtendMember member);

	}

}
