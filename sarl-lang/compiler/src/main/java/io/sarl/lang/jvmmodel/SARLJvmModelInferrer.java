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

package io.sarl.lang.jvmmodel;

import java.text.MessageFormat;
import java.util.AbstractList;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.jvmmodel.XtendJvmModelInferrer;
import org.eclipse.xtend.core.xtend.AnonymousClass;
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
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmEnumerationType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmFeature;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeParameterDeclarator;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociator;
import org.eclipse.xtext.xbase.jvmmodel.JvmAnnotationReferenceBuilder;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import com.google.inject.MembersInjector;
import com.google.inject.Singleton;

import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.DefaultJvmGenericTypeProvider;
import io.sarl.lang.jvmmodel.fragments.IInferrerFragmentContributions;
import io.sarl.lang.jvmmodel.fragments.aop.IAgentInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.IArtifactInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.IBehaviorInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.IBehaviorUnitInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.ICapacityInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.ICapacityUseInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.IEventInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.IRequireCapacityInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.ISkillInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.ISpaceInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.oop.IActionInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.oop.IAnnotationTypeInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.oop.IClassInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.oop.IConstructorInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.oop.IEnumInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.oop.IFieldInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.oop.IInterfaceInferrerFragment;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlArtifact;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.SarlSpace;
import io.sarl.lang.util.JvmVisibilityComparator;

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
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @author <a href="http://www.sebastianrodriguez.com.ar/">Sebastian Rodriguez</a>
 * @version compiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 */
@Singleton
public class SARLJvmModelInferrer extends XtendJvmModelInferrer implements IBaseJvmModelInferrer {

	@Inject
	private AbstractJvmModelInferrerFragment.Impl atomicFragment;

	@Inject
	private IFieldInferrerFragment fieldFragment;

	@Inject
	private IActionInferrerFragment actionFragment;

	@Inject
	private IConstructorInferrerFragment constructorFragment;

	@Inject
	private ICapacityUseInferrerFragment capacityUseFragment;

	@Inject
	private IRequireCapacityInferrerFragment requireCapacityInferrerFragment;
	
	@Inject
	private IBehaviorUnitInferrerFragment behaviorUnitInferrerFragment;

	@Inject
	private ISpaceInferrerFragment spaceInferrerFragment;

	@Inject
	private IArtifactInferrerFragment artifactInferrerFragment;

	@Inject
	private IInterfaceInferrerFragment interfaceInferrerFragment;

	@Inject
	private IEnumInferrerFragment enumInferrerFragment;

	@Inject
	private IAnnotationTypeInferrerFragment annotationTypeInferrerFragment;

	@Inject
	private IClassInferrerFragment classInferrerFragment;

	@Inject
	private ICapacityInferrerFragment capacityInferrerFragment;

	@Inject
	private IEventInferrerFragment eventInferrerFragment;

	@Inject
	private IBehaviorInferrerFragment behaviorInferrerFragment;

	@Inject
	private ISkillInferrerFragment skillInferrerFragment;

	@Inject
	private IAgentInferrerFragment agentInferrerFragment;

	@Inject
	private MembersInjector<GenerationContext> contextInjector;

	@Inject
	private IInferrerFragmentContributions fragmentContributions;
	
	@Inject
	private Logger log;

	@Inject
	private TypesFactory typesFactory;

	@Inject
	private IJvmModelAssociator associator;

	@Inject
	private SarlJvmModelAssociations sarlAssociations;

	@Inject
	private JvmVisibilityComparator visibilityComparator;

	@Inject
	private IDefaultVisibilityProvider defaultVisibilityProvider;

	/** Generation contexts.
	 */
	private LinkedList<GenerationContext> bufferedContexes = new LinkedList<>();

	@Override
	public final void logInternalError(Throwable exception) {
		if (exception != null && this.log.isLoggable(Level.SEVERE)) {
			this.log.log(Level.SEVERE, Messages.SARLJvmModelInferrer_0, exception);
		}
	}

	@Override
	public final void logInternalError(String message) {
		if (this.log.isLoggable(Level.SEVERE) && !Strings.isNullOrEmpty(message)) {
			this.log.log(Level.SEVERE,
					MessageFormat.format(Messages.SARLJvmModelInferrer_1,
							Messages.SARLJvmModelInferrer_0, message));
		}
	}

	@Override
	protected final void setBody(JvmExecutable executable, XExpression expression) {
		this.atomicFragment.setBody(this, executable, expression);
	}

	@Override
	public final synchronized GenerationContext openContext(EObject sarlObject, JvmDeclaredType type,
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

	@Override
	public final void closeContext(GenerationContext context) {
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

	@Override
	public final synchronized GenerationContext getContext(JvmIdentifiableElement type) {
		for (final var candidate : this.bufferedContexes) {
			if (Objects.equal(candidate.getTypeIdentifier(), type.getIdentifier())) {
				return candidate;
			}
		}
		throw new GenerationContextNotFoundInternalError(type);
	}

	@Override
	protected final void inferTypeSceleton(final XtendTypeDeclaration declaration, final IJvmDeclaredTypeAcceptor acceptor, 
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
			 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
			 * @version compiler 0.15.1 20250911-224823
			 * @mavengroupid io.sarl.lang
			 * @mavenartifactid compiler
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
	@SuppressWarnings("unchecked")
	private Stream<? extends JvmDeclaredType> doInferTypeSkeletons(
			XtendTypeDeclaration declaration,
			IJvmDeclaredTypeAcceptor acceptor, boolean preIndexingPhase,
			XtendFile sarlFile, List<Runnable> doLater) {
		if (Strings.isNullOrEmpty(declaration.getName())) {
			return null;
		}

		// Autowrap the provided runnable elements in order to avoid the internal exceptions
		// to stop the JVM generation too early.
		final var doLaterExceptionSafe = wrapDoLaterList(doLater);

		// Run the additional contributions for generating the JVM Ecore elements
		for (final var fragment : this.fragmentContributions.getBiStageFragmentContributions(declaration.getClass())) {
			final var javaTypes = new DefaultJvmGenericTypeProvider(this.typesFactory);
			fragment.prepareTransform(declaration, javaTypes, this);
			if (!preIndexingPhase) {
				doLaterExceptionSafe.add(() -> fragment.transform(declaration, javaTypes, this));
			}
			if (javaTypes.hasGeneratedType()) {
				return javaTypes.stream();
			}
		}

		// Run the hard-coded 
		if (declaration instanceof SarlSpace sarlSpace) {
			final var javaTypes = new DefaultJvmGenericTypeProvider(this.typesFactory);
			this.spaceInferrerFragment.prepareTransform(sarlSpace, javaTypes, this);
			if (!preIndexingPhase) {
				doLaterExceptionSafe.add(() -> this.spaceInferrerFragment.transform(sarlSpace, javaTypes, this));
			}
			return javaTypes.stream();
		}

		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	protected final JvmDeclaredType doInferTypeSceleton(
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

			// Run additional contributions to the generation of the JVM model
			for (final var fragment : this.fragmentContributions.getSingleStageFragmentContributions(declaration.getClass())) {
				final var javaType = this.typesFactory.createJvmGenericType();
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> fragment.transform(declaration, javaType, this));
				}
				return javaType;
			}

			// Run the hard-coded transformers 
			if (declaration instanceof SarlAgent sarlAgent) {
				final var javaType = this.typesFactory.createJvmGenericType();
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> this.agentInferrerFragment.transform(sarlAgent, javaType, this));
				}
				return javaType;
			}
			if (declaration instanceof SarlBehavior sarlBehavior) {
				final var javaType = this.typesFactory.createJvmGenericType();
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> this.behaviorInferrerFragment.transform(sarlBehavior, javaType, this));
				}
				return javaType;
			}
			if (declaration instanceof SarlEvent sarlEvent) {
				final var javaType = this.typesFactory.createJvmGenericType();
				copyTypeParameters(sarlEvent.getTypeParameters(), javaType);
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> this.eventInferrerFragment.transform(sarlEvent, javaType, this));
				}
				return javaType;
			}
			if (declaration instanceof SarlSkill sarlSkill) {
				final var javaType = this.typesFactory.createJvmGenericType();
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> this.skillInferrerFragment.transform(sarlSkill, javaType, this));
				}
				return javaType;
			}
			if (declaration instanceof SarlCapacity sarlCapacity) {
				final var javaType = this.typesFactory.createJvmGenericType();
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> this.capacityInferrerFragment.transform(sarlCapacity, javaType, this));
				}
				return javaType;
			}
			if (declaration instanceof SarlArtifact sarlArtifact) {
				final var javaType = this.typesFactory.createJvmGenericType();
				if (!preIndexingPhase) {
					doLaterExceptionSafe.add(() -> this.artifactInferrerFragment.transform(sarlArtifact, javaType, this));
				}
				return javaType;
			}

			return super.doInferTypeSceleton(declaration, acceptor, preIndexingPhase, xtendFile, doLaterExceptionSafe);
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

	@Override
	public final void inferLocalClass(AnonymousClass anonymousClass, String localClassName, JvmFeature container) {
		this.atomicFragment.inferLocalClass(this, anonymousClass, localClassName, container);
	}

	@Override
	protected final void initialize(XtendClass source, JvmGenericType inferredJvmType) {
		this.classInferrerFragment.transform(source, inferredJvmType, this,
				(source0, inferredJvmType0) -> super.initialize(source0, inferredJvmType0));
	}

	@Override
	protected final void initialize(XtendInterface source, JvmGenericType inferredJvmType) {
		this.interfaceInferrerFragment.transform(source, inferredJvmType, this,
				(source0, inferredJvmType0) -> super.initialize(source0, inferredJvmType0));
	}

	@Override
	protected final void initialize(XtendAnnotationType source, JvmAnnotationType inferredJvmType) {
		this.annotationTypeInferrerFragment.transform(source, inferredJvmType, this,
				(source0, inferredJvmType0) -> super.initialize(source0, inferredJvmType0));
	}

	@Override
	protected final void initialize(XtendEnum source, JvmEnumerationType inferredJvmType) {
		this.enumInferrerFragment.transform(source, inferredJvmType, this,
				(source0, inferredJvmType0) -> super.initialize(source0, inferredJvmType0));
	}

	@Override
	public final void transform(XtendMember sourceMember,
			JvmGenericType container, boolean allowDispatch) {
		try {
			if (sourceMember instanceof SarlBehaviorUnit cvalue) {
				this.behaviorUnitInferrerFragment.transform(cvalue, container, this);
			} else if (sourceMember instanceof SarlCapacityUses cvalue) {
				this.capacityUseFragment.transform(cvalue, container, this);
			} else if (sourceMember instanceof SarlRequiredCapacity cvalue) {
				this.requireCapacityInferrerFragment.transform(cvalue, container, this);
			} else {
				super.transform(sourceMember, container, allowDispatch);
			}
		} catch (InternalError internalError) {
			throw internalError;
		} catch (Exception exception) {
			logInternalError(exception);
		}
	}

	@Override
	protected final void transform(final XtendConstructor source, final JvmGenericType container) {
		if (source.isStatic()) {
			this.constructorFragment.transformStatic(source, container, this);
		} else {
			this.constructorFragment.transform(source, container, this);
		}
	}

	@Override
	protected final void transform(XtendField source, JvmGenericType container) {
		this.fieldFragment.transform(source, container, this,
				(source0, container0) -> super.transform(source0, container0));
	}

	@Override
	protected final void transform(final XtendFunction source, final JvmGenericType container, boolean allowDispatch) {
		this.actionFragment.transform(source, container, allowDispatch, this);
	}

	@Override
	public final void appendSyntheticDispatchMethods(XtendTypeDeclaration source, JvmGenericType target) {
		super.appendSyntheticDispatchMethods(source, target);
	}

	@Override
	protected final JvmOperation deriveGenericDispatchOperationSignature(
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

	@Override
	public final void fixTypeParameters(JvmTypeParameterDeclarator target) {
		super.fixTypeParameters(target);
	}

	@Override
	public final void translateParameter(JvmExecutable executable, XtendParameter parameter) {
		super.translateParameter(executable, parameter);
	}
	
	@Override
	public final void copyAndFixTypeParameters(List<JvmTypeParameter> typeParameters, JvmTypeParameterDeclarator target) {
		super.copyAndFixTypeParameters(typeParameters, target);
	}
	
	@Override
	public final void translateAnnotationsTo(List<XAnnotation> annotations, JvmAnnotationTarget target) {
		super.translateAnnotationsTo(annotations, target);
	}

	@Override
	public final JvmTypeReferenceBuilder getJvmTypeReferenceBuilder() {
		return this._typeReferenceBuilder;
	}

	@Override
	public final JvmAnnotationReferenceBuilder getJvmAnnotationReferenceBuilder() {
		return this._annotationTypesBuilder;
	}

	/** Internal error.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version compiler 0.15.1 20250911-224823
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid compiler
	 */
	private abstract static class InternalError extends RuntimeException {

		private static final long serialVersionUID = 4637115741105214351L;

		InternalError(String message) {
			super(message);
		}

	}

	/** Internal error.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version compiler 0.15.1 20250911-224823
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid compiler
	 */
	private static class GenerationContextNotFoundInternalError extends InternalError {

		private static final long serialVersionUID = 2275793506661573859L;

		GenerationContextNotFoundInternalError(JvmIdentifiableElement type) {
			super("generation context cannot be found for: " + type.getIdentifier()); //$NON-NLS-1$
		}

	}

}
