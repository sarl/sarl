/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.eclipse.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import javax.inject.Singleton;

import com.google.common.base.Strings;
import com.google.inject.Inject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.IAnnotatable;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IImportDeclaration;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.Signature;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XbaseFactory;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.codebuilder.builders.IBlockExpressionBuilder;
import io.sarl.lang.codebuilder.builders.IExpressionBuilder;
import io.sarl.lang.codebuilder.builders.IFormalParameterBuilder;
import io.sarl.lang.codebuilder.builders.ISarlActionBuilder;
import io.sarl.lang.codebuilder.builders.ISarlConstructorBuilder;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;
import io.sarl.lang.sarl.actionprototype.FormalParameterProvider;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.util.SarlUtils;
import io.sarl.lang.util.Utils;


/** Utilities for creating Ecore SARL elements from the JDT model.
 *
 * <p>This class extends the {@link SarlUtils} from the <code>io.sarl.lang</code> project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
@SuppressWarnings({"static-method", "checkstyle:classfanoutcomplexity"})
public class Jdt2Ecore {

	private static final String GENERATED_NAME = "Generated"; //$NON-NLS-0$

	@Inject
	private TypeReferences typeReferences;

	@Inject
	private IActionPrototypeProvider actionPrototypeProvider;

	/** Create a {@link TypeFinder} wrapper for the given Java project.
	 *
	 * <p>The wrapper invokes {@link IJavaProject#findType(String)} on the given project.
	 *
	 * @param project the project to wrap.
	 * @return the type finder based on the given Java project.
	 */
	public TypeFinder toTypeFinder(final IJavaProject project) {
		return new TypeFinder() {
			@Override
			public IType findType(String typeName) throws JavaModelException {
				return project.findType(typeName);
			}
		};
	}

	/** Replies if the given type name is valid for a super-type.
	 *
	 * <p>The type name is valid if it is not empty and not the <code>Object</code> classname.
	 *
	 * @param typeName the name of the type to test.
	 * @return <code>true</code> if the given type name is valid.
	 */
	public boolean isValidSuperType(String typeName) {
		return !Strings.isNullOrEmpty(typeName) && !"java.lang.Object".equals(typeName); //$NON-NLS-1$
	}

	/** Replies if the target feature is visible from the type.
	 *
	 * <p>The type finder could be obtained with {@link #toTypeFinder(IJavaProject)}.
	 *
	 * @param typeFinder the type finder to be used for finding the type definitions.
	 * @param fromType the type from which the feature visibility is tested.
	 * @param target the feature to test for the visibility.
	 * @return <code>true</code> if the given type can see the target feature.
	 * @throws JavaModelException if the Java model is invalid.
	 * @see #toTypeFinder(IJavaProject)
	 */
	public boolean isVisible(TypeFinder typeFinder, IType fromType, IMember target) throws JavaModelException {
		final int flags = target.getFlags();
		if (Flags.isPublic(flags)) {
			return true;
		}
		final String fromTypeName = fromType.getFullyQualifiedName();
		final String memberType = target.getDeclaringType().getFullyQualifiedName();
		if (Flags.isPrivate(flags)) {
			return target.getDeclaringType().getFullyQualifiedName().equals(fromTypeName);
		}
		if (Flags.isProtected(flags)) {
			IType t = fromType;
			while (t != null) {
				if (memberType.equals(t.getFullyQualifiedName())) {
					return true;
				}
				final String typeName = t.getSuperclassName();
				if (Strings.isNullOrEmpty(typeName)) {
					t = null;
				} else {
					t = typeFinder.findType(typeName);
				}
			}
		}
		final IPackageFragment f1 = target.getDeclaringType().getPackageFragment();
		final IPackageFragment f2 = fromType.getPackageFragment();
		if (f1.isDefaultPackage()) {
			return f2.isDefaultPackage();
		}
		return f1.getElementName().equals(f2.getElementName());
	}

	/** Replies the provider of formal parameters for the given operation.
	 *
	 * @param operation the operation.
	 * @return the provider of formal parameters for the operation.
	 * @throws JavaModelException if the Java model is invalid.
	 */
	public FormalParameterProvider getFormalParameterProvider(IMethod operation) throws JavaModelException {
		return new JdtFormalParameterList(operation);
	}

	/** Analyzing the type hierarchy of the given element, and
	 * extract any type-related information.
	 *
	 * <p>The type finder could be obtained with {@link #toTypeFinder(IJavaProject)}.
	 *
	 * @param typeFinder the type finder to be used for finding the type definitions.
	 * @param finalOperations filled with the final operations inherited by the element.
	 * @param overridableOperations filled with the oervrideable operations inherited by the element.
	 * @param inheritedFields filled with the fields inherited by the element.
	 * @param operationsToImplement filled with the abstract operations inherited by the element.
	 * @param superConstructors filled with the construstors of the super type.
	 * @param superClass the name of the super class.
	 * @param superInterfaces the super interfaces.
	 * @return the status of the operation.
	 * @throws JavaModelException if the Java model is invalid.
	 * @see #toTypeFinder(IJavaProject)
	 */
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity",
			"checkstyle:nestedifdepth", "checkstyle:parameternumber"})
	public IStatus populateInheritanceContext(
			TypeFinder typeFinder,
			Map<ActionPrototype, IMethod> finalOperations,
			Map<ActionPrototype, IMethod> overridableOperations,
			Map<String, IField> inheritedFields,
			Map<ActionPrototype, IMethod> operationsToImplement,
			Map<ActionParameterTypes, IMethod> superConstructors,
			String superClass,
			List<String> superInterfaces) throws JavaModelException {
		final Set<ActionPrototype> treatedElements = new TreeSet<>();
		final SARLEclipsePlugin plugin = SARLEclipsePlugin.getDefault();
		final List<IStatus> statuses = new ArrayList<>();
		// Get the operations that must be implemented
		if (operationsToImplement != null) {
			final SuperTypeIterator typeIterator = new SuperTypeIterator(typeFinder, true, superInterfaces);
			while (typeIterator.hasNext()) {
				final IType type = typeIterator.next();
				for (final IMethod operation : type.getMethods()) {
					if (!Flags.isStatic(operation.getFlags())
							&& !Flags.isFinal(operation.getFlags())
							&& !operation.isLambdaMethod()
							&& !operation.isConstructor()) {
						final ActionParameterTypes sig = this.actionPrototypeProvider.createParameterTypes(
								Flags.isVarargs(operation.getFlags()), getFormalParameterProvider(operation));
						final ActionPrototype actionKey = this.actionPrototypeProvider.createActionPrototype(
								operation.getElementName(),
								sig);
						if (treatedElements.add(actionKey)) {
							if (Flags.isDefaultMethod(operation.getFlags())) {
								if (!overridableOperations.containsKey(actionKey)) {
									overridableOperations.put(actionKey, operation);
								}
							} else {
								if (!operationsToImplement.containsKey(actionKey)) {
									operationsToImplement.put(actionKey, operation);
								}
							}
						}
					}
				}
			}
			statuses.addAll(typeIterator.getStatuses());
		}

		// Check on the implemented features, inherited from the super type
		if (isValidSuperType(superClass)) {
			final SuperTypeIterator typeIterator = new SuperTypeIterator(typeFinder, false, superClass);
			while (typeIterator.hasNext()) {
				final IType type = typeIterator.next();
				final boolean checkForConstructors = superConstructors != null
						&& type.getFullyQualifiedName().equals(superClass);
				for (final IMethod operation : type.getMethods()) {
					if (!Flags.isStatic(operation.getFlags())
							&& !operation.isLambdaMethod()
							&& isVisible(typeFinder, type, operation)) {
						if (!operation.isConstructor()
								&& !SarlUtils.isHiddenMember(operation.getElementName())) {
							final ActionParameterTypes sig = this.actionPrototypeProvider.createParameterTypes(
									Flags.isVarargs(operation.getFlags()), getFormalParameterProvider(operation));
							final ActionPrototype actionKey = this.actionPrototypeProvider.createActionPrototype(
									operation.getElementName(), sig);
							if (treatedElements.add(actionKey)) {
								final int flags = operation.getFlags();
								if (Flags.isAbstract(flags) && !Flags.isDefaultMethod(flags)) {
									if (operationsToImplement != null) {
										operationsToImplement.put(actionKey, operation);
									}
								} else if (Flags.isFinal(flags)) {
									if (finalOperations != null) {
										finalOperations.put(actionKey, operation);
									}
									if (operationsToImplement != null) {
										operationsToImplement.remove(actionKey);
									}
								} else {
									if (overridableOperations != null) {
										overridableOperations.put(actionKey, operation);
									}
									if (operationsToImplement != null) {
										operationsToImplement.remove(actionKey);
									}
								}
							}
						} else if (checkForConstructors && operation.isConstructor() && superConstructors != null) {
							final ActionParameterTypes sig = this.actionPrototypeProvider.createParameterTypes(
									Flags.isVarargs(operation.getFlags()), getFormalParameterProvider(operation));
							superConstructors.put(sig,  operation);
						}
					}
				}

				if (inheritedFields != null) {
					for (final IField field : type.getFields()) {
						if (!Flags.isStatic(field.getFlags())
								&& !SarlUtils.isHiddenMember(field.getElementName())
								&& isVisible(typeFinder, type, field)) {
							inheritedFields.putIfAbsent(field.getElementName(), field);
						}
					}
				}
			}
			statuses.addAll(typeIterator.getStatuses());
		}
		if (statuses.isEmpty()) {
			return plugin.createOkStatus();
		}
		if (statuses.size() == 1) {
			return statuses.get(0);
		}
		return plugin.createMultiStatus(statuses);
	}

	/** Replies if the given method is marked has automatically generated by the SARL compiler.
	 *
	 * @param method the method to check.
	 * @return <code>true</code> if the method is annoted with SyntheticMember; <code>false</code>
	 *     otherwise.
	 */
	public boolean isGeneratedOperation(IMethod method) {
		return getAnnotation(method, SyntheticMember.class.getName()) != null
				|| getAnnotation(method, GENERATED_NAME) != null;
	}

	/** Replies the annotation with the given qualified name.
	 *
	 * @param element the annoted element.
	 * @param qualifiedName the qualified name of the element.
	 * @return the annotation, or {@code null} if the element is not annoted.
	 */
	public IAnnotation getAnnotation(IAnnotatable element, String qualifiedName) {
		if (element != null) {
			try {
				final int separator = qualifiedName.lastIndexOf('.');
				final String simpleName;
				if (separator >= 0 && separator < (qualifiedName.length() - 1)) {
					simpleName = qualifiedName.substring(separator + 1, qualifiedName.length());
				} else {
					simpleName = qualifiedName;
				}
				for (final IAnnotation annotation : element.getAnnotations()) {
					final String name = annotation.getElementName();
					if (name.equals(simpleName) || name.equals(qualifiedName)) {
						return annotation;
					}
				}
			} catch (JavaModelException e) {
				//
			}
		}
		return null;
	}

	/** Create the JvmConstructor for the given JDT constructor.
	 *
	 * @param constructor the JDT constructor.
	 * @param context the context of the constructor.
	 * @return the JvmConstructor
	 * @throws JavaModelException if the Java model is invalid.
	 */
	public JvmConstructor getJvmConstructor(IMethod constructor, XtendTypeDeclaration context)
			throws JavaModelException {
		if (constructor.isConstructor()) {
			final JvmType type = this.typeReferences.findDeclaredType(
					constructor.getDeclaringType().getFullyQualifiedName(),
					context);
			if (type instanceof JvmDeclaredType) {
				final JvmDeclaredType declaredType = (JvmDeclaredType) type;
				final ActionParameterTypes jdtSignature = this.actionPrototypeProvider.createParameterTypes(
						Flags.isVarargs(constructor.getFlags()),
						getFormalParameterProvider(constructor));
				for (final JvmConstructor jvmConstructor : declaredType.getDeclaredConstructors()) {
					final ActionParameterTypes jvmSignature = this.actionPrototypeProvider.createParameterTypesFromJvmModel(
							jvmConstructor.isVarArgs(),
							jvmConstructor.getParameters());
					if (jvmSignature.equals(jdtSignature)) {
						return jvmConstructor;
					}
				}
			}
		}
		return null;
	}

	/** Create the JvmOperation for the given JDT method.
	 *
	 * @param method the JDT method.
	 * @param context the context of the constructor.
	 * @return the JvmOperation
	 * @throws JavaModelException if the Java model is invalid.
	 */
	public JvmOperation getJvmOperation(IMethod method, XtendTypeDeclaration context)
			throws JavaModelException {
		if (!method.isConstructor() && !method.isLambdaMethod() && !method.isMainMethod()) {
			final JvmType type = this.typeReferences.findDeclaredType(
					method.getDeclaringType().getFullyQualifiedName(),
					context);
			return getJvmOperation(method, type);
		}
		return null;
	}

	/** Create the JvmOperation for the given JDT method.
	 *
	 * @param method the JDT method.
	 * @param context the context of the constructor.
	 * @return the JvmOperation
	 * @throws JavaModelException if the Java model is invalid.
	 */
	private JvmOperation getJvmOperation(IMethod method, JvmType context)
			throws JavaModelException {
		if (context instanceof JvmDeclaredType) {
			final JvmDeclaredType declaredType = (JvmDeclaredType) context;
			final ActionParameterTypes jdtSignature = this.actionPrototypeProvider.createParameterTypes(
					Flags.isVarargs(method.getFlags()),
					getFormalParameterProvider(method));
			for (final JvmOperation jvmOperation : declaredType.getDeclaredOperations()) {
				final ActionParameterTypes jvmSignature = this.actionPrototypeProvider.createParameterTypesFromJvmModel(
						jvmOperation.isVarArgs(),
						jvmOperation.getParameters());
				if (jvmSignature.equals(jdtSignature)) {
					return jvmOperation;
				}
			}
		}
		return null;
	}

	private String extractDefaultValue(IMethod operation, IAnnotation annot)
			throws JavaModelException, IllegalArgumentException {
		IAnnotation annotation = annot;
		final Object value = annotation.getMemberValuePairs()[0].getValue();
		final String fieldId = (value == null) ? null : value.toString();
		if (!Strings.isNullOrEmpty(fieldId)) {
			final String methodName = Utils.createNameForHiddenDefaultValueFunction(fieldId);
			final IMethod method = operation.getDeclaringType().getMethod(methodName, new String[0]);
			if (method != null) {
				annotation = getAnnotation(method, SarlSourceCode.class.getName());
				if (annotation != null) {
					return annotation.getMemberValuePairs()[0].getValue().toString();
				}
			}
		}
		return null;
	}

	/** Create the formal parameters for the given operation.
	 *
	 * @param parameterBuilder the code builder.
	 * @param operation the operation that describes the formal parameters.
	 * @return the parameters.
	 * @throws JavaModelException if the Java model is invalid.
	 * @throws IllegalArgumentException if the signature is not syntactically correct.
	 */
	protected IFormalParameterBuilder[] createFormalParametersWith(
			ParameterBuilder parameterBuilder,
			IMethod operation) throws JavaModelException, IllegalArgumentException {
		final boolean isVarargs = Flags.isVarargs(operation.getFlags());
		final ILocalVariable[] rawParameters = operation.getParameters();
		final FormalParameterProvider parameters = getFormalParameterProvider(operation);
		final int len = parameters.getFormalParameterCount();
		final IFormalParameterBuilder[] paramBuilders = new IFormalParameterBuilder[len];
		for (int i = 0; i < len; ++i) {
			final ILocalVariable rawParameter = rawParameters[i];
			final IAnnotation annotation = getAnnotation(rawParameter, DefaultValue.class.getName());
			final String defaultValue = (annotation != null) ? extractDefaultValue(operation, annotation) : null;
			final boolean isV = isVarargs && i == len - 1;
			String type = parameters.getFormalParameterType(i, isV);
			if (isV && type.endsWith("[]")) { //$NON-NLS-1$
				type = type.substring(0, type.length() - 2);
			}
			final IFormalParameterBuilder sarlParameter = parameterBuilder.addParameter(parameters.getFormalParameterName(i));
			sarlParameter.setParameterType(type);
			if (defaultValue != null) {
				sarlParameter.getDefaultValue().setExpression(defaultValue);
			}
			if (isV) {
				sarlParameter.setVarArg(true);
			}
			paramBuilders[i] = sarlParameter;
		}
		return paramBuilders;
	}

	/** Add the given constructors to the Ecore container.
	 *
	 * @param codeBuilder the code builder to use.
	 * @param superClassConstructors the constructors defined in the super class.
	 * @param context the context of the constructors.
	 * @throws JavaModelException if the Java model is invalid.
	 */
	public void createStandardConstructorsWith(
			ConstructorBuilder codeBuilder,
			Collection<IMethod> superClassConstructors,
			XtendTypeDeclaration context) throws JavaModelException {
		if (superClassConstructors != null) {
			for (final IMethod constructor : superClassConstructors) {
				if (!isGeneratedOperation(constructor)) {
					final ISarlConstructorBuilder cons = codeBuilder.addConstructor();
					// Create parameters
					final IFormalParameterBuilder[] sarlParams = createFormalParametersWith(
						name -> cons.addParameter(name), constructor);
					// Create the block
					final IBlockExpressionBuilder block = cons.getExpression();
					// Create thre super-call expression
					final IExpressionBuilder superCall = block.addExpression();
					final XFeatureCall call = XbaseFactory.eINSTANCE.createXFeatureCall();
					superCall.setXExpression(call);
					// Set the todo comment.
					superCall.setDocumentation(block.getAutoGeneratedActionString());
					// Create the super-call XExpression
					call.setFeature(getJvmConstructor(constructor, context));
					call.setExplicitOperationCall(true);
					final List<XExpression> arguments = call.getFeatureCallArguments();
					for (final IFormalParameterBuilder currentParam : sarlParams) {
						final XFeatureCall argumentSource = XbaseFactory.eINSTANCE.createXFeatureCall();
						arguments.add(argumentSource);
						currentParam.setReferenceInto(argumentSource);
					}
				}
			}
		}
	}

	/** Create the operations into the SARL feature container.
	 *
	 * @param codeBuilder the builder of the script.
	 * @param methods the operations to create.
	 * @param context the context of the actions. If {@code null}, the block of the actions are not generated.
	 * @throws JavaModelException if the Java model is invalid.
	 * @throws IllegalArgumentException if the signature is not syntactically correct.
	 */
	public void createActionsWith(
			ActionBuilder codeBuilder,
			Collection<IMethod> methods,
			XtendTypeDeclaration context) throws JavaModelException, IllegalArgumentException {
		if (methods != null) {
			for (final IMethod operation : methods) {
				if (!isGeneratedOperation(operation)) {
					final ISarlActionBuilder action = codeBuilder.addAction(operation.getElementName());
					action.setReturnType(Signature.toString(operation.getReturnType()));
					final IFormalParameterBuilder[] sarlParams = createFormalParametersWith(
						name -> action.addParameter(name), operation);
					if (context != null) {
						final JvmType type = this.typeReferences.findDeclaredType(
								operation.getDeclaringType().getFullyQualifiedName(),
								context);
						final JvmOperation superOperation = getJvmOperation(operation, type);
						// Create the block
						final IBlockExpressionBuilder block = action.getExpression();
						if ((type.eClass() != TypesPackage.Literals.JVM_GENERIC_TYPE
								|| !((JvmGenericType) type).isInterface())
								&& superOperation != null
								&& !superOperation.isAbstract()) {
							// Create the super-call expression
							final IExpressionBuilder superCall = block.addExpression();
							final XMemberFeatureCall call = XbaseFactory.eINSTANCE.createXMemberFeatureCall();
							superCall.setXExpression(call);
							superCall.setDocumentation(block.getAutoGeneratedActionString());
							call.setFeature(superOperation);
							call.setMemberCallTarget(superCall.createReferenceToSuper());
							final List<XExpression> arguments = call.getMemberCallArguments();
							for (final IFormalParameterBuilder currentParam : sarlParams) {
								final XFeatureCall argumentSource = XbaseFactory.eINSTANCE.createXFeatureCall();
								arguments.add(argumentSource);
								currentParam.setReferenceInto(argumentSource);
							}
						} else {
							final JvmTypeReference ret = superOperation != null ? superOperation.getReturnType() : null;
							block.setDefaultAutoGeneratedContent(
									ret == null ? null : ret.getIdentifier());
						}
					}
				}
			}
		}
	}

	/** Replies if the given type is a subclass of the second type.
	 *
	 * <p>The type finder could be obtained with {@link #toTypeFinder(IJavaProject)}.
	 *
	 * @param typeFinder the type finder to be used for finding the type definitions.
	 * @param subClass the name of the sub class.
	 * @param superClass the name of the expected super class.
	 * @return <code>true</code> if it is a subclass.
	 * @throws JavaModelException if the Java model is invalid.
	 * @see #toTypeFinder(IJavaProject)
	 */
	public boolean isSubClassOf(TypeFinder typeFinder, String subClass, String superClass) throws JavaModelException {
		final SuperTypeIterator typeIterator = new SuperTypeIterator(typeFinder, false, subClass);
		while (typeIterator.hasNext()) {
			final IType type = typeIterator.next();
			if (Objects.equals(type.getFullyQualifiedName(), superClass)) {
				return true;
			}
		}
		return false;
	}

	/** Iterator on the super types of a given type.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class SuperTypeIterator implements Iterator<IType> {

		private final TypeFinder typeFinder;

		private final Set<String> encountered = new TreeSet<>();

		private final Deque<String> queue = new LinkedList<>();

		private final boolean isInterface;

		private final List<IStatus> statuses = new ArrayList<>();

		private IType current;

		/** Constructor.
		 * @param typeFinder the type finder to be used for finding the type definitions.
		 * @param isInterface indicates if the exploration is for interfaces or for classes.
		 * @param typeNames the initial types.
		 */
		SuperTypeIterator(TypeFinder typeFinder, boolean isInterface, String... typeNames) {
			this(typeFinder, isInterface, Arrays.asList(typeNames));
		}

		/** Constructor.
		 * @param typeFinder the type finder to be used for finding the type definitions.
		 * @param isInterface indicates if the exploration is for interfaces or for classes.
		 * @param typeNames the initial types.
		 */
		SuperTypeIterator(TypeFinder typeFinder, boolean isInterface, Collection<String> typeNames) {
			this.isInterface = isInterface;
			this.typeFinder = typeFinder;
			this.queue.addAll(typeNames);
			updateCurrent();
		}

		/** Replies the statuses related to the iteration.
		 *
		 * @return the statuses, or nothing if no issue occured.
		 */
		public Collection<IStatus> getStatuses() {
			return this.statuses;
		}

		private void updateCurrent() {
			this.current = null;
			while (this.current == null && !this.queue.isEmpty()) {
				final String typeName = this.queue.removeFirst();
				if (isValidSuperType(typeName) && !this.encountered.contains(typeName)) {
					try {
						this.current = this.typeFinder.findType(typeName);
						if (this.current == null) {
							this.statuses.add(SARLEclipsePlugin.getDefault().createStatus(
									IStatus.ERROR, new ClassNotFoundException(typeName)));
						}
					} catch (JavaModelException e) {
						this.current = null;
						this.statuses.add(SARLEclipsePlugin.getDefault().createStatus(
								IStatus.ERROR, e));
					}
				}
			}
		}

		@Override
		public boolean hasNext() {
			return this.current != null;
		}

		@Override
		public IType next() {
			final IType c = this.current;
			if (c == null) {
				throw new NoSuchElementException();
			}
			final String name = c.getFullyQualifiedName();
			this.encountered.add(name);
			try {
				final String[] superTypes;
				if (this.isInterface) {
					superTypes = c.getSuperInterfaceTypeSignatures();
				} else {
					superTypes = new String[] {c.getSuperclassTypeSignature()};
				}
				for (final String signature : superTypes) {
					if (!Strings.isNullOrEmpty(signature)) {
						final String resolvedSignature = resolveType(c, signature);
						if (!Strings.isNullOrEmpty(resolvedSignature)) {
							this.queue.add(resolvedSignature);
						}
					}
				}
			} catch (Exception exception) {
				//
			}
			updateCurrent();
			return c;
		}

		private String resolveType(IType type, String signature) throws JavaModelException {
			final String[][] resolved = type.resolveType(Signature.toString(signature));
			if (resolved != null) {
				for (final String[] entry : resolved) {
					if (Strings.isNullOrEmpty(entry[0])) {
						return entry[1];
					}
					return entry[0] + "." + entry[1]; //$NON-NLS-1$
				}
			}
			return null;
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

	/** Provider of SARL formal parameters from a the JDT formal parameter list.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class JdtFormalParameterList implements FormalParameterProvider {

		private final int nb;

		private final String[] names;

		private final String[] types;

		/** Constructor.
		 * @param operation the operation.
		 * @throws JavaModelException if the parameters cannot be retreived.
		 */
		JdtFormalParameterList(IMethod operation) throws JavaModelException {
			this.nb = operation.getNumberOfParameters();
			this.names = operation.getParameterNames();
			this.types = new String[this.nb];
			final ILocalVariable[] unresolvedParameters = operation.getParameters();
			for (int i = 0; i < this.nb; ++i) {
				this.types[i] = resolve(operation, unresolvedParameters[i].getTypeSignature());
			}
		}

		private String resolve(IMethod operation, String typename) throws JavaModelException {
			if (Signature.C_UNRESOLVED == Signature.getElementType(typename).charAt(0)) {
				final ICompilationUnit unit = operation.getCompilationUnit();
				if (unit != null) {
					final String post = "." + Signature.toString(typename); //$NON-NLS-1$
					for (final IImportDeclaration decl : unit.getImports()) {
						if (decl.getElementName().endsWith(post)) {
							return decl.getElementName();
						}
					}
				}
			}
			return Signature.toString(typename);
		}

		@Override
		public int getFormalParameterCount() {
			return this.nb;
		}

		@Override
		public String getFormalParameterName(int position) {
			return this.names[position];
		}

		@Override
		public String getFormalParameterType(int position, boolean isVarArgs) {
			return this.types[position];
		}

		@Override
		public JvmTypeReference getFormalParameterTypeReference(int position, boolean isVarargs) {
			throw new UnsupportedOperationException();
		}

		@Override
		public boolean hasFormalParameterDefaultValue(int position) {
			return false;
		}

		@Override
		public XExpression getFormalParameterDefaultValue(int position) {
			return null;
		}

		@Override
		public String getFormalParameterDefaultValueString(int position) {
			return null;
		}

		@Override
		public EObject getFormalParameter(int position) {
			throw new UnsupportedOperationException();
		}

	}

	/** The interface permits to find the JDT definition of a type.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@FunctionalInterface
	public interface TypeFinder {

		/** Find the definition of a type.
		 *
		 * @param typeName the name of the type to search for.
		 * @return the type, or {@code null} if the type was not found.
		 * @throws JavaModelException if this project does not exist or if an
		 *     exception occurs while accessing its corresponding resource.
		 */
		IType findType(String typeName) throws JavaModelException;

	}

	/** Parameter builder.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@FunctionalInterface
	private interface ParameterBuilder {
		IFormalParameterBuilder addParameter(String name);
	}

	/** Action builder.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@FunctionalInterface
	public interface ActionBuilder {
		/** Add an action.
		 *
		 * @param name the name of the action.
		 * @return the builder of the action.
		 */
		ISarlActionBuilder addAction(String name);
	}

	/** Constructor builder.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@FunctionalInterface
	public interface ConstructorBuilder {
		/** Add a constructor.
		 *
		 * @return the builder of the constructor.
		 */
		ISarlConstructorBuilder addConstructor();
	}

}
