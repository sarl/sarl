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

package io.sarl.eclipse.util;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;

import com.google.common.base.Strings;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.IAnnotatable;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.Signature;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmGenericArrayTypeReference;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XbaseFactory;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.lang.codebuilder.builders.IFormalParameterBuilder;
import io.sarl.lang.codebuilder.builders.ISarlActionBuilder;
import io.sarl.lang.codebuilder.builders.ISarlConstructorBuilder;
import io.sarl.lang.codebuilder.builders.ITypeParameterBuilder;
import io.sarl.lang.core.annotation.DefaultValue;
import io.sarl.lang.core.annotation.SarlSourceCode;
import io.sarl.lang.core.annotation.SyntheticMember;
import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;
import io.sarl.lang.sarl.actionprototype.FormalParameterProvider;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.util.Utils;


/** Utilities for creating Ecore SARL elements from the JDT model.
 *
 * <p>This class extends the {@link SarlUtils} from the {@code io.sarl.lang} project.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
@Singleton
@SuppressWarnings("restriction")
public class Jdt2Ecore {

	private static final String GENERATED_NAME = "Generated"; //$NON-NLS-1$

	@Inject
	private TypeReferences ecoreTypeReferences;

	@Inject
	private TypesFactory ecoreTypeFactory;

	@Inject
	private IActionPrototypeProvider actionPrototypeProvider;

	/** Create a conversion context for the given Ecore type declaration.
	 *
	 * @param context the Ecore context, may be {@code null}.
	 * @param references the tool for obtaining the references to the Ecore types, never {@code null}.
	 * @return the conversion context, never {@code null}.
	 * @since 0.15
	 */
	public static ConversionContext contextualize(XtendTypeDeclaration context, TypeReferences references) {
		assert references != null;
		return new EcoreTypeConversionContext(context, references);
	}
	
	/** Create a {@link TypeFinder} wrapper for the given Java project.
	 *
	 * <p>The wrapper invokes {@link IJavaProject#findType(String)} on the given project.
	 *
	 * @param project the project to wrap.
	 * @return the type finder based on the given Java project.
	 */
	@SuppressWarnings("static-method")
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
	 * <p>The type name is valid if it is not empty and not the {@code Object} classname.
	 *
	 * @param typeName the name of the type to test.
	 * @return {@code true} if the given type name is valid.
	 */
	@SuppressWarnings("static-method")
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
	 * @return {@code true} if the given type can see the target feature.
	 * @throws JavaModelException if the Java model is invalid.
	 * @see #toTypeFinder(IJavaProject)
	 */
	@SuppressWarnings("static-method")
	public boolean isVisible(TypeFinder typeFinder, IType fromType, IMember target) throws JavaModelException {
		final var flags = target.getFlags();
		if (Flags.isPublic(flags)) {
			return true;
		}
		final var fromTypeName = fromType.getFullyQualifiedName();
		final var memberType = target.getDeclaringType().getFullyQualifiedName();
		if (Flags.isPrivate(flags)) {
			return target.getDeclaringType().getFullyQualifiedName().equals(fromTypeName);
		}
		if (Flags.isProtected(flags)) {
			var t = fromType;
			while (t != null) {
				if (memberType.equals(t.getFullyQualifiedName())) {
					return true;
				}
				final var typeName = t.getSuperclassName();
				if (Strings.isNullOrEmpty(typeName)) {
					t = null;
				} else {
					t = typeFinder.findType(typeName);
				}
			}
		}
		final var f1 = target.getDeclaringType().getPackageFragment();
		final var f2 = fromType.getPackageFragment();
		if (f1.isDefaultPackage()) {
			return f2.isDefaultPackage();
		}
		return f1.getElementName().equals(f2.getElementName());
	}

	/** Replies the provider of formal parameters for the given operation.
	 *
	 * @param context the context in which the conversion is done.
	 * @param operation the operation.
	 * @return the provider of formal parameters for the operation.
	 * @throws JavaModelException if the Java model is invalid.
	 * @since 0.15
	 */
	public FormalParameterProvider getFormalParameterProvider(ConversionContext context, IMethod operation) throws JavaModelException {
		if (context == null) {
			throw new IllegalArgumentException("Null conversion context"); //$NON-NLS-1$
		}
		return new JdtFormalParameterList(context, operation);
	}

	@SuppressWarnings("unchecked")
	private static void fillTypeParameters(XtendTypeDeclaration type, Map<String, JvmParameterizedTypeReference> typeParameters,
			TypeReferences references) {
		if (type != null && !type.isAnonymous()) {
			// First declare the enclosing type definitions.
			if (!type.isStatic()) {
				fillTypeParameters(type.getDeclaringType(), typeParameters, references);
			}
			try {
				final var method = type.getClass().getDeclaredMethod("getTypeParameters"); //$NON-NLS-1$
				List<? extends JvmTypeParameter> params = (List<? extends JvmTypeParameter>) method.invoke(type);
				for (final var param : params) {
					final var ref = references.createTypeRef(param);
					typeParameters.put(param.getName(), ref);
				}
			} catch (Throwable ex) {
				//
			}
		}
	}

	private void fillTypeParameters(IMethod operation, LocalConversionContext context) {
		try {
			final var resolver = new TypeBasedLocalTypeResolver(operation.getDeclaringType());
			for (final var param : operation.getTypeParameters()) {
				final var bounds = param.getBoundsSignatures();
				final var boundTypes = new JvmTypeReference[bounds.length];
				var i = 0;
				for (final var bound : bounds) {
					boundTypes[i] = convertSignatureFromJdt2Ecore(bound, resolver, context);
					++i;
				}
				final var parameter = this.ecoreTypeFactory.createJvmTypeParameter();
				parameter.setName(param.getElementName());
				for (final var constraint : boundTypes) {
					final var upper = this.ecoreTypeFactory.createJvmUpperBound();
					upper.setOwner(parameter);
					upper.setTypeReference(constraint);
					parameter.getConstraints().add(upper);
				}
				context.declareTypeParameter(param.getElementName(), this.ecoreTypeReferences.createTypeRef(parameter));
			}
		} catch (Throwable ex) {
			//
		}
	}

	/** Analyzing the type hierarchy of the given element, and
	 * extract any type-related information.
	 *
	 * <p>The type finder could be obtained with {@link #toTypeFinder(IJavaProject)}.
	 *
	 * @param context the context in which the conversion is done.
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
	 * @since 0.15
	 */
	public IStatus populateInheritanceContext(
			ConversionContext context,
			TypeFinder typeFinder,
			Map<ActionPrototype, IMethod> finalOperations,
			Map<ActionPrototype, IMethod> overridableOperations,
			Map<String, IField> inheritedFields,
			Map<ActionPrototype, IMethod> operationsToImplement,
			Map<ActionParameterTypes, IMethod> superConstructors,
			String superClass,
			List<String> superInterfaces) throws JavaModelException {
		final var plugin = SARLEclipsePlugin.getDefault();
		if (context == null) {
			return plugin.createStatus(IStatus.ERROR, "Null conversion context"); //$NON-NLS-1$
		}
		final var treatedElements = new TreeSet<ActionPrototype>();
		final var statuses = new ArrayList<IStatus>();
		// Get the operations that must be implemented
		if (operationsToImplement != null) {
			final var typeIterator = new SuperTypeIterator(context, typeFinder, true, superInterfaces);
			while (typeIterator.hasNext()) {
				final var type = typeIterator.next();
				for (final var operation : type.getMethods()) {
					if (!Flags.isStatic(operation.getFlags())
							&& !Flags.isFinal(operation.getFlags())
							&& !operation.isLambdaMethod()
							&& !operation.isConstructor()) {
						final var localContext = new LocalConversionContext(context);
						fillTypeParameters(operation, localContext);
						final var sig = this.actionPrototypeProvider.createParameterTypes(
								Flags.isVarargs(operation.getFlags()), getFormalParameterProvider(localContext, operation));
						final var actionKey = this.actionPrototypeProvider.createActionPrototype(
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
			final var typeIterator = new SuperTypeIterator(context, typeFinder, false, superClass);
			while (typeIterator.hasNext()) {
				final var type = typeIterator.next();
				final var checkForConstructors = superConstructors != null
						&& type.getFullyQualifiedName().equals(superClass);
				for (final var operation : type.getMethods()) {
					if (!Flags.isStatic(operation.getFlags())
							&& !operation.isLambdaMethod()
							&& isVisible(typeFinder, type, operation)) {
						final var localContext = new LocalConversionContext(context);
						fillTypeParameters(operation, localContext);
						if (!operation.isConstructor()
								&& !SarlUtils.isHiddenMember(operation.getElementName())) {
							final var sig = this.actionPrototypeProvider.createParameterTypes(
									Flags.isVarargs(operation.getFlags()), getFormalParameterProvider(localContext, operation));
							final var actionKey = this.actionPrototypeProvider.createActionPrototype(
									operation.getElementName(), sig);
							if (treatedElements.add(actionKey)) {
								final var flags = operation.getFlags();
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
							final var sig = this.actionPrototypeProvider.createParameterTypes(
									Flags.isVarargs(operation.getFlags()), getFormalParameterProvider(localContext, operation));
							superConstructors.put(sig,  operation);
						}
					}
				}

				if (inheritedFields != null) {
					for (final var field : type.getFields()) {
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
	 * @return {@code true} if the method is annoted with SyntheticMember; {@code false}
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
	@SuppressWarnings("static-method")
	public IAnnotation getAnnotation(IAnnotatable element, String qualifiedName) {
		if (element != null) {
			try {
				final var separator = qualifiedName.lastIndexOf('.');
				final String simpleName;
				if (separator >= 0 && separator < (qualifiedName.length() - 1)) {
					simpleName = qualifiedName.substring(separator + 1, qualifiedName.length());
				} else {
					simpleName = qualifiedName;
				}
				for (final var annotation : element.getAnnotations()) {
					final var name = annotation.getElementName();
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
	 * @since 0.15
	 */
	public JvmConstructor getJvmConstructor(IMethod constructor, ConversionContext context)
			throws JavaModelException {
		if (context != null && constructor.isConstructor()) {
			final var type = this.ecoreTypeReferences.findDeclaredType(
					constructor.getDeclaringType().getFullyQualifiedName(),
					context.getEcoreContext());
			if (type instanceof JvmDeclaredType declaredType) {
				final var jdtSignature = this.actionPrototypeProvider.createParameterTypes(
						Flags.isVarargs(constructor.getFlags()),
						getFormalParameterProvider(context, constructor));
				for (final var jvmConstructor : declaredType.getDeclaredConstructors()) {
					final var jvmSignature = this.actionPrototypeProvider.createParameterTypesFromJvmModel(
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
	 * @since 0.15
	 */
	public JvmOperation getJvmOperation(IMethod method, ConversionContext context)
			throws JavaModelException {
		if (context != null && !method.isConstructor() && !method.isLambdaMethod() && !method.isMainMethod()) {
			final var type = this.ecoreTypeReferences.findDeclaredType(
					method.getDeclaringType().getFullyQualifiedName(),
					context.getEcoreContext());
			return getJvmOperation(context, method, type);
		}
		return null;
	}

	/** Create the JvmOperation for the given JDT method.
	 *
	 * @param conversionContext the context in which the conversion is done.
	 * @param method the JDT method.
	 * @param context the context of the constructor.
	 * @return the JvmOperation
	 * @throws JavaModelException if the Java model is invalid.
	 * @since 0.15
	 */
	private JvmOperation getJvmOperation(ConversionContext conversionContext, IMethod method, JvmType context)
			throws JavaModelException {
		if (context instanceof JvmDeclaredType declaredType) {
			final var jdtSignature = this.actionPrototypeProvider.createParameterTypes(
					Flags.isVarargs(method.getFlags()),
					getFormalParameterProvider(conversionContext, method));
			for (final var jvmOperation : declaredType.getDeclaredOperations()) {
				final var jvmSignature = this.actionPrototypeProvider.createParameterTypesFromJvmModel(
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
		var annotation = annot;
		final var value = annotation.getMemberValuePairs()[0].getValue();
		final var fieldId = (value == null) ? null : value.toString();
		if (!Strings.isNullOrEmpty(fieldId)) {
			final var methodName = Utils.createNameForHiddenDefaultValueFunction(fieldId);
			final var method = operation.getDeclaringType().getMethod(methodName, new String[0]);
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
	 * @param resolver the resolver of type names according to the local JDT context.
	 * @param context the context in which the conversion is done.
	 * @return the parameters.
	 * @throws JavaModelException if the Java model is invalid.
	 * @throws IllegalArgumentException if the signature is not syntactically correct.
	 * @since 0.15
	 */
	protected IFormalParameterBuilder[] createFormalParametersWith(
			ParameterBuilder parameterBuilder,
			IMethod operation,
			LocalTypeResolver resolver,
			ConversionContext context) throws JavaModelException, IllegalArgumentException {
		final var isVarargs = Flags.isVarargs(operation.getFlags());
		final var rawParameters = operation.getParameters();
		final var parameters = getFormalParameterProvider(context, operation);
		final var len = parameters.getFormalParameterCount();
		final var paramBuilders = new IFormalParameterBuilder[len];
		for (var i = 0; i < len; ++i) {
			final var rawParameter = rawParameters[i];
			final var annotation = getAnnotation(rawParameter, DefaultValue.class.getName());
			final var defaultValue = (annotation != null) ? extractDefaultValue(operation, annotation) : null;
			final var isV = isVarargs && i == len - 1;
			var type = parameters.getFormalParameterTypeReference(i, isV);
			if (isV && type instanceof JvmGenericArrayTypeReference arrayType) {
				type = arrayType.getComponentType();
			}
			final var sarlParameter = parameterBuilder.addParameter(parameters.getFormalParameterName(i));
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

	/** Convert the JDT signature (type) to its equivalent Ecore type.
	 *
	 * <p>This function considers the locally declared type parameters.
	 *
	 * @param jdtSignature the signature (type) from JDT API.
	 * @param resolver the resolver of type names according the local context of the type.
	 * @param context the context in which the conversion is done.
	 * @return the Ecore signature.
	 * @since 0.15
	 */
	protected JvmTypeReference convertSignatureFromJdt2Ecore(String jdtSignature, 
			LocalTypeResolver resolver, ConversionContext context) {
		final var kind = Signature.getTypeSignatureKind(jdtSignature);
		final var isWildcard = kind == Signature.WILDCARD_TYPE_SIGNATURE;
		var wildcard = '\0';
		String jdtErasure;
		if (isWildcard) {
			wildcard = jdtSignature.charAt(0); 
			jdtErasure = Signature.getTypeErasure(jdtSignature.substring(1));
		} else {
			jdtErasure = Signature.getTypeErasure(jdtSignature);
		}
		final var ecoreTypeName = Signature.toString(jdtErasure);
		final var ecoreTypeParameter = context.getDeclaredTypeParameter(ecoreTypeName);
		if (ecoreTypeParameter != null) {
			if (isWildcard) {
				return wildcard(wildcard, ecoreTypeParameter);
			}
			return ecoreTypeParameter;
		}
		final var ecoreRawType = context.findType(ecoreTypeName, resolver);
		if (ecoreRawType != null) {
			final var jdtTypeParameters = Signature.getTypeArguments(jdtSignature);
			if (jdtTypeParameters.length > 0) {
				final var ecoreTypeParameters = new JvmTypeReference[jdtTypeParameters.length];
				var i = 0;
				for (final var jdtTypeParameter : jdtTypeParameters) {
					ecoreTypeParameters[i] = convertSignatureFromJdt2Ecore(jdtTypeParameter, resolver, context);
					++i;
				}
				final var result = this.ecoreTypeReferences.createTypeRef(ecoreRawType.getType(), ecoreTypeParameters);
				if (isWildcard) {
					return wildcard(wildcard, result);
				}
				return result;
			}
		}
		if (isWildcard) {
			return wildcard(wildcard, ecoreRawType);
		}
		return ecoreRawType;
	}

	private JvmTypeReference wildcard(char wildcard, JvmParameterizedTypeReference type) {
		switch (wildcard) {
		case Signature.C_STAR:
			return this.ecoreTypeReferences.wildCard();
		case Signature.C_EXTENDS:
			return this.ecoreTypeReferences.wildCardExtends(type);
		case Signature.C_SUPER:
			return this.ecoreTypeReferences.wildCardSuper(type);
		default:
		}
		return type;
	}

	/** Create the type parameters for the given operation.
	 *
	 * @param parameterBuilder the code builder.
	 * @param operation the operation that describes the formal parameters.
	 * @param resolver the resolver of type names according to the local JDT context.
	 * @param context the context in which the conversion is done.
	 * @return the new context with the type parameters declared inside.
	 * @throws JavaModelException if the Java model is invalid.
	 * @throws IllegalArgumentException if the signature is not syntactically correct.
	 * @since 0.15
	 */
	protected ConversionContext createTypeParametersWith(
			TypeParameterBuilder parameterBuilder,
			IMethod operation,
			LocalTypeResolver resolver,
			ConversionContext context) throws JavaModelException, IllegalArgumentException {
		final var len = operation.getTypeParameters().length;
		if (len > 0) {
			final var localContext = new LocalConversionContext(context);
			for (final var operationTypeParameter : operation.getTypeParameters()) {
				final var typeParameterBuilder = parameterBuilder.addTypeParameter(operationTypeParameter.getElementName());
				for (final var bound : operationTypeParameter.getBoundsSignatures()) {
					typeParameterBuilder.addUpperConstraint(convertSignatureFromJdt2Ecore(bound, resolver, context));
				}
				localContext.declareTypeParameter(operationTypeParameter.getElementName(),
						this.ecoreTypeReferences.createTypeRef(typeParameterBuilder.getJvmTypeParameter()));
			}
			return localContext;
		}
		return context;
	}

	/** Add the given constructors to the Ecore container.
	 *
	 * @param codeBuilder the code builder to use.
	 * @param superClassConstructors the constructors defined in the super class.
	 * @param context the context of the constructors.
	 * @throws JavaModelException if the Java model is invalid.
	 * @since 0.15
	 */
	public void createStandardConstructorsWith(
			ConstructorBuilder codeBuilder,
			Collection<IMethod> superClassConstructors,
			ConversionContext context) throws JavaModelException {
		if (superClassConstructors != null && context != null) {
			for (final var constructor : superClassConstructors) {
				if (!isGeneratedOperation(constructor)) {
					final var resolver = new TypeBasedLocalTypeResolver(constructor.getDeclaringType());
					final var cons = codeBuilder.addConstructor();
					// Create type parameters
					final var localContext = createTypeParametersWith(name -> cons.addTypeParameter(name), constructor, resolver, context);
					// Create parameters
					final var sarlParams = createFormalParametersWith(name -> cons.addParameter(name), constructor, resolver, localContext);
					// Create the block
					final var block = cons.getExpression();
					// Create the super-call expression
					final var superCall = block.addExpression();
					final var call = XbaseFactory.eINSTANCE.createXFeatureCall();
					superCall.setXExpression(call);
					// Set the todo comment.
					superCall.setDocumentation(block.getAutoGeneratedActionString());
					// Create the super-call XExpression
					call.setFeature(getJvmConstructor(constructor, localContext));
					call.setExplicitOperationCall(true);
					final var arguments = call.getFeatureCallArguments();
					for (final var currentParam : sarlParams) {
						final var argumentSource = XbaseFactory.eINSTANCE.createXFeatureCall();
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
	 * @since 0.15
	 */
	public void createActionsWith(
			ActionBuilder codeBuilder,
			Collection<IMethod> methods,
			ConversionContext context) throws JavaModelException, IllegalArgumentException {
		if (methods != null && context != null) {
			for (final var operation : methods) {
				if (!isGeneratedOperation(operation)) {
					final var resolver = new TypeBasedLocalTypeResolver(operation.getDeclaringType());
					final var action = codeBuilder.addAction(operation.getElementName());
					// Create the type parameters
					final var localContext = createTypeParametersWith(name -> action.addTypeParameter(name), operation, resolver, context);
					// Specify the return type
					action.setReturnType(convertSignatureFromJdt2Ecore(operation.getReturnType(), resolver, localContext));
					// Create the formal parameters
					final var sarlParams = createFormalParametersWith(name -> action.addParameter(name), operation, resolver, localContext);
					// Do the body
					if (localContext.getEcoreContext() != null) {
						final var type = this.ecoreTypeReferences.findDeclaredType(
								operation.getDeclaringType().getFullyQualifiedName(),
								localContext.getEcoreContext());
						final var superOperation = getJvmOperation(localContext, operation, type);
						// Create the block
						final var block = action.getExpression();
						if ((type.eClass() != TypesPackage.Literals.JVM_GENERIC_TYPE
								|| !((JvmGenericType) type).isInterface())
								&& superOperation != null
								&& !superOperation.isAbstract()) {
							// Create the super-call expression
							final var superCall = block.addExpression();
							final var call = XbaseFactory.eINSTANCE.createXMemberFeatureCall();
							superCall.setXExpression(call);
							superCall.setDocumentation(block.getAutoGeneratedActionString());
							call.setFeature(superOperation);
							call.setMemberCallTarget(superCall.createReferenceToSuper());
							final var arguments = call.getMemberCallArguments();
							for (final var currentParam : sarlParams) {
								final var argumentSource = XbaseFactory.eINSTANCE.createXFeatureCall();
								arguments.add(argumentSource);
								currentParam.setReferenceInto(argumentSource);
							}
						} else {
							final var ret = superOperation != null ? superOperation.getReturnType() : null;
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
	 * @param context the context in which the conversion is done.
	 * @param typeFinder the type finder to be used for finding the type definitions.
	 * @param subClass the name of the sub class.
	 * @param superClass the name of the expected super class.
	 * @return {@code true} if it is a subclass.
	 * @throws JavaModelException if the Java model is invalid.
	 * @see #toTypeFinder(IJavaProject)
	 * @since 0.15
	 */
	public boolean isSubClassOf(ConversionContext context, TypeFinder typeFinder, String subClass, String superClass) throws JavaModelException {
		if (context != null) {
			final var typeIterator = new SuperTypeIterator(context, typeFinder, false, subClass);
			while (typeIterator.hasNext()) {
				final var type = typeIterator.next();
				if (Objects.equals(type.getFullyQualifiedName(), superClass)) {
					return true;
				}
			}
		}
		return false;
	}

	/** Replies if the given type is a subclass of the second type.
	 *
	 * <p>The type finder could be obtained with {@link #toTypeFinder(IJavaProject)}.
	 *
	 * @param typeFinder the type finder to be used for finding the type definitions.
	 * @param subClass the name of the sub class.
	 * @param superClass the name of the expected super class.
	 * @return {@code true} if it is a subclass.
	 * @throws JavaModelException if the Java model is invalid.
	 * @see #toTypeFinder(IJavaProject)
	 */
	public boolean isSubClassOf(TypeFinder typeFinder, String subClass, String superClass) throws JavaModelException {
		return isSubClassOf(contextualize(null, this.ecoreTypeReferences), typeFinder, subClass, superClass);
	}

	/** Iterator on the super types of a given type.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 */
	private class SuperTypeIterator implements Iterator<IType> {

		private final ConversionContext context;
		
		private final TypeFinder typeFinder;

		private final Set<String> encountered = new TreeSet<>();

		private final Deque<String> queue = new LinkedList<>();

		private final boolean isInterface;

		private final List<IStatus> statuses = new ArrayList<>();

		private IType current;

		/** Constructor.
		 *
		 * @param context the context in which the conversion is done.
		 * @param typeFinder the type finder to be used for finding the type definitions.
		 * @param isInterface indicates if the exploration is for interfaces or for classes.
		 * @param typeNames the initial types.
		 * @since 0.15
		 */
		SuperTypeIterator(ConversionContext context, TypeFinder typeFinder, boolean isInterface, String... typeNames) {
			this(context, typeFinder, isInterface, Arrays.asList(typeNames));
		}

		/** Constructor.
		 *
		 * @param context the context in which the conversion is done.
		 * @param typeFinder the type finder to be used for finding the type definitions.
		 * @param isInterface indicates if the exploration is for interfaces or for classes.
		 * @param typeNames the initial types.
		 * @since 0.15
		 */
		SuperTypeIterator(ConversionContext context, TypeFinder typeFinder, boolean isInterface, Collection<String> typeNames) {
			this.context = context;
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
				final var typeName = this.queue.removeFirst();
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
			final var c = this.current;
			if (c == null) {
				throw new NoSuchElementException();
			}
			final var name = c.getFullyQualifiedName();
			this.encountered.add(name);
			try {
				final String[] superTypes;
				if (this.isInterface) {
					superTypes = c.getSuperInterfaceTypeSignatures();
				} else {
					superTypes = new String[] {c.getSuperclassTypeSignature()};
				}
				for (final var signature : superTypes) {
					if (!Strings.isNullOrEmpty(signature)) {
						final var resolvedSignature = resolveType(c, signature);
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

		private String resolveType(IType type, String signature) {
			final var resolver = new TypeBasedLocalTypeResolver(type);
			return convertSignatureFromJdt2Ecore(signature, resolver, this.context).getType().getIdentifier();
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

	/** Provider of SARL formal parameters from a the JDT formal parameter list.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 */
	private class JdtFormalParameterList implements FormalParameterProvider {

		private final ConversionContext context;
	
		private final int nb;

		private final String[] names;

		private final JvmTypeReference[] types;

		/** Constructor.
		 *
		 * @param context the context in which the conversion is done.
		 * @param operation the operation.
		 * @throws JavaModelException if the parameters cannot be retreived.
		 * @since 0.15
		 */
		JdtFormalParameterList(ConversionContext context, IMethod operation) throws JavaModelException {
			this.context = context;
			this.nb = operation.getNumberOfParameters();
			this.names = operation.getParameterNames();
			this.types = new JvmTypeReference[this.nb];
			final var unresolvedParameters = operation.getParameters();
			for (var i = 0; i < this.nb; ++i) {
				this.types[i] = resolve(operation, unresolvedParameters[i].getTypeSignature());
			}
		}

		private JvmTypeReference resolve(IMethod operation, String typename) throws JavaModelException {
			final var resolver = new TypeBasedLocalTypeResolver(operation.getDeclaringType());
			if (Signature.C_UNRESOLVED == Signature.getElementType(typename).charAt(0)) {
				final var unit = operation.getCompilationUnit();
				if (unit != null) {
					final var post = "." + convertSignatureFromJdt2Ecore(typename, resolver, this.context); //$NON-NLS-1$
					for (final var decl : unit.getImports()) {
						if (decl.getElementName().endsWith(post)) {
							return this.context.findType(decl.getElementName(), resolver);
						}
					}
				}
			}
			return convertSignatureFromJdt2Ecore(typename, resolver, this.context);
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
			return this.types[position].getIdentifier();
		}

		@Override
		public JvmTypeReference getFormalParameterTypeReference(int position, boolean isVarargs) {
			return this.types[position];
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
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
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
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 */
	@FunctionalInterface
	protected interface ParameterBuilder {
		IFormalParameterBuilder addParameter(String name);
	}

	/** Type parameter builder.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 * @since 0.15
	 */
	@FunctionalInterface
	protected interface TypeParameterBuilder {
		ITypeParameterBuilder addTypeParameter(String name);
	}

	/** Action builder.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
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
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 */
	@FunctionalInterface
	public interface ConstructorBuilder {
		/** Add a constructor.
		 *
		 * @return the builder of the constructor.
		 */
		ISarlConstructorBuilder addConstructor();
	}

	/** The interface permits to characterize the context of the JDT 2 Ecore conversion.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 * @since 0.15
	 */
	public interface ConversionContext {

		/** Replies the context in Ecore. The context scopes the features such as the types.
		 *
		 * @return the context. It could be {@code null}.
		 */
		Notifier getEcoreContext();

		/** Replies the reference to the type parameter with the given name.
		 * The type parameter must be declared in the conversion context.
		 *
		 * @param name the name of the type parameter to search for.
		 * @return the reference to the type parameter, or {@code null} if there is
		 *    no declared type parameter with the given name.
		 */
		JvmParameterizedTypeReference getDeclaredTypeParameter(String name);

		/** Find the definition of a type.
		 *
		 * @param typeName the name of the type to search for.
		 * @param resolver the tool for resolving a type name according to a local context.
		 * @return the type, never {@code null}.
		 * @throws TypeNotPresentException if the given type was not found.
		 */
		JvmParameterizedTypeReference findType(String typeName, LocalTypeResolver resolver);

	}

	/** The interface permits to resolve a type according to a local context based on the
	 * current package and imported types.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 * @since 0.15
	 */
	@FunctionalInterface
	public interface LocalTypeResolver {

		/** Resolve the given type.
		 *
		 * @param type the name of the type to resolve.
		 * @return the resolved type.
		 * @throws JavaModelException if the JDT element cannot be resolved.
		 */
		String resolve(String type) throws JavaModelException;

	}
	
	/** Resolve a type according to a local context based on the
	 * current package and imported types.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 * @since 0.15
	 */
	protected static class TypeBasedLocalTypeResolver  implements LocalTypeResolver, Serializable {

		private static final long serialVersionUID = -1468256062447840456L;

		private final IType type;
		
		/** Constructor.
		 *
		 * @param type the context type.
		 */
		public TypeBasedLocalTypeResolver(IType type) {
			this.type = type;
		}
		
		@Override
		public String resolve(String type) throws JavaModelException {
			final var resolved = this.type.resolveType(type);
			if (resolved != null) {
				for (final var entry : resolved) {
					if (Strings.isNullOrEmpty(entry[0])) {
						return entry[1];
					}
					return entry[0] + "." + entry[1]; //$NON-NLS-1$
				}
			}
			return type;
		}

	}

	/** A conversion context that enables to add local declarations of type parameters.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 * @since 0.15
	 */
	protected static class LocalConversionContext implements ConversionContext, Serializable {

		private static final long serialVersionUID = 4491570488424452153L;

		private final ConversionContext parent;

		private final Map<String, JvmParameterizedTypeReference> localTypeParameters = new HashMap<>();

		/** Constructor.
		 *
		 * @param parent the parent context, never {@code null}.
		 */
		protected LocalConversionContext(ConversionContext parent) {
			assert parent != null;
			this.parent = parent;
		}

		@Override
		public Notifier getEcoreContext() {
			return this.parent.getEcoreContext();
		}

		/** Add the declaration of a local type parameter.
		 *
		 * @param name the name of the type parameter.
		 * @param reference the reference to the type parameter in the context.
		 * @return {@code this}.
		 */
		public LocalConversionContext declareTypeParameter(String name, JvmParameterizedTypeReference reference) {
			this.localTypeParameters.put(name, reference);
			return this;
		}
		
		@Override
		public JvmParameterizedTypeReference getDeclaredTypeParameter(String name) {
			final var reference = this.localTypeParameters.get(name);
			if (reference != null) {
				return reference;
			}
			return this.parent.getDeclaredTypeParameter(name);
		}

		@Override
		public JvmParameterizedTypeReference findType(String typeName, LocalTypeResolver resolver) {
			return this.parent.findType(typeName, resolver);
		}

	}

	/** A conversion context based on an Ecore type declaration.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.0 20250909-115751
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 * @since 0.15
	 */
	protected static class EcoreTypeConversionContext implements ConversionContext, Serializable {

		private static final long serialVersionUID = -3929314423261518377L;

		private final TypeReferences references;
		
		private final XtendTypeDeclaration context;
		
		private Map<String, JvmParameterizedTypeReference> typeParameters;

		/** Constructor.
		 *
		 * @param context the Ecore context, may be {@code null}.
		 * @param references the tool for obtaining Ecore type references, never {@code null}.
		 */
		protected EcoreTypeConversionContext(XtendTypeDeclaration context, TypeReferences references) {
			assert references != null;
			this.context = context;
			this.references = references;
		}

		@Override
		public Notifier getEcoreContext() {
			return this.context;
		}
		
		@Override
		public JvmParameterizedTypeReference getDeclaredTypeParameter(String name) {
			if (this.typeParameters == null) {
				this.typeParameters = new HashMap<>();
				fillTypeParameters(this.context, this.typeParameters, this.references);
			}
			return this.typeParameters.get(name);
		}

		@Override
		public JvmParameterizedTypeReference findType(String typeName, LocalTypeResolver resolver) {
			String tn;
			try {
				tn = resolver.resolve(typeName);
			} catch (JavaModelException ex) {
				tn = typeName;
			}
			var type = this.references.findDeclaredType(tn, this.context == null ? new ResourceSetImpl() : this.context);
			if (type != null) {
				return this.references.createTypeRef(type);
			}
			throw new TypeNotPresentException(typeName, null);
		}

	}

}
