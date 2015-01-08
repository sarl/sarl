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
package io.sarl.eclipse.util;

import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.Generated;
import io.sarl.lang.genmodel.SARLCodeGenerator.GeneratedCode;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.FeatureContainer;
import io.sarl.lang.sarl.ParameterizedFeature;
import io.sarl.lang.signature.ActionKey;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.signature.ActionSignatureProvider.FormalParameterProvider;
import io.sarl.lang.signature.SignatureKey;
import io.sarl.lang.util.ModelUtil;

import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.emf.common.util.EList;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.IAnnotatable;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.Signature;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XbaseFactory;

import com.google.common.base.Strings;


/** Utilities for creating Ecore SARL elements from the JDT model.
 *
 * This class extends the {@link ModelUtil} from the <code>io.sarl.lang</code> project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see ModelUtil
 */
public final class Jdt2Ecore {

	private Jdt2Ecore() {
		//
	}

	/** Find the definition of a type in the classpath of the given project.
	 *
	 * @param project - the project to explore.
	 * @param typeName - the name of the type to search for.
	 * @return the type, or <code>null</code> if the type was not found.
	 * @throws JavaModelException if this project does not exist or if an
	 *		exception occurs while accessing its corresponding resource.
	 */
	public static IType findType(IJavaProject project, String typeName) throws JavaModelException {
		if (project != null) {
			return project.findType(typeName);
		}
		return null;
	}

	/** Replies if the given type name is valid for a super-type.
	 *
	 * The type name is valid if it is not empty and not the <code>Object</code> classname.
	 *
	 * @param typeName - the name of the type to test.
	 * @return <code>true</code> if the given type name is valid.
	 */
	protected static boolean isValidSuperType(String typeName) {
		return !Strings.isNullOrEmpty(typeName) && !"java.lang.Object".equals(typeName); //$NON-NLS-1$
	}

	/** Replies if the target feature is visible from the type.
	 *
	 * @param fromType - the type from which the feature visibility is tested.
	 * @param target - the feature to test for the visibility.
	 * @return <code>true</code> if the given type can see the target feature.
	 * @throws JavaModelException if the Java model is invalid.
	 */
	public static boolean isVisible(IType fromType, IMember target) throws JavaModelException {
		int flags = target.getFlags();
		if (Flags.isPublic(flags) || Flags.isProtected(flags)) {
			return true;
		}
		if (Flags.isPrivate(flags)) {
			return false;
		}
		IPackageFragment f1 = target.getDeclaringType().getPackageFragment();
		IPackageFragment f2 = fromType.getPackageFragment();
		if (f1.isDefaultPackage()) {
			return f2.isDefaultPackage();
		}
		return f1.getElementName().equals(f2.getElementName());
	}

	/** Replies the provider of formal parameters for the given operation.
	 *
	 * @param operation - the operation.
	 * @return the provider of formal parameters for the operation.
	 * @throws JavaModelException if the Java model is invalid.
	 */
	public static FormalParameterProvider getFormalParameterProvider(IMethod operation) throws JavaModelException {
		return new FormalParameterList(operation);
	}

	/** Analyzing the type hierarchy of the given element, and
	 * extract any type-related information.
	 *
	 * @param project - the project to explore.
	 * @param finalOperations - filled with the final operations inherited by the element.
	 * @param overridableOperations - filled with the oervrideable operations inherited by the element.
	 * @param inheritedFields - filled with the fields inherited by the element.
	 * @param operationsToImplement - filled with the abstract operations inherited by the element.
	 * @param superConstructors - filled with the construstors of the super type.
	 * @param sarlSignatureProvider - provider of tools related to action signatures.
	 * @param superClass - the name of the super class.
	 * @param superInterfaces - the super interfaces.
	 * @throws JavaModelException if the Java model is invalid.
	 */
	public static void populateInheritanceContext(
			IJavaProject project,
			Map<ActionKey, IMethod> finalOperations,
			Map<ActionKey, IMethod> overridableOperations,
			Map<String, IField> inheritedFields,
			Map<ActionKey, IMethod> operationsToImplement,
			Map<SignatureKey, IMethod> superConstructors,
			ActionSignatureProvider sarlSignatureProvider,
			String superClass,
			List<String> superInterfaces) throws JavaModelException {
		// Get the operations that must be implemented
		if (operationsToImplement != null) {
			Iterator<IType> typeIterator = new SuperTypeIterator(project, true, superInterfaces);
			while (typeIterator.hasNext()) {
				IType type = typeIterator.next();
				for (IMethod operation : type.getMethods()) {
					if (!Flags.isStatic(operation.getFlags())
							&& !Flags.isFinal(operation.getFlags())
							&& !operation.isLambdaMethod()
							&& !operation.isConstructor()) {
						SignatureKey sig = sarlSignatureProvider.createSignatureID(
								Flags.isVarargs(operation.getFlags()), getFormalParameterProvider(operation));
						ActionKey actionKey = sarlSignatureProvider.createActionID(
								operation.getElementName(),
								sig);
						operationsToImplement.put(actionKey, operation);
					}
				}
			}
		}

		// Check on the implemented features, inherited from the super type
		if (isValidSuperType(superClass)) {
			Iterator<IType> typeIterator = new SuperTypeIterator(project, false, superClass);
			while (typeIterator.hasNext()) {
				IType type = typeIterator.next();
				boolean checkForConstructors = (superConstructors != null && type.getFullyQualifiedName().equals(superClass));
				for (IMethod operation : type.getMethods()) {
					if (!Flags.isStatic(operation.getFlags())
							&& !operation.isLambdaMethod()
							&& isVisible(type, operation)) {
						if (!operation.isConstructor()
								&& !ModelUtil.isHiddenAction(operation.getElementName())) {
							SignatureKey sig = sarlSignatureProvider.createSignatureID(
									Flags.isVarargs(operation.getFlags()), getFormalParameterProvider(operation));
							ActionKey actionKey = sarlSignatureProvider.createActionID(
									operation.getElementName(), sig);
							int flags = operation.getFlags();
							if (Flags.isAbstract(flags)) {
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
						} else if (checkForConstructors && operation.isConstructor() && superConstructors != null) {
							SignatureKey sig = sarlSignatureProvider.createSignatureID(
									Flags.isVarargs(operation.getFlags()), getFormalParameterProvider(operation));
							superConstructors.put(sig,  operation);
						}
					}
				}

				if (inheritedFields != null) {
					for (IField field : type.getFields()) {
						if (!Flags.isStatic(field.getFlags())
								&& !ModelUtil.isHiddenAttribute(field.getElementName())
								&& isVisible(type, field)) {
							inheritedFields.put(field.getElementName(), field);
						}
					}
				}
			}
		}
	}

	/** Replies if the given method is marked has automatically generated by the SARL compiler.
	 *
	 * @param method - the method to check.
	 * @return <code>true</code> if the method is annoted with Generated; <code>false</code>
	 * otherwise.
	 */
	public static boolean isGeneratedOperation(IMethod method) {
		return getAnnotation(method, Generated.class.getName()) != null;
	}

	/** Replies the annotation with the given qualified name.
	 *
	 * @param element - the annoted element.
	 * @param qualifiedName - the qualified name of the element.
	 * @return the annotation, or <code>null</code> if the element is not annoted.
	 */
	public static IAnnotation getAnnotation(IAnnotatable element, String qualifiedName) {
		if (element != null) {
			try {
				int separator = qualifiedName.lastIndexOf('.');
				String simpleName;
				if (separator >= 0 && separator < (qualifiedName.length() - 1)) {
					simpleName = qualifiedName.substring(separator + 1, qualifiedName.length());
				} else {
					simpleName = qualifiedName;
				}
				for (IAnnotation annotation : element.getAnnotations()) {
					String name = annotation.getElementName();
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

	/** Create the JvmConstructor for the given JDT method.
	 *
	 * @param code - the generated code.
	 * @param constructor - the JDT constructor.
	 * @param context - the context of the constructor.
	 * @return the JvmConstructor
	 * @throws JavaModelException if the Java model is invalid.
	 */
	public static JvmConstructor getJvmConstructor(GeneratedCode code, IMethod constructor, FeatureContainer context)
			throws JavaModelException {
		if (constructor.isConstructor()) {
			JvmType type = code.getCodeGenerator().getTypeReferences().findDeclaredType(
					constructor.getDeclaringType().getFullyQualifiedName(),
					context);
			if (type instanceof JvmDeclaredType) {
				JvmDeclaredType declaredType = (JvmDeclaredType) type;
				ActionSignatureProvider sigProvider = code.getCodeGenerator().getActionSignatureProvider();
				SignatureKey jdtSignature = sigProvider.createSignatureID(
						Flags.isVarargs(constructor.getFlags()),
						Jdt2Ecore.getFormalParameterProvider(constructor));
				for (JvmConstructor jvmConstructor : declaredType.getDeclaredConstructors()) {
					SignatureKey jvmSignature = sigProvider.createSignatureIDFromJvmModel(
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

	private static String extractDefaultValue(IMethod operation, IAnnotation annot)
			throws JavaModelException, IllegalArgumentException {
		IAnnotation annotation = annot;
		Object value = annotation.getMemberValuePairs()[0].getValue();
		String fieldId = (value == null) ? null : value.toString();
		if (!Strings.isNullOrEmpty(fieldId)) {
			String fieldName = ModelUtil.PREFIX_ATTRIBUTE_DEFAULT_VALUE + fieldId;
			IField field = operation.getDeclaringType().getField(fieldName);
			if (field != null) {
				annotation = Jdt2Ecore.getAnnotation(field, Generated.class.getName());
				if (annotation != null) {
					return annotation.getMemberValuePairs()[0].getValue().toString();
				}
			}
		}
		return null;
	}

	/** Create the formal parameters for the given operation.
	 *
	 * @param code - the generated code.
	 * @param operation - the operation that describes the formal parameters.
	 * @param container - the container of the created formal parameters.
	 * @throws JavaModelException if the Java model is invalid.
	 * @throws IllegalArgumentException if the signature is not syntactically correct.
	 */
	public static void createFormalParameters(GeneratedCode code, IMethod operation,
			ParameterizedFeature container) throws JavaModelException, IllegalArgumentException {
		boolean isVarargs = Flags.isVarargs(operation.getFlags());
		container.setVarargs(isVarargs);
		ILocalVariable[] parameters = operation.getParameters();
		for (int i = 0; i < parameters.length; ++i) {
			ILocalVariable parameter = parameters[i];
			IAnnotation annotation = Jdt2Ecore.getAnnotation(parameter, DefaultValue.class.getName());
			String defaultValue = (annotation != null) ? extractDefaultValue(operation, annotation) : null;
			String type = Signature.toString(parameter.getTypeSignature());
			if (isVarargs && i == parameters.length - 1 && type.endsWith("[]")) { //$NON-NLS-1$
				type = type.substring(0, type.length() - 2);
			}
			code.getCodeGenerator().createFormalParameter(code, container,
					parameter.getElementName(),
					type,
					defaultValue,
					code.getResourceSet());
		}
	}

	/** Add the given constructors to the Ecore container.
	 *
	 * @param code - the generated code.
	 * @param superClassConstructors - the constructors defined in the super class.
	 * @param container - the container of the created constructors.
	 * @throws JavaModelException if the Java model is invalid.
	 */
	public static void createStandardConstructors(GeneratedCode code,
			Collection<IMethod> superClassConstructors, FeatureContainer container) throws JavaModelException {
		if (superClassConstructors != null) {
			for (IMethod constructor : superClassConstructors) {
				if (!isGeneratedOperation(constructor)) {
					XBlockExpression block = XbaseFactory.eINSTANCE.createXBlockExpression();
					//
					JvmConstructor jvmConstructor = getJvmConstructor(code, constructor, container);
					XFeatureCall call = XbaseFactory.eINSTANCE.createXFeatureCall();
					call.setFeature(jvmConstructor);
					call.setExplicitOperationCall(true);
					EList<XExpression> arguments = call.getFeatureCallArguments();
					for (JvmFormalParameter param : jvmConstructor.getParameters()) {
						XFeatureCall paramRef = XbaseFactory.eINSTANCE.createXFeatureCall();
						paramRef.setFeature(param);
						arguments.add(paramRef);
					}
					block.getExpressions().add(call);
					//
					Constructor cons = code.getCodeGenerator().createConstructor(code, container, block);
					createFormalParameters(code, constructor, cons);
				}
			}
		}
	}

	/** Create the operations into the SARL feature container.
	 *
	 * @param code - the generated code.
	 * @param methods - the operations to create.
	 * @param container - the container of the created formal parameters.
	 * @throws JavaModelException if the Java model is invalid.
	 * @throws IllegalArgumentException if the signature is not syntactically correct.
	 */
	public static void createActions(GeneratedCode code,
			Collection<IMethod> methods, FeatureContainer container) throws JavaModelException, IllegalArgumentException {
		if (methods != null) {
			for (IMethod operation : methods) {
				if (!isGeneratedOperation(operation)) {
					Action action = code.getCodeGenerator().createAction(code, container,
							operation.getElementName(),
							Signature.toString(operation.getReturnType()),
							null);
					createFormalParameters(code, operation, action);
				}
			}
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class SuperTypeIterator implements Iterator<IType> {

		private final IJavaProject project;
		private final Set<String> encountered = new TreeSet<>();
		private final Deque<String> queue = new LinkedList<>();
		private final boolean isInterface;
		private IType current;

		/**
		 * @param project
		 * @param isInterface
		 * @param typeNames
		 */
		public SuperTypeIterator(IJavaProject project, boolean isInterface, String... typeNames) {
			this(project, isInterface, Arrays.asList(typeNames));
		}

		/**
		 * @param project
		 * @param isInterface
		 * @param typeNames
		 */
		public SuperTypeIterator(IJavaProject project, boolean isInterface, Collection<String> typeNames) {
			this.isInterface = isInterface;
			this.project = project;
			this.queue.addAll(typeNames);
			updateCurrent();
		}

		private void updateCurrent() {
			this.current = null;
			while (this.current == null && !this.queue.isEmpty()) {
				String typeName = this.queue.removeFirst();
				if (isValidSuperType(typeName) && !this.encountered.contains(typeName)) {
					try {
						this.current = findType(this.project, typeName);
					} catch (JavaModelException e) {
						this.current = null;
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
			if (this.current == null) {
				throw new NoSuchElementException();
			}
			IType c = this.current;
			String name = c.getFullyQualifiedName();
			this.encountered.add(name);
			try {
				if (this.isInterface) {
					this.queue.addAll(Arrays.asList(c.getSuperInterfaceNames()));
				} else {
					this.queue.add(c.getSuperclassName());
				}
			} catch (JavaModelException _) {
				//
			}
			updateCurrent();
			return c;
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class FormalParameterList implements FormalParameterProvider {

		private final ILocalVariable[] parameters;

		/**
		 * @param operation - the operation.
		 * @throws JavaModelException if the parameters cannot be retreived.
		 */
		public FormalParameterList(IMethod operation) throws JavaModelException {
			this.parameters = operation.getParameters();
		}

		@Override
		public int getFormalParameterCount() {
			return this.parameters.length;
		}

		@Override
		public String getFormalParameterName(int position) {
			return this.parameters[position].getElementName();
		}

		@Override
		public String getFormalParameterType(int position, boolean isVarArgs) {
			return Signature.toString(this.parameters[position].getTypeSignature());
		}

	}

}
