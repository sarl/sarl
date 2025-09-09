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

import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.AnonymousClass;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmFeature;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeParameterDeclarator;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.jvmmodel.JvmAnnotationReferenceBuilder;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder;

/** Provide the base functions and context used for inferring JVM elements from SARL element.
 *
 * @author $Author: sgalland$
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.15
 */
public interface IBaseJvmModelInferrer {

	/** Infer and transform the local (anonymous) class.
	 *
	 * @param anonymousClass the anonymous class to be inferred.
	 * @param localClassName the name of the class.
	 * @param container the container feature of the anonymous class.
	 */
	void inferLocalClass(AnonymousClass anonymousClass, String localClassName, JvmFeature container);
	
	/** Open the context for the generation of a SARL-specific element.
	 *
	 * @param sarlObject the SARL object that is the cause of the generation.
	 * @param type the generated type.
	 * @param supportedMemberTypes the types of the supported members.
	 * @return the created context.
	 */
	GenerationContext openContext(EObject sarlObject, JvmDeclaredType type, final Iterable<Class<? extends XtendMember>> supportedMemberTypes);
	
	/** Replies the SARL-specific generation context.
	 *
	 * @param type the generated type.
	 * @return the SARL-specific generation context.
	 */
	GenerationContext getContext(JvmIdentifiableElement type);

	/** Close a generation context.
	 *
	 * @param context the context to be closed.
	 */
	void closeContext(GenerationContext context);

	/** Translate the given parameter to the given executable using the default translation rules
	 * provided by Xtext/Xtend.
	 * 
	 * @param executable the executable to extend.
	 * @param parameter the parameter to convert an put inside the {@code executable}.
	 */
	void translateParameter(JvmExecutable executable, XtendParameter parameter);

	/** Translate the given annotations and put them into the given target, using the
	 * standard rules provided by Xtext/Xtend.
	 * 
	 * @param annotations the annotations to translate.
	 * @param target the receiver of the annotations.
	 */
	void translateAnnotationsTo(List<XAnnotation> annotations, JvmAnnotationTarget target);
	
	/** Copy the given type parameters and fix their definition for following the Ecore specifications.
	 * Usually, the major fix is the adding of the upper bounds {@code Object} when the type parameter
	 * does not specify an upper bound.
	 *
	 * @param typeParameters the type parameters to copy and fix.
	 * @param target the receiver of the type parameters.
	 */
	void copyAndFixTypeParameters(List<JvmTypeParameter> typeParameters, JvmTypeParameterDeclarator target);

	/** Fix the definition of the type parameters.
	 * Usually, the major fix is the adding of the upper bounds {@code Object} when the type parameter
	 * does not specify an upper bound.
	 *
	 * @param target the receiver of the type parameters to be fixed.
	 */
	void fixTypeParameters(JvmTypeParameterDeclarator target);

	/** Replies the builder of JVM type references that is linked to the generation context.
	 *
	 * @return the builder.
	 */
	JvmTypeReferenceBuilder getJvmTypeReferenceBuilder();

	/** Replies the builder of JVM annotations that is linked to the generation context.
	 *
	 * @return the builder.
	 */
	JvmAnnotationReferenceBuilder getJvmAnnotationReferenceBuilder();

	/** Log an internal error but do not fail.
	 *
	 * @param message the internal message.
	 */
	void logInternalError(String message);

	/** Log an internal error but do not fail.
	 *
	 * @param exception the exception to log.
	 */
	void logInternalError(Throwable exception);

	/** Transform the given member to JVM elements.
	 *
	 * @param sourceMember the member to transform.
	 * @param container the receiver.
	 * @param allowDispatch indicates if dispatching functions are allowed.
	 */
	void transform(XtendMember sourceMember, JvmGenericType container, boolean allowDispatch);

	/** Append the synthetic methods for declared dispatch methods.
	 *
	 * @param source the type in which the dispatch methods are defined.
	 * @param target the received of the synthetic methods.
	 */
	void appendSyntheticDispatchMethods(XtendTypeDeclaration source, final JvmGenericType target);

}
