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

package io.sarl.lang.compiler;

import com.google.inject.ImplementedBy;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.lib.Inline;


/** Compiler for creating inline expressions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.4
 * @see Inline
 */
@ImplementedBy(JavaInlineExpressionCompiler.class)
public interface IInlineExpressionCompiler {

	/** Append the inline annotation to the given operation.
	 *
	 * @param target the target of the annotation.
	 * @param resourcetSet the resource set that is associated to the given operation.
	 * @param inlineExpression the inline expression.
	 * @param types the types to import if the inline expression is used. The references are cloned by this function.
	 */
	void appendInlineAnnotation(JvmAnnotationTarget target, ResourceSet resourcetSet,
			String inlineExpression, JvmTypeReference... types);

	/** Append the inline annotation to the given operation.
	 *
	 * @param target the target of the annotation.
	 * @param source the feature for which the inline annotation should be added.
	 */
	void appendInlineAnnotation(JvmAnnotationTarget target, XtendExecutable source);

}
