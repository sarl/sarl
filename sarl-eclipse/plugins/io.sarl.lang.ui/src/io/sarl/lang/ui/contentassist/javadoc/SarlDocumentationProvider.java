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

package io.sarl.lang.ui.contentassist.javadoc;

import java.text.MessageFormat;
import java.util.List;

import com.google.inject.Singleton;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.util.Utils;

/** Provides SARL documentation for JVM elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@Singleton
@SuppressWarnings("restriction")
public class SarlDocumentationProvider extends AbstractSarlDocumentationProvider {

	@Override
	protected String getDocumentationStart() {
		return "/** " + Messages.SarlDocumentationProvider_0; //$NON-NLS-1$
	}

	@Override
	protected String getDocumentationEnd() {
		return " */"; //$NON-NLS-1$
	}

	@Override
	protected String getDocumentationLinePrefix() {
		return " * "; //$NON-NLS-1$
	}

	@Override
	public boolean isValidElement(EObject element) {
		return element instanceof XtendConstructor
				|| element instanceof XtendFunction
				|| element instanceof XtendField
				|| element instanceof XtendTypeDeclaration
				|| element instanceof SarlBehaviorUnit;
	}

	/** Generate the documentation for the given constructor.
	 *
	 * @param object the element for which a documentation must be generated.
	 * @param it the receiver of the documentation..
	 */
	protected void _generate(XtendConstructor object, ISourceAppender it) {
		generateFirstBlock(it, object);
		generateLastBlock(it, object);
	}

	/** Generate the documentation for the given field.
	 *
	 * @param object the element for which a documentation must be generated.
	 * @param it the receiver of the documentation..
	 */
	protected void _generate(XtendField object, ISourceAppender it) {
		if (isDeprecated(object)) {
			generateDeprecated(it);
		}
	}

	/** Generate the documentation for the given type.
	 *
	 * @param object the element for which a documentation must be generated.
	 * @param it the receiver of the documentation..
	 */
	protected void _generate(XtendTypeDeclaration object, ISourceAppender it) {
		it.append("@author ").append(Messages.SarlDocumentationProvider_1).newLine(); //$NON-NLS-1$
		it.append("@version ").append(Messages.SarlDocumentationProvider_2).newLine(); //$NON-NLS-1$
		if (isDeprecated(object)) {
			generateDeprecated(it);
		}
	}

	/** Generate the documentation for the given SARL behavioral unit.
	 *
	 * @param object the element for which a documentation must be generated.
	 * @param it the receiver of the documentation..
	 */
	protected void _generate(SarlBehaviorUnit object, ISourceAppender it) {
		//
	}

	/** Generate the documentation for the given operation.
	 *
	 * @param object the element for which a documentation must be generated.
	 * @param it the receiver of the documentation..
	 */
	protected void _generate(XtendFunction object, ISourceAppender it) {
		generateFirstBlock(it, object);
		final var returnType = getReturnType(object);
		if (returnType != null && !returnType.isPrimitiveVoid()) {
			generateReturnType(it, returnType);
		}
		generateLastBlock(it, object);
	}

	private static void generateFirstBlock(ISourceAppender it, XtendExecutable executable) {
		generateGenericTypes(it, executable.getTypeParameters());
		for (final var parameter : executable.getParameters()) {
			generateFormalParameters(it, parameter);
		}
	}

	private void generateLastBlock(ISourceAppender it, XtendExecutable executable) {
		for (final var exception : executable.getExceptions()) {
			generateException(it, exception);
		}
		if (isDeprecated(executable)) {
			generateDeprecated(it);
		}
	}

	private static void generateGenericTypes(ISourceAppender it, List<JvmTypeParameter> types) {
		for (final var genericType : types) {
			it.append("@param <").append(genericType.getSimpleName()).append("> ") //$NON-NLS-1$ //$NON-NLS-2$
				.append(Messages.SarlDocumentationProvider_3).newLine();
		}
	}

	private static void generateFormalParameters(ISourceAppender it, XtendParameter parameter) {
		it.append("@param ").append(parameter.getName()).append(" - ").append(Messages.SarlDocumentationProvider_4); //$NON-NLS-1$ //$NON-NLS-2$
		if (parameter instanceof SarlFormalParameter sarlParameter) {
			if (sarlParameter.getDefaultValue() != null) {
				it.append(" ").append(MessageFormat.format(Messages.SarlDocumentationProvider_5, Utils.getSarlCodeFor(sarlParameter.getDefaultValue()))); //$NON-NLS-1$
			}
		}
		it.newLine();
	}

	private static void generateReturnType(ISourceAppender it, LightweightTypeReference returnType) {
		it.append("@return ").append(Messages.SarlDocumentationProvider_6).newLine(); //$NON-NLS-1$
	}

	private static void generateException(ISourceAppender it, JvmTypeReference exception) {
		it.append("@throws ").append(exception.getSimpleName()).append(" ").append(Messages.SarlDocumentationProvider_7).newLine(); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static void generateDeprecated(ISourceAppender it) {
		it.append("@deprecated ").append(Messages.SarlDocumentationProvider_8); //$NON-NLS-1$
	}

}
