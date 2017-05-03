/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.ui.codebuilder;

import java.util.List;

import javax.inject.Inject;

import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.ide.codebuilder.AbstractParameterBuilder;
import org.eclipse.xtend.ide.codebuilder.XtendMethodBuilder;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.jvmmodel.IDefaultVisibilityProvider;
import io.sarl.lang.services.SARLGrammarKeywordAccess;


/** Code builder for methods.
 *
 * <p>This implementation generates the code according to the SARL syntax (not the Xtend syntax).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SarlMethodBuilder extends XtendMethodBuilder {

	@Inject
	private SARLGrammarKeywordAccess keywords;

	@Inject
	private IDefaultVisibilityProvider visiblityProvider;

	@Override
	public ISourceAppender build(ISourceAppender appendable) {
		final JvmVisibility defaultVisibility = this.visiblityProvider.getDefaultJvmVisibility(getOwner(),
				XtendPackage.eINSTANCE.getXtendFunction());
		appendVisibility(appendable, getVisibility(), defaultVisibility);
		if (isStaticFlag()) {
			appendable.append(this.keywords.getStaticStaticKeyword()).append(" "); //$NON-NLS-1$
		} else if (isAbstractFlag()) {
			appendable.append(this.keywords.getAbstractKeyword()).append(" "); //$NON-NLS-1$
		}
		if (isOverrideFlag()) {
			appendable.append(this.keywords.getOverrideKeyword());
		} else {
			appendable.append(this.keywords.getDefKeyword());
		}
		appendable.append(" "); //$NON-NLS-1$
		appendable.append(this.keywords.protectKeyword(getMethodName()));
		appendParameters(appendable);
		final LightweightTypeReference retType = getReturnType();
		if (!retType.isPrimitiveVoid()) {
			appendable.append(" "); //$NON-NLS-1$
			appendable.append(this.keywords.getColonKeyword());
			appendable.append(" "); //$NON-NLS-1$
			appendType(appendable, retType, void.class.getSimpleName());
		}
		appendThrowsClause(appendable);
		if (!isAbstractFlag()) {
			appendBody(appendable, ""); //$NON-NLS-1$
		}
		return appendable;
	}

	@Override
	protected ISourceAppender appendParameters(ISourceAppender appendable) {
		final List<AbstractParameterBuilder> builders = getParameterBuilders();
		if (builders.isEmpty()) {
			return appendable;
		}
		if (isAbstractFlag()) {
			((SarlParameterBuilder) builders.get(builders.size())).setDefaultValue(null);
		}
		return super.appendParameters(appendable);
	}

}
