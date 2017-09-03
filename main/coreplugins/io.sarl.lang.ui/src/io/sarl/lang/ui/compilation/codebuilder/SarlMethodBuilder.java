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

package io.sarl.lang.ui.compilation.codebuilder;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.inject.Inject;

import com.google.common.collect.Iterables;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.ide.codebuilder.AbstractParameterBuilder;
import org.eclipse.xtend.ide.codebuilder.XtendMethodBuilder;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmUpperBound;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import io.sarl.lang.compilation.jvmmodel.IDefaultVisibilityProvider;
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

	@Inject
	private CommonTypeComputationServices services;

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
		if (retType != null && !retType.isPrimitiveVoid()) {
			appendable.append(" "); //$NON-NLS-1$
			appendable.append(this.keywords.getColonKeyword());
			appendable.append(" "); //$NON-NLS-1$
			appendType(appendable, retType, void.class.getSimpleName());
		}
		appendTypeParameters(appendable, getTypeParameters());
		appendThrowsClause(appendable);
		if (!isAbstractFlag()) {
			appendBody(appendable, ""); //$NON-NLS-1$
		}
		return appendable;
	}

	@Override
	protected ISourceAppender appendTypeParameters(ISourceAppender appendable, List<JvmTypeParameter> typeParameters) {
		final Iterator<JvmTypeParameter> iterator = typeParameters.iterator();
		if (iterator.hasNext()) {
			appendable.append(" ").append(this.keywords.getWithKeyword()).append(" "); //$NON-NLS-1$//$NON-NLS-2$
			final String objectId = Object.class.getName();
			do {
				final JvmTypeParameter typeParameter = iterator.next();
				appendable.append(this.keywords.protectKeyword(typeParameter.getName()));
				final Iterable<JvmUpperBound> upperBounds =
						Iterables.filter(Iterables.filter(typeParameter.getConstraints(), JvmUpperBound.class),
						(it) -> !it.getTypeReference().getIdentifier().equals(objectId));
				final Iterator<JvmUpperBound> iterator2 = upperBounds.iterator();
				if (iterator2.hasNext()) {
					appendable.append(" ").append(this.keywords.getExtendsKeyword()).append(" "); //$NON-NLS-1$ //$NON-NLS-2$
					boolean isFirst = true;
					final StandardTypeReferenceOwner owner = new StandardTypeReferenceOwner(this.services, getContext());
					for (final JvmUpperBound upperBound: upperBounds) {
						if (!isFirst) {
							appendable.append(" ").append(this.keywords.getAmpersandKeyword()).append(" "); //$NON-NLS-1$ //$NON-NLS-2$
						}
						isFirst = false;
						appendType(appendable,
								owner.toLightweightTypeReference(upperBound.getTypeReference()),
								Object.class.getSimpleName());
					}
				}
				if (iterator.hasNext()) {
					appendable.append(this.keywords.getCommaKeyword()).append(" "); //$NON-NLS-1$
				}
			} while (iterator.hasNext());
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
