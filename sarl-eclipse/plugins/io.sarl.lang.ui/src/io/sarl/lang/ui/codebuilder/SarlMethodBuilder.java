/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

import java.util.Collections;
import java.util.List;

import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.ide.codebuilder.XtendMethodBuilder;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmUpperBound;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

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
@SuppressWarnings("restriction")
public class SarlMethodBuilder extends XtendMethodBuilder {

	@Inject
	private SARLGrammarKeywordAccess keywords;

	@Inject
	private IDefaultVisibilityProvider visiblityProvider;

	@Inject
	private CommonTypeComputationServices services;

	private List<LightweightTypeReference> fires;

	/** Set the fired types.
	 *
	 * @param firedTypes the list of the fired types.
	 */
	public void setFires(List<LightweightTypeReference> firedTypes) {
		this.fires = firedTypes;
	}

	/** Replies the fired types.
	 *
	 * @return the list of the fired types.
	 */
	public List<LightweightTypeReference> getFires() {
		if (this.fires == null) {
			return Collections.emptyList();
		}
		return this.fires;
	}

	@Override
	public ISourceAppender build(ISourceAppender appendable) {
		final ISourceAppender mAppender;
		if (appendable instanceof SourceAppenderWithTypeMapping) {
			mAppender = appendable;
		} else {
			mAppender = new SourceAppenderWithTypeMapping(appendable, this.keywords);
		}
		final var defaultVisibility = this.visiblityProvider.getDefaultJvmVisibility(getOwner(),
				XtendPackage.eINSTANCE.getXtendFunction());
		appendVisibility(mAppender, getVisibility(), defaultVisibility);
		if (isStaticFlag()) {
			mAppender.append(this.keywords.getStaticStaticKeyword()).append(" "); //$NON-NLS-1$
		} else if (isAbstractFlag()) {
			mAppender.append(this.keywords.getAbstractKeyword()).append(" "); //$NON-NLS-1$
		}
		if (isOverrideFlag()) {
			mAppender.append(this.keywords.getOverrideKeyword());
		} else {
			mAppender.append(this.keywords.getDefKeyword());
		}
		mAppender.append(" "); //$NON-NLS-1$
		mAppender.append(this.keywords.protectKeyword(getMethodName()));
		appendParameters(mAppender);
		final var retType = getReturnType();
		if (retType != null && !retType.isPrimitiveVoid()) {
			mAppender.append(" "); //$NON-NLS-1$
			mAppender.append(this.keywords.getColonKeyword());
			mAppender.append(" "); //$NON-NLS-1$
			appendType(mAppender, retType, void.class.getSimpleName());
		}
		appendTypeParameters(mAppender, getTypeParameters());
		appendThrowsClause(mAppender);
		appendFiresClause(mAppender);
		if (!isAbstractFlag()) {
			appendBody(mAppender, ""); //$NON-NLS-1$
		}
		return mAppender;
	}

	/** Append the "fires" clause.
	 *
	 * @param appendable the receiver.
	 * @return the appendable.
	 */
	protected ISourceAppender appendFiresClause(ISourceAppender appendable) {
		final var types = getFires();
		final var iterator = types.iterator();
		if (iterator.hasNext()) {
			appendable.append(" ").append(this.keywords.getFiresKeyword()).append(" "); //$NON-NLS-1$ //$NON-NLS-2$
			do {
				final var type = iterator.next();
				appendable.append(type);
				if (iterator.hasNext()) {
					appendable.append(this.keywords.getCommaKeyword()).append(" "); //$NON-NLS-1$
				}
			} while (iterator.hasNext());
		}
		return appendable;
	}

	@Override
	protected ISourceAppender appendTypeParameters(ISourceAppender appendable, List<JvmTypeParameter> typeParameters) {
		final var iterator = typeParameters.iterator();
		if (iterator.hasNext()) {
			appendable.append(" ").append(this.keywords.getWithKeyword()).append(" "); //$NON-NLS-1$//$NON-NLS-2$
			final var objectId = Object.class.getName();
			do {
				final var typeParameter = iterator.next();
				appendable.append(this.keywords.protectKeyword(typeParameter.getName()));
				final var upperBounds =
					Iterables.filter(Iterables.filter(typeParameter.getConstraints(), JvmUpperBound.class),
						it -> !it.getTypeReference().getIdentifier().equals(objectId));
				final var iterator2 = upperBounds.iterator();
				if (iterator2.hasNext()) {
					appendable.append(" ").append(this.keywords.getExtendsKeyword()).append(" "); //$NON-NLS-1$ //$NON-NLS-2$
					var isFirst = true;
					final var owner = new StandardTypeReferenceOwner(this.services, getContext());
					for (final var upperBound: upperBounds) {
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
		final var builders = getParameterBuilders();
		if (builders.isEmpty()) {
			return appendable;
		}
		if (isAbstractFlag()) {
			((SarlParameterBuilder) builders.get(builders.size())).setDefaultValue(null);
		}
		return super.appendParameters(appendable);
	}

}
