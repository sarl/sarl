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

package io.sarl.lang.ui.codebuilder;

import javax.inject.Inject;

import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.ide.codebuilder.XtendFieldBuilder;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;

import io.sarl.lang.jvmmodel.IDefaultVisibilityProvider;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Code builder for fields.
 *
 * <p>This implementation generates the code according to the SARL syntax (not the Xtend syntax).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SarlFieldBuilder extends XtendFieldBuilder {

	@Inject
	private SARLGrammarKeywordAccess keywords;

	@Inject
	private IDefaultVisibilityProvider visiblityProvider;

	@Override
	public ISourceAppender build(ISourceAppender appendable) {
		final ISourceAppender mAppender;
		if (appendable instanceof SourceAppenderWithTypeMapping) {
			mAppender = appendable;
		} else {
			mAppender = new SourceAppenderWithTypeMapping(appendable, this.keywords);
		}
		final JvmVisibility defaultVisibility = this.visiblityProvider.getDefaultJvmVisibility(getOwner(),
				XtendPackage.eINSTANCE.getXtendField());
		appendVisibility(mAppender, getVisibility(), defaultVisibility);
		if (isStaticFlag()) {
			mAppender.append(this.keywords.getStaticStaticKeyword()).append(" "); //$NON-NLS-1$
		}
		mAppender.append(this.keywords.protectKeyword(getFieldName()));
		mAppender.append(" ").append(this.keywords.getColonKeyword()).append(" "); //$NON-NLS-1$ //$NON-NLS-2$
		appendType(mAppender, getFieldType(), Object.class.getName());
		return mAppender;
	}

}
