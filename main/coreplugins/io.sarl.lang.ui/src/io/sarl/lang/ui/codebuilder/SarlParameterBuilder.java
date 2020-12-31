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

import org.eclipse.xtend.ide.codebuilder.XtendParameterBuilder;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.typesystem.references.ArrayTypeReference;

import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Code builder for formal parameters.
 *
 * <p>This implementation generates the code according to the SARL syntax (not the Xtend syntax).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SarlParameterBuilder extends XtendParameterBuilder {

	@Inject
	private SARLGrammarKeywordAccess keywords;

	private String defaultValue;

	/** Replies the default value for the parameter.
	 *
	 * @return the default value, or {@code null}.
	 */
	public String getDefaultValue() {
		return this.defaultValue;
	}

	/** Change the default value for the parameter.
	 *
	 * @param defaultValue the default value, or {@code null}.
	 */
	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}

	@Override
	public ISourceAppender build(ISourceAppender appendable) {
		final ISourceAppender mAppender;
		if (appendable instanceof SourceAppenderWithTypeMapping) {
			mAppender = appendable;
		} else {
			mAppender = new SourceAppenderWithTypeMapping(appendable, this.keywords);
		}
		appendModifiers(mAppender);
		mAppender.append(this.keywords.protectKeyword(getName())).append(" "); //$NON-NLS-1$
		mAppender.append(this.keywords.getColonKeyword()).append(" "); //$NON-NLS-1$
		if (isVarArgsFlag()) {
			appendType(mAppender,
					((ArrayTypeReference) getType()).getComponentType(),
					Object.class.getSimpleName());
			mAppender.append(this.keywords.getWildcardAsteriskKeyword());
		} else  {
			appendType(mAppender, getType(), Object.class.getName());
			final String defaultVal = getDefaultValue();
			if (!Strings.isEmpty(defaultVal)) {
				mAppender.append(" ").append(this.keywords.getEqualsSignKeyword()); //$NON-NLS-1$
				mAppender.append(" ").append(defaultVal); //$NON-NLS-1$
			}
		}
		return mAppender;
	}

}
