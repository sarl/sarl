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

package io.sarl.lang.ui.labeling;

import java.util.Iterator;

import javax.inject.Inject;

import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericArrayTypeReference;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.validation.UIStrings;

import io.sarl.lang.services.SARLGrammarKeywordAccess;

/**
 * UI Strings.
 *
 * <p>This class extends the standard Xtend serializer by replacing the example of code, written
 * in Java or Xtend, by the same example with the SARL syntax.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
public class SARLUIStrings extends UIStrings {

	private static final String NULL_TYPE = "[null]"; //$NON-NLS-1$

	@Inject
	private SARLGrammarKeywordAccess keywords;

	@Override
	protected String parametersToString(Iterable<? extends JvmFormalParameter> elements, boolean isVarArgs, boolean includeName) {
		return getParameterString(elements, isVarArgs, includeName, this.keywords, this);
	}

	/** Format the parameters.
	 *
	 * @param elements the list of the formal parameters.
	 * @param isVarArgs indicates if the function has var args.
	 * @param includeName indicates if the names are included in the replied valued.
	 * @param keywords the keyword provider.
	 * @param utils the utility.
	 * @return the string representation of the parameters.
	 */
	public static String getParameterString(Iterable<? extends JvmFormalParameter> elements, boolean isVarArgs,
			boolean includeName, SARLGrammarKeywordAccess keywords, UIStrings utils) {
		final StringBuilder result = new StringBuilder();
		boolean needsSeparator = false;
		final Iterator<? extends JvmFormalParameter> iterator = elements.iterator();
		while (iterator.hasNext()) {
			final JvmFormalParameter parameter = iterator.next();
			if (needsSeparator) {
				result.append(keywords.getCommaKeyword()).append(" "); //$NON-NLS-1$
			}
			needsSeparator = true;
			if (includeName) {
				result.append(parameter.getName()).append(" ");  //$NON-NLS-1$
				result.append(keywords.getColonKeyword()).append(" "); //$NON-NLS-1$
			}
			JvmTypeReference typeRef = parameter.getParameterType();
			if (isVarArgs && !iterator.hasNext() && typeRef instanceof JvmGenericArrayTypeReference) {
				typeRef = ((JvmGenericArrayTypeReference) typeRef).getComponentType();
				result.append(utils.referenceToString(typeRef, NULL_TYPE));
				result.append(keywords.getWildcardAsteriskKeyword());
			} else {
				result.append(utils.referenceToString(typeRef, NULL_TYPE));
			}
		}
		return result.toString();
	}

}
