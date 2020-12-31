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

package io.sarl.lang.ui.validation;

import java.util.Iterator;
import javax.inject.Inject;
import javax.inject.Singleton;

import org.eclipse.jface.preference.JFacePreferences;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.StyledString.Styler;
import org.eclipse.swt.graphics.TextStyle;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericArrayTypeReference;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.xbase.validation.UIStrings;

import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/**
 * UI Strings.
 *
 * <p>This class extends the standard XBase tool by replacing the example of code, written
 * in Java or Xtend, by the same example in the SARL syntax.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
@Singleton
public class SARLUIStrings extends UIStrings {

	/**
	 * A built-in styler for optional formal parameters.
	 *
	 * @since 0.6
	 */
	public static final Styler OPTIONAL_ELEMENT_STYLER =  new Styler() {
		@Override
		public void applyStyles(TextStyle textStyle) {
			textStyle.foreground = JFaceResources.getColorRegistry().get(JFacePreferences.QUALIFIER_COLOR);
		}
	};

	private static final String NULL_TYPE = "[null]"; //$NON-NLS-1$

	@Inject
	private SARLGrammarKeywordAccess keywords;

	@Inject
	private AnnotationLookup annotationFinder;

	/** Replies the styled parameters.
	 *
	 * @param element the element from which the parameters are extracted.
	 * @return the styled parameters
	 * @since 0.6
	 */
	public StyledString styledParameters(JvmIdentifiableElement element) {
		final StyledString str = new StyledString();
		if (element instanceof JvmExecutable) {
			final JvmExecutable executable = (JvmExecutable) element;
			str.append(this.keywords.getLeftParenthesisKeyword());
			str.append(parametersToStyledString(
					executable.getParameters(),
					executable.isVarArgs(),
					false));
			str.append(this.keywords.getRightParenthesisKeyword());
		}
		return str;
	}

	@Override
	protected String parametersToString(Iterable<? extends JvmFormalParameter> elements, boolean isVarArgs, boolean includeName) {
		return getParameterString(elements, isVarArgs, includeName, this.keywords, this.annotationFinder, this);
	}

	/** Replies the styled string representation of the parameters.
	 *
	 * @param elements the parameters.
	 * @param isVarArgs indicates if the last parameter is variadic.
	 * @param includeName indicates if the names are included.
	 * @return the styled string.
	 * @since 0.6
	 */
	protected StyledString parametersToStyledString(Iterable<? extends JvmFormalParameter> elements, boolean isVarArgs, boolean includeName) {
		return getParameterStyledString(elements, isVarArgs, includeName, this.keywords, this.annotationFinder, this);
	}

	/** Format the parameters.
	 *
	 * @param elements the list of the formal parameters.
	 * @param isVarArgs indicates if the function has var args.
	 * @param includeName indicates if the names are included in the replied valued.
	 * @param keywords the keyword provider.
	 * @param annotationFinder the finder of attached annotations.
	 * @param utils the utility.
	 * @return the string representation of the parameters.
	 */
	public static String getParameterString(Iterable<? extends JvmFormalParameter> elements, boolean isVarArgs,
			boolean includeName, SARLGrammarKeywordAccess keywords, AnnotationLookup annotationFinder, UIStrings utils) {
		final StringBuilder result = new StringBuilder();
		boolean needsSeparator = false;
		final Iterator<? extends JvmFormalParameter> iterator = elements.iterator();
		while (iterator.hasNext()) {
			final JvmFormalParameter parameter = iterator.next();
			if (needsSeparator) {
				result.append(keywords.getCommaKeyword()).append(" "); //$NON-NLS-1$
			}
			needsSeparator = true;
			final boolean isDefaultValued = annotationFinder.findAnnotation(parameter, DefaultValue.class) != null;
			if (isDefaultValued) {
				result.append(keywords.getLeftSquareBracketKeyword());
			}
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
			if (isDefaultValued) {
				result.append(keywords.getRightSquareBracketKeyword());
			}
		}
		return result.toString();
	}

	/** Format the parameters.
	 *
	 * @param elements the list of the formal parameters.
	 * @param isVarArgs indicates if the function has var args.
	 * @param includeName indicates if the names are included in the replied valued.
	 * @param keywords the keyword provider.
	 * @param annotationFinder the finder of attached annotations.
	 * @param utils the utility.
	 * @return the string representation of the parameters.
	 * @since 0.6
	 */
	public static StyledString getParameterStyledString(Iterable<? extends JvmFormalParameter> elements, boolean isVarArgs,
			boolean includeName, SARLGrammarKeywordAccess keywords, AnnotationLookup annotationFinder, UIStrings utils) {
		final StyledString result = new StyledString();
		boolean needsSeparator = false;
		final Iterator<? extends JvmFormalParameter> iterator = elements.iterator();
		while (iterator.hasNext()) {
			final JvmFormalParameter parameter = iterator.next();
			if (needsSeparator) {
				result.append(keywords.getCommaKeyword()).append(" "); //$NON-NLS-1$
			}
			needsSeparator = true;
			final boolean isDefaultValued = annotationFinder.findAnnotation(parameter, DefaultValue.class) != null;
			final Styler styler;
			if (isDefaultValued) {
				styler = OPTIONAL_ELEMENT_STYLER;
				result.append(keywords.getLeftSquareBracketKeyword(), styler);
			} else {
				styler = null;
			}
			if (includeName) {
				result.append(parameter.getName(), styler).append(" ", styler);  //$NON-NLS-1$
				result.append(keywords.getColonKeyword(), styler).append(" ", styler); //$NON-NLS-1$
			}
			JvmTypeReference typeRef = parameter.getParameterType();
			if (isVarArgs && !iterator.hasNext() && typeRef instanceof JvmGenericArrayTypeReference) {
				typeRef = ((JvmGenericArrayTypeReference) typeRef).getComponentType();
				result.append(utils.referenceToString(typeRef, NULL_TYPE), styler);
				result.append(keywords.getWildcardAsteriskKeyword(), styler);
			} else {
				result.append(utils.referenceToString(typeRef, NULL_TYPE), styler);
			}
			if (isDefaultValued) {
				result.append(keywords.getRightSquareBracketKeyword(), styler);
			}
		}
		return result;
	}

}
