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

package io.sarl.docs.doclet.j11.utils;

import java.util.Iterator;

import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeMirror;

import jdk.javadoc.internal.doclets.toolkit.BaseConfiguration;
import jdk.javadoc.internal.doclets.toolkit.util.Utils;

import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Re-implementation of utilities for SARL.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Replace the Java syntax for variadic arguments {@code ...} by its SARL equivalent</li>
 * <li>Add the {@code []} marks around each optional formal parameter in order to make their optionallity explicit.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlJavadocUtils extends Utils {

	private final SARLGrammarKeywordAccess sarlKeywords;
	
	/** Constructor.
	 *
	 * @param configuration the configuration.
	 * @param sarlKeywords the accessor to the SARL keywords.
	 */
	public SarlJavadocUtils(BaseConfiguration configuration, SARLGrammarKeywordAccess sarlKeywords) {
		super(configuration);
		this.sarlKeywords = sarlKeywords;
	}

	@Override
	public String makeSignature(ExecutableElement e, boolean full, boolean ignoreTypeParameters) {
        StringBuilder result = new StringBuilder();
        result.append(this.sarlKeywords.getLeftParenthesisKeyword());
        Iterator<? extends VariableElement> iterator = e.getParameters().iterator();
        boolean first = true;
        boolean isLastOptional = false;
        while (iterator.hasNext()) {
        	if (first) {
        		first = false;
        	} else {
            	result.append(this.sarlKeywords.getCommaKeyword());
                result.append(" ");
        	}
            VariableElement next = iterator.next();
            isLastOptional = next.getAnnotation(DefaultValue.class) != null;
            TypeMirror type = next.asType();
            String sig = getTypeSignature(type, full, ignoreTypeParameters);
            if (isLastOptional) {
            	sig = GeneralUtils.getText(GeneralUtils.DOCLET_OPTIONAL_FORMAL_PARAMETER, this.configuration.getLocale(), sig);
            }
        	result.append(sig);
        }
        if (!isLastOptional && e.isVarArgs()) {
            int len = result.length();
            result.replace(len - 2, len, this.sarlKeywords.getWildcardAsteriskKeyword());
        }
        result.append(this.sarlKeywords.getRightParenthesisKeyword());
        return result.toString();
	}

}
