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

package io.sarl.docs.doclet.j11.writers;

import static jdk.javadoc.internal.doclets.formats.html.LinkInfoImpl.Kind.MEMBER;

import java.util.Iterator;
import java.util.List;

import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeMirror;

import com.google.common.collect.Iterables;
import jdk.javadoc.internal.doclets.formats.html.Contents;
import jdk.javadoc.internal.doclets.formats.html.LinkInfoImpl;
import jdk.javadoc.internal.doclets.formats.html.SubWriterHolderWriter;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlStyle;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTag;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTree;
import jdk.javadoc.internal.doclets.formats.html.markup.StringContent;
import jdk.javadoc.internal.doclets.toolkit.Content;

import io.sarl.docs.doclet.j11.bugfixes.FixedMethodWriterImpl;
import io.sarl.docs.doclet.j11.utils.GeneralUtils;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.FiredEvent;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Writer of methods dedicated to the SARL doclet.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Apply the SARL syntax for the method signature</li>
 * <li>Ensure that modifiers are valid from SARL specification point of view</li>
 * <li>Add the marks {@code []} around the optional formal parameters</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlMethodWriter extends FixedMethodWriterImpl {

	private final SARLGrammarKeywordAccess sarlKeywords;

	/** Constructor.
	 *
	 * @param writer the writer for the class that the methods belong to.
	 * @param typeElement the type being documented.
	 * @param access accessor to the SARL keywords.
	 */
	public SarlMethodWriter(SubWriterHolderWriter writer, TypeElement typeElement, SARLGrammarKeywordAccess access) {
		super(writer, typeElement);
		this.sarlKeywords = access;
	}

	@Override
	public Content getSignature(ExecutableElement method) {
		HtmlTree pre = new HtmlTree(HtmlTag.PRE);
		pre.setStyle(HtmlStyle.methodSignature);

		// Hidden annotations are removed because they are not documented with @Documented.
		this.writer.addAnnotationInfo(method, pre);
		int annotationLength = pre.charCount();

		addModifiers(method, pre);

		pre.addContent(this.sarlKeywords.getDefKeyword());
		pre.addContent(Contents.SPACE);

		final String name = name(method);
		if (this.configuration.linksource) {
			Content methodName = new StringContent(name);
			this.writer.addSrcLink(method, methodName, pre);
		} else {
			addName(name, pre);
		}

		int indent = pre.charCount() - annotationLength;
		
		final List<? extends VariableElement> parameters = method.getParameters();
		if (!parameters.isEmpty()) {
			addParameters(method, pre, indent);
		}
		
		final TypeMirror type = this.utils.getReturnType(method);
        if (type != null) {
    		pre.addContent(Contents.SPACE);
    		pre.addContent(this.sarlKeywords.getColonKeyword());
    		pre.addContent(Contents.SPACE);
    		addReturnType(method, pre);
        }

        final Content typeParameters = getTypeParameters(method);
        if (!typeParameters.isEmpty()) {
    		pre.addContent(Contents.SPACE);
    		pre.addContent(this.sarlKeywords.getWithKeyword());
    		pre.addContent(Contents.SPACE);
        	addTypeParameters(method, pre);
        }

        final List<? extends TypeMirror> exceptions = method.getThrownTypes();
        if (!exceptions.isEmpty()) {
    		pre.addContent(Contents.SPACE);
    		pre.addContent(this.sarlKeywords.getThrowsKeyword());
    		pre.addContent(Contents.SPACE);
    		addExceptions(method, pre, indent);
        }

        final Iterable<? extends AnnotationMirror> fires = Iterables.filter(method.getAnnotationMirrors(),
        		it -> {
        			return it.getAnnotationType().getAnnotation(FiredEvent.class) != null;
        		});
        final Iterator<? extends AnnotationMirror> firesIt = fires.iterator();
        if (firesIt.hasNext()) {
    		pre.addContent(Contents.SPACE);
    		pre.addContent(this.sarlKeywords.getFiresKeyword());
    		pre.addContent(Contents.SPACE);
    		boolean first = true;
    		while (firesIt.hasNext()) {
    			final AnnotationMirror annotationMirror = firesIt.next();
    			final FiredEvent annotation = annotationMirror.getAnnotationType().getAnnotation(FiredEvent.class);
    			if (annotation != null) {
    				if (first) {
    					first = false;
    				} else {
    					pre.addContent(", ");
    				}
                    final Content link = writer.getLink(new LinkInfoImpl(this.configuration, MEMBER, annotationMirror.getAnnotationType()));
                    pre.addContent(link);
    			}
    		}
        }

		return pre;
	}

	@Override
	protected void addModifiers(Element member, Content htmltree) {
		GeneralUtils.addMethodModifiers(member, htmltree, this.writer, this.utils, this.sarlKeywords);
	}

    @Override
    protected void addParam(ExecutableElement member, VariableElement param, boolean isVarArg, Content tree) {
        GeneralUtils.addParam(member, param, name(param),
        		param.getAnnotation(DefaultValue.class) != null,
        		isVarArg, tree,
        		this.writer, this.configuration, this.sarlKeywords);
    }
	
}
