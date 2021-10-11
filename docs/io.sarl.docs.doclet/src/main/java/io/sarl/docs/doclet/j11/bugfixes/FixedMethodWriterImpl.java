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

package io.sarl.docs.doclet.j11.bugfixes;

import java.util.List;

import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeMirror;

import jdk.javadoc.internal.doclets.formats.html.Contents;
import jdk.javadoc.internal.doclets.formats.html.MethodWriterImpl;
import jdk.javadoc.internal.doclets.formats.html.SubWriterHolderWriter;
import jdk.javadoc.internal.doclets.formats.html.markup.ContentBuilder;
import jdk.javadoc.internal.doclets.toolkit.Content;
import jdk.javadoc.internal.doclets.toolkit.util.DocletConstants;

/** Provide bug fixes for the method writer.
 * 
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>In the standard API, the final ")" of a parameter list is not output</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public abstract class FixedMethodWriterImpl extends MethodWriterImpl {

	/** Constructor.
	 *
	 * @param writer the writer for the class that the methods belong to.
	 * @param typeElement the type being documented.
	 */
	public FixedMethodWriterImpl(SubWriterHolderWriter writer, TypeElement typeElement) {
		super(writer, typeElement);
	}

	@Override
    protected void addParameters(ExecutableElement member,
            boolean includeAnnotations, Content htmltree, int indentSize) {
        Content paramTree = new ContentBuilder();
        String sep = "";
        List<? extends VariableElement> parameters = member.getParameters();
        CharSequence indent = makeSpace(indentSize + 1);
        TypeMirror rcvrType = member.getReceiverType();
        if (includeAnnotations && rcvrType != null && utils.isAnnotated(rcvrType)) {
            List<? extends AnnotationMirror> annotationMirrors = rcvrType.getAnnotationMirrors();
            addReceiverAnnotations(member, rcvrType, annotationMirrors, paramTree);
            sep = "," + DocletConstants.NL + indent;
        }
        int paramstart;
        for (paramstart = 0; paramstart < parameters.size(); paramstart++) {
            paramTree.addContent(sep);
            VariableElement param = parameters.get(paramstart);

            if (param.getKind() != ElementKind.INSTANCE_INIT) {
                if (includeAnnotations) {
                    boolean foundAnnotations =
                            writer.addAnnotationInfo(indent.length(),
                            member, param, paramTree);
                    if (foundAnnotations) {
                        paramTree.addContent(DocletConstants.NL);
                        paramTree.addContent(indent);
                    }
                }
                addParam(member, param,
                    (paramstart == parameters.size() - 1) && member.isVarArgs(), paramTree);
                break;
            }
        }

        for (int i = paramstart + 1; i < parameters.size(); i++) {
            paramTree.addContent(",");
            paramTree.addContent(DocletConstants.NL);
            paramTree.addContent(indent);
            if (includeAnnotations) {
                boolean foundAnnotations =
                        writer.addAnnotationInfo(indent.length(), member, parameters.get(i),
                        paramTree);
                if (foundAnnotations) {
                    paramTree.addContent(DocletConstants.NL);
                    paramTree.addContent(indent);
                }
            }
            addParam(member, parameters.get(i), (i == parameters.size() - 1) && member.isVarArgs(),
                    paramTree);
        }
        if (paramTree.isEmpty()) {
            htmltree.addContent("()");
        } else {
            htmltree.addContent(Contents.ZERO_WIDTH_SPACE);
            htmltree.addContent("(");
            htmltree.addContent(paramTree);
            // TODO: The following line is fixing a bug into the JDK API.
            htmltree.addContent(")");
        }
    }

}
