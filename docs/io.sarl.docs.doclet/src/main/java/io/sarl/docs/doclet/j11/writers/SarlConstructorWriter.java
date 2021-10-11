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

import java.util.List;

import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeMirror;

import jdk.javadoc.internal.doclets.formats.html.Contents;
import jdk.javadoc.internal.doclets.formats.html.LinkInfoImpl;
import jdk.javadoc.internal.doclets.formats.html.SubWriterHolderWriter;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlStyle;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTag;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTree;
import jdk.javadoc.internal.doclets.formats.html.markup.StringContent;
import jdk.javadoc.internal.doclets.toolkit.Content;

import io.sarl.docs.doclet.j11.bugfixes.FixedConstructorWriterImpl;
import io.sarl.docs.doclet.j11.utils.GeneralUtils;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Writer of constructors dedicated to the SARL doclet.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Apply the SARL syntax for the constructor signature</li>
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
public class SarlConstructorWriter extends FixedConstructorWriterImpl {

	private final SARLGrammarKeywordAccess sarlKeywords;

	/** Constructor.
	 *
	 * @param writer the writer for the class that the methods belong to.
	 * @param typeElement the type being documented.
	 * @param access accessor to the SARL keywords.
	 */
	public SarlConstructorWriter(SubWriterHolderWriter writer, TypeElement typeElement, SARLGrammarKeywordAccess access) {
		super(writer, typeElement);
		this.sarlKeywords = access;
	}

	@Override
	public Content getSignature(ExecutableElement constructor) {
		final Content pre = new HtmlTree(HtmlTag.PRE);

		this.writer.addAnnotationInfo(constructor, pre);
		int annotationLength = pre.charCount();

		addModifiers(constructor, pre);

		final String name = this.sarlKeywords.getNewKeyword();
		if (this.configuration.linksource) {
			final Content constructorName = new StringContent(name);
			this.writer.addSrcLink(constructor, constructorName, pre);
		} else {
			addName(name, pre);
		}

		int indent = pre.charCount() - annotationLength;

		final List<? extends VariableElement> parameters = constructor.getParameters();
		if (!parameters.isEmpty()) {
			addParameters(constructor, pre, indent);
		}

		final List<? extends TypeMirror> exceptions = constructor.getThrownTypes();
		if (!exceptions.isEmpty()) {
			pre.addContent(Contents.SPACE);
			pre.addContent(this.sarlKeywords.getThrowsKeyword());
			pre.addContent(Contents.SPACE);
			addExceptions(constructor, pre, indent);
		}

		return pre;
	}

	@Override
	protected void addModifiers(Element member, Content htmltree) {
		GeneralUtils.addConstructorModifiers(member, htmltree, this.writer, this.utils, this.sarlKeywords);
	}

	@Override
	protected void addParam(ExecutableElement member, VariableElement param, boolean isVarArg, Content tree) {
		GeneralUtils.addParam(member, param, name(param),
        		param.getAnnotation(DefaultValue.class) != null,
				isVarArg, tree,
				this.writer, this.configuration, this.sarlKeywords);
	}

	@Override
	protected void addSummaryLink(LinkInfoImpl.Kind context,
            TypeElement typeElement, Element member, Content tdSummary) {
        final ExecutableElement executableElement = (ExecutableElement) member;
        final String name = this.sarlKeywords.getNewKeyword();
        final Content memberLink = HtmlTree.SPAN(HtmlStyle.memberNameLink,
                this.writer.getDocLink(context, typeElement, executableElement,
                name, false));
        final Content code = HtmlTree.CODE(memberLink);
        addParameters(executableElement, false, code, name.length() - 1);
        tdSummary.addContent(code);
	}

}
