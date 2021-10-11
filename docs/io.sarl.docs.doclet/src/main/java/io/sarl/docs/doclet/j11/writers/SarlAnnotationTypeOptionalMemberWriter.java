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

import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;

import jdk.javadoc.internal.doclets.formats.html.AnnotationTypeOptionalMemberWriterImpl;
import jdk.javadoc.internal.doclets.formats.html.Contents;
import jdk.javadoc.internal.doclets.formats.html.LinkInfoImpl;
import jdk.javadoc.internal.doclets.formats.html.SubWriterHolderWriter;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTag;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTree;
import jdk.javadoc.internal.doclets.formats.html.markup.StringContent;
import jdk.javadoc.internal.doclets.toolkit.Content;

import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Writer of annotation type's required members dedicated to the SARL doclet.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Apply the SARL syntax for the enum constant</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlAnnotationTypeOptionalMemberWriter extends AnnotationTypeOptionalMemberWriterImpl {

	private final SARLGrammarKeywordAccess sarlKeywords;

	/** Constructor.
	 *
	 * @param writer the writer for the class that the fields belong to.
	 * @param typeElement the type being documented.
	 * @param access accessor to the SARL keywords.
	 */
	public SarlAnnotationTypeOptionalMemberWriter(SubWriterHolderWriter writer, TypeElement typeElement, SARLGrammarKeywordAccess access) {
		super(writer, typeElement);
		this.sarlKeywords = access;
	}

	@Override
	public Content getSignature(Element member) {
		final Content pre = new HtmlTree(HtmlTag.PRE);
		this.writer.addAnnotationInfo(member, pre);
		addModifiers(member, pre);

		pre.addContent(this.sarlKeywords.getWriteableVarKeyword());
		pre.addContent(Contents.SPACE);

		final String name = name(member);
		if (this.configuration.linksource) {
			Content memberName = new StringContent(name);
			this.writer.addSrcLink(member, memberName, pre);
		} else {
			addName(name, pre);
		}

		pre.addContent(Contents.SPACE);
		pre.addContent(this.sarlKeywords.getColonKeyword());
		pre.addContent(Contents.SPACE);

		final Content link = this.writer.getLink(new LinkInfoImpl(this.configuration,
				LinkInfoImpl.Kind.MEMBER, getType(member)));
		pre.addContent(link);

		return pre;
	}

	private TypeMirror getType(Element member) {
		if (utils.isConstructor(member)) {
			return null;
		}
		if (utils.isExecutableElement(member)) {
			return utils.getReturnType((ExecutableElement)member);
		}
		return member.asType();
	}

}
