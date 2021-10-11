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
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;

import jdk.javadoc.internal.doclets.formats.html.Contents;
import jdk.javadoc.internal.doclets.formats.html.FieldWriterImpl;
import jdk.javadoc.internal.doclets.formats.html.LinkInfoImpl;
import jdk.javadoc.internal.doclets.formats.html.SubWriterHolderWriter;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTag;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTree;
import jdk.javadoc.internal.doclets.formats.html.markup.StringContent;
import jdk.javadoc.internal.doclets.toolkit.Content;

import io.sarl.docs.doclet.j11.utils.GeneralUtils;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Writer of fields dedicated to the SARL doclet.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Apply the SARL syntax for the field signature</li>
 * <li>Ensure that modifiers are valid from SARL specification point of view</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlFieldWriter extends FieldWriterImpl {

	private final SARLGrammarKeywordAccess sarlKeywords;

	/** Constructor.
	 *
	 * @param writer the writer for the class that the methods belong to.
	 * @param typeElement the type being documented.
	 * @param access accessor to the SARL keywords.
	 */
	public SarlFieldWriter(SubWriterHolderWriter writer, TypeElement typeElement, SARLGrammarKeywordAccess access) {
		super(writer, typeElement);
		this.sarlKeywords = access;
	}
	
	/** Apply the SARL syntax to the output of the field.
	 */
	@Override
	public Content getSignature(VariableElement field) {
		final Content pre = new HtmlTree(HtmlTag.PRE);

		this.writer.addAnnotationInfo(field, pre);

		addModifiers(field, pre);

		if ((field.getModifiers().contains(Modifier.FINAL))) {
			pre.addContent(this.sarlKeywords.getValKeyword());
		} else {
			pre.addContent(this.sarlKeywords.getWriteableVarKeyword());
		}
		pre.addContent(Contents.SPACE);

		if (this.configuration.linksource) {
			Content fieldName = new StringContent(name(field));
			this.writer.addSrcLink(field, fieldName, pre);
		} else {
			addName(name(field), pre);
		}

		pre.addContent(Contents.SPACE);
		pre.addContent(this.sarlKeywords.getColonKeyword());
		pre.addContent(Contents.SPACE);

		final Content fieldlink = this.writer.getLink(new LinkInfoImpl(
				this.configuration, LinkInfoImpl.Kind.MEMBER, field.asType()));
		pre.addContent(fieldlink);
		return pre;
	}

	/** Do not show up the "final" modifier; and add "package" modifier.
	 */
	@Override
	protected void addModifiers(Element member, Content htmltree) {
		GeneralUtils.addFieldModifiers(member, htmltree, this.writer, this.utils, this.sarlKeywords);
	}

}
