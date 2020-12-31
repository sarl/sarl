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

package io.sarl.docs.doclet.writers;

import java.lang.reflect.Modifier;

import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.FieldDoc;
import com.sun.javadoc.MemberDoc;
import com.sun.tools.doclets.formats.html.FieldWriterImpl;
import com.sun.tools.doclets.formats.html.LinkInfoImpl;
import com.sun.tools.doclets.formats.html.SubWriterHolderWriter;
import com.sun.tools.doclets.formats.html.markup.HtmlTag;
import com.sun.tools.doclets.formats.html.markup.HtmlTree;
import com.sun.tools.doclets.formats.html.markup.StringContent;
import com.sun.tools.doclets.internal.toolkit.Content;

import io.sarl.docs.doclet.utils.Utils;

/** Writer of fields dedicated to the SARL doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SarlFieldWriter extends FieldWriterImpl {

	/** Constructor.
	 *
	 * @param writer the writer for the class that the methods belong to.
	 * @param classDoc the class being documented.
	 */
	public SarlFieldWriter(SubWriterHolderWriter writer, ClassDoc classDoc) {
		super(writer, classDoc);
	}

	/** Constructor.
	 *
	 * @param writer the writer for the class that the methods belong to.
	 */
	public SarlFieldWriter(SubWriterHolderWriter writer) {
		super(writer);
	}

	@Override
	public Content getSignature(FieldDoc field) {
		final Content pre = new HtmlTree(HtmlTag.PRE);
		addAnnotations(field, pre);
		addModifiers(field, pre);
		if ((field.modifierSpecifier() & Modifier.FINAL) != 0) {
			pre.addContent(Utils.keyword(Utils.getKeywords().getValKeyword()));
		} else {
			pre.addContent(Utils.keyword(Utils.getKeywords().getVarKeyword()));
		}
		pre.addContent(this.writer.getSpace());
		if (this.configuration.linksource) {
			final Content fieldName = new StringContent(field.name());
			this.writer.addSrcLink(field, fieldName, pre);
		} else {
			addName(field.name(), pre);
		}
		pre.addContent(this.writer.getSpace());
		pre.addContent(Utils.getKeywords().getColonKeyword());
		pre.addContent(" "); //$NON-NLS-1$
		final  Content fieldlink = this.writer.getLink(new LinkInfoImpl(
				this.configuration, LinkInfoImpl.Kind.MEMBER, field.type()));
		pre.addContent(fieldlink);
		return pre;
	}

	/** Add annotations, except the reserved annotations.
	 *
	 * @param member the members.
	 * @param htmlTree the output.
	 */
	protected void addAnnotations(FieldDoc member, Content htmlTree) {
		this.writer.addAnnotationInfo(member, htmlTree);
	}

	@Override
	protected String modifierString(MemberDoc member) {
		final int ms = member.modifierSpecifier();
		final int no = Modifier.FINAL | Modifier.NATIVE | Modifier.SYNCHRONIZED;
		return Modifier.toString(ms & ~no);
	}

}
