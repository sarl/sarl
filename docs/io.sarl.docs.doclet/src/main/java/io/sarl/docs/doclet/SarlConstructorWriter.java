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

package io.sarl.docs.doclet;

import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.ConstructorDoc;
import com.sun.javadoc.ExecutableMemberDoc;
import com.sun.javadoc.Parameter;
import com.sun.tools.doclets.formats.html.ConstructorWriterImpl;
import com.sun.tools.doclets.formats.html.LinkInfoImpl;
import com.sun.tools.doclets.formats.html.SubWriterHolderWriter;
import com.sun.tools.doclets.formats.html.markup.HtmlTag;
import com.sun.tools.doclets.formats.html.markup.HtmlTree;
import com.sun.tools.doclets.internal.toolkit.Content;

/** Writer of constructors dedicated to the SARL doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SarlConstructorWriter extends ConstructorWriterImpl {

	/** Constructor.
	 *
	 * @param writer the writer for the class that the methods belong to.
	 * @param classDoc the class being documented.
	 */
	public SarlConstructorWriter(SubWriterHolderWriter writer, ClassDoc classDoc) {
		super(writer, classDoc);
	}

	/** Constructor.
	 *
	 * @param writer the writer for the class that the methods belong to.
	 */
	public SarlConstructorWriter(SubWriterHolderWriter writer) {
		super(writer);
	}

	@Override
	public Content getSignature(ConstructorDoc constructor) {
		final Content pre = new HtmlTree(HtmlTag.PRE);
		addAnnotations(constructor, pre);
		addModifiers(constructor, pre);
		pre.addContent(Utils.keyword(Utils.getKeywords().getNewKeyword()));
		final int indent = pre.charCount();
		addParameters(constructor, pre, indent);
		addTypeParameters(constructor, pre);
		addExceptions(constructor, pre, indent);
		return pre;
	}

	/** Add annotations, except the reserved annotations.
	 *
	 * @param member the members.
	 * @param htmlTree the output.
	 */
	protected void addAnnotations(ExecutableMemberDoc member, Content htmlTree) {
		this.writer.addAnnotationInfo(member, htmlTree);
	}

	@Override
	protected void addTypeParameters(ExecutableMemberDoc arg0, Content arg1) {
		super.addTypeParameters(arg0, arg1);
	}

	@Override
	protected void addParam(ExecutableMemberDoc member, Parameter param, boolean isVarArg, Content htmlTree) {
        final String defaultValue = Utils.getParameterDefaultValue(member, param, this.configuration);
        final boolean addDefaultValueBrackets = Utils.isNullOrEmpty(defaultValue)
        		&& Utils.isDefaultValuedParameter(member, param, this.configuration);
        if (addDefaultValueBrackets) {
        	htmlTree.addContent("["); //$NON-NLS-1$
        }
        if (param.name().length() > 0) {
            htmlTree.addContent(param.name());
        }
		htmlTree.addContent(this.writer.getSpace());
		htmlTree.addContent(Utils.getKeywords().getColonKeyword());
		htmlTree.addContent(" "); //$NON-NLS-1$
        if (param.type() != null) {
            final Content link = this.writer.getLink(new LinkInfoImpl(
                    this.configuration, LinkInfoImpl.Kind.EXECUTABLE_MEMBER_PARAM,
                    param.type()).varargs(isVarArg));
            htmlTree.addContent(link);
        }
        if (addDefaultValueBrackets) {
        	htmlTree.addContent("]"); //$NON-NLS-1$
        } else if (!Utils.isNullOrEmpty(defaultValue)) {
    		htmlTree.addContent(" "); //$NON-NLS-1$
    		htmlTree.addContent(Utils.getKeywords().getEqualsSignKeyword());
    		htmlTree.addContent(this.writer.getSpace());
    		htmlTree.addContent(defaultValue);
        }
	}

}
