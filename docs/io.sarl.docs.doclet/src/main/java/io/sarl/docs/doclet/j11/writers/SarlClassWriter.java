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

import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;

import com.google.common.base.Strings;
import jdk.javadoc.internal.doclets.formats.html.ClassWriterImpl;
import jdk.javadoc.internal.doclets.formats.html.HtmlConfiguration;
import jdk.javadoc.internal.doclets.formats.html.LinkInfoImpl;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlStyle;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTag;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTree;
import jdk.javadoc.internal.doclets.formats.html.markup.StringContent;
import jdk.javadoc.internal.doclets.toolkit.Content;
import jdk.javadoc.internal.doclets.toolkit.util.ClassTree;
import jdk.javadoc.internal.doclets.toolkit.util.DocletConstants;
import jdk.javadoc.internal.doclets.toolkit.util.links.LinkFactory;

import io.sarl.docs.doclet.j11.factories.LinkFactoryFactory;
import io.sarl.docs.doclet.j11.links.SarlLinkFactory;
import io.sarl.docs.doclet.j11.utils.GeneralUtils;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Writer of classes dedicated to the SARL doclet.
 *
 * <p>Caution: this writer is used for all the types of SARL, including agent, behavior, etc.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Apply the SARL syntax for the type signatures</li>
 * <li>Override the link API for using {@link SarlLinkFactory}</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SarlClassWriter extends ClassWriterImpl {

	private final SARLGrammarKeywordAccess sarlKeywords;

	private final LinkFactoryFactory linkFactory;

	/** Constructor.
	 *
	 * @param configuration the configuration data for the doclet
	 * @param typeElement the class being documented.
	 * @param classTree the class tree for the given class.
	 * @param access accessor to the SARL keywords.
	 * @param linkFactory link factory.
	 */
	public SarlClassWriter(HtmlConfiguration configuration, TypeElement typeElement,
			ClassTree classTree, SARLGrammarKeywordAccess access, LinkFactoryFactory linkFactory) {
		super(configuration, typeElement, classTree);
		this.sarlKeywords = access;
		this.linkFactory = linkFactory;
	}

	@Override
	public Content getLink(LinkInfoImpl linkInfo) {
        final LinkFactory factory = this.linkFactory.create(this);
        return factory.getLink(linkInfo);
	}

	@Override
	public Content getTypeParameterLinks(LinkInfoImpl linkInfo) {
        final LinkFactory factory = this.linkFactory.create(this);
        return factory.getTypeParameterLinks(linkInfo);
	}

	@Override
	public Content getHeader(String header) {
		final String hd = GeneralUtils.getHeader(header, getTypeElement(), this.configuration.getLocale());
		return super.getHeader(hd);
	}

	@Override
	public void addClassSignature(String modifiers, Content classInfoTree) {
		final String kw = GeneralUtils.getSarlKeyword(getTypeElement(), this.sarlKeywords);
		if (!Strings.isNullOrEmpty(kw)) {
			final Content hr = new HtmlTree(HtmlTag.HR);
			classInfoTree.addContent(hr);

			final Content pre = new HtmlTree(HtmlTag.PRE);

			addAnnotationInfo(this.typeElement, pre);
			pre.addContent(modifiers.replaceAll("(class)|(interface)", kw));

			final LinkInfoImpl linkInfo = new LinkInfoImpl(this.configuration,
					LinkInfoImpl.Kind.CLASS_SIGNATURE, this.typeElement);
			//Let's not link to ourselves in the signature.
			linkInfo.linkToSelf = false;
			final Content className = new StringContent(this.utils.getSimpleName(this.typeElement));
			//Content parameterLinks = getTypeParameterLinks(linkInfo);
			if (this.configuration.linksource) {
				addSrcLink(this.typeElement, className, pre);
				//pre.addContent(parameterLinks);
			} else {
				Content span = HtmlTree.SPAN(HtmlStyle.typeNameLabel, className);
				//span.addContent(parameterLinks);
				pre.addContent(span);
			}
			if (!this.utils.isInterface(this.typeElement)) {
				final TypeMirror superclass = utils.getFirstVisibleSuperClass(this.typeElement);
				if (superclass != null) {
					pre.addContent(DocletConstants.NL);
					pre.addContent(this.sarlKeywords.getExtendsKeyword());
					pre.addContent(" ");
					final Content link = getLink(new LinkInfoImpl(this.configuration,
							LinkInfoImpl.Kind.CLASS_SIGNATURE_PARENT_NAME,
							superclass));
					pre.addContent(link);
				}
			}
			final List<? extends TypeMirror> interfaces = this.typeElement.getInterfaces();
			if (!interfaces.isEmpty()) {
				boolean isFirst = true;
				for (final TypeMirror type : interfaces) {
					final TypeElement tDoc = this.utils.asTypeElement(type);
					if (!(this.utils.isPublic(tDoc) || this.utils.isLinkable(tDoc))) {
						continue;
					}
					if (isFirst) {
						pre.addContent(DocletConstants.NL);
						pre.addContent(this.utils.isInterface(this.typeElement) 
								? this.sarlKeywords.getExtendsKeyword()
								: this.sarlKeywords.getImplementsKeyword());
						pre.addContent(" ");
						isFirst = false;
					} else {
						pre.addContent(this.sarlKeywords.getCommaKeyword());
						pre.addContent(" ");
					}
					final Content link = getLink(new LinkInfoImpl(configuration,
							LinkInfoImpl.Kind.CLASS_SIGNATURE_PARENT_NAME,
							type));
					pre.addContent(link);
				}
			}
			classInfoTree.addContent(pre);
		} else {
			super.addClassSignature(modifiers, classInfoTree);
		}
	}

}
