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

import javax.lang.model.element.TypeElement;

import com.google.common.base.Strings;
import jdk.javadoc.internal.doclets.formats.html.AnnotationTypeWriterImpl;
import jdk.javadoc.internal.doclets.formats.html.HtmlConfiguration;
import jdk.javadoc.internal.doclets.formats.html.LinkInfoImpl;
import jdk.javadoc.internal.doclets.toolkit.Content;
import jdk.javadoc.internal.doclets.toolkit.util.links.LinkFactory;

import io.sarl.docs.doclet.j11.factories.LinkFactoryFactory;
import io.sarl.docs.doclet.j11.links.SarlLinkFactory;
import io.sarl.docs.doclet.j11.utils.GeneralUtils;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Writer of classes dedicated to the SARL doclet.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Apply the SARL syntax for the annotation signature</li>
 * <li>Override the link API for using {@link SarlLinkFactory}</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlAnnotationWriter extends AnnotationTypeWriterImpl {

	private final SARLGrammarKeywordAccess sarlKeywords;

	private final LinkFactoryFactory linkFactory;

	/** Constructor.
	 *
	 * @param configuration the configuration data for the doclet
	 * @param typeElement the class being documented.
	 * @param access accessor to the SARL keywords.
	 * @param linkFactory link factory.
	 */
	public SarlAnnotationWriter(HtmlConfiguration configuration, TypeElement typeElement,
			SARLGrammarKeywordAccess access, LinkFactoryFactory linkFactory) {
		super(configuration, typeElement);
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
		final String hd = GeneralUtils.getHeader(header,
				SarlPackage.SARL_ANNOTATION_TYPE,
				this.annotationType.getSimpleName().toString(),
				this.configuration.getLocale());
		return super.getHeader(hd);
	}

	@Override
	public void addAnnotationTypeSignature(String modifiers, Content annotationInfoTree) {
		final String kw = GeneralUtils.getSarlKeyword(this.annotationType, this.sarlKeywords);
		if (!Strings.isNullOrEmpty(kw)) {
	        final String mods = modifiers.replaceAll("\\@interface", kw);
			super.addAnnotationTypeSignature(mods, annotationInfoTree);
		} else {
			super.addAnnotationTypeSignature(modifiers, annotationInfoTree);
		}
	}

}
