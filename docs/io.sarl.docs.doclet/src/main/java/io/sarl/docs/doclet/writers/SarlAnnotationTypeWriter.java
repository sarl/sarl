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

import com.sun.javadoc.AnnotationTypeDoc;
import com.sun.javadoc.Type;
import com.sun.tools.doclets.formats.html.AnnotationTypeWriterImpl;
import com.sun.tools.doclets.formats.html.ConfigurationImpl;
import com.sun.tools.doclets.formats.html.LinkInfoImpl;
import com.sun.tools.doclets.internal.toolkit.Content;
import com.sun.tools.doclets.internal.toolkit.util.links.LinkFactory;

import io.sarl.docs.doclet.SarlConfiguration;

/** Writer of classes dedicated to the SARL doclet.
 *
 * @author <a href="http://www.ciad-lab.fr/author-10836/">St&eacute;phane Galland</a>
 * @version io.sarl.docs.doclet 0.12.0 20210527-171007
 * @mavengroupid io.sarl.docs
 * @mavenartifactid io.sarl.docs.doclet
 * @since 0.7
 */
public class SarlAnnotationTypeWriter extends AnnotationTypeWriterImpl {

	/** Constructor.
	 * @param configuration the configuration.
	 * @param annotationType the type.
	 * @param prevType the previous type.
	 * @param nextType the next type.
	 * @throws Exception in case of error.
	 */
	public SarlAnnotationTypeWriter(ConfigurationImpl configuration, AnnotationTypeDoc annotationType, Type prevType,
			Type nextType) throws Exception {
		super(configuration, annotationType, prevType, nextType);
	}

	@Override
	public Content getLink(LinkInfoImpl linkInfo) {
        final LinkFactory factory = ((SarlConfiguration) this.configuration).getLinkFactory(this);
        return factory.getLink(linkInfo);
	}

}
