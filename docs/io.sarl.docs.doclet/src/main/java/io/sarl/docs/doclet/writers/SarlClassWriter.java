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

import java.io.IOException;

import com.sun.javadoc.ClassDoc;
import com.sun.tools.doclets.formats.html.ClassWriterImpl;
import com.sun.tools.doclets.formats.html.ConfigurationImpl;
import com.sun.tools.doclets.formats.html.LinkInfoImpl;
import com.sun.tools.doclets.internal.toolkit.Content;
import com.sun.tools.doclets.internal.toolkit.util.ClassTree;
import com.sun.tools.doclets.internal.toolkit.util.links.LinkFactory;

import io.sarl.docs.doclet.SarlConfiguration;
import io.sarl.docs.doclet.utils.SARLFeatureAccess;
import io.sarl.docs.doclet.utils.SarlMessageRetreiver;
import io.sarl.docs.doclet.utils.Utils;

/** Writer of classes dedicated to the SARL doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SarlClassWriter extends ClassWriterImpl {

	/** Constructor.
	 * @param configuration the configuration.
	 * @param classDoc the documentation.
	 * @param prevClass the previous class.
	 * @param nextClass the next class.
	 * @param classTree the class hierarchy.
	 * @throws IOException in case of error.
	 */
	public SarlClassWriter(ConfigurationImpl configuration, ClassDoc classDoc, ClassDoc prevClass, ClassDoc nextClass,
			ClassTree classTree) throws IOException {
		super(configuration, classDoc, prevClass, nextClass, classTree);
	}

	@Override
	public Content getHeader(String header) {
		final ClassDoc doc = getClassDoc();
		if (doc != null) {
			final Integer sarlType = Utils.getSarlClassification(doc);
			if (sarlType != null) {
				final String label;
				switch (sarlType.intValue()) {
				case SARLFeatureAccess.SARL_AGENT:
					label = this.configuration.standardmessage.getText(SarlMessageRetreiver.DOCLET_AGENT);
					break;
				case SARLFeatureAccess.SARL_EVENT:
					label = this.configuration.standardmessage.getText(SarlMessageRetreiver.DOCLET_EVENT);
					break;
				case SARLFeatureAccess.SARL_BEHAVIOR:
					label = this.configuration.standardmessage.getText(SarlMessageRetreiver.DOCLET_BEHAVIOR);
					break;
				case SARLFeatureAccess.SARL_CAPACITY:
					label = this.configuration.standardmessage.getText(SarlMessageRetreiver.DOCLET_CAPACITY);
					break;
				case SARLFeatureAccess.SARL_SKILL:
					label = this.configuration.standardmessage.getText(SarlMessageRetreiver.DOCLET_SKILL);
					break;
				case SARLFeatureAccess.SARL_SPACE:
					label = this.configuration.standardmessage.getText(SarlMessageRetreiver.DOCLET_SPACE);
					break;
				case SARLFeatureAccess.SARL_ARTIFACT:
					label = this.configuration.standardmessage.getText(SarlMessageRetreiver.DOCLET_ARTIFACT);
					break;
				default:
					label = null;
				}
				if (!Utils.isNullOrEmpty(label)) {
					return super.getHeader(label + " " + doc.name()); //$NON-NLS-1$
				}
			}
		}
		return super.getHeader(header);
	}

	@Override
	public Content getLink(LinkInfoImpl linkInfo) {
        final LinkFactory factory = ((SarlConfiguration) this.configuration).getLinkFactory(this);
        return factory.getLink(linkInfo);
	}

}
