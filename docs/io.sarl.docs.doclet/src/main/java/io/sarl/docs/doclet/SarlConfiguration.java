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

import com.sun.tools.doclets.formats.html.ConfigurationImpl;
import com.sun.tools.doclets.formats.html.HtmlDocletWriter;
import com.sun.tools.doclets.internal.toolkit.WriterFactory;
import com.sun.tools.doclets.internal.toolkit.util.links.LinkFactory;

/** Configuration for the SARL doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SarlConfiguration extends ConfigurationImpl {

	private final WriterFactory writerFactory = new SarlWriterFactory(this);

	private ApidocExcluder apidocExcluder;

	private ProxyInstaller proxyInstaller;

	/** Constructor.
	 */
	public SarlConfiguration() {
		Reflect.setField(this, ConfigurationImpl.class, "standardmessage", new SarlMessageRetreiver(this)); //$NON-NLS-1$
		final SARLFeatureAccess sarlKeywords = new SARLFeatureAccess();
		Utils.setKeywords(sarlKeywords);
	}

	@Override
	public WriterFactory getWriterFactory() {
		return this.writerFactory;
	}

	/** Replies the link factory.
	 *
	 * @param writer the owner of the factory.
	 * @return the link factory.
	 */
	@SuppressWarnings("static-method")
	public LinkFactory getLinkFactory(HtmlDocletWriter writer) {
		return new SarlLinkFactory(writer);
	}

	/** Replies a API documentation excluder.
	 *
	 * @return the excluder.
	 */
	public ApidocExcluder getApidocExcluder() {
		if (this.apidocExcluder == null) {
			this.apidocExcluder = new DefaultApidocExcluder();
		}
		return this.apidocExcluder;
	}

	/** Replies a proxy installer.
	 *
	 * @return the installer.
	 */
	public ProxyInstaller getProxyInstaller() {
		if (this.proxyInstaller == null) {
			this.proxyInstaller = new NoProxyInstaller(this);
		}
		return this.proxyInstaller;
	}

}
