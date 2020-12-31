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

package io.sarl.docs.doclet.proxy;

import com.sun.javadoc.AnnotationTypeDoc;
import com.sun.javadoc.RootDoc;

import io.sarl.docs.doclet.SarlConfiguration;

/** Install the proxies for the {@code Doc}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class NoProxyInstaller implements ProxyInstaller {

	/** Constructor.
	 *
	 * @param configuration the configuration.
	 */
	public NoProxyInstaller(SarlConfiguration configuration) {
		//
	}

	@Override
	public RootDoc installProxies(RootDoc obj) {
		return obj;
	}

	@Override
	public void installProxies(SarlConfiguration configuration) {
		//
	}

	@Override
	public void uninstallProxies(SarlConfiguration configuration) {
		//
	}

	@Override
	public <T> T unwrap(T obj) {
		return obj;
	}

	@Override
	public AnnotationTypeDoc wrap(AnnotationTypeDoc obj) {
		return obj;
	}

}
