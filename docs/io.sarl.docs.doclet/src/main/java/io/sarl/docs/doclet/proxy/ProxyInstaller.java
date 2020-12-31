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
 * This object is filtering the arrays that are replied functions in {@code RootDoc} or {@code ProgramElementDoc}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public interface ProxyInstaller {

	/** Filter the given document.
	 *
	 * @param obj the document to filter.
	 * @return the filtered {@code obj}.
	 */
	RootDoc installProxies(RootDoc obj);

	/** Install the proxies into the configuration.
	 *
	 * <p>Usually, the root documentation is updated.
	 *
	 * @param configuration the configuration to change.
	 */
	void installProxies(SarlConfiguration configuration);

	/** Remove the proxies from the configuration.
	 *
	 * <p>Usually, the root documentation is updated.
	 *
	 * @param configuration the configuration to change.
	 */
	void uninstallProxies(SarlConfiguration configuration);

	/** Wrap the given annotation type documentation.
	 *
	 * @param obj the document to filter.
	 * @return the filtered {@code obj}.
	 */
	AnnotationTypeDoc wrap(AnnotationTypeDoc obj);

	/** Unwrap the given element if it is a proxy.
	 *
	 * @param <T> the type of the wrapped element.
	 * @param obj the element to unwrap.
	 * @return the element.
	 */
	<T> T unwrap(T obj);

	/** Run the given function without proxy.
	 *
	 * @param configuration the configuration.
	 * @param function the function to run.
	 */
	default void noProxy(SarlConfiguration configuration, Runnable function) {
		if (function != null) {
			uninstallProxies(configuration);
			try {
				function.run();
			} finally {
				installProxies(configuration);
			}
		}
	}

}
