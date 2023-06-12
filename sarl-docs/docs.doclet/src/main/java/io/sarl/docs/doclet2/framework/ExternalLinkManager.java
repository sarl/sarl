/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.doclet2.framework;

import java.net.URI;
import java.net.URL;
import java.util.Set;

import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;

import jdk.javadoc.doclet.Reporter;

import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContext;

/** Manager of external links
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public interface ExternalLinkManager {

	/** Replies the external links.
	 *
	 * @return the links
	 */
	Set<URI> getExternalLinks();

	/** Add an external link.
	 *
	 * @param uri the URI to be added.
	 */
	void addExternalLink(URI url);

	/** Add an external link.
	 *
	 * @param url the URL to be added.
	 */
	void addExternalLink(URL url);

	/** Replies the URI to the external resource that contains the documentation of the given type.
	 *
	 * @param element the element to search for.
	 * @param context the caller context.
	 * @return the URI, or {@code null} if there is no known external resource.
	 */
	default URI getExternalLink(Element element, ExternalLinkManagerContext context) {
		return getExternalLink(element, null, context);
	}

	/** Replies the URI to the external resource that contains the documentation of the given type.
	 *
	 * @param element the element to search for.
	 * @param anchorName the name of the anchor in the pointed page.
	 * @param context the caller context.
	 * @return the URI, or {@code null} if there is no known external resource.
	 */
	URI getExternalLink(Element element, String anchorName, ExternalLinkManagerContext context);

	/** Replies the URL to the external resource that contains the documentation of the given type.
	 *
	 * @param element the element to search for.
	 * @param anchorName the name of the anchor in the pointed page.
	 * @param context the caller context.
	 * @return the URI, or {@code null} if there is no known external resource.
	 */
	default URL getExternalURL(Element element, String anchorName, ExternalLinkManagerContext context) {
		final URI uri = getExternalLink(element, anchorName, context);
		if (uri != null) {
			try {
				return uri.toURL();
			} catch (Throwable ex) {
				//
			}
		}
		return null;
	}

	/** Replies the URL to the external resource that contains the documentation of the given type.
	 *
	 * @param element the element to search for.
	 * @param context the caller context.
	 * @return the URL, or {@code null} if there is no known external resource.
	 */
	default URL getExternalURL(TypeElement element, HtmlFactoryContext context) {
		final URI uri = getExternalLink(element, context);
		if (uri != null) {
			try {
				return uri.toURL();
			} catch (Throwable ex) {
				//
			}
		}
		return null;
	}

	/** Replies the URL to the external resource that contains the documentation of the given type.
	 *
	 * @param element the element to search for.
	 * @param context the caller context.
	 * @return the URL, or {@code null} if there is no known external resource.
	 */
	default URL getExternalURL(VariableElement element, HtmlFactoryContext context) {
		final URI uri = getExternalLink(element.getEnclosingElement(), element.getSimpleName().toString(), context);
		if (uri != null) {
			try {
				return uri.toURL();
			} catch (Throwable ex) {
				//
			}
		}
		return null;
	}

	/** Replies the URL to the external resource that contains the documentation of the given type.
	 *
	 * @param element the element to search for.
	 * @param context the caller context.
	 * @return the URL, or {@code null} if there is no known external resource.
	 */
	default URL getExternalURL(ExecutableElement element, HtmlFactoryContext context) {
		final URI uri = getExternalLink(element.getEnclosingElement(), element.getSimpleName().toString(), context);
		if (uri != null) {
			try {
				return uri.toURL();
			} catch (Throwable ex) {
				//
			}
		}
		return null;
	}

	/** Context for an external link manager.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	interface ExternalLinkManagerContext {

		/** Replies the generation environment.
		 *
		 * @return the environment.
		 */
		SarlDocletEnvironment getEnvironment();
		
		/** Replies the doclet's CLI options.
		 *
		 * @return the options.
		 */
		DocletOptions getCliOptions();

		/** Replies the issue reporter.
		 *
		 * @return the reporter.
		 */
		Reporter getReporter();

	}

}
