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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.Charset;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.inject.Inject;
import javax.lang.model.element.Element;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic.Kind;

import com.google.common.base.Strings;

/** Manager of external links
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class ExternalLinkManagerImpl implements ExternalLinkManager {

	private static final String MODULE_PREFIX = "module:"; //$NON-NLS-1$

	private static final String PACKAGE_LIST_NAME = "package-list"; //$NON-NLS-1$

	private static final String ELEMENT_LIST_NAME = "element-list"; //$NON-NLS-1$

	private ElementUtils elementUtils;

	private Set<URI> externalUrls = new HashSet<>();

	private Map<String, Item> externalModules;

	private Map<String, Map<String, Item>> externalPackages;

	/** Read the external link sources.
	 *
	 * @param context the caller context.
	 */
	protected void synchronizeExternalLinks(ExternalLinkManagerContext context) {
		if (this.externalModules == null || this.externalPackages == null) {
			this.externalModules = new TreeMap<>();
			this.externalPackages = new TreeMap<>();
			for (final URI uri : getExternalLinks()) {
				context.getReporter().print(Kind.NOTE, MessageFormat.format(Messages.ExternalLinkManagerImpl_0, uri.toASCIIString()));
				// Read the element-list file
				boolean read = true;
				try {
					final URL url = uri.resolve(ELEMENT_LIST_NAME).toURL();
					try (final InputStream is = url.openStream()) {
						readElementList(is, uri, context);
					}
				} catch (IOException exception) {
					read = false;
					context.getReporter().print(Kind.WARNING, MessageFormat.format(Messages.ExternalLinkManagerImpl_3, ELEMENT_LIST_NAME, exception.getLocalizedMessage()));
				}
				// Read the package-list file
				try {
					final URL url = uri.resolve(PACKAGE_LIST_NAME).toURL();
					try (final InputStream is = url.openStream()) {
						readElementList(is, uri, context);
					}
				} catch (IOException exception) {
					if (!read) {
						context.getReporter().print(Kind.WARNING, MessageFormat.format(Messages.ExternalLinkManagerImpl_3, PACKAGE_LIST_NAME, exception.getLocalizedMessage()));
					}
				}
			}
		}
	}

	/**
	 * Read the file "element-list" and for each element name found.
	 *
	 * @param input the "element-list" file.
	 * @param path URL to the elements.
	 * @param context the caller context.
	 * @throws IOException if there is a problem reading or closing the stream.
	 */
	protected void readElementList(InputStream input, URI path, ExternalLinkManagerContext context) throws IOException {
		try (BufferedReader in = new BufferedReader(new InputStreamReader(input))) {
			String moduleName = null;
			String elementName = in.readLine();
			while (elementName != null) {
				if (!elementName.isBlank()) {
					if (elementName.startsWith(MODULE_PREFIX)) {
						moduleName = elementName.replace(MODULE_PREFIX, ""); //$NON-NLS-1$
						final URI fixedPath = path.resolve("/"); //$NON-NLS-1$
						final Item item = new Item(moduleName, fixedPath);
						this.externalModules.put(moduleName, item);
					} else {
						final String packagePath = elementName.replace('.', '/');
						final URI elementPath;
						if (moduleName != null && !moduleName.isBlank()) {
							final URI moduleUri = path.resolve(moduleName + '/');
							elementPath = moduleUri.resolve(packagePath + '/');
						} else {
							elementPath = path.resolve(packagePath + '/');
						}
						final String actualModuleName = checkLinkCompatibility(elementName, moduleName, path, context);
						final Item item = new Item(elementName, elementPath);
						this.externalPackages.computeIfAbsent(actualModuleName, k -> new TreeMap<>()).put(elementName, item);
					}
				}
				elementName = in.readLine();
			}
		}
	}

	/**
	 * Check if the external documentation format matches our internal model of the code.
	 * Returns the module name to use for external reference lookup according to the actual
	 * modularity of the external package (and regardless of modularity of documentation).
	 *
	 * @param packageName the package name
	 * @param moduleName the module name or null
	 * @param path the documentation path
	 * @return the module name to use according to actual modularity of the package
	 */
	private String checkLinkCompatibility(String packageName, String moduleName, URI path, ExternalLinkManagerContext context)  {
		final PackageElement packageElement = context.getEnvironment().getElementUtils().getPackageElement(packageName);
		if (packageElement != null) {
			final ModuleElement moduleElement = (ModuleElement) packageElement.getEnclosingElement();
			if (moduleElement == null || moduleElement.isUnnamed()) {
				if (moduleName != null) {
					context.getReporter().print(Kind.WARNING,
							MessageFormat.format(Messages.ExternalLinkManagerImpl_1, path.toASCIIString()));
				}
				// library is not modular, ignore module name even if documentation is modular
				return ElementUtilsImpl.DEFAULT_ELEMENT_NAME;
			} else if (moduleName == null) {
				// suppress the warning message in the case of automatic modules
				if (!isAutomaticModule(moduleElement)) {
					context.getReporter().print(Kind.WARNING,
							MessageFormat.format(Messages.ExternalLinkManagerImpl_2, path.toASCIIString()));
				}
				return moduleElement.getQualifiedName().toString();
			}
		}
		return moduleName == null ? ElementUtilsImpl.DEFAULT_ELEMENT_NAME : moduleName;
	}

	//TODO: The following should be replaced by a new method such as Elements.isAutomaticModule
	@SuppressWarnings("static-method")
	private boolean isAutomaticModule(ModuleElement moduleElement) {
		if (moduleElement == null) {
			return false;
		}
		//final ModuleSymbol msym = (ModuleSymbol) moduleElement;
		//return (msym.flags() & Flags.AUTOMATIC_MODULE) != 0;
		return false;
	}

	/** Change the element utility.
	 *
	 * @param utils the utility.
	 */
	@Inject
	public void setElementUtils(ElementUtils utils) {
		this.elementUtils = utils;
	}

	/** Replies the element utility.
	 *
	 * @return the utility.
	 */
	public ElementUtils getElementUtils() {
		return this.elementUtils;
	}

	@Override
	public Set<URI> getExternalLinks() {
		return Collections.unmodifiableSet(this.externalUrls);
	}

	@Override
	public void addExternalLink(URI uri) {
		assert uri != null;
		if (this.externalUrls.add(uri)) {
			this.externalModules = null;
			this.externalPackages = null;
		}
	}

	@Override
	public void addExternalLink(URL url) {
		assert url != null;
		try {
			final URI uri = url.toURI();
			addExternalLink(uri);
		} catch (URISyntaxException ex) {
			throw new RuntimeException(ex);
		}
	}

	@Override
	public URI getExternalLink(Element element, String anchorName, ExternalLinkManagerContext context) {
		if (context.getDocletOptions().isOffline()) {
			return null;
		}
		final Item item = getCachedItem(element, context);
		if (item == null) {
			return null;
		}
		final URI fullUri;
		if (element instanceof TypeElement) {
			final TypeElement typeElement = (TypeElement) element;
			fullUri = item.path.resolve(typeElement.getSimpleName() + ".html"); //$NON-NLS-1$
		} else {
			fullUri = item.path;
		}
		if (fullUri != null && !Strings.isNullOrEmpty(anchorName)) {
			try {
				final String anchorName0 = URLEncoder.encode(anchorName, Charset.defaultCharset().displayName());
				final String completedUri = fullUri.toString() + "#" + anchorName0; //$NON-NLS-1$
				return URI.create(completedUri);
			} catch (UnsupportedEncodingException ex) {
				throw new RuntimeException(ex);
			}
		}
		return fullUri;
	}

	/** Relies any cached item.
	 *
	 * @param element the element for which the item must be replied.
	 * @param context the context.
	 * @return the item or {@code null}.
	 */
	protected Item getCachedItem(Element element, ExternalLinkManagerContext context) {
		Item item = null;
		if (getElementUtils().isExternal(element, context.getEnvironment())) {
			synchronizeExternalLinks(context);
			if (element instanceof ModuleElement) {
	            final ModuleElement moduleElement = (ModuleElement) element;
	            item = this.externalModules.get(getElementUtils().getElementName(moduleElement));
			} else if (element instanceof PackageElement) {
	            final PackageElement packageElement = (PackageElement) element;
	            final ModuleElement moduleElement = context.getEnvironment().getElementUtils().getModuleOf(packageElement);
	            final Map<String, Item> packageMap = this.externalPackages.get(getElementUtils().getElementName(moduleElement));
	            item = packageMap != null ? packageMap.get(getElementUtils().getElementName(packageElement)) : null;
			} else if (element instanceof TypeElement) {
	            final TypeElement typeElement = (TypeElement) element;
	            final PackageElement packageElement = context.getEnvironment().getElementUtils().getPackageOf(typeElement);
	            final ModuleElement moduleElement = context.getEnvironment().getElementUtils().getModuleOf(packageElement);
	            final Map<String, Item> packageMap = this.externalPackages.get(getElementUtils().getElementName(moduleElement));
	            item = packageMap != null ? packageMap.get(getElementUtils().getElementName(packageElement)) : null;
			}
		}
		return item;
	}

	/** External link source.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	public static class Item {

		/** Name of the item.
		 */
		public final String name;

		/** Path to the item.
		 */
		public final URI path;

		/** Constructor.
		 *
		 * @param name is the item name.
		 * @param path is the path to the item.
		 */
		public Item(String name, URI path) {
			this.name = name;
			this.path = path;
		}

	}

}

