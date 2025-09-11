/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
 *
 *------- FORKED SOURCE CODE:
 *
 * THIS CODE IS FORKED FROM JDK.JAVADOC INTERNAL PACKAGE AND ADAPTED TO THE SARL PURPOSE.
 * THE FORK WAS NECESSARY BECAUSE IT IS IMPOSSIBLE TO SUBCLASS THE TYPES FOR THE.
 * STANDARD HTML DOCLET THAT IS PROVIDED BY JDK.JAVADOC MODULE.
 *
 * Copyright (c) 2003, 2021, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package io.sarl.docs.doclet2.html.framework;

import java.nio.file.Files;
import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Pattern;

import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.RecordComponentElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.ElementScanner9;
import javax.tools.Diagnostic.Kind;

import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.sun.source.doctree.BlockTagTree;
import com.sun.source.doctree.DocTree;

import io.sarl.docs.doclet2.framework.DocUtils;
import io.sarl.docs.doclet2.framework.DocumentationRepository;
import io.sarl.docs.doclet2.framework.ElementUtils;
import io.sarl.docs.doclet2.framework.QualifiedNameSetBuilder;
import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.framework.StandardQualifiedNameSetBuilder;
import io.sarl.docs.doclet2.framework.TagletManager;
import io.sarl.docs.doclet2.framework.TypeHierarchy;
import io.sarl.docs.doclet2.framework.TypeRepository;
import io.sarl.docs.doclet2.html.taglets.SarlTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ExcludeFromApidocTaglet;
import io.sarl.docs.doclet2.html.taglets.block.HiddenTaglet;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import jdk.javadoc.doclet.Reporter;
import jdk.javadoc.doclet.Taglet.Location;

/** Abstract implementation of a generator for the documentation.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public abstract class AbstractDocumentationGenerator implements HtmlFactoryContext {

	private static final String PROPERTY_GETTER_PREFIX = "get"; //$NON-NLS-1$

	private static final String PROPERTY_SETTER_PREFIX = "set"; //$NON-NLS-1$

	private static final Pattern PROPERTY_GETTER_PATTERN = Pattern.compile("^" + PROPERTY_GETTER_PREFIX + "([A-Z][A-Za-z0-9_]*)$"); //$NON-NLS-1$ //$NON-NLS-2$

	private static final Pattern PROPERTY_SETTER_PATTERN = Pattern.compile("^" + PROPERTY_SETTER_PREFIX + "([A-Z][A-Za-z0-9_]*)$"); //$NON-NLS-1$ //$NON-NLS-2$

	private Reporter reporter;

	private String lastTitle;

	private SarlDocletEnvironment environment;

	private DocletOptions docletOptions;

	private String baseUri;

	private Path relativePath;

	private Path pathToRoot;

	private PathBuilder pathBuilder;

	private HtmlFactory htmlFactory;

	private HtmlAccessor htmlAccessor;

	private Provider<Navigation> navigationProvider;

	private Navigation navigation;

	private ElementUtils elementUtils;

	private TypeHierarchy typeHierarchy;

	private TypeRepository typeRepository;

	private DocUtils docUtils;

	private SARLGrammarKeywordAccess keywords;

	private TagletManager tagletManager;

	private BlockTagSorter blockTagSorter;

	private final List<Path> cssStylesheets = new ArrayList<>();

	private final List<Path> jsScripts = new ArrayList<>();

	private DocumentationRepository repository;

	/** Initialize the generator.
	 *
	 * @param cssStylesheets the list of CSS files.
	 * @param jsScripts the list of Javascript scripts.
	 * @param reporter the issue reporter.
	 * @param environment the documentation environment.
	 * @param cliOptions the list of command-line options. 
	 */
	protected void initGenerator(Collection<Path> cssStylesheets, Collection<Path> jsScripts, Reporter reporter, SarlDocletEnvironment environment, DocletOptions cliOptions) {
		setCssStylesheets(cssStylesheets);
		setJsScripts(jsScripts);
		setReporter(reporter);
		setEnvironment(environment);
		setDocletOptions(cliOptions);
	}

	/** Change the sorter of the block tags.
	 *
	 * @param sorter the sorter.
	 */
	@Inject
	public void setBlockTagSorter(BlockTagSorter sorter) {
		this.blockTagSorter = sorter;
	}
	
	/** Replies the sorter of the block tags.
	 *
	 * @return the sorter.
	 */
	public BlockTagSorter getBlockTagSorter() {
		return this.blockTagSorter;
	}

	/** Change the HTML path builder.
	 *
	 * @param builder the builder.
	 */
	@Inject
	public void setPathBuilder(PathBuilder builder) {
		this.pathBuilder = builder;
	}

	/** Replies the HTML path builder.
	 *
	 * @return the builder.
	 */
	public PathBuilder getPathBuilder() {
		return this.pathBuilder;
	}

	/** Change the documentation repository.
	 *
	 * @param repository the repository.
	 */
	@Inject
	public void setDocumentationRepository(DocumentationRepository repository) {
		this.repository = repository;
	}

	/** Replies the documentation repository.
	 *
	 * @return the repository.
	 */
	public DocumentationRepository getDocumentationRepository() {
		return this.repository;
	}

	/** Change the taglet manager.
	 *
	 * @param manager the manager.
	 */
	@Inject
	public void setTagletManager(TagletManager manager) {
		this.tagletManager = manager;
	}

	@Override
	public TagletManager getTagletManager() {
		return this.tagletManager;
	}

	/** Change the accessor to the SARL keywords.
	 *
	 * @param accessor is the accessor.
	 */
	@Inject
	public void setSARLGrammarKeywordAccess(SARLGrammarKeywordAccess accessor) {
		this.keywords = accessor;
	}

	/** Replies the accessor to the SARL keywords.
	 *
	 * @return the accessor.
	 */
	public SARLGrammarKeywordAccess getSARLGrammarKeywordAccess() {
		return this.keywords;
	}

	/** Change the type hierarchy.
	 *
	 * @param hierarchy the manager of type hierarchy.
	 */
	@Inject
	public void setTypeHierarchy(TypeHierarchy hierarchy) {
		this.typeHierarchy = hierarchy;
	}

	/** Replies the type hierarchy.
	 *
	 * @return the hierarchy.
	 */
	public TypeHierarchy getTypeHierarchy() {
		return this.typeHierarchy;
	}

	/** Change the type repository.
	 *
	 * @param repository the manager of type repository.
	 */
	@Inject
	public void setTypeRepository(TypeRepository repository) {
		this.typeRepository = repository;
	}

	/** Replies the type repository.
	 *
	 * @return the repository.
	 */
	public TypeRepository getTypeRepository() {
		return this.typeRepository;
	}

	/** Change the doc utilities.
	 *
	 * @param utils the doc utilities.
	 */
	@Inject
	public void setDocUtils(DocUtils utils) {
		this.docUtils = utils;
	}

	@Override
	public DocUtils getDocUtils() {
		return this.docUtils;
	}

	/** Change the SARL-specific element utilities.
	 *
	 * @param utils the element utilities.
	 */
	@Inject
	public void setElementUtils(ElementUtils utils) {
		this.elementUtils = utils;
	}

	/** Replies the SARL-specific element utilities.
	 *
	 * @return the utilities.
	 */
	public ElementUtils getElementUtils() {
		return this.elementUtils;
	}

	/** Change the navigation bar that is specific to this generator.
	 *
	 * @param navProvider a provider of navigation tool.
	 */
	@Inject
	public void setNavigationProvider(Provider<Navigation> navProvider) {
		this.navigationProvider = navProvider;
	}

	/** Replies the navigation bar that is specific to this generator.
	 *
	 * @return the navigation bar.
	 */
	public Navigation getNavigation() {
		if (this.navigation == null) {
			this.navigation = this.navigationProvider.get();
			initNavigation(this.navigation);
		}
		return this.navigation;
	}

	/** Initialize the navigation object for this generator.
	 *
	 * @param navigation the navigation.
	 */
	protected abstract void initNavigation(Navigation navigation);

	/** Change the HTML factory.
	 *
	 * @param factory the factory.
	 */
	@Inject
	public void setHtmlFactory(HtmlFactory factory) {
		this.htmlFactory = factory;
	}

	/** Replies the HTML factory.
	 *
	 * @return the factory.
	 */
	public HtmlFactory getHtmlFactory() {
		return this.htmlFactory;
	}

	/** Change the HTML accessor.
	 *
	 * @param accessor the accessor.
	 */
	@Inject
	public void setHtmlAccessor(HtmlAccessor accessor) {
		this.htmlAccessor = accessor;
	}

	/** Replies the HTML accessor.
	 *
	 * @return the accessor.
	 */
	public HtmlAccessor getHtmlAccessor() {
		return this.htmlAccessor;
	}

	@Override
	public String getBaseUri() {
		return this.baseUri;
	}

	/** Replies the last computed relative path for the documentation file.
	 *
	 * @return the relative path of the documentation file.
	 */
	public Path getRelativePath() {
		return this.relativePath;
	}

	@Override
	public Path getPathToRoot() {
		return this.pathToRoot;
	}

	@Override
	public DocletOptions getDocletOptions() {
		return this.docletOptions;
	}

	/** Change the doclet's CLI options.
	 *
	 * @param cliOptions the doclet's CLI options.
	 */
	protected void setDocletOptions(DocletOptions cliOptions) {
		this.docletOptions = cliOptions;
	}

	@Override
	public SarlDocletEnvironment getEnvironment() {
		return this.environment;
	}

	/** Change the generation environment.
	 *
	 * @param environment the generation environment.
	 */
	protected void setEnvironment(SarlDocletEnvironment environment) {
		this.environment = environment;
	}

	/** Replies the last computed title.
	 *
	 * @return the last computed title.
	 */
	public String getLastTitle() {
		return this.lastTitle;
	}

	/** Change the last computed title.
	 *
	 * @param title the last computed title.
	 */
	protected void setLastTitle(String title) {
		this.lastTitle = title;
	}

	@Override
	public Reporter getReporter() {
		return this.reporter;
	}

	/** Change the reporter.
	 *
	 * @param reporter the reporter.
	 */
	protected void setReporter(Reporter reporter) {
		this.reporter = reporter;
	}

	/** Compute the baseUri, relative path and path to root for the given element.
	 *
	 * <p>This function sets the internal attributes that could be read with the appropriate getter functions.
	 *
	 * @param element is the element qualified name.
	 * @param isTypeName indicates if the given element is a fully qualified name of type. If {@code false}, it
	 *     is assumed to be a filename.
	 * @see #getBaseUri()
	 * @see #getRelativePath()
	 * @see #getPathToRoot()
	 */
	protected void computePaths(String element, boolean isTypeName) {
		final var elements = isTypeName ? element.split("[$.]") : new String[] {element}; //$NON-NLS-1$
		final var baseUri = new StringBuilder("file:"); //$NON-NLS-1$
		Path relativePath = null;
		Path pathToRoot = null;
		for (var i = 0; i < elements.length - 1; ++i) {
			baseUri.append(elements[i]);
			baseUri.append('/');
			final var currentPath = Path.of(elements[i]);
			if (relativePath == null) {
				relativePath = currentPath;
			} else {
				relativePath = relativePath.resolve(currentPath);
			}
			final var currentParent = Path.of(".."); //$NON-NLS-1$
			if (pathToRoot == null) {
				pathToRoot = currentParent;
			} else {
				pathToRoot = pathToRoot.resolve(currentParent);
			}
		}
		this.pathToRoot = pathToRoot == null ? Path.of(".") : pathToRoot; //$NON-NLS-1$
		var basename = elements[elements.length - 1];
		if (!basename.endsWith(".html")) { //$NON-NLS-1$
			basename += ".html"; //$NON-NLS-1$
		}
		if (relativePath == null) {
			this.relativePath = Path.of(basename);
		} else {
			this.relativePath = relativePath.resolve(basename);
		}
		this.baseUri = baseUri.toString();
	}

	/** Compute the baseUri, relative path and path to root for the given element.
	 *
	 * <p>This function sets the internal attributes that could be read with the appropriate getter functions.
	 *
	 * @param element is the element qualified name.
	 * @param addHtmlExtension indicates if the ".html" file extension must be added to the filename.
	 * @see #getBaseUri()
	 * @see #getRelativePath()
	 * @see #getPathToRoot()
	 */
	protected void computePaths(Path element, boolean addHtmlExtension) {
		final var n = element.getNameCount();
		final var baseUri = new StringBuilder("file:"); //$NON-NLS-1$
		Path relativePath = null;
		Path pathToRoot = null;
		for (var i = 0; i < n - 1; ++i) {
			baseUri.append(element.getName(i));
			baseUri.append('/');
			final var currentPath = element.getName(i);
			if (relativePath == null) {
				relativePath = currentPath;
			} else {
				relativePath = relativePath.resolve(currentPath);
			}
			final var currentParent = Path.of(".."); //$NON-NLS-1$
			if (pathToRoot == null) {
				pathToRoot = currentParent;
			} else {
				pathToRoot = pathToRoot.resolve(currentParent);
			}
		}
		this.pathToRoot = pathToRoot == null ? Path.of(".") : pathToRoot; //$NON-NLS-1$
		var basename = element.getFileName().toString();
		if (addHtmlExtension && !basename.endsWith(".html")) { //$NON-NLS-1$
			basename += ".html"; //$NON-NLS-1$
		}
		if (relativePath == null) {
			this.relativePath = Path.of(basename);
		} else {
			this.relativePath = relativePath.resolve(basename);
		}
		this.baseUri = baseUri.toString();
	}

	/** Create the sequence of path elements.
	 *
	 * @param filename the sequence of elements.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected static void mkdirs(Path filename) throws Exception {
		final var par = filename.getParent();
		if (!Files.exists(par)) {
			Files.createDirectories(par);
		}
	}

	/** Write the HTML document into the given filename.
	 *
	 * @param filename the sequence of elements.
	 * @param htmlDocument the document to write.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected static void writeDocument(Path filename, Document htmlDocument) throws Exception {
		mkdirs(filename);
		try (var writer = Files.newBufferedWriter(filename)) {
			writer.write(htmlDocument.toString());
		}
	}

	/** Write the raw text document into the given filename.
	 *
	 * @param filename the sequence of elements.
	 * @param htmlDocument the document to write.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected static void writeDocument(Path filename, String htmlDocument) throws Exception {
		mkdirs(filename);
		try (var writer = Files.newBufferedWriter(filename)) {
			writer.write(htmlDocument.toString());
		}
	}

	/** Replies the title of the document.
	 *
	 * @param elementName the name of the element for which the document title must be replied.
	 * @return the document title.
	 */
	protected abstract String getDocumentTitleFor(String elementName);

	/** Replies the list of CSS style sheets to be considered for generated the HTML output files.
	 *
	 * @return the list of CSS style sheets.
	 */
	public Iterable<Path> getCssStylesheets() {
		return this.cssStylesheets;
	}

	/** Change the list of CSS style sheets to be considered for generated the HTML output files.
	 *
	 * @param styles is the list of CSS style sheets.
	 */
	public void setCssStylesheets(Collection<Path> styles) {
		this.cssStylesheets.clear();
		if (styles != null) {
			this.cssStylesheets.addAll(styles);
		}
	}

	/** Replies the list of Javascript scripts to be considered for generated the HTML output files.
	 *
	 * @return the list of JS scripts.
	 */
	public Iterable<Path> getJsScripts() {
		return this.jsScripts;
	}

	/** Change the list of the Javascript scripts to be considered for generated the HTML output files.
	 *
	 * @param scripts is the list of JS scripts.
	 */
	public void setJsScripts(Collection<Path> scripts) {
		this.jsScripts.clear();
		if (scripts != null) {
			this.jsScripts.addAll(scripts);
		}
	}

	/** Create a standard structure for a block tag.
	 *
	 * @param tagLocation the expected location of the tag.
	 * @param documentedElement the documented element.
	 * @param tagTrees the comments tree of the block tag.
	 * @param parent the parent receiver.
	 * @param style the CSS style to apply.
	 * @param defaultPackages the lambda that enables to apply the default packages to the typename.
	 */
	protected void createBlockTagStructure(Location tagLocation,
			javax.lang.model.element.Element documentedElement,
			List<BlockTagTree> tagTrees, Element parent, CssStyles style,
			QualifiedNameSetBuilder defaultPackages) {
		if (!tagTrees.isEmpty() ) {
			final var tagName = tagTrees.get(0).getTagName();
			final var taglet = getTagletManager().getBlockTaglet(tagLocation, tagName);
			if (taglet != null) {
				final var dtTag = getHtmlFactory().createDtTag(parent, style);
				final var ddTag = getHtmlFactory().createDdTag(parent, style);
				if (taglet instanceof SarlTaglet staglet) {
					if (staglet.isActiveTaglet(getDocletOptions())) {
						dtTag.appendText(staglet.getTagBlockLabel());
						final var extractor = new TagContentExtractor();
						staglet.appendNode(ddTag, tagTrees, documentedElement, null, style, extractor);
					}
				} else {
					dtTag.appendText(SarlTaglet.buildBlockLabel(taglet.getName()));
					var changed = false;
					final var memory = getHtmlFactory().createCommentTextMemory(ddTag, documentedElement, this);
					for (final var tagTree : tagTrees) {
						if (changed) {
							ddTag.append(","); //$NON-NLS-1$
							getHtmlFactory().createSecableSpace(ddTag);
						}
						if (getHtmlFactory().createCommentText(memory, tagTree, style)) {
							changed = true;
						}
					}
				}
			} else {
				getReporter().print(Kind.ERROR, MessageFormat.format(Messages.AbstractDocumentationGenerator_0, tagName, tagLocation));
			}
		}
	}

	@Override
	public QualifiedNameSetBuilder getQualifiedNameSetBuilder(javax.lang.model.element.Element element) {
		final var defaultPackage = getEnvironment().getElementUtils().getPackageOf(element);
		final var defaultPackages = buildCallbackForTypeFinding(defaultPackage,
				buildImportedPackageSet(element));
		return defaultPackages;
	}

	/** Replies the imported packages for the given element.
	 * This function is created for enabling the overriding in the sub classes.
	 * 
	 * @param element the element to analyze.
	 * @return the imported package set.
	 */
	protected Set<String> buildImportedPackageSet(javax.lang.model.element.Element element) {
		final var importScanner = new ImportScanner();
		importScanner.build(element);
		final var importedPackages = importScanner.getBaseSearchpath();
		return importedPackages;
	}

	/** Replies the object that build a search path for types.
	 * This function is created for enabling the overriding in the sub classes.
	 * 
	 * @param currentPackage the current package.
	 * @param importedPackages the list of imported packages.
	 * @return the type's search path builder.
	 */
	@SuppressWarnings("static-method")
	protected QualifiedNameSetBuilder buildCallbackForTypeFinding(PackageElement currentPackage, Set<String> importedPackages) {
		return new StandardQualifiedNameSetBuilder(currentPackage, importedPackages);
	}

	/** Replies if the given name is the name of a property's getter function.
	 *
	 * @param name the name to check.
	 * @return {@code true} if the given name if valid for a getter function.
	 */
	@SuppressWarnings("static-method")
	protected boolean isPropertyGetterName(String name) {
		if (!Strings.isEmpty(name)) {
			final var matcher = PROPERTY_GETTER_PATTERN.matcher(name);
			if (matcher.matches()) {
				return true;
			}
		}
		return false;
	}

	/** Replies if the given name is the name of a property's setter function.
	 *
	 * @param name the name to check.
	 * @return {@code true} if the given name if valid for a setter function.
	 */
	@SuppressWarnings("static-method")
	protected boolean isPropertySetterName(String name) {
		if (!Strings.isEmpty(name)) {
			final var matcher = PROPERTY_SETTER_PATTERN.matcher(name);
			if (matcher.matches()) {
				return true;
			}
		}
		return false;
	}

	/** Replies the property name for a given getter function name.
	 *
	 * @param getterName the name of the getter function.
	 * @return the name of the property, or {@code null} if it is not a getter function name for a property.
	 */
	@SuppressWarnings("static-method")
	protected String getterName2property(String getterName) {
		if (!Strings.isEmpty(getterName)) {
			final var matcher = PROPERTY_GETTER_PATTERN.matcher(getterName);
			if (matcher.matches()) {
				final var name = matcher.group(1);
				if (!Strings.isEmpty(name)) {
					return Strings.toFirstLower(name);
				}
			}
		}
		return null;
	}

	/** Replies the property name for a given setter function name.
	 *
	 * @param setterName the name of the setter function.
	 * @return the name of the property, or {@code null} if it is not a setter function name for a property.
	 */
	@SuppressWarnings("static-method")
	protected String setterName2property(String setterName) {
		if (!Strings.isEmpty(setterName)) {
			final var matcher = PROPERTY_SETTER_PATTERN.matcher(setterName);
			if (matcher.matches()) {
				final var name = matcher.group(1);
				if (!Strings.isEmpty(name)) {
					return Strings.toFirstLower(name);
				}
			}
		}
		return null;
	}

	/** Replies the getter function name from a property name.
	 *
	 * @param name the name of the property.
	 * @return the name of the getter function.
	 */
	@SuppressWarnings("static-method")
	protected String property2getterName(String name) {
		if (!Strings.isEmpty(name)) {
			return PROPERTY_GETTER_PREFIX + Strings.toFirstUpper(name);
		}
		return null;
	}


	/** Replies the setter function name from a property name.
	 *
	 * @param name the name of the property.
	 * @return the name of the setter function.
	 */
	@SuppressWarnings("static-method")
	protected String property2setterName(String name) {
		if (!Strings.isEmpty(name)) {
			return PROPERTY_SETTER_PREFIX + Strings.toFirstUpper(name);
		}
		return null;
	}

	/** Generate the copyright box.
	 *
	 * @param parent the container.
	 */
	protected void createCopyrightBox(Element parent) {
		final var copyrightText = getDocletOptions().getCopyrightText();
		if (!Strings.isEmpty(copyrightText)) {
			final var copyrightDiv = getHtmlFactory().createDivTag(parent, CssStyles.COPYRIGHT_BOX);
			copyrightDiv.append(Messages.AbstractDocumentationGenerator_1);
			getHtmlFactory().createSecableSpace(copyrightDiv);
			copyrightDiv.appendText(copyrightText);
		}
	}

	/** Generate a detail box.
	 * 
	 * @param <T> the type of the elements.
	 * @param boxTitle the title of the box.
	 * @param boxId the identifier of the box (for hyperlink reference).
	 * @param receiver the receiver tag.
	 * @param elements the list of elements to be included into the box.
	 * @param sorter the tool for sorting the elements.
	 * @param keyExtractor the extractor of the identifier that must be used as anchor. It may be {@code null} to avoid the generation of the ID anchors.
	 * @param titleExtractor the extractor of the text that must be used as title for an element.
	 * @param contentExtractor the extractor of the text that must be used as content for an element.
	 */
	protected <T extends javax.lang.model.element.Element> void createDetailBox(String boxTitle,
			String boxId, Element receiver,
			Iterable<? extends T> elements,
			Comparator<? super T> sorter,
			Function1<T, String> keyExtractor,
			Function1<T, Collection<? extends Node>> titleExtractor,
			Function1<T, Collection<? extends Node>> contentExtractor) {
		final var rootBox = getHtmlFactory().createDivTag(null, CssStyles.DETAIL_BOX);
		if (!Strings.isEmpty(boxId)) {
			rootBox.id(boxId);
			getNavigation().addDetailBoxAnchor(boxId);
		}
		final var btitle = getHtmlFactory().createDivTag(rootBox, CssStyles.DETAIL_BOX_TITLE);
		btitle.appendText(boxTitle);
		//
		final Iterable<? extends T> theElements;
		if (sorter != null) {
			final var sortedList = new ArrayList<T>();
			for (final var element : elements) {
				sortedList.add(element);
			}
			Collections.sort(sortedList, sorter);
			theElements = sortedList;
		} else {
			theElements = elements;
		}
		final var elementsDiv = getHtmlFactory().createDlTag(null, CssStyles.DETAIL_BOX);
		boolean hasElements = false;
		for (final var element : theElements) {
			hasElements = true;
			final Element elementTitleDiv = getHtmlFactory().createDtTag(elementsDiv, CssStyles.DETAIL_BOX);
			if (keyExtractor != null) {
				final var id = keyExtractor.apply(element);
				if (!Strings.isEmpty(id)) {
					elementTitleDiv.id(id);
				}
			}
			final var elementDescriptionDiv = getHtmlFactory().createDdTag(elementsDiv, CssStyles.DETAIL_BOX);
			elementTitleDiv.appendChildren(titleExtractor.apply(element));
			elementDescriptionDiv.appendChildren(contentExtractor.apply(element));
		}
		if (hasElements) {
			rootBox.appendChild(elementsDiv);
		}
		//
		if (hasElements) {
			receiver.appendChild(rootBox);
		}
	}

	/** Generate a summary box with single column.
	 *
	 * @param <T> the type of the elements to be in the summary.
	 * @param boxTitle the title of the box.
	 * @param tableTitle the title of the element table.
	 * @param tableColumn the title of the column.
	 * @param boxId the identifier of the box to be used for html anchors.
	 * @param receiver the HTML receiver.
	 * @param elements the list of elements.
	 * @param sorter the sorter of the elements.
	 * @param contentExtractor the extractor of content.
	 */
	protected <T extends javax.lang.model.element.Element> void createSummaryBox1(
			String boxTitle, String tableTitle, String tableColumn, String boxId,
			Element receiver, Iterable<? extends T> elements, Comparator<? super T> sorter,
			Function1<T, Collection<? extends Node>> contentExtractor) {
		createSummaryBox1(boxTitle, tableTitle, tableColumn, boxId, receiver,
				elements, sorter, contentExtractor, null);
	}

	/** Generate a summary box with single column.
	 *
	 * @param <T> the type of the elements to be in the summary.
	 * @param boxTitle the title of the box.
	 * @param tableTitle the title of the element table.
	 * @param tableColumn the title of the column.
	 * @param boxId the identifier of the box to be used for html anchors.
	 * @param receiver the HTML receiver.
	 * @param elements the list of elements.
	 * @param sorter the sorter of the elements.
	 * @param contentExtractor the extractor of content.
	 * @param inheritedExtractor the extractor of inherited elements.
	 */
	protected <T extends javax.lang.model.element.Element> void createSummaryBox1(
			String boxTitle, String tableTitle, String tableColumn, String boxId,
			Element receiver, Iterable<? extends T> elements, Comparator<? super T> sorter,
			Function1<T, Collection<? extends Node>> contentExtractor,
			Function0<Collection<? extends Node>> inheritedExtractor) {
		createSummaryBox1(boxTitle, tableColumn, boxId, receiver,
				Collections.singletonMap(tableTitle, elements), sorter, contentExtractor, null);
	}

	/** Generate a summary box with single column.
	 *
	 * @param <T> the type of the elements to be in the summary.
	 * @param boxTitle the title of the box.
	 * @param tableColumn the title of the column.
	 * @param boxId the identifier of the box to be used for html anchors.
	 * @param receiver the HTML receiver.
	 * @param elements the map of the tabs. Keys are the names of the tabs. Values are the elements.
	 * @param sorter the sorter of the elements.
	 * @param contentExtractor the extractor of content.
	 * @param inheritedExtractor the extractor of inherited elements.
	 */
	protected <T extends javax.lang.model.element.Element> void createSummaryBox1(
			String boxTitle, String tableColumn, String boxId,
			Element receiver, Map<String, Iterable<? extends T>> elements, Comparator<? super T> sorter,
			Function1<T, Collection<? extends Node>> contentExtractor,
			Function0<Collection<? extends Node>> inheritedExtractor) {
		final var rootBox = getHtmlFactory().createDivTag(null, CssStyles.SUMMARY_BOX);
		if (!Strings.isEmpty(boxId)) {
			rootBox.id(boxId);
			getNavigation().addSummaryBoxAnchor(boxId);
		}
		final var btitle = getHtmlFactory().createDivTag(rootBox, CssStyles.SUMMARY_BOX_TITLE);
		btitle.appendText(boxTitle);
		//
		final var tabsFactory = getHtmlFactory().createTabBox(CssStyles.SUMMARY_BOX_TAB_TITLE, CssStyles.SUMMARY_BOX_TAB_CONTENT);
		//
		var hasElements = false;
		for (final var tabEntry : elements.entrySet()) {
			tabsFactory.addTab(tabEntry.getKey());
			//
			final var table = getHtmlFactory().createTableTag(tabsFactory.getLastContent(), CssStyles.SUMMARY_TABLE);
			final var tableHeader = getHtmlFactory().createTableHeaderTag(table, CssStyles.SUMMARY_TABLE);
			final var tableHeaderRow = getHtmlFactory().createTableRowTag(tableHeader, CssStyles.SUMMARY_TABLE);
			final var tableColumnHeader0 = getHtmlFactory().createTableColumnHeadTag(tableHeaderRow, CssStyles.SUMMARY_TABLE);
			tableColumnHeader0.appendText(tableColumn);
			//
			final var tableBody = getHtmlFactory().createTableBodyTag(table, CssStyles.SUMMARY_TABLE);
			final Iterable<? extends T> theElements;
			if (sorter != null) {
				final var sortedList = new ArrayList<T>();
				for (final var element : tabEntry.getValue()) {
					sortedList.add(element);
				}
				Collections.sort(sortedList, sorter);
				theElements = sortedList;
			} else {
				theElements = tabEntry.getValue();
			}
			var hasTabElements = false;
			for (final var element : theElements) {
				hasElements = true;
				hasTabElements = true;
				final var tableRow = getHtmlFactory().createTableRowTag(tableBody, CssStyles.SUMMARY_TABLE);
				final var typeCell = getHtmlFactory().createTableCellTag(tableRow, CssStyles.SUMMARY_TABLE);
				final var typeText = contentExtractor.apply(element);
				typeCell.appendChildren(typeText);
			}
			if (!hasTabElements) {
				tabsFactory.removeLastTab();
			}
		}
		if (hasElements) {
			tabsFactory.createSelectors(rootBox);
			tabsFactory.createContents(rootBox);
		}
		//
		if (inheritedExtractor != null) {
			final var inheritedNodes = inheritedExtractor.apply();
			if (inheritedNodes != null && !inheritedNodes.isEmpty()) {
				hasElements = true;
				final var inheritedDiv = getHtmlFactory().createDivTag(rootBox, CssStyles.SUMMARY_BOX_INHERITED);
				final var inheritedSpan = getHtmlFactory().createSpanTag(inheritedDiv, CssStyles.SUMMARY_BOX_INHERITED);
				inheritedSpan.appendText(Messages.AbstractDocumentationGenerator_2);
				getHtmlFactory().createSecableSpace(inheritedSpan);
				inheritedDiv.appendChildren(inheritedNodes);
			}
		}
		//
		if (hasElements) {
			receiver.appendChild(rootBox);
		}
	}

	/** Generate a summary box with two columns.
	 *
	 * @param <T> the type of the elements to be in the summary.
	 * @param boxTitle the title of the box.
	 * @param tableTitle the title of the element table.
	 * @param tableTypeColumn the title of the type column.
	 * @param tableDescriptionColumn the title of the description column.
	 * @param boxId the identifier of the box to be used for html anchors.
	 * @param receiver the HTML receiver.
	 * @param elements the list of elements.
	 * @param sorter the sorter of the elements.
	 * @param typeExtractor the extractor of type string.
	 * @param descriptionExtractor the extractor of type description.
	 */
	protected <T extends javax.lang.model.element.Element> void createSummaryBox2(
			String boxTitle, String tableTitle, String tableTypeColumn, String tableDescriptionColumn,
			String boxId, Element receiver, Iterable<? extends T> elements, Comparator<? super T> sorter,
			Function1<T, Collection<? extends Node>> typeExtractor,
			Function1<T, Collection<? extends Node>> descriptionExtractor) {
		createSummaryBox2(boxTitle, tableTitle, tableTypeColumn, tableDescriptionColumn,
				boxId, receiver, elements, sorter, typeExtractor, descriptionExtractor, null);
	}

	/** Generate a summary box with two columns.
	 *
	 * @param <T> the type of the elements to be in the summary.
	 * @param boxTitle the title of the box.
	 * @param tableTitle the title of the element table.
	 * @param tableTypeColumn the title of the type column.
	 * @param tableDescriptionColumn the title of the description column.
	 * @param boxId the identifier of the box to be used for html anchors.
	 * @param receiver the HTML receiver.
	 * @param elements the list of elements.
	 * @param sorter the sorter of the elements.
	 * @param typeExtractor the extractor of type string.
	 * @param descriptionExtractor the extractor of type description.
	 * @param inheritedExtractor the extractor of inherited elements.
	 */
	protected <T extends javax.lang.model.element.Element> void createSummaryBox2(
			String boxTitle, String tableTitle, String tableTypeColumn, String tableDescriptionColumn,
			String boxId, Element receiver, Iterable<? extends T> elements, Comparator<? super T> sorter,
			Function1<T, Collection<? extends Node>> typeExtractor,
			Function1<T, Collection<? extends Node>> descriptionExtractor,
			Function0<Collection<? extends Node>> inheritedExtractor) {
		final var tabElements = Collections.<String, Iterable<? extends T>>singletonMap(tableTitle, elements);
		createSummaryBox2(boxTitle, tableTypeColumn, tableDescriptionColumn, boxId, receiver,
				tabElements, sorter, typeExtractor, descriptionExtractor, inheritedExtractor);
	}

	/** Generate a summary box with two columns.
	 *
	 * @param <T> the type of the elements to be in the summary.
	 * @param boxTitle the title of the box.
	 * @param tableTypeColumn the title of the type column.
	 * @param tableDescriptionColumn the title of the description column.
	 * @param boxId the identifier of the box to be used for html anchors.
	 * @param receiver the HTML receiver.
	 * @param elements the list of elements. The keys of the map are the title of the tabs; the values are the elements to show in a tab.
	 * @param sorter the sorter of the elements.
	 * @param typeExtractor the extractor of type string.
	 * @param descriptionExtractor the extractor of type description.
	 * @param inheritedExtractor the extractor of inherited elements.
	 */
	protected <T extends javax.lang.model.element.Element> void createSummaryBox2(
			String boxTitle, String tableTypeColumn, String tableDescriptionColumn,
			String boxId, Element receiver, Map<String, Iterable<? extends T>> elements,
			Comparator<? super T> sorter,
			Function1<T, Collection<? extends Node>> typeExtractor,
			Function1<T, Collection<? extends Node>> descriptionExtractor,
			Function0<Collection<? extends Node>> inheritedExtractor) {
		final var rootBox = getHtmlFactory().createDivTag(null, CssStyles.SUMMARY_BOX);
		if (!Strings.isEmpty(boxId)) {
			rootBox.id(boxId);
			getNavigation().addSummaryBoxAnchor(boxId);
		}
		final var btitle = getHtmlFactory().createDivTag(rootBox, CssStyles.SUMMARY_BOX_TITLE);
		btitle.appendText(boxTitle);
		//
		final var tabsFactory = getHtmlFactory().createTabBox(CssStyles.SUMMARY_BOX_TAB_TITLE, CssStyles.SUMMARY_BOX_TAB_CONTENT);
		//
		var hasElements = false;
		for (final var tabEntry : elements.entrySet()) {
			tabsFactory.addTab(tabEntry.getKey());
			//
			final var table = getHtmlFactory().createTableTag(tabsFactory.getLastContent(), CssStyles.SUMMARY_TABLE);
			final var tableHeader = getHtmlFactory().createTableHeaderTag(table, CssStyles.SUMMARY_TABLE);
			final var tableHeaderRow = getHtmlFactory().createTableRowTag(tableHeader, CssStyles.SUMMARY_TABLE);
			final var tableColumnHeader0 = getHtmlFactory().createTableColumnHeadTag(tableHeaderRow, CssStyles.SUMMARY_TABLE);
			tableColumnHeader0.appendText(tableTypeColumn);
			final var tableColumnHeader1 = getHtmlFactory().createTableColumnHeadTag(tableHeaderRow, CssStyles.SUMMARY_TABLE);
			tableColumnHeader1.appendText(tableDescriptionColumn);
			//
			final var tableBody = getHtmlFactory().createTableBodyTag(table, CssStyles.SUMMARY_TABLE);
			final Iterable<? extends T> theElements;
			if (sorter != null) {
				final var sortedList = new ArrayList<T>();
				for (final var element : tabEntry.getValue()) {
					sortedList.add(element);
				}
				Collections.sort(sortedList, sorter);
				theElements = sortedList;
			} else {
				theElements = tabEntry.getValue();
			}
			var hasTabElements = false;
			for (final var element : theElements) {
				hasElements = true;
				hasTabElements = true;
				final var tableRow = getHtmlFactory().createTableRowTag(tableBody, CssStyles.SUMMARY_TABLE);
				final var typeCell = getHtmlFactory().createTableCellTag(tableRow, CssStyles.SUMMARY_TABLE);
				final var typeText = typeExtractor.apply(element);
				typeCell.appendChildren(typeText);
				final var descriptionCell = getHtmlFactory().createTableCellTag(tableRow, CssStyles.SUMMARY_TABLE);
				final var descriptionText = descriptionExtractor.apply(element);
				descriptionCell.appendChildren(descriptionText);
			}
			if (!hasTabElements) {
				tabsFactory.removeLastTab();
			}
		}
		if (hasElements) {
			tabsFactory.createSelectors(rootBox);
			tabsFactory.createContents(rootBox);
		}
		if (inheritedExtractor != null) {
			final var inheritedNodes = inheritedExtractor.apply();
			if (inheritedNodes != null && !inheritedNodes.isEmpty()) {
				hasElements = true;
				final var inheritedDiv = getHtmlFactory().createDivTag(rootBox, CssStyles.SUMMARY_BOX_INHERITED);
				final var inheritedSpan = getHtmlFactory().createSpanTag(inheritedDiv, CssStyles.SUMMARY_BOX_INHERITED);
				inheritedSpan.appendText(Messages.AbstractDocumentationGenerator_2);
				getHtmlFactory().createSecableSpace(inheritedSpan);
				var first = true;
				for (final var node : inheritedNodes) {
					if (first) {
						first = false;
					} else {
						inheritedDiv.appendText(", "); //$NON-NLS-1$
					}
					inheritedDiv.appendChild(node);
				}
			}
		}
		//
		if (hasElements) {
			receiver.appendChild(rootBox);
		}
	}

	/** Replies the documentation title.
	 *
	 * @return the documentation title.
	 */
	protected String getDocumentationTitle() {
		final var cli = getDocletOptions().getTitle();
		if (!Strings.isEmpty(cli)) {
			return cli;
		}
		return Messages.AbstractDocumentationGenerator_3;
	}

	/** Generate the short deprecation message for the given element.
	 *
	 * @param element the element.
	 * @param nodes the receiving array for the created nodes.
	 * @param addTitle indicates if the prefix title must be added.
	 */
	protected void createShortDeprecationMessage(javax.lang.model.element.Element element, List<Node> nodes, boolean addTitle) {
		final var deprecatedElement = getElementUtils().getFirstEnclosingDeprecatedElement(element);
		if (deprecatedElement != null) {
			final var deprecatedDiv = getHtmlFactory().createDivTag(null, CssStyles.DEPRECATION_INFO);
			final var isForRemoval = getElementUtils().isDeprecatedForRemoval(deprecatedElement);
			final var since = getElementUtils().getDeprecatedSince(deprecatedElement);
			if (addTitle) {
				final var prefix = getHtmlFactory().createSpanTag(null, CssStyles.DEPRECATION_INFO_TITLE);
				if (Strings.isEmpty(since)) {
					if (isForRemoval) {
						prefix.appendText(Messages.AbstractDocumentationGenerator_4);
					} else {
						prefix.appendText(Messages.AbstractDocumentationGenerator_5);
					}
				} else if (isForRemoval) {
					prefix.appendText(MessageFormat.format(Messages.AbstractDocumentationGenerator_6, since));
				} else {
					prefix.appendText(MessageFormat.format(Messages.AbstractDocumentationGenerator_7, since));
				}
				deprecatedDiv.appendChild(prefix);
				deprecatedDiv.appendChild(getHtmlFactory().createSecableSpace(null));
			}
			final var deprs = getDocUtils().getBlockTags(deprecatedElement, DocTree.Kind.DEPRECATED, getEnvironment());
			if (!deprs.isEmpty()) {
				for (final var comment : deprs) {
					final var text = getDocUtils().getCommentForDeprecatedTag(comment);
					if (!text.isEmpty()) {
						final var memory = getHtmlFactory().createCommentTextMemory(deprecatedDiv, deprecatedElement, this);
						for (final var tree : text) {
							getHtmlFactory().createCommentText(memory, tree, CssStyles.DEPRECATION_INFO);
						}
					}
				}
			}
			if (!addTitle && (isForRemoval || !Strings.isEmpty(since))) {
				getHtmlFactory().createSecableSpace(deprecatedDiv);
				if (isForRemoval) {
					if (Strings.isEmpty(since)) {
						deprecatedDiv.appendText(Messages.AbstractDocumentationGenerator_8);
					} else {
						deprecatedDiv.appendText(MessageFormat.format(Messages.AbstractDocumentationGenerator_9, since));
					}
				} else {
					deprecatedDiv.appendText(MessageFormat.format(Messages.AbstractDocumentationGenerator_10, since));
				}
			}
			nodes.add(deprecatedDiv);
		}
	}

	/** Generate the first sentence of the element's description.
	 *
	 * @param element the element.
	 * @param nodes the receiving array for the created nodes.
	 * @param newLine indicates if a new-line entity must be added before the description.
	 * @param div indicates if the description must be enclosed by a {@code <div/>}.
	 */
	protected void createFirstSentence(javax.lang.model.element.Element element, List<Node> nodes, boolean newLine, boolean div) {
		final var env = getEnvironment();
		final var docTree = env.getDocTrees().getDocCommentTree(element);
		if (docTree != null) {
			final var description = docTree.getFirstSentence();
			if (description != null && !description.isEmpty()) {
				if (newLine) {
					nodes.add(getHtmlFactory().createNewLineTag());
				}
				if (div) {
					final var container = getHtmlFactory().createDivTag(null, null);
					nodes.add(container);
					final var memory = getHtmlFactory().createCommentTextMemory(container, element, this);
					for (final var docTreeElement : description) {
						getHtmlFactory().createCommentText(memory, docTreeElement, null);
					}
				} else {
					final var container = getHtmlFactory().createSpanTag(null, null);
					final var memory = getHtmlFactory().createCommentTextMemory(container, element, this);
					for (final var docTreeElement : description) {
						getHtmlFactory().createCommentText(memory, docTreeElement, null);
					}
					nodes.addAll(container.childNodes());
				}
			}
		}
	}

	/** Generate the full body of the element's description.
	 *
	 * @param element the element.
	 * @param nodes the receiving array for the created nodes.
	 * @param newLine indicates if a new-line entity must be added before the description.
	 * @param div indicates if the description must be enclosed by a {@code <div/>}.
	 */
	protected void createFullDescriptionBody(javax.lang.model.element.Element element, List<Node> nodes, boolean newLine, boolean div) {
		final var env = getEnvironment();
		final var docTree = env.getDocTrees().getDocCommentTree(element);
		if (docTree != null) {
			final var description = docTree.getFullBody();
			if (description != null && !description.isEmpty()) {
				if (newLine) {
					nodes.add(getHtmlFactory().createNewLineTag());
				}
				if (div) {
					final var container = getHtmlFactory().createDivTag(null, null);
					nodes.add(container);
					final var memory = getHtmlFactory().createCommentTextMemory(container, element, this);
					for (final var docTreeElement : description) {
						getHtmlFactory().createCommentText(memory, docTreeElement, null);
					}
				} else {
					final var container = getHtmlFactory().createSpanTag(null, null);
					final var memory = getHtmlFactory().createCommentTextMemory(container, element, this);
					for (final var docTreeElement : description) {
						getHtmlFactory().createCommentText(memory, docTreeElement, null);
					}
					nodes.addAll(container.childNodes());
				}
			}
		}
	}

	/** Generate the block tags for the given element.
	 *
	 * @param element the element.
	 * @param nodes the receiving array for the created nodes.
	 * @param tagLocation the location of the block tags.
	 * @param style the CSS style to use.
	 */
	protected void createBlockTagsFor(javax.lang.model.element.Element element, List<Node> nodes, Location tagLocation, CssStyles style) {
		final var dlTag = getHtmlFactory().createDlTag(null, style);
		createTagInfo(dlTag, element, tagLocation, style);
		if (dlTag.childNodeSize() > 0) {
			nodes.add(dlTag);
		}
	}

	/** Generate the documentation tags for a given element.
	 *
	 * @param dlTag is the container element.
	 * @param element is the element for which the tags info must be generated.
	 * @param tagLocation is the expected location of the tag.
	 * @param style the CSS style.
	 */
	protected void createTagInfo(Element dlTag, javax.lang.model.element.Element element, Location tagLocation, CssStyles style) {
		final var comments = getDocUtils().getBlockTags(element, getEnvironment());
		if (!comments.isEmpty()) {
			final var currentPackage = getEnvironment().getElementUtils().getPackageOf(element);
			//
			final var defaultPackages = buildCallbackForTypeFinding(currentPackage,
					buildImportedPackageSet(element));
			// Build the groups of block tags
			final var groups = new TreeMap<String, List<BlockTagTree>>(getBlockTagSorter());
			for (final var comment : comments) {
				final var tagName = comment.getTagName().toLowerCase();
				final var list = groups.computeIfAbsent(tagName, it -> new ArrayList<>());
				if (!ExcludeFromApidocTaglet.TAG_NAME.equalsIgnoreCase(tagName)
					&& !HiddenTaglet.TAG_NAME.equalsIgnoreCase(tagName)) {
					list.add(comment);
				}
			}
			// Generate the block tags
			for (final var group : groups.values()) {
				createBlockTagStructure(tagLocation, element, group, dlTag, style, defaultPackages);
			}
		}
	}

	/** Implementation of an extractor for the tag content.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	protected class TagContentExtractor implements HtmlFactoryContentExtractor {

		/** Constructor.
		 */
		protected TagContentExtractor() {
			//
		}

		@Override
		public HtmlFactoryContext getContext() {
			return AbstractDocumentationGenerator.this;
		}

		@Override
		public javax.lang.model.element.Element extractReferencedElement(DocTree docNode) {
			throw new UnsupportedOperationException();
		}

		@Override
		public List<Node> extractReference(DocTree docNode, List<Node> label, boolean isplain) {
			throw new UnsupportedOperationException();
		}

		@Override
		public Element extractSimpleText(List<? extends DocTree> text) {
			throw new UnsupportedOperationException();
		}
		
	}

	/** Scanner of import statements from a Java code.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	protected class ImportScanner extends ElementScanner9<Void, Void> {

		private Set<String> searchpath = new TreeSet<>();

		private void addQN(String fullyQualifiedName) {
			if (!Strings.isEmpty(fullyQualifiedName)) {
				final var index = fullyQualifiedName.lastIndexOf('.');
				if (index >= 1) {
					final var pkg = fullyQualifiedName.substring(0, index);
					this.searchpath.add(pkg);
				}
			}
		}

		private void add(TypeElement type) {
			if (type != null) {
				addQN(type.getQualifiedName().toString());
			}
		}

		private void add(TypeMirror mirror) {
			if (mirror != null) {
				add(getElementUtils().asTypeElement(mirror, getEnvironment().getTypeUtils()));
			}
		}

		/** Build the list of packages from the given element.
		 * This function clear any previously registered package.
		 * It explores the enclosing type declaration of the given element.
		 *
		 * @param element the element to analyze
		 */
		public void build(javax.lang.model.element.Element element) {
			final TypeElement type;
			if (element instanceof TypeElement cvalue) {
				type = cvalue;
			} else {
				type = getElementUtils().getEnclosingTypeElement(element);
			}
			// Add the current type
			add(type);
			// Scan the type and its content
			javax.lang.model.element.Element elt;
			if (type != null) {
				scan(type, null);
				elt =  type.getEnclosingElement();
			} else {
				scan(element, null);
				elt = null;
			}
			// Add the enclosing types
			while (elt != null) {
				if (elt instanceof TypeElement te) {
					addQN(te.getQualifiedName().toString());
					elt = te.getEnclosingElement();
				} else {
					elt = null;
				}
			}
		}

		/** Replies the collection of imported packages (not the types).
		 *
		 * @return the imported packages.
		 */
		public Set<String> getBaseSearchpath() {
			return this.searchpath;
		}

		@Override
		public Void visitType(TypeElement e, Void p) {
			for( final var interfaceType : e.getInterfaces()) {
				add(interfaceType);
			}
			add(e.getSuperclass());
			return super.visitType(e, p);
		}

		@Override
		public Void visitExecutable(ExecutableElement e, Void p) {
			if(e.getReturnType().getKind() == TypeKind.DECLARED) {
				add(e.getReturnType());
			}
			for (final var tm : e.getThrownTypes()) {
				if(tm.getKind() == TypeKind.DECLARED) {
					add(tm);
				}
			}
			return super.visitExecutable(e, p);
		}

		@Override
		public Void visitTypeParameter(TypeParameterElement e, Void p) {
			if(e.asType().getKind() == TypeKind.DECLARED) {
				add(e.asType());
			}
			return super.visitTypeParameter(e, p);
		}

		@Override
		public Void visitVariable(VariableElement e, Void p) {
			if(e.asType().getKind() == TypeKind.DECLARED) {
				add(e.asType());
			}
			return super.visitVariable(e, p);
		}

		@Override
		public Void visitRecordComponent(RecordComponentElement e, Void p) {
			if(e.asType().getKind() == TypeKind.DECLARED) {
				add(e.asType());
			}
			return scan(e.getEnclosedElements(), p);
		}

	}

}
