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

package io.sarl.docs.doclet2.html.framework;

import static com.sun.source.doctree.DocTree.Kind.CODE;

import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Pattern;

import javax.inject.Inject;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.ArrayType;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.SimpleAnnotationValueVisitor9;
import javax.lang.model.util.SimpleTypeVisitor9;
import javax.tools.Diagnostic;
import javax.tools.Diagnostic.Kind;
import javax.tools.JavaFileObject;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.sun.source.doctree.AttributeTree;
import com.sun.source.doctree.AttributeTree.ValueKind;
import com.sun.source.doctree.AuthorTree;
import com.sun.source.doctree.CommentTree;
import com.sun.source.doctree.DeprecatedTree;
import com.sun.source.doctree.DocCommentTree;
import com.sun.source.doctree.DocRootTree;
import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.DocTreeVisitor;
import com.sun.source.doctree.EndElementTree;
import com.sun.source.doctree.EntityTree;
import com.sun.source.doctree.ErroneousTree;
import com.sun.source.doctree.IdentifierTree;
import com.sun.source.doctree.InheritDocTree;
import com.sun.source.doctree.LinkTree;
import com.sun.source.doctree.LiteralTree;
import com.sun.source.doctree.ParamTree;
import com.sun.source.doctree.ReferenceTree;
import com.sun.source.doctree.ReturnTree;
import com.sun.source.doctree.SeeTree;
import com.sun.source.doctree.SerialDataTree;
import com.sun.source.doctree.SerialFieldTree;
import com.sun.source.doctree.SerialTree;
import com.sun.source.doctree.SinceTree;
import com.sun.source.doctree.StartElementTree;
import com.sun.source.doctree.TextTree;
import com.sun.source.doctree.ThrowsTree;
import com.sun.source.doctree.UnknownBlockTagTree;
import com.sun.source.doctree.UnknownInlineTagTree;
import com.sun.source.doctree.ValueTree;
import com.sun.source.doctree.VersionTree;
import jdk.javadoc.doclet.Taglet;
import jdk.javadoc.doclet.Taglet.Location;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure0;
import org.jsoup.nodes.Comment;
import org.jsoup.nodes.DataNode;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.DocumentType;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;
import org.jsoup.nodes.TextNode;

import io.sarl.docs.doclet2.framework.ElementUtils;
import io.sarl.docs.doclet2.framework.ExternalLinkManager;
import io.sarl.docs.doclet2.framework.TagletManager;
import io.sarl.docs.doclet2.html.taglets.SarlTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.CodeTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.DocRootTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.InheritDocTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.LinkTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.LiteralTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.ValueTaglet;
import io.sarl.lang.core.annotation.DefaultValue;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Builder of HTML elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class HtmlFactoryImpl implements HtmlFactory, HtmlTags {

	private static final String SPACE = " "; //$NON-NLS-1$

	private HtmlAccessor accessor;

	private PathBuilder pathBuilder;

	private ElementUtils elementUtils;

	private ExternalLinkManager externalLinkManager;

	private TagletManager tagletManager;

	private SARLGrammarKeywordAccess keywords;

	private long nextId = 1;

	private long nextTabContentClass = 1;

	private long nextTabButtonClass = 1;

	/** Compute an unique ID.
	 *
	 * @return the string representation of the ID.
	 */
	protected String computeId() {
		final long id = this.nextId;
		++this.nextId;
		return "ID" + id; //$NON-NLS-1$
	}

	/** Compute an unique tab content class name.
	 *
	 * @return the string representation of the class name.
	 */
	protected String computeTabContentClassname() {
		final long id = this.nextTabContentClass;
		++this.nextTabContentClass;
		return "doclet-tabs-content-instance-" + id; //$NON-NLS-1$
	}

	/** Compute an unique tab button class name.
	 *
	 * @return the string representation of the class name.
	 */
	protected String computeTabButtonClassname() {
		final long id = this.nextTabButtonClass;
		++this.nextTabButtonClass;
		return "doclet-tabs-button-instance-" + id; //$NON-NLS-1$
	}

	/** Change the taglet manager.
	 *
	 * @param manager the manager.
	 */
	@Inject
	public void setTagletManager(TagletManager manager) {
		this.tagletManager = manager;
	}

	/** Replies the taglet manager.
	 *
	 * @return the manager.
	 */
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

	/** Change the external link manager.
	 *
	 * @param manager the manager.
	 */
	@Inject
	public void setExternalLinkManager(ExternalLinkManager manager) {
		this.externalLinkManager = manager;
	}

	/** Replies the external link manager.
	 *
	 * @return the manager.
	 */
	public ExternalLinkManager getExternalLinkManager() {
		return this.externalLinkManager;
	}

	/** Change the element utilities.
	 *
	 * @param utils the element utilities.
	 */
	@Inject
	public void setElementUtils(ElementUtils utils) {
		this.elementUtils = utils;
	}

	/** Replies the element utilities.
	 *
	 * @return the utilities.
	 */
	public ElementUtils getElementUtils() {
		return this.elementUtils;
	}

	/** Change the HTML accessor.
	 *
	 * @param accessor the HTML accessor.
	 */
	@Inject
	public void setHtmlAccessor(HtmlAccessor accessor) {
		this.accessor = accessor;
	}

	/** Replies the HTML accessor.
	 *
	 * @return the accessor.
	 */
	public HtmlAccessor getHtmlAccessor() {
		return this.accessor;
	}

	/** Change the builder of paths.
	 *
	 * @param builder the builder.
	 */
	@Inject
	public void setPathBuilder(PathBuilder builder) {
		this.pathBuilder = builder;
	}

	/** Replies the builder of paths.
	 *
	 * @return the builder.
	 */
	public PathBuilder getPathBuilder() {
		return this.pathBuilder;
	}

	@Override
	public DocumentType createDocumentType(String name, String publicId, String systemId) {
		if (!Strings.isNullOrEmpty(name) && !Strings.isNullOrEmpty(publicId) && !Strings.isNullOrEmpty(systemId)) {
			final DocumentType type = new DocumentType(name, publicId, systemId);
			return type;
		}
		return null;
	}

	@Override
	public Document createDocument(DocumentType docType, HtmlFactoryContext context) {
		Document doc = Document.createShell(context.getBaseUri());
		//
		if (docType != null) {
			doc.insertChildren(0, docType);
		} else {
			doc.insertChildren(0, new DocumentType("html", "", ""));
		}
		//
		Element htmlTag = getHtmlAccessor().getChildNode(doc, HTML_TAG);
		if (htmlTag == null) {
			htmlTag = doc.appendElement(HTML_TAG);
		}
		htmlTag.attr(LANG_ATTR, DEFAULT_LANG_ATTR_VALUE);
		//
		Element headTag = getHtmlAccessor().getChildNode(htmlTag, HEAD_TAG);
		if (headTag == null) {
			headTag = htmlTag.appendElement(HEAD_TAG);
		}
		//
		Element metaTag = getHtmlAccessor().getChildNode(headTag, META_TAG);
		if (metaTag == null) {
			metaTag = headTag.appendElement(META_TAG);
		}
		metaTag.attr(CHARSET_ATTR, Charset.defaultCharset().name());
		return doc;
	}

	@Override
	public Element createHeadTag(Element parent) {
		if (parent == null) {
			return new Element(HEAD_TAG);
		}
		Element child = getHtmlAccessor().getChildNode(parent, HEAD_TAG);
		if (child == null) {
			child = parent.appendElement(HEAD_TAG);
		}
		return child;
	}

	@Override
	public Element createTitleTag(Element parent, String title) {
		Element child;
		if (parent == null) {
			child = new Element(TITLE_TAG);
		} else {
			child = getHtmlAccessor().getChildNode(parent, TITLE_TAG);
			if (child == null) {
				child = parent.appendElement(TITLE_TAG);
			}
		}
		child.appendText(title);
		return child;
	}

	@Override
	public String path2UrlPath(Path path, String anchor) {
		final StringBuilder refPath = new StringBuilder();
		if (path != null) {
			final Iterator<Path> iterator = path.iterator();
			while (iterator.hasNext()) {
				final Path elt = iterator.next();
				if (refPath.length() > 0) {
					refPath.append("/"); //$NON-NLS-1$
				}
				refPath.append(elt.toString());
			}
			if (path.isAbsolute()) {
				refPath.insert(0, "/"); //$NON-NLS-1$
			}
		}
		if (!Strings.isNullOrEmpty(anchor)) {
			refPath.append("#"); //$NON-NLS-1$
			refPath.append(anchor);
		}
		return refPath.toString();
	}

	@Override
	public Element createCssLinkTag(Element parent, Path cssStyle) {
		final Element linkElement;
		if (parent == null) {
			linkElement = new Element(LINK_TAG);
		} else {
			linkElement = parent.appendElement(LINK_TAG);
		}
		linkElement.attr(REL_ATTR, REL_ATTR_VALUE);
		linkElement.attr(HREF_ATTR, path2UrlPath(cssStyle));
		return linkElement;
	}
	
	@Override
	public Element createJsLinkTag(Element parent, Path jsScript) {
		final Element scriptElement;
		if (parent == null) {
			scriptElement = new Element(SCRIPT_TAG);
		} else {
			scriptElement = parent.appendElement(SCRIPT_TAG);
		}
		scriptElement.attr(SRC_ATTR, path2UrlPath(jsScript));
		return scriptElement;
	}

	@Override
	public Element createBodyTag(Element parent) {
		Element child;
		if (parent == null) {
			child = new Element(BODY_TAG);
		} else {
			child = getHtmlAccessor().getChildNode(parent, BODY_TAG);
			if (child == null) {
				child = parent.appendElement(BODY_TAG);
			}
		}
		return child;
	}

	@Override
	public Element createFramesetTag(Element parent) {
		Element child;
		if (parent == null) {
			child = new Element(FRAMESET_TAG);
		} else {
			child = parent.appendElement(FRAMESET_TAG);
		}
		return child;
	}

	@Override
	public Element createFrameTag(Element parent) {
		Element child;
		if (parent == null) {
			child = new Element(FRAME_TAG);
		} else {
			child = parent.appendElement(FRAME_TAG);
		}
		return child;
	}

	@Override
	public Element createNoFramesTag(Element parent) {
		Element child;
		if (parent == null) {
			child = new Element(NOFRAMES_TAG);
		} else {
			child = parent.appendElement(NOFRAMES_TAG);
		}
		return child;
	}

	@Override
	public Element createScriptTag(Element parent, String type) {
		Element child;
		if (parent == null) {
			child = new Element(SCRIPT_TAG);
		} else {
			child = parent.appendElement(SCRIPT_TAG);
		}
		if (!Strings.isNullOrEmpty(type)) {
			child.attr(TYPE_ATTR, type);
		}
		return child;
	}

	@Override
	public Element createNoScriptTag(Element parent) {
		Element child;
		if (parent == null) {
			child = new Element(NOSCRIPT_TAG);
		} else {
			child = parent.appendElement(NOSCRIPT_TAG);
		}
		return child;
	}

	@Override
	public Element createH2Tag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(H2_TAG);
		} else {
			elt = parent.appendElement(H2_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createDivTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(DIV_TAG);
		} else {
			elt = parent.appendElement(DIV_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createParagraphTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(P_TAG);
		} else {
			elt = parent.appendElement(P_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createDlTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(DL_TAG);
		} else {
			elt = parent.appendElement(DL_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createDtTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(DT_TAG);
		} else {
			elt = parent.appendElement(DT_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createDdTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(DD_TAG);
		} else {
			elt = parent.appendElement(DD_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createSpanTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(SPAN_TAG);
		} else {
			elt = parent.appendElement(SPAN_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createPreTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(PRE_TAG);
		} else {
			elt = parent.appendElement(PRE_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createUlTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(UL_TAG);
		} else {
			elt = parent.appendElement(UL_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createLiTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(LI_TAG);
		} else {
			elt = parent.appendElement(LI_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Node createUnsecableSpace(Element parent) {
		if (parent == null) {
			return new DataNode(SPACE_ENTITY);
		}
		return parent.append(SPACE_ENTITY);
	}

	@Override
	public Node createSecableSpace(Element parent) {
		if (parent == null) {
			return new TextNode(" "); //$NON-NLS-1$
		}
		return parent.appendText(" "); //$NON-NLS-1$
	}

	/** Append the given child to the given parent. This function ignores the pseudo tags
	 * and adds the children of a pseudo tag directly (not the pseudo tag itself).
	 *
	 * @param parent the receiver.
	 * @param child the node to add into the parent node.
	 * @see #appendChildren(Element, Iterable)
	 */
	protected void appendChildren(Element parent, Node child) {
		assert parent != null : "parent argument must not be null";
		assert child != null : "child argument must not be null";
		if (HtmlTags.isPseudoTag(child.nodeName()) && child instanceof Element) {
			for (final Node chld : child.childNodes()) {
				parent.appendChild(chld.clone());
			}
		} else {
			parent.appendChild(child.clone());
		}
	}

	/** Append the given child to the given parent. This function ignores the pseudo tags
	 * and adds the children of a pseudo tag directly (not the pseudo tag itself).
	 *
	 * @param parent the receiver.
	 * @param children the nodes to add into the parent node.
	 * @see #appendChildren(Element, Node)
	 */
	protected void appendChildren(Element parent, Iterable<? extends Node> children) {
		assert parent != null : "parent argument must not be null";
		assert children != null : "children argument must not be null";
		for (final Node child : children) {
			if (children != null) {
				appendChildren(parent, child);
			}
		}
	}

	@Override
	public List<Node> createLink(Path path, String anchor, List<Node> label, CssStyles style) {
		final Element tag = new Element(A_TAG);
		tag.attr(HREF_ATTR, path2UrlPath(path, anchor));
		appendChildren(tag, label);
		if (style != null) {
			tag.addClass(style.getCssClassname());
		}
		final List<Node> list = new ArrayList<>();
		list.add(tag);
		return list;
	}

	@Override
	public List<Node> createLink(Path path, String anchor, String label, CssStyles style) {
		final Element tag = new Element(A_TAG);
		tag.attr(HREF_ATTR, path2UrlPath(path, anchor));
		tag.appendText(label);
		if (style != null) {
			tag.addClass(style.getCssClassname());
		}
		final List<Node> list = new ArrayList<>();
		list.add(tag);
		return list;
	}

	@Override
	public List<Node> createLink(URL path, List<Node> label, CssStyles style) {
		final Element tag = new Element(A_TAG);
		tag.attr(HREF_ATTR, path.toExternalForm());
		appendChildren(tag, label);
		if (style != null) {
			tag.addClass(style.getCssClassname());
		}
		final List<Node> list = new ArrayList<>();
		list.add(tag);
		return list;
	}

	@Override
	public List<Node> createLink(URL path, String label, CssStyles style) {
		final Element tag = new Element(A_TAG);
		tag.attr(HREF_ATTR, path.toExternalForm());
		tag.appendText(label);
		if (style != null) {
			tag.addClass(style.getCssClassname());
		}
		final List<Node> list = new ArrayList<>();
		list.add(tag);
		return list;
	}

	@Override
	public List<Node> createModuleLink(ModuleElement module, List<Node> label, CssStyles style, HtmlFactoryContext context) {
		final boolean included = context.getEnvironment().isIncluded(module);
		if (included) {
			final Path modulePath = getPathBuilder().moduleSummary(module);
			final Path linkModulePath = context.getPathToRoot().resolve(modulePath);
			return createLink(linkModulePath, label, style);
		}
		return label;
	}

	@Override
	public List<Node> createModuleLink(ModuleElement module, String label, CssStyles style, HtmlFactoryContext context) {
		final boolean included = context.getEnvironment().isIncluded(module);
		if (included) {
			final String rlabel;
			if (label == null || label.isBlank()) {
				rlabel = Messages.HtmlFactoryImpl_0;
			} else {
				rlabel = label;
			}
			final Path modulePath = getPathBuilder().moduleSummary(module);
			final Path linkModulePath = context.getPathToRoot().resolve(modulePath);
			return createLink(linkModulePath, rlabel, style);
		}
		return new ArrayList<>();
	}

	@Override
	public List<Node> createPackageLink(PackageElement pkg, List<Node> label, CssStyles style, HtmlFactoryContext context) {
		final boolean included = context.getEnvironment().isIncluded(pkg);
		if (included) {
			final Path packagePath = getPathBuilder().packageSummary(pkg);
			final Path linkPackagePath = context.getPathToRoot().resolve(packagePath);
			return createLink(linkPackagePath, label, style);
		}
		return label;
	}

	@Override
	public List<Node> createPackageLink(PackageElement pkg, String label, CssStyles style, HtmlFactoryContext context) {
		final boolean included = context.getEnvironment().isIncluded(pkg);
		if (included) {
			final String rlabel;
			if (label == null || label.isBlank()) {
				rlabel = Messages.HtmlFactoryImpl_1;
			} else {
				rlabel = label;
			}
			final Path packagePath = getPathBuilder().packageSummary(pkg);
			final Path linkPackagePath = context.getPathToRoot().resolve(packagePath);
			return createLink(linkPackagePath, rlabel, style);
		}
		return new ArrayList<>();
	}

	@Override
	public List<Node> createTypeLink(TypeElement element, String anchor, List<Node> label, CssStyles style, HtmlFactoryContext context) {
		if (getElementUtils().isExternal(element, context.getEnvironment())) {
			final URL externalLink = getExternalLinkManager().getExternalURL(element, anchor, context);
			if (externalLink != null) {
				return createLink(externalLink, label, style);
			}
			final Element tag = new Element(SPAN_TAG);
			appendChildren(tag, label);
			if (style != null) {
				tag.addClass(style.getCssClassname());
			}
			final List<Node> list = new ArrayList<>();
			list.add(tag);
			return list; 
		}
		if (context.getEnvironment().isIncluded(element)) {
			final Path typePath = getPathBuilder().typeIndex(element);
			final Path linkTypePath = context.getPathToRoot().resolve(typePath);
			return createLink(linkTypePath, anchor, label, style);
		}
		throw new InvalidLinkException(Messages.HtmlFactoryImpl_12, element.getQualifiedName().toString());
	}

	@Override
	public List<Node> createTypeLink(TypeElement element, String anchor, List<Node> linkLabel, Node rawLabel, CssStyles style, HtmlFactoryContext context) {
		if (getElementUtils().isExternal(element, context.getEnvironment())) {
			final URL externalLink = getExternalLinkManager().getExternalURL(element, anchor, context);
			if (externalLink != null) {
				return createLink(externalLink, linkLabel, style);
			}
			final Element tag = new Element(SPAN_TAG);
			appendChildren(tag, rawLabel);
			if (style != null) {
				tag.addClass(style.getCssClassname());
			}
			final List<Node> list = new ArrayList<>();
			list.add(tag);
			return list; 
		}
		if (context.getEnvironment().isIncluded(element)) {
			final Path typePath = getPathBuilder().typeIndex(element);
			final Path linkTypePath = context.getPathToRoot().resolve(typePath);
			return createLink(linkTypePath, anchor, linkLabel, style);
		}
		throw new InvalidLinkException(Messages.HtmlFactoryImpl_12, element.getQualifiedName().toString());
	}

	@Override
	public List<Node> createTypeLink(TypeElement element, String anchor, String linkLabel, String rawLabel, CssStyles style, HtmlFactoryContext context) {
		if (getElementUtils().isExternal(element, context.getEnvironment())) {
			final URL externalLink = getExternalLinkManager().getExternalURL(element, anchor, context);
			if (externalLink != null) {
				return createLink(externalLink, linkLabel, style);
			}
			final Element tag = new Element(SPAN_TAG);
			tag.appendText(rawLabel);
			if (style != null) {
				tag.addClass(style.getCssClassname());
			}
			final List<Node> list = new ArrayList<>();
			list.add(tag);
			return list; 
		}
		if (context.getEnvironment().isIncluded(element)) {
			final Path typePath = getPathBuilder().typeIndex(element);
			final Path linkTypePath = context.getPathToRoot().resolve(typePath);
			return createLink(linkTypePath, anchor, linkLabel, style);
		}
		throw new InvalidLinkException(Messages.HtmlFactoryImpl_12, element.getQualifiedName().toString());
	}

	@Override
	public List<Node> createTypeLink(TypeMirror type, String anchor, List<Node> label, CssStyles style, HtmlFactoryContext context) {
		final List<Node> baseType = filterType(type);
		if (baseType != null) {
			return baseType;
		}
		final TypeElement element = getElementUtils().asTypeElement(type, context.getEnvironment().getTypeUtils());
		if (element == null) {
			final List<Node> list = new ArrayList<>();
			list.add(new TextNode(type.toString()));
			return list; 
		}
		return createTypeLink(element, anchor, label, style, context);
	}

	@Override
	public List<Node> createTypeLink(TypeMirror type, String anchor, String label, CssStyles style, HtmlFactoryContext context) {
		final List<Node> baseType = filterType(type);
		if (baseType != null) {
			return baseType;
		}
		final TypeElement element = getElementUtils().asTypeElement(type, context.getEnvironment().getTypeUtils());
		if (element == null) {
			final List<Node> list = new ArrayList<>();
			list.add(new TextNode(type.toString()));
			return list; 
		}
		return createTypeLink(element, anchor, label, label, style, context);
	}

	@Override
	public List<Node> createTypeLink(TypeElement element, String anchor, boolean addTypeParameters, CssStyles style, HtmlFactoryContext context) {
		final List<Node> link = createTypeLink(element, anchor,
				element.getSimpleName().toString(),
				element.getQualifiedName().toString(),
				style,
				context);
		if (addTypeParameters) {
			addTypeParameters(element, link);
		}
		return link;
	}

	private void addTypeParameters(TypeElement element, List<? super Node> link) {
		if (!element.getTypeParameters().isEmpty()) {
			link.add(new TextNode(getSARLGrammarKeywordAccess().getLessThanSignKeyword()));
			boolean first = true;
			for (final TypeParameterElement parameter : element.getTypeParameters()) {
				if (first) {
					first = false;
				} else {
					link.add(new TextNode(getSARLGrammarKeywordAccess().getCommaKeyword()));
				}
				link.add(new TextNode(parameter.getSimpleName().toString()));
			}
			link.add(new TextNode(getSARLGrammarKeywordAccess().getGreaterThanSignKeyword()));
		}
	}

	private boolean isVoidType(TypeMirror type) {
		return type == null || type.getKind() == TypeKind.VOID || type.getKind() == TypeKind.NULL;
	}
	
	private List<Node> filterType(TypeMirror type) {
		if (isVoidType(type)) {
			final List<Node> list = new ArrayList<>();
			list.add(new TextNode(getSARLGrammarKeywordAccess().getVoidKeyword()));
			return list; 
		}
		if (type.getKind().isPrimitive() || type.getKind() == TypeKind.TYPEVAR) {
			final List<Node> list = new ArrayList<>();
			list.add(new TextNode(type.toString()));
			return list; 
		}
		return null;
	}
	
	@Override
	public List<Node> createTypeLink(TypeMirror type, String anchor, boolean addTypeParameters, CssStyles style, HtmlFactoryContext context) {
		final List<Node> baseType = filterType(type);
		if (baseType != null) {
			return baseType;
		}
		final TypeElement element = getElementUtils().asTypeElement(type, context.getEnvironment().getTypeUtils());
		if (element == null) {
			final List<Node> list = new ArrayList<>();
			list.add(new TextNode(type.toString()));
			return list; 
		}
		return createTypeLink(element, anchor, addTypeParameters, style, context);
	}

	/** Replies the name of the anchor for the given member variable.
	 *
	 * @param element the member.
	 * @return the anchor name.
	 */
	@Override
	public String toVariableAnchor(VariableElement element) {
		return element.getSimpleName().toString();
	}

	@Override
	public List<Node> createVariableLink(VariableElement variable, List<Node> label, CssStyles style, HtmlFactoryContext context) {
		final String anchorName = toVariableAnchor(variable);
		if (getElementUtils().isExternal(variable, context.getEnvironment())) {
			final URL externalLink = getExternalLinkManager().getExternalURL(variable.getEnclosingElement(), anchorName, context);
			if (externalLink != null) {
				return createLink(externalLink, label, style);
			}
			final Element tag = new Element(SPAN_TAG);
			tag.appendChildren(label);
			if (style != null) {
				tag.addClass(style.getCssClassname());
			}
			final List<Node> list = new ArrayList<>();
			list.add(tag);
			return list; 
		}
		if (context.getEnvironment().isIncluded(variable)) {
			final Path typePath = getPathBuilder().typeIndex((TypeElement) variable.getEnclosingElement());
			final Path linkTypePath = context.getPathToRoot().resolve(typePath);
			return createLink(linkTypePath, anchorName, label, style);
		}
		throw new InvalidLinkException(Messages.HtmlFactoryImpl_12, getElementUtils().getFullyQualifiedName(variable, true));
	}

	@Override
	public List<Node> createVariableLink(VariableElement variable, String label, CssStyles style, HtmlFactoryContext context) {
		final String anchorName = toVariableAnchor(variable);
		if (getElementUtils().isExternal(variable, context.getEnvironment())) {
			final URL externalLink = getExternalLinkManager().getExternalURL(variable.getEnclosingElement(), anchorName, context);
			if (externalLink != null) {
				return createLink(externalLink, label, style);
			}
			final Element tag = new Element(SPAN_TAG);
			tag.appendText(label);
			if (style != null) {
				tag.addClass(style.getCssClassname());
			}
			final List<Node> list = new ArrayList<>();
			list.add(tag);
			return list; 
		}
		if (context.getEnvironment().isIncluded(variable)) {
			final Path typePath = getPathBuilder().typeIndex((TypeElement) variable.getEnclosingElement());
			final Path linkTypePath = context.getPathToRoot().resolve(typePath);
			return createLink(linkTypePath, anchorName, label, style);
		}
		throw new InvalidLinkException(Messages.HtmlFactoryImpl_12, getElementUtils().getFullyQualifiedName(variable, true));
	}

	@Override
	public String toExecutableAnchor(ExecutableElement element) {
		final String simpleName = element.getSimpleName().toString();
		final String parameters = toFlatParameterList(element.getParameters());
		final StringBuilder buf = new StringBuilder();
		buf.append(simpleName);
		buf.append("(");
		buf.append(parameters);
		buf.append(")");
		return buf.toString();
	}

	@Override
	public String toEventHandlerAnchor(ExecutableElement element) {
		final String simpleName = element.getSimpleName().toString();
		final String parameters = toFlatParameterList(element.getParameters());
		final StringBuilder buf = new StringBuilder();
		buf.append(simpleName);
		buf.append("(");
		buf.append(parameters);
		buf.append(")");
		return buf.toString();
	}

	private static String toFlatParameterList(List<? extends VariableElement> parameters) {
		final StringBuilder buf = new StringBuilder();
		for (final VariableElement parameter : parameters) {
			if (buf.length() > 0) {
				buf.append(",");
			}
			buf.append(parameter.asType().toString());
		}
		return buf.toString();
	}

	@Override
	public List<Node> createExecutableLink(ExecutableElement executable, List<Node> label, CssStyles style, HtmlFactoryContext context) {
		final String anchorName = toExecutableAnchor(executable);
		if (getElementUtils().isExternal(executable, context.getEnvironment())) {
			final URL externalLink = getExternalLinkManager().getExternalURL(executable.getEnclosingElement(), anchorName, context);
			if (externalLink != null) {
				return createLink(externalLink, label, style);
			}
			final Element tag = new Element(SPAN_TAG);
			tag.appendChildren(label);
			if (style != null) {
				tag.addClass(style.getCssClassname());
			}
			final List<Node> list = new ArrayList<>();
			list.add(tag);
			return list; 
		}
		if (context.getEnvironment().isIncluded(executable)) {
			final Path typePath = getPathBuilder().typeIndex((TypeElement) executable.getEnclosingElement());
			final Path linkTypePath = context.getPathToRoot().resolve(typePath);
			return createLink(linkTypePath, anchorName, label, style);
		}
		throw new InvalidLinkException(Messages.HtmlFactoryImpl_12, getElementUtils().getFullyQualifiedName(executable, true));
	}

	@Override
	public List<Node> createExecutableLink(ExecutableElement executable, String label, CssStyles style, HtmlFactoryContext context) {
		final String anchorName = toExecutableAnchor(executable);
		if (getElementUtils().isExternal(executable, context.getEnvironment())) {
			final URL externalLink = getExternalLinkManager().getExternalURL(executable.getEnclosingElement(), anchorName, context);
			if (externalLink != null) {
				return createLink(externalLink, label, style);
			}
			final Element tag = new Element(SPAN_TAG);
			tag.appendText(label);
			if (style != null) {
				tag.addClass(style.getCssClassname());
			}
			final List<Node> list = new ArrayList<>();
			list.add(tag);
			return list; 
		}
		if (context.getEnvironment().isIncluded(executable)) {
			final Path typePath = getPathBuilder().typeIndex((TypeElement) executable.getEnclosingElement());
			final Path linkTypePath = context.getPathToRoot().resolve(typePath);
			return createLink(linkTypePath, anchorName, label, style);
		}
		throw new InvalidLinkException(Messages.HtmlFactoryImpl_12, getElementUtils().getFullyQualifiedName(executable, true));
	}

	@Override
	public Element createTypeInheritanceTree(Element parent, TypeMirror type, CssStyles listStyle, CssStyles elementStyle, HtmlFactoryContext context) {
		assert parent != null : "parent argument must not be null";
		final LinkedList<TypeMirror> sequence = new LinkedList<>();
		sequence.add(type);
		TypeMirror sup = type;
		do {
			final TypeElement typeElement = getElementUtils().asTypeElement(sup, context.getEnvironment().getTypeUtils());
			sup = getElementUtils().getFirstVisibleSuperType(
					typeElement,
					typeElement.getKind() == ElementKind.CLASS,
					context.getEnvironment());
			if (sup != null) {
				sequence.addFirst(sup);
			}
		} while (sup != null);
		Element supElement = parent;
		Element rootElement = null;
		for (final TypeMirror type0 : sequence) {
			final Element ulElement = createUlTag(supElement, listStyle);
			if (rootElement == null) {
				rootElement = ulElement;
			}
			final Element liElement = createLiTag(ulElement, listStyle);
			appendChildren(liElement, createTypeLink(type0, false, elementStyle, context));
			supElement = liElement;
		}
		return rootElement;
	}

	@Override
	public List<Element> getAnnotationsFor(int indent, List<? extends AnnotationMirror> descList, boolean lineBreak, CssStyles style, CssStyles valueStyle, HtmlFactoryContext context) {
		final List<Element> results = new ArrayList<>();
		for (final AnnotationMirror annotationMirror : descList) {
			final TypeElement annotationElement = (TypeElement) annotationMirror.getAnnotationType().asElement();
			// If an annotation is not documented, do not add it to the list. If
			// the annotation is of a repeatable type, and if it is not documented
			// and also if its container annotation is not documented, do not add it
			// to the list. If an annotation of a repeatable type is not documented
			// but its container is documented, it will be added to the list.
			if (!getElementUtils().isDocumentedAnnotation(annotationElement)) {
				continue;
			}
			final Element htmlElement = new Element(SPAN_TAG);
			if (style != null) {
				htmlElement.addClass(style.getCssClassname());
			}
			final List<Node> annotationLink = createTypeLink(annotationElement, false, style, context);
			final Map<? extends ExecutableElement, ? extends AnnotationValue> pairs = annotationMirror.getElementValues();
			// If the annotation is synthesized, do not print the container.
			if (getElementUtils().isSynthetized(annotationMirror)) {
				for (final AnnotationValue annotationValue : pairs.values()) {
					final List<AnnotationValue> annotationTypeValues = new ArrayList<>();
					new SimpleAnnotationValueVisitor9<Void, List<AnnotationValue>>() {
						@Override
						public Void visitArray(List<? extends AnnotationValue> vals, List<AnnotationValue> p) {
							p.addAll(vals);
							return null;
						}
						@Override
						protected Void defaultAction(Object o, List<AnnotationValue> p) {
							p.add(annotationValue);
							return null;
						}
					}.visit(annotationValue, annotationTypeValues);
					final Element valueElement = createSpanTag(htmlElement, valueStyle);
					boolean first = true;
					for (AnnotationValue av : annotationTypeValues) {
						if (first) {
							first = false;
						} else {
							valueElement.appendText(SPACE);
						}
						annotationValueToContent(valueElement, av, valueStyle, context);
					}
				}
			} else {
				final boolean isArray = getElementUtils().isAnnotationArray(pairs);
				addAnnotations(annotationElement, annotationLink, htmlElement, pairs, indent, !isArray && lineBreak, valueStyle, context);
			}
			if (lineBreak) {
				appendChildren(htmlElement, createNewLineTag());
			}
			results.add(htmlElement);
		}
		return results;
	}

	private void addAnnotations(TypeElement annotationElement, List<Node> annotationLink, Element annotation,
			Map<? extends ExecutableElement, ? extends AnnotationValue> annotationValues, int indent, boolean lineBreak,
			CssStyles valueStyle, HtmlFactoryContext context) {
		annotation.appendText(getSARLGrammarKeywordAccess().getCommercialAtKeyword());
		appendChildren(annotation, annotationLink);
		// Output the values
		if (!annotationValues.isEmpty()) {
			final Element valueElement = createSpanTag(annotation, valueStyle);
			valueElement.appendText("(");
			boolean first = true;
			final boolean multipleValues = annotationValues.size() > 1;
			for (Map.Entry<? extends ExecutableElement, ? extends AnnotationValue> valuePair : annotationValues.entrySet()) {
				if (first) {
					first = false;
				} else {
					valueElement.appendText(",");
					if (lineBreak) {
						appendChildren(valueElement, createNewLineTag());
						int spaces = annotationElement.getSimpleName().length() + 2;
						for (int k = 0; k < (spaces + indent); k++) {
							valueElement.appendText(SPACE);
						}
					}
				}
				final String simpleName = valuePair.getKey().getSimpleName().toString();
				if (multipleValues || !"value".equals(simpleName)) { // Omit "value=" where unnecessary
					// TODO: Link to the element pointed by valuePair.getKey()
					valueElement.appendText(simpleName);
					valueElement.appendText("=");
				}
				List<AnnotationValue> annotationTypeValues = new ArrayList<>();
				new SimpleAnnotationValueVisitor9<Void, AnnotationValue>() {
					@Override
					public Void visitArray(List<? extends AnnotationValue> vals, AnnotationValue p) {
						annotationTypeValues.addAll(vals);
						return null;
					}
					@Override
					protected Void defaultAction(Object o, AnnotationValue p) {
						annotationTypeValues.add(p);
						return null;
					}
				}.visit(valuePair.getValue(), valuePair.getValue());

				final boolean multipleTypeValues = annotationTypeValues.size() > 1;
				if (multipleTypeValues) {
					valueElement.appendText("{");
				}
				boolean first0 = true;
				for (final AnnotationValue av : annotationTypeValues) {
					if (first0) {
						first0 = false;
					} else {
						valueElement.appendText(",");
					}
					annotationValueToContent(valueElement, av, valueStyle, context);
				}
				if (multipleTypeValues) {
					valueElement.appendText("}");
				}
			}
			valueElement.appendText(")");
		}
	}

	private void annotationValueToContent(Element output, AnnotationValue annotationValue, CssStyles valueStyle, HtmlFactoryContext context) {
		new SimpleAnnotationValueVisitor9<Void, Void>() {
			@Override
			public Void visitType(TypeMirror t, Void p) {
				new SimpleTypeVisitor9<Void, Void>() {
					@Override
					public Void visitDeclared(DeclaredType t, Void p) {
						final List<Node> linkElement = createTypeLink(t, false, valueStyle, context);
						appendChildren(output, linkElement);
						output.appendText(getElementUtils().getDimension(t));
						return null;
					}
					@Override
					protected Void defaultAction(TypeMirror e, Void p) {
						final List<Node> linkElement = createTypeLink(t, false, valueStyle, context);
						appendChildren(output, linkElement);
						output.appendText(getElementUtils().getDimension(t));
						return null;
					}
				}.visit(t);
				return null;
			}
			@Override
			public Void visitAnnotation(AnnotationMirror a, Void p) {
				for (final Element c : getAnnotationsFor(0, Collections.singletonList(a), false, valueStyle, valueStyle, context)) {
					appendChildren(output, c);
				}
				return null;
			}
			@Override
			public Void visitEnumConstant(VariableElement c, Void p) {
				// TODO: Add link to the member.
				output.appendText(c.getSimpleName().toString());
				return null;
			}
			@Override
			public Void visitArray(List<? extends AnnotationValue> vals, Void p) {
				boolean first = true;
				for (AnnotationValue av : vals) {
					if (first) {
						first = false;
					} else {
						output.appendText(SPACE);
					}
					visit(av);
				}
				return null;
			}
			@Override
			protected Void defaultAction(Object o, Void p) {
				output.appendText(annotationValue.toString());
				return null;
			}
		}.visit(annotationValue);
	}

	@Override
	public Element createNewLineTag() {
		return new Element(BR_TAG);
	}

	@Override
	public CommentTextMemory createCommentTextMemory(Element parent, javax.lang.model.element.Element element, HtmlFactoryContext context) {
		assert parent != null : "parent argument must not be null";
		return new CommentTextMemoryImpl(parent, element, context);
	}
	
	@Override
	public boolean createCommentText(CommentTextMemory memory, DocTree documentationTree, CssStyles style) {
		final TextContentExtractor visitor = new TextContentExtractor(memory, documentationTree, style);
		documentationTree.accept(visitor, null);
		memory.collapseStack();
		return memory.hasDocumentationText();
	}

	private String normalizeNewlines(String text) {
		if (Strings.isNullOrEmpty(text)) {
			return "";
		}
		final StringBuilder sb = new StringBuilder();
		final int textLength = text.length();
		final String NL = HtmlTags.BR_TAG;
		int pos = 0;
		for (int i = 0; i < textLength; i++) {
			char ch = text.charAt(i);
			switch (ch) {
			case '\n':
				sb.append(text, pos, i);
				sb.append(NL);
				pos = i + 1;
				break;
			case '\r':
				sb.append(text, pos, i);
				sb.append(NL);
				if (i + 1 < textLength && text.charAt(i + 1) == '\n')
					i++;
				pos = i + 1;
				break;
			}
		}
		sb.append(text, pos, textLength);
		return sb.toString();
	}

	@Override
	public Element createCodeTag(Element parent, String content) {
		final String ncontent = normalizeNewlines(content);
		final Element elt;
		if (parent == null) {
			elt = new Element(CODE_TAG);
		} else {
			elt = parent.appendElement(CODE_TAG);
		}
		elt.appendText(ncontent);
		return elt;
	}

	@Override
	public Element createTableTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(TABLE_TAG);
		} else {
			elt = parent.appendElement(TABLE_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createTableHeaderTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(THEAD_TAG);
		} else {
			elt = parent.appendElement(THEAD_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createTableBodyTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(TBODY_TAG);
		} else {
			elt = parent.appendElement(TBODY_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createTableRowTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(TR_TAG);
		} else {
			elt = parent.appendElement(TR_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createTableCellTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(TD_TAG);
		} else {
			elt = parent.appendElement(TD_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Element createTableColumnHeadTag(Element parent, CssStyles style) {
		final Element elt;
		if (parent == null) {
			elt = new Element(TH_TAG);
		} else {
			elt = parent.appendElement(TH_TAG);
		}
		if (style != null) {
			elt.addClass(style.getCssClassname());
		}
		return elt;
	}

	@Override
	public Comment createHtmlComment(Element parent, String text) {
		final Comment cmt = new Comment(Strings.nullToEmpty(text));
		if (parent != null) {
			parent.appendChild(cmt);
		}
		return cmt;
	}

	@Override
	public HtmlTabsFactory createTabBox(CssStyles titleStyle, CssStyles contentStyle) {
		return new HtmlTabsFactoryImpl(titleStyle, contentStyle);
	}

	@Override
	public Element keyword(Element receiver, String keyword) {
		final Element kw = createSpanTag(receiver, CssStyles.KEYWORD).appendText(keyword);
		if (receiver == null) {
			return kw;
		}
		return receiver;
	}

	@Override
	public List<Node> getExecutablePrototype(ExecutableElement element, String name, HtmlFactoryContext context) {
		final List<Node> realName;
		if (Strings.isNullOrEmpty(name)) {
			if (element.getKind() == ElementKind.CONSTRUCTOR) {
				final Element realName0 = keyword(null, getSARLGrammarKeywordAccess().getNewKeyword());
				realName = Collections.singletonList(realName0);
			} else {
				realName = Collections.singletonList(new TextNode(element.getSimpleName().toString()));
			}
		} else {
			realName = Collections.singletonList(new TextNode(name));
		}
		return getExecutablePrototype(element, realName, context);
	}

	@Override
	public List<Node> getExecutablePrototype(ExecutableElement element, List<Node> name, HtmlFactoryContext context) {
		List<Node> prototype = new ArrayList<>();
		prototype.addAll(name);
		final List<? extends VariableElement> parameters = element.getParameters();
		if (!parameters.isEmpty()) {
			prototype.add(new TextNode(getSARLGrammarKeywordAccess().getLeftParenthesisKeyword()));
			for (int i = 0; i < parameters.size(); ++i) {
				if (i > 0) {
					prototype.add(new TextNode(getSARLGrammarKeywordAccess().getCommaKeyword()));
				}
				final boolean isVararg = element.isVarArgs() && i == parameters.size() - 1;
				final boolean isOptional = !isVararg && parameters.get(i).getAnnotation(DefaultValue.class) != null;
				final TypeMirror type = parameters.get(i).asType();
				if (isOptional) {
					prototype.add(new TextNode("[")); //$NON-NLS-1$
				}
				if (isVararg && type instanceof ArrayType) {
					final ArrayType arrayType = (ArrayType) type;
					final TypeMirror componentType = arrayType.getComponentType();
					final TypeElement componentTypeElement = getElementUtils().asTypeElement(componentType, context.getEnvironment().getTypeUtils());
					prototype.add(new TextNode(componentTypeElement.getSimpleName().toString()));
					addTypeParameters(componentTypeElement, prototype);
				} else if (type.getKind().isPrimitive() || type.getKind() == TypeKind.TYPEVAR) {
					prototype.add(new TextNode(type.toString()));
				} else {
					final TypeElement typeElement = getElementUtils().asTypeElement(type, context.getEnvironment().getTypeUtils());
					if (typeElement != null) { 
						prototype.add(new TextNode(typeElement.getSimpleName().toString()));
						addTypeParameters(typeElement, prototype);
					} else {
						prototype.add(new TextNode(type.toString()));
					}
				}
				if (isVararg) {
					prototype.add(new TextNode(getSARLGrammarKeywordAccess().getWildcardAsteriskKeyword()));
				} else if (isOptional) {
					prototype.add(new TextNode("]")); //$NON-NLS-1$
				}
			}
			prototype.add(new TextNode(getSARLGrammarKeywordAccess().getRightParenthesisKeyword()));
		}
		return prototype;
	}

	@Override
	public void addLinkTargetFrame(List<? extends Node> link, String target) {
		if (!Strings.isNullOrEmpty(target)) {
			for (final Node node : link) {
				if (HtmlTags.A_TAG.equalsIgnoreCase(node.nodeName()) && node instanceof Element) {
					final Element element = (Element) node;
					element.attr(HtmlTags.TARGET_ATTR, target);
				}
			}
		}
	}

	/** Implementation of the tab factory.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	private class HtmlTabsFactoryImpl implements HtmlTabsFactory {

		private CssStyles titleStyle;

		private CssStyles contentStyle;

		private final List<Element> titles = new ArrayList<>();
		
		private final List<Element> contents = new ArrayList<>();

		private final String tabButtonClassname;

		private final String tabContentClassname;

		HtmlTabsFactoryImpl(CssStyles titleStyle, CssStyles contentStyle) {
			this.titleStyle = titleStyle;
			this.contentStyle = contentStyle;
			this.tabButtonClassname = computeTabButtonClassname();
			this.tabContentClassname = computeTabContentClassname();
		}
		
		@Override
		public int size() {
			return this.titles.size();
		}

		@Override
		public Element getSelector(int index) {
			return this.titles.get(index);
		}

		@Override
		public Element getContent(int index) {
			return this.contents.get(index);
		}

		private Element addTabElements() {
			final String id = computeId();
			final boolean isFirst = isEmpty();
			//
			final Element titleBox = new Element(BUTTON_TAG);
			titleBox.addClass(CssStyles.TABS_TITLE.getCssClassname());
			if (this.titleStyle != null) {
				titleBox.addClass(this.titleStyle.getCssClassname());
			}
			titleBox.attr(ONCLICK_ATTR, "openTabElement(this,'" + id + "', '" + this.tabButtonClassname + "', '" + this.tabContentClassname + "')");
			titleBox.addClass(this.tabButtonClassname);
			if (isFirst) {
				titleBox.addClass(ACTIVE_ATTR_VALUE);
			}
			//
			final Element contentBox = new Element(DIV_TAG);
			contentBox.id(id);
			contentBox.addClass(CssStyles.TABS_CONTENT.getCssClassname());
			contentBox.addClass(this.tabContentClassname);
			if (this.contentStyle != null) {
				contentBox.addClass(this.contentStyle.getCssClassname());
			}
			if (!isFirst) {
				contentBox.attr(STYLE_ATTR, STYLE_NO_DISPLAY_ATTR_VALUE);
			}
			//
			this.titles.add(titleBox);
			this.contents.add(contentBox);
			return titleBox;
		}
		
		@Override
		public void addTab(String title) {
			final Element titleBox = addTabElements();
			titleBox.appendText(title);
		}

		@Override
		public void addTab(Node title) {
			final Element titleBox = addTabElements();
			titleBox.appendChild(title);
		}

		@Override
		public void removeLastTab() {
			this.titles.remove(this.titles.size() - 1);
			this.contents.remove(this.contents.size() - 1);
		}
		
		@Override
		public Element createSelectors(Element parent) {
			final Element selectors;
			if (parent == null) {
				selectors = new Element(DIV_TAG);
			} else {
				selectors = parent.appendElement(DIV_TAG);
			}
			for (final Element selector : this.titles) {
				selectors.appendChild(selector);
			}
			return selectors;
		}

		@Override
		public Element createContents(Element parent) {
			final Element contents;
			if (parent == null) {
				contents = new Element(DIV_TAG);
			} else {
				contents = parent;
			}
			for (final Element cnt : this.contents) {
				contents.appendChild(cnt);
			}
			return contents;
		}
		
	}

	/** Element in the stack of {@link HtmlFactoryImpl#createCommentText(Element, DocTree, PackageElement, String, io.sarl.docs.doclet2.html.HtmlFactory.HtmlFactoryContext)}.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	private static class StackElement {

		public final Node node;

		public final TypeElement typeElement;

		public final VariableElement variableElement;

		public final ExecutableElement executableElement;

		public final String name;

		public StackElement(String name, Node node) {
			this.name = name;
			this.node = node;
			this.typeElement = null;
			this.variableElement = null;
			this.executableElement = null;
		}

		public StackElement(String name, TypeElement element) {
			this.name = name;
			this.node = null;
			this.typeElement = element;
			this.variableElement = null;
			this.executableElement = null;
		}

		public StackElement(String name, VariableElement element) {
			this.name = name;
			this.node = null;
			this.typeElement = null;
			this.variableElement = element;
			this.executableElement = null;
		}

		public StackElement(String name, ExecutableElement element) {
			this.name = name;
			this.node = null;
			this.typeElement = null;
			this.variableElement = null;
			this.executableElement = element;
		}

		@Override
		public String toString() {
			if (this.typeElement != null) {
				return this.name + ": " + this.typeElement.toString();
			}
			if (this.variableElement != null) {
				return this.name + ": " + this.variableElement.toString();
			}
			if (this.executableElement != null) {
				return this.name + ": " + this.executableElement.toString();
			}
			return this.name + ": " + this.node.toString();
		}

	}

	/** Malformed HTML text.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	public static class InvalidLinkException extends RuntimeException {

		private static final long serialVersionUID = -8363502825243065010L;

		/** Constructor.
		 *
		 * @param message the explanation of the error
		 * @param element the name of the element for which the link is invalid.
		 */
		public InvalidLinkException(String message, String element) {
			super(MessageFormat.format(message, element));
		}

	}

	/** Taglet is not found.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	public static class NotFoundTagletException extends RuntimeException {

		private static final long serialVersionUID = -335570634022907846L;

		/** Constructor.
		 *
		 * @param name the name of the not-found taglet.
		 */
		public NotFoundTagletException(String name ) {
			super(name );
		}

	}

	/** Implementation of a visitor of the documentation for extracting a text.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	protected class TextContentExtractor implements DocTreeVisitor<Void, Void>, HtmlFactoryContentExtractor {

		private final DocTree elementDocumentation;

		private final CssStyles cssStyle;
		
		private final CommentTextMemory memory;

		/** Constructor.
		 *
		 * @param memory the memory.
		 * @param elementDocumentation the original documentation for the {@code element}.
		 * @param style the CSS style to use by default.
		 */
		protected TextContentExtractor(CommentTextMemory memory, DocTree elementDocumentation,
				CssStyles style) {
			this.memory = memory;
			this.elementDocumentation = elementDocumentation;
			this.cssStyle = style;
		}

		@Override
		public HtmlFactoryContext getContext() {
			return this.memory.getContext();
		}
		
		@Override
		public Element extractSimpleText(List<? extends DocTree> text) {
			final Element lblElement = new Element(HIDDEN_PSEUDO_TAG);
			if (text != null) {
				this.memory.pushElement(HIDDEN_PSEUDO_TAG, lblElement);
				groupAccept(text);
				this.memory.removeTo(lblElement);
			}
			return lblElement;
		}
		
		@Override
		public javax.lang.model.element.Element extractReferencedElement(DocTree docNode) {
			final StackElement stackElement = extractReferencedElement(docNode, () -> docNode.accept(this, null));
			if (stackElement != null) {
				if (stackElement.typeElement != null) {
					return stackElement.typeElement;
				}
				if (stackElement.variableElement != null) {
					return stackElement.variableElement;
				}
				if (stackElement.executableElement != null) {
					return stackElement.executableElement;
				}
			}
			return null;
		}

		private StackElement extractReferencedElement(DocTree docNode, Procedure0 callback) {
			// Add a reference pseudo tag to be sure it is present on the stack.
			// If a real reference is provided by the callback, another reference pseudo tag will be
			// pushed on the stack. The top-most reference pseudo tag is the one containing the reference.
			final Node defaultNode = new Element(HtmlTags.REFERENCE_PSEUDO_TAG);
			this.memory.pushElement(HtmlTags.REFERENCE_PSEUDO_TAG, defaultNode);

			// Infer the reference
			callback.apply();

			// Extract the inferred reference
			StackElement stackElement = ((CommentTextMemoryImpl) this.memory).removeTo(HtmlTags.REFERENCE_PSEUDO_TAG);
			this.memory.removeUntil(HtmlTags.REFERENCE_PSEUDO_TAG);

			return stackElement;
		}

		@Override
		public List<Node> extractReference(DocTree docNode, List<Node> label, boolean isplain) {
			return extractReference(docNode, label, isplain, () -> docNode.accept(this, null));
		}

		private List<Node> extractReference(DocTree docNode, List<Node> label, boolean isplain, Procedure0 callback) {
			// Extract the inferred reference
			final StackElement stackElement = extractReferencedElement(docNode, callback);

			final CssStyles theStyle = isplain ? null : this.cssStyle;
			
			// Treat the extracted element
			String innerLabel = null;
			if (stackElement != null) {
				if (stackElement.typeElement != null) {
					if (label == null) {
						return createTypeLink(stackElement.typeElement, false, theStyle, getContext());
					}
					return createTypeLink(stackElement.typeElement, label, theStyle, getContext());
				}
				if (stackElement.variableElement != null) {
					if (label == null) {
						return createVariableLink(stackElement.variableElement,
								stackElement.variableElement.getSimpleName().toString(), theStyle, getContext());
					}
					return createVariableLink(stackElement.variableElement, label, theStyle, getContext());
				}
				if (stackElement.executableElement != null) {
					if (label == null) {
						final List<Node> defaultLabel = getExecutablePrototype(stackElement.executableElement, getContext());
						return createExecutableLink(stackElement.executableElement, defaultLabel, theStyle, getContext());
					}
					return createExecutableLink(stackElement.executableElement, label, theStyle, getContext());
				}
				if (stackElement.node != null) {
					if (stackElement.node instanceof TextNode) {
						innerLabel = ((TextNode) stackElement.node).getWholeText().trim();
					} else if (stackElement.node instanceof Element) {
						final Element elt = (Element) stackElement.node;
						if (HtmlTags.isPseudoTag(elt.tagName())) {
							innerLabel = elt.wholeText().trim();
						} else {
							innerLabel = stackElement.node.toString().trim();
						}
					} else {
						innerLabel = stackElement.node.toString().trim();
					}
					if (innerLabel.startsWith("\"")) {
						// The node contains an URL
						int index = innerLabel.lastIndexOf('"');
						if (index >=2) {
							innerLabel = innerLabel.substring(1, index);
							if (!Strings.isNullOrEmpty(innerLabel)) {
								try {
									final URL url = new URL(innerLabel);
									if (label == null) {
										return createLink(url, url.toString(), theStyle);
									}
									return createLink(url, label, theStyle);
								} catch (Throwable ex) {
									//
								}
							}
						}
					} else if (innerLabel.startsWith("<")) {
						// The node contains an hyperref tag
						final List<Node> list = new ArrayList<>();
						list.add(new TextNode(innerLabel));
						return list; 
					} else {
						final List<Node> list = new ArrayList<>();
						list.add(stackElement.node);
						return list; 
					}
				}
			}
			// Try to find a reference based on text
			getContext().getReporter().print(Kind.WARNING, MessageFormat.format(Messages.HtmlFactoryImpl_4,
					docNode.toString(), getElementUtils().getFullIdentifier(this.memory.getElement())));
			if (label != null) {
				return label;
			}
			if (innerLabel != null) {
				final List<Node> list = new ArrayList<>();
				list.add(new TextNode(innerLabel));
				return list; 
			}
			final List<Node> list = new ArrayList<>();
			list.add(new TextNode(""));
			return list; 
		}

		private void groupAccept(List<? extends DocTree> docContent) {
			for (final DocTree child : docContent) {
				child.accept(this, null);
			}
		}

		private Void errorAction(String tagName, DocTree node) {
			this.memory.changeDocumentationText();
			final Element top = this.memory.getTop();
			top.appendText(node.toString());
			getContext().getReporter().print(Kind.ERROR, MessageFormat.format(Messages.HtmlFactoryImpl_10, tagName));
			return null;
		}

		/** Invoke the inline taglet with the given name.
		 *
		 * @param name the name of the taglet.
		 * @param top the receiver element.
		 * @param children the documentation nodes.
		 */
		protected void invokeInlineTaglet(String name, Element top, List<? extends DocTree> children) {
			final Taglet taglet = getTagletManager().getInlineTaglet(name);
			if (taglet instanceof SarlTaglet) {
				final SarlTaglet staglet = (SarlTaglet) taglet;
				final boolean chg = staglet.appendNode(top, children, this.memory.getElement(), this.elementDocumentation, this.cssStyle, this);
				if (chg) {
					this.memory.changeDocumentationText();
				}
			} else if (taglet != null) {
				final String content = taglet.toString(children, this.memory.getElement());
				if (!Strings.isNullOrEmpty(content)) {
					this.memory.changeDocumentationText();
					top.append(content);
				}
			} else {
				throw new NotFoundTagletException(name);
			}
		}

		@Override
		public Void visitOther(DocTree node, Void p) {
			return errorAction(node.toString(), node);
		}

		@Override
		public Void visitDocComment(DocCommentTree node, Void p) {
			return errorAction("<!-- -->", node);
		}

		@Override
		public Void visitErroneous(ErroneousTree node, Void p) {
			final Diagnostic<JavaFileObject> diag = node.getDiagnostic();
			if (diag != null) {
				final String msg = MessageFormat.format(Messages.HtmlFactoryImpl_11,
						diag.getSource().getName(), diag.getLineNumber(), diag.getColumnNumber(),
						diag.getMessage(Locale.getDefault()), node.getBody());
				getContext().getReporter().print(diag.getKind(), msg);
			}
			return null;
		}

		@Override
		public Void visitAttribute(AttributeTree node, Void noparam) {
			this.memory.changeDocumentationText();
			final String name = node.getName().toString();
			final Element top = this.memory.getTop();
			if (node.getValueKind() == ValueKind.EMPTY) {
				top.attr(name, "");
			} else {
				final Element valueElement = new Element(HtmlTags.HIDDEN_PSEUDO_TAG);
				this.memory.pushElement(HtmlTags.HIDDEN_PSEUDO_TAG, valueElement);
				groupAccept(node.getValue());
				this.memory.removeTo(valueElement);
				if (valueElement.childNodeSize() == 0) {
					top.attr(name, "");
				} else {
					top.attr(name, valueElement.text());
				}
			}
			return null;
		}

		@Override
		public Void visitStartElement(StartElementTree node, Void noparam) {
			final String name = node.getName().toString();
			final Element tag = new Element(name);
			this.memory.pushElement(name, tag);
			groupAccept(node.getAttributes());
			if (P_TAG.equalsIgnoreCase(name)) {
				this.memory.removeTo(tag);
				this.memory.changeDocumentationText();
				final Element top = this.memory.getTop();
				appendChildren(top, tag);
				/*getContext().getReporter().print(Kind.NOTE, MessageFormat.format(Messages.HtmlFactoryImpl_5,
						getElementUtils().getFullIdentifier(this.memory.getElement())));*/
			}

			return null;
		}

		@Override
		public Void visitEndElement(EndElementTree node, Void noparam) {
			final String name = node.getName().toString();
			if (!P_TAG.equalsIgnoreCase(name)) {
				final StackElement child = ((CommentTextMemoryImpl) this.memory).removeTo(name);
				if (child != null) {
					final Element top = this.memory.getTop();
					if (child.typeElement != null) {
						this.memory.changeDocumentationText();
						appendChildren(top, createTypeLink(child.typeElement, false, this.cssStyle, getContext()));
						return null;
					}
					if (child.variableElement != null) {
						this.memory.changeDocumentationText();
						appendChildren(top, createVariableLink(child.variableElement,
								child.variableElement.getSimpleName().toString(), this.cssStyle, getContext()));
						return null;
					}
					if (child.executableElement != null) {
						this.memory.changeDocumentationText();
						appendChildren(top, createExecutableLink(child.executableElement,
								child.executableElement.getSimpleName().toString(), this.cssStyle, getContext()));
						return null;
					}
					if (child.node != null) {
						this.memory.changeDocumentationText();
						appendChildren(top, child.node);
						return null;
					}
				} else {
					getContext().getReporter().print(Kind.WARNING, MessageFormat.format(Messages.HtmlFactoryImpl_6,
							name, getElementUtils().getFullIdentifier(this.memory.getElement())));
				}
			}
			return null;
		}

		@Override
		public Void visitLink(LinkTree node, Void noparam) {
			invokeInlineTaglet(LinkTaglet.TAGLET_NAME, this.memory.getTop(), Collections.singletonList(node));
			return null;
		}

		@Override
		public Void visitEntity(EntityTree node, Void noparam) {
			this.memory.changeDocumentationText();
			final Element top = this.memory.getTop();
			top.appendText(node.toString());
			return null;
		}

		@Override
		public Void visitLiteral(LiteralTree node, Void noparam) {
			this.memory.changeDocumentationText();
			final Element top = this.memory.getTop();
			if (node.getKind() == CODE) {
				invokeInlineTaglet(CodeTaglet.TAGLET_NAME, top, Collections.singletonList(node.getBody()));
				return null;
			}
			invokeInlineTaglet(LiteralTaglet.TAGLET_NAME, top, Collections.singletonList(node.getBody()));
			return null;
		}

		@Override
		public Void visitSerial(SerialTree node, Void noparam) {
			groupAccept(node.getDescription());
			return null;
		}

		@Override
		public Void visitSee(SeeTree node, Void noparam) {
			// Treat the first element as a reference, and the rest as an explanation text
			final List<? extends DocTree> ref = node.getReference();
			final List<DocTree> refref = new ArrayList<>();
			final List<DocTree> text = new ArrayList<>();
			for (final DocTree doc : ref) {
				if (doc instanceof ReferenceTree) {
					refref.add(doc);
				} else {
					text.add(doc);
				}
			}
			final List<Node> n = extractReference(node, null, false, () -> groupAccept(refref));
			this.memory.changeDocumentationText();
			final Element top = this.memory.getTop();
			appendChildren(top, n);
			createSecableSpace(top);
			groupAccept(text);
			return null;
		}

		@Override
		public Void visitAuthor(AuthorTree node, Void p) {
			groupAccept(node.getName());
			return null;
		}

		@Override
		public Void visitComment(CommentTree node, Void p) {
			// The javadoc parser keeps the HTML comment tags in the text.
			// They should be removed.
			String text = node.getBody();
			text = text.replaceFirst("^\\s*" + Pattern.quote("<!--") + "\\s*", "");
			text = text.replaceFirst("\\s*" + Pattern.quote("-->") + "\\s*$", "");
			//
			final Comment cmt = new Comment(text);
			this.memory.changeDocumentationText();
			final Element top = this.memory.getTop();
			appendChildren(top, cmt);
			return null;
		}

		@Override
		public Void visitDeprecated(DeprecatedTree node, Void p) {
			groupAccept(node.getBody());
			return null;
		}

		@Override
		public Void visitDocRoot(DocRootTree node, Void p) {
			final Element top = this.memory.getTop();
			invokeInlineTaglet(DocRootTaglet.TAGLET_NAME, top, Collections.emptyList());
			return null;
		}

		@Override
		public Void visitIdentifier(IdentifierTree node, Void p) {
			this.memory.changeDocumentationText();
			final Element top = this.memory.getTop();
			top.appendText(node.getName().toString());
			return null;
		}

		@Override
		public Void visitInheritDoc(InheritDocTree node, Void p) {
			invokeInlineTaglet(InheritDocTaglet.TAGLET_NAME, this.memory.getTop(), Collections.singletonList(node));
			return null;
		}

		@Override
		public Void visitParam(ParamTree node, Void p) {
			groupAccept(node.getDescription());
			return null;
		}

		@Override
		public Void visitReturn(ReturnTree node, Void p) {
			groupAccept(node.getDescription());
			return null;
		}

		@Override
		public Void visitSerialData(SerialDataTree node, Void p) {
			groupAccept(node.getDescription());
			return null;
		}

		@Override
		public Void visitSerialField(SerialFieldTree node, Void p) {
			groupAccept(node.getDescription());
			return null;
		}

		@Override
		public Void visitSince(SinceTree node, Void p) {
			groupAccept(node.getBody());
			return null;
		}

		@Override
		public Void visitThrows(ThrowsTree node, Void p) {
			groupAccept(node.getDescription());
			return null;
		}

		@Override
		public Void visitVersion(VersionTree node, Void p) {
			groupAccept(node.getBody());
			return null;
		}

		@Override
		public Void visitUnknownInlineTag(UnknownInlineTagTree node, Void p) {
			final String tagName = node.getTagName().toString().trim();
			final Taglet taglet = getTagletManager().getInlineTaglet(tagName);
			visitUnknownTag(node.getContent(), tagName, taglet, Messages.HtmlFactoryImpl_7);
			return null;
		}

		@Override
		public Void visitUnknownBlockTag(UnknownBlockTagTree node, Void noparam) {
			final String tagName = node.getTagName().toString().trim();
			final Location location = getContext().getDocUtils().getTagletLocation(this.memory.getElement());
			final Taglet taglet = getTagletManager().getBlockTaglet(location, tagName);
			visitUnknownTag(node.getContent(), tagName, taglet, Messages.HtmlFactoryImpl_8);
			return null;
		}

		private void visitUnknownTag(List<? extends DocTree> nodes, String tagName, Taglet taglet, String errorMessage) {
			if (taglet != null) {
				final Element top = this.memory.getTop();
				if (taglet instanceof SarlTaglet) {
					final SarlTaglet staglet = (SarlTaglet) taglet;
					final boolean chg = staglet.appendNode(top, nodes, this.memory.getElement(), this.elementDocumentation, this.cssStyle, this);
					if (chg) {
						this.memory.changeDocumentationText();
					}
				} else {
					final String content = taglet.toString(nodes, this.memory.getElement());
					this.memory.changeDocumentationText();
					top.appendText(content);
				}
			} else {
				getContext().getReporter().print(Kind.ERROR, MessageFormat.format(errorMessage, tagName,
						getElementUtils().getFullIdentifier(this.memory.getElement())));
			}
		}

		@Override
		public Void visitValue(ValueTree node, Void noparam) {
			final Element top = this.memory.getTop();
			invokeInlineTaglet(ValueTaglet.TAGLET_NAME, top, Collections.singletonList(node.getReference()));
			return null;
		}

		private TypeMirror getCurrentType() {
			final javax.lang.model.element.Element element = this.memory.getElement();
			assert element != null;
			switch (element.getKind()) {
			case ANNOTATION_TYPE:
			case CLASS:
			case ENUM:
			case INTERFACE:
				return element.asType();
			default:
				break;
			}
			final TypeElement type = getElementUtils().getEnclosingTypeElement(element);
			if (type != null) {
				return type.asType();
			}
			return null;
		}

		@Override
		public Void visitReference(ReferenceTree node, Void noparam) {
			final javax.lang.model.element.Element referencedElement = getElementUtils().getReferencedElement(
					node,
					getCurrentType(),
					getContext().getQualifiedNameSetBuilder(this.memory.getElement()));
			
			if (referencedElement instanceof TypeElement) {
				this.memory.pushElement(HtmlTags.REFERENCE_PSEUDO_TAG, (TypeElement) referencedElement);
				return null;
			}
			if (referencedElement instanceof ExecutableElement) {
				this.memory.pushElement(HtmlTags.REFERENCE_PSEUDO_TAG, (ExecutableElement) referencedElement);
				return null;
			}
			if (referencedElement instanceof VariableElement) {
				this.memory.pushElement(HtmlTags.REFERENCE_PSEUDO_TAG, (VariableElement) referencedElement);
				return null;
			}

			// Problem for finding the element.
			final String signature = node.getSignature();
			final String packs;
			if (getContext().getQualifiedNameSetBuilder(this.memory.getElement()) != null) {
				final Set<String> packages = getContext().getQualifiedNameSetBuilder(this.memory.getElement()).buildCandidateList(signature);
				packs = Iterables.toString(packages);
			} else {
				packs = "[]"; //$NON-NLS-1$
			}
			getContext().getReporter().print(Kind.WARNING, MessageFormat.format(Messages.HtmlFactoryImpl_9,
					signature, packs));
			this.memory.pushElement(HtmlTags.REFERENCE_PSEUDO_TAG, new TextNode(signature));
			return null;
		}

		@Override
		public Void visitText(TextTree node, Void noparam) {
			this.memory.changeDocumentationText();
			final Element top = this.memory.getTop();
			top.appendText(node.getBody());
			return null;
		}

	}

	/** Memory of the generation of comment text.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	protected class CommentTextMemoryImpl implements CommentTextMemory {

		private final LinkedList<StackElement> stack = new LinkedList<>();

		private final AtomicBoolean changed = new AtomicBoolean();

		private final Element parent;

		private final HtmlFactoryContext context;

		private final javax.lang.model.element.Element element;

		/** Constructor.
		 *
		 * @param parent the container of the extracted text.
		 * @param element the Java element for which the documentation must be extracted.
		 * @param context the context of the HTML factory.
		 */
		public CommentTextMemoryImpl(Element parent, javax.lang.model.element.Element element, HtmlFactoryContext context) {
			this.parent = parent;
			this.element = element;
			this.context = context;
			pushElement(this.parent.tagName(), this.parent);
		}

		@Override
		public Element getRootParent() {
			return this.parent;
		}

		@Override
		public HtmlFactoryContext getContext() {
			return this.context;
		}

		@Override
		public javax.lang.model.element.Element getElement() {
			return this.element;
		}

		@Override
		public void pushElement(String name, Node element) {
			this.stack.addFirst(new StackElement(name, element));
		}

		@Override
		public void pushElement(String name, TypeElement element) {
			this.stack.addFirst(new StackElement(name, element));
		}

		@Override
		public void pushElement(String name, VariableElement element) {
			this.stack.addFirst(new StackElement(name, element));
		}

		@Override
		public void pushElement(String name, ExecutableElement element) {
			this.stack.addFirst(new StackElement(name, element));
		}

		@Override
		public Element getTop() {
			final StackElement top = this.stack.getFirst();
			if (top.node instanceof Element) {
				return (Element) top.node;
			}
			return null;
		}

		/** Remove top elements until a top element has the given key.
		 *
		 * @param key the key.
		 * @return the removed element.
		 */
		public StackElement removeTo(String key) {
			final Iterator<StackElement> iterator = this.stack.iterator();
			while (iterator.hasNext()) {
				final StackElement elt = iterator.next();
				iterator.remove();
				if (key.equals(elt.name)) {
					return elt;
				}
			}
			return null;
		}

		@Override
		public void removeTo(Node element) {
			final Iterator<StackElement> iterator = this.stack.iterator();
			while (iterator.hasNext()) {
				final StackElement elt = iterator.next();
				iterator.remove();
				if (element == elt.node) {
					return;
				}
			}
		}

		@Override
		public void removeUntil(String key) {
			final Iterator<StackElement> iterator = this.stack.iterator();
			while (iterator.hasNext()) {
				final StackElement elt = iterator.next();
				if (key.equals(elt.name)) {
					iterator.remove();
				} else {
					return;
				}
			}
		}

		@Override
		public void changeDocumentationText() {
			this.changed.set(true);
		}
		
		@Override
		public boolean hasDocumentationText() {
			return this.changed.get();
		}

		@Override
		public void collapseStack() {
			if (!this.stack.isEmpty()) {
				final StackElement stackElement = this.stack.getFirst();
				if (stackElement != null && HtmlTags.REFERENCE_PSEUDO_TAG.equals(stackElement.name)) {
					this.stack.removeFirst();
					final Element top = getTop();
					if (top != null) {
						if (stackElement.typeElement != null) {
							final Collection<? extends Node> list = createTypeLink(stackElement.typeElement, true, null, this.context);
							if (!list.isEmpty()) {
								this.changed.set(true);
								top.appendChildren(list);
							}
						} else if (stackElement.variableElement != null) {
							final VariableElement variable = stackElement.variableElement;
							final Collection<? extends Node> list = createVariableLink(variable, variable.getSimpleName().toString(), null, this.context);
							if (!list.isEmpty()) {
								this.changed.set(true);
								top.appendChildren(list);
							}
						} else if (stackElement.executableElement != null) {
							final ExecutableElement executable = stackElement.executableElement;
							final String basename = element.getKind() == ElementKind.CONSTRUCTOR ? getSARLGrammarKeywordAccess().getNewKeyword() : element.getSimpleName().toString();
							final List<Node> label = getExecutablePrototype(executable, basename, this.context);
							final Collection<? extends Node> list = createExecutableLink(executable, label, null, this.context);
							if (!list.isEmpty()) {
								this.changed.set(true);
								top.appendChildren(list);
							}
						}
					}
				}
			}
		}

	}

}
