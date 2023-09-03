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

import java.net.URL;
import java.nio.file.Path;
import java.util.List;

import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeMirror;

import com.sun.source.doctree.DocTree;
import org.jsoup.nodes.Comment;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.DocumentType;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;

/** Builder of HTML elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 * @see HtmlAccessor
 */
public interface HtmlFactory {

	/** Create the description of a document type.
	 *
	 * @param name is the name of the type.
	 * @param publicId is the public identifier.
	 * @param systemId is the system identifier.
	 * @return the document type.
	 */
	DocumentType createDocumentType(String name, String publicId, String systemId);

	/** Create the description of a HTML document type.
	 *
	 * @param publicId is the public identifier.
	 * @param systemId is the system identifier.
	 * @return the document type.
	 */
	default DocumentType createHtmlDocumentType(String publicId, String systemId) {
		return createDocumentType("HTML", publicId, systemId); //$NON-NLS-1$
	}

	/** Replies the name of the anchor for the given member variable.
	 *
	 * @param element the member.
	 * @return the anchor name.
	 */
	String toVariableAnchor(VariableElement element);

	/** Replies the name of the anchor for the given member variable.
	 *
	 * @param element the element.
	 * @return the anchor name.
	 */
	String toExecutableAnchor(ExecutableElement element);

	/** Replies the name of the anchor for the event handler on the given type.
	 *
	 * @param element the element.
	 * @return the anchor name.
	 */
	String toEventHandlerAnchor(ExecutableElement element);

	/** Convert the given path to an URL path, preserving the relative path state of the input.
	 * This function does not reply a complete URIL, but only the path part.
	 * 
	 * @param path the input path
	 * @return the path part of the URL.
	 */
	default String path2UrlPath(Path path) {
		return path2UrlPath(path, null);
	}

	/** Convert the given path to an URL path, preserving the relative path state of the input.
	 * This function does not reply a complete URIL, but only the path part.
	 * 
	 * @param path the input path.
	 * @param anchor the anchor name in the target page.
	 * @return the path part of the URL.
	 */
	String path2UrlPath(Path path, String anchor);

	/** Create the HTML document with the &lt;html/&gt; tag.
	 *
	 * @param docType the document type (!DOCTYPE tag).
	 * @param context the creation context.
	 * @return the document.
	 */
	Document createDocument(DocumentType docType, HtmlFactoryContext context);

	/** Create the HTML document with the &lt;html/&gt; tag.
	 *
	 * @param context the creation context.
	 * @return the document.
	 */
	default Document createDocument(HtmlFactoryContext context) {
		return createDocument(null, context);
	}

	/** Create the &lt;head/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @return the head node.
	 */
	Element createHeadTag(Element parent);

	/** Create the &lt;title/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param title the title.
	 * @return the title node.
	 */
	Element createTitleTag(Element parent, String title);

	/** Create the &lt;link/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param cssStyle the relative path to the CSS file.
	 * @return the body node.
	 */
	Element createCssLinkTag(Element parent, Path cssStyle);

	/** Create the &lt;script/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param jsScript the relative path to the Javascript script.
	 * @return the body node.
	 */
	Element createJsLinkTag(Element parent, Path jsScript);

	/** Create the &lt;body/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @return the body node.
	 */
	Element createBodyTag(Element parent);

	/** Create the &lt;frameset/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @return the frameset node.
	 */
	Element createFramesetTag(Element parent);

	/** Create the &lt;frame/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @return the frame node.
	 */
	Element createFrameTag(Element parent);

	/** Create the &lt;noframes/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @return the noframes node.
	 */
	Element createNoFramesTag(Element parent);

	/** Create the &lt;script/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param type the type of script.
	 * @return the script node.
	 */
	Element createScriptTag(Element parent, String type);

	/** Create the &lt;noscript/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @return the script node.
	 */
	Element createNoScriptTag(Element parent);

	/** Create the &lt;div/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param style the CSS style.
	 * @return the div node.
	 */
	Element createDivTag(Element parent, CssStyles style);

	/** Create the &lt;h2/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param style the CSS style.
	 * @return the div node.
	 */
	Element createH2Tag(Element parent, CssStyles style);

	/** Create the &lt;p/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param style the CSS style.
	 * @return the div node.
	 */
	Element createParagraphTag(Element parent, CssStyles style);

	/** Create the &lt;dl/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param style the CSS style.
	 * @return the div node.
	 */
	Element createDlTag(Element parent, CssStyles style);

	/** Create the &lt;dd/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param style the CSS style.
	 * @return the div node.
	 */
	Element createDdTag(Element parent, CssStyles style);

	/** Create the &lt;dt/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param style the CSS style.
	 * @return the div node.
	 */
	Element createDtTag(Element parent, CssStyles style);

	/** Create the &lt;span/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param style the CSS style.
	 * @return the span node.
	 */
	Element createSpanTag(Element parent, CssStyles style);

	/** Create the &lt;pre/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param style the CSS style.
	 * @return the span node.
	 */
	Element createPreTag(Element parent, CssStyles style);

	/** Create the &lt;ul/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param style the CSS style.
	 * @return the UL node.
	 */
	Element createUlTag(Element parent, CssStyles style);

	/** Create the &lt;li/&gt; HTML tag into the given parent.
	 *
	 * @param parent the parent node.
	 * @param style the CSS style.
	 * @return the LI node.
	 */
	Element createLiTag(Element parent, CssStyles style);

	/** Create an &amp;nbsp; space character.
	 *
	 * @param parent the parent node.
	 * @return the element.
	 * @see #createSecableSpace(Element)
	 */
	Node createUnsecableSpace(Element parent);

	/** Create an secable space character.
	 *
	 * @param parent the parent node.
	 * @return the element.
	 * @see #createUnsecableSpace(Element)
	 */
	Node createSecableSpace(Element parent);

	/** Create an HTML link.
	 *
	 * @param path the path of the link
	 * @param label the label of the link.
	 * @param style the CSS style.
	 * @return the link
	 */
	default List<Node> createLink(Path path, List<Node> label, CssStyles style) {
		return createLink(path, null, label, style);
	}

	/** Create an HTML link.
	 *
	 * @param path the path of the link
	 * @param label the label of the link.
	 * @param style the CSS style.
	 * @return the link
	 */
	default List<Node> createLink(Path path, String label, CssStyles style) {
		return createLink(path, null, label, style);
	}

	/** Create an HTML link.
	 *
	 * @param path the path of the link
	 * @param anchor the name of the anchor.
	 * @param label the label of the link.
	 * @param style the CSS style.
	 * @return the link
	 */
	List<Node> createLink(Path path, String anchor, List<Node> label, CssStyles style);

	/** Create an HTML link.
	 *
	 * @param path the path of the link
	 * @param anchor the name of the anchor.
	 * @param label the label of the link.
	 * @param style the CSS style.
	 * @return the link
	 */
	List<Node> createLink(Path path, String anchor, String label, CssStyles style);

	/** Create an HTML link.
	 *
	 * @param path the path of the link
	 * @param label the label of the link.
	 * @param style the CSS style.
	 * @return the link
	 */
	List<Node> createLink(URL path, List<Node> label, CssStyles style);

	/** Create an HTML link.
	 *
	 * @param path the path of the link
	 * @param label the label of the link.
	 * @param style the CSS style.
	 * @return the link
	 */
	List<Node> createLink(URL path, String label, CssStyles style);

	/**
	 * Create the module link.
	 *
	 * @param module the module being documented.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the module link, or {@code label} if the given module is not included.
	 */
	List<Node> createModuleLink(ModuleElement module, List<Node> label, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the module link.
	 *
	 * @param module the module being documented.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the module link, or {@code null} if the given module is not included.
	 */
	List<Node> createModuleLink(ModuleElement module, String label, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the package link.
	 *
	 * @param pkg the package being documented.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the package link, or {@code label} if the given package is not included.
	 */
	List<Node> createPackageLink(PackageElement pkg, List<Node> label, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the package link.
	 *
	 * @param pkg the package being documented.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the package link, or {@code label} if the given package is not included.
	 */
	List<Node> createPackageLink(PackageElement pkg, String label, CssStyles style, HtmlFactoryContext context);

	/**
	 * Add a target frame to the given link.
	 *
	 * @param link the nodes that contain the link to update.
	 * @param target the name of the target frame.
	 */
	void addLinkTargetFrame(List<? extends Node> link, String target);

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	default List<Node> createTypeLink(TypeMirror type, List<Node> label, CssStyles style, HtmlFactoryContext context) {
		return createTypeLink(type, null, label, style, context);
	}

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	default List<Node> createTypeLink(TypeMirror type, String label, CssStyles style, HtmlFactoryContext context) {
		return createTypeLink(type, null, label, style, context);
	}

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param anchor the internal anchor to use, or {@code null} if none.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	List<Node> createTypeLink(TypeMirror type, String anchor, List<Node> label, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param anchor the internal anchor to use, or {@code null} if none.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	List<Node> createTypeLink(TypeMirror type, String anchor, String label, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param addTypeParameters indicates if the type parameters must be added.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	default List<Node> createTypeLink(TypeMirror type, boolean addTypeParameters, CssStyles style, HtmlFactoryContext context) {
		return createTypeLink(type, null, addTypeParameters, style, context);
	}

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param anchor the internal anchor to use, or {@code null} if none.
	 * @param addTypeParameters indicates if the type parameters must be added.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	List<Node> createTypeLink(TypeMirror type, String anchor, boolean addTypeParameters, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	default List<Node> createTypeLink(TypeElement type, List<Node> label, CssStyles style, HtmlFactoryContext context) {
		return createTypeLink(type, null, label, style, context);
	}

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param anchor the internal anchor to use, or {@code null} if none.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	List<Node> createTypeLink(TypeElement type, String anchor, List<Node> label, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param linkLabel label for the link.
	 * @param rawLabel label when there is no link to output.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	default List<Node> createTypeLink(TypeElement type, List<Node> linkLabel, Node rawLabel, CssStyles style, HtmlFactoryContext context) {
		return createTypeLink(type, null, linkLabel, rawLabel, style, context);
	}

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param anchor the internal anchor to use, or {@code null} if none.
	 * @param linkLabel label for the link.
	 * @param rawLabel label when there is no link to output.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	List<Node> createTypeLink(TypeElement type, String anchor, List<Node> linkLabel, Node rawLabel, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	default List<Node> createTypeLink(TypeElement type, String label, CssStyles style, HtmlFactoryContext context) {
		return createTypeLink(type, null, label, label, style, context);
	}

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param linkLabel label for the link.
	 * @param rawLabel label when there is no link to output.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	default List<Node> createTypeLink(TypeElement type, String linkLabel, String rawLabel, CssStyles style, HtmlFactoryContext context) {
		return createTypeLink(type, null, linkLabel, rawLabel, style, context);
	}

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param anchor the internal anchor to use, or {@code null} if none.
	 * @param linkLabel label for the link.
	 * @param rawLabel label when there is no link to output.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	List<Node> createTypeLink(TypeElement type, String anchor, String linkLabel, String rawLabel, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param addTypeParameters indicates if the type parameters must be added.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	default List<Node> createTypeLink(TypeElement type, boolean addTypeParameters, CssStyles style, HtmlFactoryContext context) {
		return createTypeLink(type, null, addTypeParameters, style, context);
	}

	/**
	 * Create the type link.
	 *
	 * @param type the type being documented.
	 * @param anchor the internal anchor to use, or {@code null} if none.
	 * @param addTypeParameters indicates if the type parameters must be added.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the type link, or {@code null} if the given type is not included.
	 */
	List<Node> createTypeLink(TypeElement type, String anchor, boolean addTypeParameters, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the variable link.
	 *
	 * @param variable the variable being documented.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the variable link, or {@code null} if the given variable is not included.
	 */
	List<Node> createVariableLink(VariableElement variable, List<Node> label, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the variable link.
	 *
	 * @param variable the variable being documented.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the variable link, or {@code null} if the given variable is not included.
	 */
	List<Node> createVariableLink(VariableElement variable, String label, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the variable link.
	 *
	 * @param executable the executable being documented.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the executable link, or {@code null} if the given executable is not included.
	 */
	List<Node> createExecutableLink(ExecutableElement executable, List<Node> label, CssStyles style, HtmlFactoryContext context);

	/**
	 * Create the variable link.
	 *
	 * @param executable the executable being documented.
	 * @param label tag for the link.
	 * @param style the CSS style.
	 * @param context the creation context.
	 * @return a content for the executable link, or {@code null} if the given executable is not included.
	 */
	List<Node> createExecutableLink(ExecutableElement executable, String label, CssStyles style, HtmlFactoryContext context);

	/**
     * Get the class hierarchy tree for the given type.
     *
	 * @param parent the parent node.
     * @param type the type to print the hierarchy for.
	 * @param listStyle the CSS style for the list.
	 * @param elementStyle the CSS style for the elements.
	 * @param context the creation context.
     * @return a content tree for class inheritance.
     */
    Element createTypeInheritanceTree(Element parent, TypeMirror type, CssStyles listStyle, CssStyles elementStyle, HtmlFactoryContext context);

    /**
	 * Return the HTML representations of the given annotation types.
	 *
	 * @param indent the number of extra spaces to indent the annotations.
	 * @param descList a list of annotation mirrors.
	 * @param lineBreak if true, add new line between each member value.
	 * @param style the CSS style.
	 * @param valueStyle the CSS substyle for the values.
	 * @param context the creation context.
	 * @return a list of HTML representing the annotations being
	 *         documented.
	 */
	List<Element> getAnnotationsFor(int indent, List<? extends AnnotationMirror> descList, boolean lineBreak, CssStyles style, CssStyles valueStyle, HtmlFactoryContext context);

	/** Return the HTML representation of a new line.
	 *
	 * @return the new-line tag. 
	 */
	Element createNewLineTag();

	/** Create the HTML representation of the documentation for the given doc-tree.
	 *
	 * @param memory contains a memory of the generation of comment texts. Never {@code null}.
	 * @param documentationTree the documentation provider.
	 * @param style the CSS style.
	 * @return {@code true} if something has changed.
	 */
	boolean createCommentText(CommentTextMemory memory, DocTree documentationTree, CssStyles style);

	/** Create a memory for the generation of comment texts.
	 *
	 * @param parent the container of the extracted text.
	 * @param element the Java element for which the document is generated.
	 * @param context the creation context.
	 * @return the new memory.
	 */
	CommentTextMemory createCommentTextMemory(Element parent, javax.lang.model.element.Element element, HtmlFactoryContext context);

	/** Create the HTML representation of a code.
	 *
	 * @param parent the parent receiver.
	 * @param content the code content.
	 * @return the code tag.
	 */
	Element createCodeTag(Element parent, String content);

	/** Create a real HTML comment &lt;-- -- &gt;.
	 *
	 * @param parent the container of the comment.
	 * @return the comment node.
	 */
	default Comment createHtmlComment(Element parent) {
		return createHtmlComment(parent, null);
	}

	/** Create a real HTML comment &lt;-- -- &gt;.
	 *
	 * @param parent the container of the comment.
	 * @param text the text of the comment.
	 * @return the comment node.
	 */
	Comment createHtmlComment(Element parent, String text);

	/** Create the HTML tag &lt;table/&gt;
	 *
	 * @param parent the receiver or {@code null} if none.
	 * @param style the CSS style of the table.
	 * @return the table element.
	 */
	Element createTableTag(Element parent, CssStyles style);

	/** Create the HTML tag &lt;thead/&gt;
	 *
	 * @param parent the receiver or {@code null} if none.
	 * @param style the CSS style of the table.
	 * @return the table element.
	 */
	Element createTableHeaderTag(Element parent, CssStyles style);

	/** Create the HTML tag &lt;tbody/&gt;
	 *
	 * @param parent the receiver or {@code null} if none.
	 * @param style the CSS style of the table.
	 * @return the table element.
	 */
	Element createTableBodyTag(Element parent, CssStyles style);

	/** Create the HTML tag &lt;tr/&gt;
	 *
	 * @param parent the receiver or {@code null} if none.
	 * @param style the CSS style of the table.
	 * @return the table element.
	 */
	Element createTableRowTag(Element parent, CssStyles style);

	/** Create the HTML tag &lt;td/&gt;
	 *
	 * @param parent the receiver or {@code null} if none.
	 * @param style the CSS style of the table.
	 * @return the table element.
	 */
	Element createTableCellTag(Element parent, CssStyles style);

	/** Create the HTML tag &lt;th/&gt;
	 *
	 * @param parent the receiver or {@code null} if none.
	 * @param style the CSS style of the table.
	 * @return the table element.
	 */
	Element createTableColumnHeadTag(Element parent, CssStyles style);

	/** Create a sub-factory for tabs. A tab is composed by a title box and a content box.
	 *
	 * @param titleStyle the CSS style of the tab's title.
	 * @param contentStyle the CSS style of the tab's content.
	 * @return the factory.
	 */
	HtmlTabsFactory createTabBox(CssStyles titleStyle, CssStyles contentStyle);

	/** Replies the string representation for the given executable, using the given executable name.
	 *
	 * @param element the element to analyze.
	 * @param context the generator context.
	 * @return the string representation for the executable.
	 */
	default List<Node> getExecutablePrototype(ExecutableElement element, HtmlFactoryContext context) {
		return getExecutablePrototype(element, (String) null, context);
	}

	/** Replies the string representation for the given executable, using the given executable name.
	 *
	 * @param element the element to analyze.
	 * @param name the name of the executable.
	 * @param context the generator context.
	 * @return the string representation for the executable.
	 */
	List<Node> getExecutablePrototype(ExecutableElement element, String name, HtmlFactoryContext context);

	/** Replies the string representation for the given executable, using the given executable name.
	 *
	 * @param element the element to analyze.
	 * @param name the name of the executable.
	 * @param context the generator context.
	 * @return the string representation for the executable.
	 */
	List<Node> getExecutablePrototype(ExecutableElement element, List<Node> name, HtmlFactoryContext context);

	/** Append a keyword into the receiver.
	 *
	 * @param receiver the receiver of the text.
	 * @param keyword the text of the keyword.
	 * @return the receiver or the keyword element.
	 */
	Element keyword(Element receiver, String keyword);

	/** Builder of HTML tabs.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 * @see HtmlFactory
	 */
	public interface HtmlTabsFactory {

		/** Add a tab element.
		 *
		 * @param title the title of the tab.
		 */
		void addTab(String title);

		/** Add a tab element.
		 *
		 * @param title the title of the tab.
		 */
		void addTab(Node title);

		/** Remove the last tab.
		 */
		void removeLastTab();

		/** Replies the number of tabs. 
		 *
		 * @return the number of tabs.
		 */
		int size();

		/** Replies if this container has no tab. 
		 *
		 * @return {@code true} if no tab.
		 */
		default boolean isEmpty() {
			return size() == 0;
		}

		/** Create the element that contains the selectors for tabs. 
		 *
		 * @param parent the parent container, or {@code null}.
		 * @return the element.
		 */
		Element createSelectors(Element parent);

		/** Create the element that contains the content of the tabs. 
		 *
		 * @param parent the parent container, or {@code null}.
		 * @return the element.
		 */
		Element createContents(Element parent);

		/** Replies the element at the given index that contains the selectors for tabs.
		 * The replied element may be not added into a parent element. 
		 *
		 * @param index the index of the element.
		 * @return the element.
		 */
		Element getSelector(int index);

		/** Replies the element at the given index that contains the contents of the tabs.
		 * The replied element may be not added into a parent element. 
		 *
		 * @param index the index of the element.
		 * @return the element.
		 */
		Element getContent(int index);

		/** Replies the lastly added element that contains the selectors for tabs.
		 * The replied element may be not added into a parent element. 
		 *
		 * @return the element.
		 */
		default Element getLastSelector() {
			return getSelector(size() - 1);
		}

		/** Replies the lastly added element that contains the contents of the tabs.
		 * The replied element may be not added into a parent element. 
		 *
		 * @return the element.
		 */
		default Element getLastContent() {
			return getContent(size() - 1);
		}

	}

	/** Memory of the building of a comment text.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	public interface CommentTextMemory {

		/** Replies the parent container of the generated HTML code.
		 *
		 * @return the parent container.
		 */
		Element getRootParent();

		/** Replies the context of the HTML factory caller.
		 *
		 * @return the context.
		 */
		HtmlFactoryContext getContext();

		/** Replies the element for which the text must be generated.
		 *
		 * @return the element.
		 */
		javax.lang.model.element.Element getElement();
		
		/** Add to memory.
		 *
		 * @param name the name.
		 * @param element the element.
		 */
		void pushElement(String name, Node element);

		/** Add to memory.
		 *
		 * @param name the name.
		 * @param element the element.
		 */
		void pushElement(String name, TypeElement element);

		/** Add to memory.
		 *
		 * @param name the name.
		 * @param element the element.
		 */
		void pushElement(String name, VariableElement element);

		/** Add to memory.
		 *
		 * @param name the name.
		 * @param element the element.
		 */
		void pushElement(String name, ExecutableElement element);

		/** Replies the top element.
		 *
		 * @return the top element.
		 */
		Element getTop();

		/** Remove top elements until the element is found.
		 *
		 * @param element the element.
		 */
		void removeTo(Node element);

		/** Remove all the elements at the top of the memory when they have the given key.
		 *
		 * @param key the key of the element to remove.
		 */
		void removeUntil(String key);

		/** Replies if a documentation text has been extracted.
		 *
		 * @return {@code true} if the documentation text is extracted.
		 */
		boolean hasDocumentationText();

		/** Mark the documentation text has changed.
		 */
		void changeDocumentationText();

		/** Collapse the stack in order to obtain a valid stack content.
		 * This function basically force the injection of references that are not consumed.
		 * It enables to generate the reference's output when only a reference is provided
		 * as input.
		 */
		void collapseStack();

	}
	
}
