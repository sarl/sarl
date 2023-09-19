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

package io.sarl.docs.doclet2.html.types;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.NoType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.SimpleAnnotationValueVisitor9;
import javax.lang.model.util.SimpleElementVisitor9;
import javax.tools.Diagnostic.Kind;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.sun.source.doctree.DocCommentTree;
import com.sun.source.doctree.DocTree;
import jdk.javadoc.doclet.Reporter;
import jdk.javadoc.doclet.Taglet.Location;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;
import org.jsoup.nodes.TextNode;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.AbstractDocumentationGenerator;
import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.Navigation;
import io.sarl.docs.doclet2.html.framework.HtmlFactory.CommentTextMemory;
import io.sarl.docs.doclet2.html.framework.Navigation.NavigationKind;
import io.sarl.docs.doclet2.html.taglets.inline.ValueTaglet;
import io.sarl.lang.core.annotation.FiredEvent;
import io.sarl.lang.core.annotation.ImportedCapacityFeature;
import io.sarl.lang.core.annotation.PerceptGuardEvaluator;

/** Abstract implementation of a generator for the documentation for a type.
 *
 * @author $Author: sgalland$
 * @version docs.doclet 0.13.0 20230919-093059
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public abstract class AbstractTypeDocumentationGenerator extends AbstractDocumentationGenerator implements TypeDocumentationGenerator {

	/** Id for the event handler details' box.
	 */
	public static final String ID_EVENT_HANDLER_DETAILS = "event.handlers.details.box"; //$NON-NLS-1$

	/** Id for the action details' box.
	 */
	public static final String ID_ACTION_DETAILS = "action.details.box"; //$NON-NLS-1$

	/** Id for the constructor details' box.
	 */
	public static final String ID_CONSTRUCTOR_DETAILS = "constructors.details.box"; //$NON-NLS-1$

	/** Id for the field details' box.
	 */
	public static final String ID_FIELD_DETAILS = "fields.details.box"; //$NON-NLS-1$

	/** Id for the property details' box.
	 */
	public static final String ID_PROPERTY_DETAILS = "properties.details.box"; //$NON-NLS-1$

	/** Id for the enumeration constant details' box.
	 */
	public static final String ID_ENUM_CONSTANT_DETAILS = "enumeration.constants.details.box"; //$NON-NLS-1$

	/** Id for the capacity use summary' box.
	 */
	public static final String ID_CAPACITY_USE_SUMMARY = "capacity.uses.summary.box"; //$NON-NLS-1$

	/** Id for the property summary' box.
	 */
	public static final String ID_PROPERTY_SUMMARY = "properties.summary.box"; //$NON-NLS-1$

	/** Id for the field summary' box.
	 */
	public static final String ID_FIELD_SUMMARY = "fields.summary.box"; //$NON-NLS-1$

	/** Id for the field summary' box.
	 */
	public static final String ID_ENUM_CONSTANT_SUMMARY = "enumeration.constants.summary.box"; //$NON-NLS-1$

	/** Id for the nested class summary' box.
	 */
	public static final String ID_NESTED_CLASS_SUMMARY = "nested.classes.summary.box"; //$NON-NLS-1$

	/** Id for the constructor summary' box.
	 */
	public static final String ID_CONSTRUCTOR_SUMMARY = "constructors.summary.box"; //$NON-NLS-1$

	/** Id for the action summary' box.
	 */
	public static final String ID_ACTION_SUMMARY = "actions.summary.box"; //$NON-NLS-1$

	/** Id for the event handler summary' box.
	 */
	public static final String ID_EVENT_HANDLER_SUMMARY = "event.handlers.summary.box"; //$NON-NLS-1$

	@Override
	protected void initNavigation(Navigation navigation) {
		navigation.setKind(NavigationKind.TYPE);
	}

	@Override
	protected String getDocumentTitleFor(String elementName) {
		return MessageFormat.format(Messages.AbstractTypeDocumentationGenerator_3, elementName);
	}

	@Override
	public final void generate(TypeElement type, Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		try {
			initGenerator(cssStylesheets, jsScripts, reporter, environment, cliOptions);
			getReporter().print(Kind.NOTE, MessageFormat.format(Messages.AbstractTypeDocumentationGenerator_0, type.getQualifiedName().toString()));
			//
			computePaths(type.getQualifiedName().toString(), true);
			final Path outputPath = cliOptions.getOutputDirectory().resolve(getRelativePath());
			//
			final Document document = getHtmlFactory().createDocument(cliOptions.getCharset(), this);
			final String title = getDocumentTitleFor(type.getSimpleName().toString());
			setLastTitle(title);
			final Element htmlTag = getHtmlAccessor().getRootElement(document);
			//
			generateTypeDocumentation(type, environment, htmlTag);
			//
			if (!cliOptions.isFakeOutput()) {
				getReporter().print(Kind.NOTE, MessageFormat.format(Messages.AbstractTypeDocumentationGenerator_1, outputPath.toString()));
				writeDocument(outputPath, document);
			}
		} catch (Exception ex) {
			final Reporter rep = getReporter();
			if (rep == null) {
				throw new RuntimeException(ex);
			}
			final StringWriter writer = new StringWriter();
			try (final PrintWriter printWriter = new PrintWriter(writer)) {
				String msg = ex.getLocalizedMessage();
				if (Strings.isNullOrEmpty(msg)) {
					msg = ex.getMessage();
				}
				if (Strings.isNullOrEmpty(msg)) {
					msg = ex.getClass().getName();
				}
				printWriter.print(msg);
				printWriter.print(" ("); //$NON-NLS-1$
				printWriter.print(type.getQualifiedName().toString());
				printWriter.println(")"); //$NON-NLS-1$
				ex.printStackTrace(printWriter);
			}
			rep.print(Kind.ERROR, writer.toString());
		}
	}

	/** Generate the type documentation.
	 *
	 * @param type is the type for which the documentation must be generated.
	 * @param environment the generation environment.
	 * @param htmlTag the root HTML tag.
	 * @throws Exception if the documentation cannot be generated.
	 */
	protected void generateTypeDocumentation(TypeElement type, SarlDocletEnvironment environment, Element htmlTag) throws Exception {
		final List<? extends Node> linkContent = getHtmlFactory().createModuleLink(
				getEnvironment().getElementUtils().getModuleOf(type), Messages.AbstractTypeDocumentationGenerator_2, 
				CssStyles.NAVIGATION, this);
		getNavigation().setModuleLink(linkContent);
		//
		generateHtmlHeader(htmlTag, type);
		generateHtmlBody(htmlTag, type);
	}

	/** Generate the HTML header.
	 *
	 * @param htmlTag the container.
	 * @param typeElement the type element.
	 * @return the header.
	 */
	protected Element generateHtmlHeader(Element htmlTag, TypeElement typeElement) {
		final Element headerTree = getHtmlFactory().createHeadTag(htmlTag);
		getHtmlFactory().createTitleTag(headerTree, getLastTitle());
		final Path pathToRoot = getPathToRoot();
		for (final Path cssStyle : getCssStylesheets()) {
			getHtmlFactory().createCssLinkTag(headerTree, pathToRoot.resolve(cssStyle));
		}
		for (final Path jsScript : getJsScripts()) {
			getHtmlFactory().createJsLinkTag(headerTree, pathToRoot.resolve(jsScript));
		}
		return headerTree;
	}

	/** Generate the body of the documentation.
	 *
	 * @param htmlTag the container.
	 * @param typeElement the type element.
	 * @return the body.
	 */
	protected Element generateHtmlBody(Element htmlTag, TypeElement typeElement) {
		final Element bodyTag = getHtmlFactory().createBodyTag(htmlTag);
		generateBodyHeader(bodyTag, typeElement);
		final Element contentTag = getHtmlFactory().createDivTag(bodyTag, CssStyles.CONTENT);
		generateTypeIntroduction(contentTag, typeElement);
		generateTypeTree(contentTag, typeElement);
		generateTypeInfo(contentTag, typeElement);
		generateMemberSummary(contentTag, typeElement);
		generateMemberDetails(contentTag, typeElement);
		generateBodyFooter(bodyTag, typeElement);
		getNavigation().generateNavigationBars(typeElement, this);
		return bodyTag;
	}

	/** Generate the member details defined in the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateMemberDetails(Element parent, TypeElement typeElement) {
		generateEnumConstantsDetails(parent, typeElement);
        generatePropertiesDetails(parent, typeElement);
        generateFieldsDetails(parent, typeElement);
        generateConstructorsDetails(parent, typeElement);
        generateActionsDetails(parent, typeElement);
		generateEventHandlersDetails(parent, typeElement);
	}

	/** Generate details of the constructors of the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateEventHandlersDetails(Element parent, TypeElement typeElement) {
		if (!getElementUtils().isEventHandlerContainer(typeElement)) {
			return;
		}
		final Iterable<ExecutableElement> source = Iterables.filter(typeElement.getEnclosedElements(), ExecutableElement.class);
		final Iterable<ExecutableElement> eventHandlers = Iterables.filter(source,
				it -> it.getKind() == ElementKind.METHOD && !getElementUtils().isStatic(it)
				&& it.getAnnotation(PerceptGuardEvaluator.class) != null);
		//
		createDetailBox(Messages.AbstractTypeDocumentationGenerator_58,
				ID_EVENT_HANDLER_DETAILS, parent, eventHandlers,
				getElementUtils().getExecutableElementComparator(),
				element -> getHtmlFactory().toExecutableAnchor(element), 
				element -> {
					final List<Node> nodes = new ArrayList<>();
					nodes.add(new TextNode(getSARLGrammarKeywordAccess().getOnKeyword()));
					nodes.add(getHtmlFactory().createSecableSpace(null));
					final TypeMirror eventType = element.getParameters().get(0).asType();
					final TypeElement eventTypeElement = getElementUtils().asTypeElement(eventType, getEnvironment().getTypeUtils());
					nodes.add(new TextNode(eventTypeElement.getSimpleName().toString()));
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final Element prototype = getHtmlFactory().createPreTag(null, null);
					getHtmlFactory().keyword(prototype, getSARLGrammarKeywordAccess().getOnKeyword());
					getHtmlFactory().createSecableSpace(prototype);
					final TypeMirror eventType = element.getParameters().get(0).asType();
					prototype.appendChildren(getHtmlFactory().createTypeLink(eventType, false, null, this));
					nodes.add(prototype);
					//
					createFullDescriptionBody(element, nodes, false, true);
					//
					createBlockTagsFor(element, nodes, Location.METHOD, CssStyles.DETAIL_BOX_TAG);
					return nodes;
				});
	}

	/** Generate the syntax for the specification of a return type.
	 *
	 * @param element the element.
	 * @param receiver is the element that will receive the return type.
	 */
	protected void generateReturnTypeConstruct(ExecutableElement element, Element receiver) {
		final TypeMirror rtype = element.getReturnType();
		if (rtype != null && rtype.getKind() != TypeKind.VOID) {
			final List<? extends Node> type = getHtmlFactory().createTypeLink(rtype, true, null, this);
			if (type != null) {
				getHtmlFactory().createSecableSpace(receiver);
				receiver.appendText(getSARLGrammarKeywordAccess().getColonKeyword());
				getHtmlFactory().createSecableSpace(receiver);
				receiver.appendChildren(type);
			}
		}
	}
	
	/** Generate the syntax for the specification of type parameters.
	 *
	 * @param element the element.
	 * @param receiver is the element that will receive the return type.
	 * @param addNewLine if {@code true}, add a new line before the {@code with} construct.
	 */
	protected void generateTypeParameterConstruct(ExecutableElement element, Element receiver, boolean addNewLine) {
		final List<? extends TypeParameterElement> parameters = element.getTypeParameters();
		if (parameters != null && !parameters.isEmpty()) {
			Element realReceiver = receiver;
			if (addNewLine) {
				realReceiver = getHtmlFactory().createParagraphTag(receiver, CssStyles.PRE_INDENT);
			}
			getHtmlFactory().createSecableSpace(realReceiver);
			getHtmlFactory().keyword(realReceiver, getSARLGrammarKeywordAccess().getWithKeyword());
			boolean first = true;
			for (final TypeParameterElement typeParam : parameters) {
				if (first) {
					first = false;
				} else {
					realReceiver.appendText(getSARLGrammarKeywordAccess().getCommaKeyword());
				}
				getHtmlFactory().createSecableSpace(realReceiver);
				realReceiver.appendText(typeParam.getSimpleName().toString());
			    final List<? extends TypeMirror> bounds = typeParam.getBounds();
			    if (bounds != null && !bounds.isEmpty()) {
			    	final Iterable<? extends TypeMirror> boundTypes = Iterables.filter(bounds, it -> {
			    		final TypeElement te = getElementUtils().asTypeElement(it, getEnvironment().getTypeUtils());
			    		return te == null || !Object.class.getName().equals(getElementUtils().getFullyQualifiedName(te, true));
			    	});
			    	if (boundTypes.iterator().hasNext()) {
						getHtmlFactory().createSecableSpace(realReceiver);
						getHtmlFactory().keyword(realReceiver, getSARLGrammarKeywordAccess().getExtendsKeyword());
			    		boolean first0 = true;
				    	for (final TypeMirror tm : boundTypes) {
							getHtmlFactory().createSecableSpace(realReceiver);
				    		if (first0) {
				    			first0 = false;
				    		} else {
				    			realReceiver.appendText(getSARLGrammarKeywordAccess().getAmpersandKeyword());
				    			getHtmlFactory().createSecableSpace(realReceiver);
				    		}
				    		final List<Node> typeRef = getHtmlFactory().createTypeLink(tm, true, null, this);
				    		realReceiver.appendChildren(typeRef);
				    	}
			    	}
			    }
			}
		}
	}

	/** Generate the syntax for the specification of thrown exceptions.
	 *
	 * @param element the element.
	 * @param receiver is the element that will receive the return type.
	 * @param addNewLine if {@code true}, add a new line before the {@code throws} construct.
	 */
	protected void generateThrowsConstruct(ExecutableElement element, Element receiver, boolean addNewLine) {
		final List<? extends TypeMirror> exceptions = element.getThrownTypes();
		if (exceptions != null && !exceptions.isEmpty()) {
			Element realReceiver = receiver;
			if (addNewLine) {
				realReceiver = getHtmlFactory().createParagraphTag(receiver, CssStyles.PRE_INDENT);
			}
			getHtmlFactory().createSecableSpace(realReceiver);
			getHtmlFactory().keyword(realReceiver, getSARLGrammarKeywordAccess().getThrowsKeyword());
			boolean first = true;
			for (final TypeMirror exception : exceptions) {
				final List<? extends Node> type = getHtmlFactory().createTypeLink(exception, true, null, this);
				if (type != null) {
					if (first) {
						first = false;
					} else {
						realReceiver.appendText(getSARLGrammarKeywordAccess().getCommaKeyword());
					}
					getHtmlFactory().createSecableSpace(realReceiver);
					realReceiver.appendChildren(type);
				}
			}
		}
	}

	/** Generate the syntax for the specification of event fires.
	 *
	 * @param element the element.
	 * @param receiver is the element that will receive the return type.
	 * @param addNewLine if {@code true}, add a new line before the {@code fires} construct.
	 */
	protected void generateFiresConstruct(ExecutableElement element, Element receiver, boolean addNewLine) {
		final Iterable<? extends AnnotationMirror> annotations = Iterables.filter(element.getAnnotationMirrors(), it -> {
			final DeclaredType dt = it.getAnnotationType();
			final String qn = getElementUtils().getFullyQualifiedName(dt.asElement(), true);
			if (FiredEvent.class.getName().equals(qn)) {
				final Map<? extends ExecutableElement, ? extends AnnotationValue> values = it.getElementValues();
				if (values.size() == 1) {
					final AnnotationValue value = values.values().iterator().next();
					if (value != null) {
						boolean rvalue = new SimpleAnnotationValueVisitor9<Boolean, Void>() {
							@Override
							public Boolean visitArray(List<? extends AnnotationValue> vals, Void p) {
								return !vals.isEmpty();
							}
							@Override
							protected Boolean defaultAction(Object o, Void p) {
								return false;
							}
						}.visit(value);
						return rvalue;
					}
				}
			}
			return false;
		});
		if (annotations.iterator().hasNext()) {
			final Element realReceiver = addNewLine ? getHtmlFactory().createParagraphTag(receiver, CssStyles.PRE_INDENT) : receiver;
			getHtmlFactory().createSecableSpace(realReceiver);
			getHtmlFactory().keyword(realReceiver, getSARLGrammarKeywordAccess().getFiresKeyword());
			final boolean[] first = new boolean[] { true };
			for (final AnnotationMirror annotation : annotations) {
				final AnnotationValue value = annotation.getElementValues().values().iterator().next();
				new SimpleAnnotationValueVisitor9<Void, Void>() {
					@Override
					public Void visitArray(List<? extends AnnotationValue> vals, Void p) {
						for (final AnnotationValue eventType : vals) {
							final TypeMirror tm = new SimpleAnnotationValueVisitor9<TypeMirror, Void>() {
								@Override
								public TypeMirror visitType(TypeMirror t, Void p) {
									return t;
								}
								@Override
								protected TypeMirror defaultAction(Object o, Void p) {
									return null;
								}
							}.visit(eventType);
							if (tm != null) {
								if (first[0]) {
									first[0] = false;
								} else {
									realReceiver.appendText(getSARLGrammarKeywordAccess().getCommaKeyword());
								}
								getHtmlFactory().createSecableSpace(realReceiver);
								final List<Node> typeNodes = getHtmlFactory().createTypeLink(tm, true, null, AbstractTypeDocumentationGenerator.this);
								realReceiver.appendChildren(typeNodes);
							}
						}
						return null;
					}
					@Override
					protected Void defaultAction(Object o, Void p) {
						return null;
					}
				}.visit(value);
			}
		}
	}

	/** Generate details of the actions of the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateActionsDetails(Element parent, TypeElement typeElement) {
		final Iterable<ExecutableElement> source = Iterables.filter(typeElement.getEnclosedElements(), ExecutableElement.class);
		final Iterable<ExecutableElement> actions = Iterables.filter(source,
				it -> it.getKind() == ElementKind.METHOD && getEnvironment().isIncluded(it));
		//
		createDetailBox(Messages.AbstractTypeDocumentationGenerator_54,
				ID_ACTION_DETAILS, parent, actions,
				getElementUtils().getExecutableElementComparator(),
				element -> getHtmlFactory().toExecutableAnchor(element), 
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String simpleName = element.getSimpleName().toString();
					final List<Node> constructorPrototype = getHtmlFactory().getExecutablePrototype(element, simpleName, this);
					nodes.addAll(constructorPrototype);
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final Element prototype = getHtmlFactory().createPreTag(null, null);
					final String modifierStr = getElementUtils().getVisibilityModifiersString(element, false);
					if (!Strings.isNullOrEmpty(modifierStr)) {
						getHtmlFactory().keyword(prototype, modifierStr);
						getHtmlFactory().createSecableSpace(prototype);
					}
					getHtmlFactory().keyword(prototype, getSARLGrammarKeywordAccess().getDefKeyword());
					getHtmlFactory().createSecableSpace(prototype);
					final String simpleName = element.getSimpleName().toString();
					final List<Node> constructorPrototype = getHtmlFactory().getExecutablePrototype(element, simpleName, this);
					prototype.appendChildren(constructorPrototype);
					generateReturnTypeConstruct(element, prototype);
					generateTypeParameterConstruct(element, prototype, true);
					generateThrowsConstruct(element, prototype, true);
					generateFiresConstruct(element, prototype, true);
					nodes.add(prototype);
					//
					createFullDescriptionBody(element, nodes, false, true);
					//
					createBlockTagsFor(element, nodes, Location.METHOD, CssStyles.DETAIL_BOX_TAG);
					return nodes;
				});
	}

	/** Generate details of the constructors of the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateConstructorsDetails(Element parent, TypeElement typeElement) {
		if (typeElement.getKind() != ElementKind.CLASS) {
			return;
		}
		final Iterable<? extends ExecutableElement> source = Iterables.filter(typeElement.getEnclosedElements(), ExecutableElement.class);
		final Iterable<? extends ExecutableElement> constructors = Iterables.filter(source,
				it -> it.getKind() == ElementKind.CONSTRUCTOR && getEnvironment().isIncluded(it));
		//
		final String constructorName = getSARLGrammarKeywordAccess().getNewKeyword();
		//
		createDetailBox(Messages.AbstractTypeDocumentationGenerator_53, ID_CONSTRUCTOR_DETAILS, parent, constructors,
				getElementUtils().getExecutableElementComparator(),
				element -> getHtmlFactory().toExecutableAnchor(element), 
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final List<Node> constructorPrototype = getHtmlFactory().getExecutablePrototype(element, constructorName, this);
					nodes.addAll(constructorPrototype);
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final Element prototype = getHtmlFactory().createPreTag(null, null);
					final String modifierStr = getElementUtils().getVisibilityModifiersString(element, false);
					if (!Strings.isNullOrEmpty(modifierStr)) {
						getHtmlFactory().keyword(prototype, modifierStr);
						getHtmlFactory().createSecableSpace(prototype);
					}
					final List<Node> label = Collections.singletonList(getHtmlFactory().keyword(null, constructorName));
					final List<Node> constructorPrototype = getHtmlFactory().getExecutablePrototype(element, label, this);
					prototype.appendChildren(constructorPrototype);
					generateTypeParameterConstruct(element, prototype, true);
					generateThrowsConstruct(element, prototype, true);
					generateFiresConstruct(element, prototype, true);
					nodes.add(prototype);
					//
					createFullDescriptionBody(element, nodes, false, true);
					//
					createBlockTagsFor(element, nodes, Location.CONSTRUCTOR, CssStyles.DETAIL_BOX_TAG);
					return nodes;
				});
	}

	/** Generate details of the fields of the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateFieldsDetails(Element parent, TypeElement typeElement) {
		final Iterable<VariableElement> source = Iterables.filter(typeElement.getEnclosedElements(), VariableElement.class);
		final Iterable<VariableElement> fields = Iterables.filter(source, it -> it.getKind() == ElementKind.FIELD && getEnvironment().isIncluded(it));
		//
		createDetailBox(Messages.AbstractTypeDocumentationGenerator_52, ID_FIELD_DETAILS, parent, fields,
				getElementUtils().getVariableElementComparator(),
				element -> getHtmlFactory().toVariableAnchor(element), 
				element -> {
					final List<Node> nodes = new ArrayList<>();
					nodes.add(new TextNode(element.getSimpleName().toString()));
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final Element prototype = getHtmlFactory().createPreTag(null, null);
					final String modifierStr = getElementUtils().getModifiersString(element, false, false, true);
					if (!Strings.isNullOrEmpty(modifierStr)) {
						getHtmlFactory().keyword(prototype, modifierStr);
						getHtmlFactory().createSecableSpace(prototype);
					}
					final String fieldName = element.getSimpleName().toString();
					prototype.appendText(fieldName);
					getHtmlFactory().createSecableSpace(prototype);
					prototype.appendText(getSARLGrammarKeywordAccess().getColonKeyword());
					getHtmlFactory().createSecableSpace(prototype);
					final TypeMirror type = element.asType();
					if (type != null) {
						final List<Node> typeNodes = getHtmlFactory().createTypeLink(type, true, null, this);
						prototype.appendChildren(typeNodes);
					} else {
						prototype.appendChild(new TextNode(element.toString()));
					}
					final Object constantValue = element.getConstantValue();
					if (constantValue != null) {
						getHtmlFactory().createSecableSpace(prototype);
						prototype.appendText(getSARLGrammarKeywordAccess().getEqualsSignKeyword());
						getHtmlFactory().createSecableSpace(prototype);
						prototype.appendChild(new TextNode(ValueTaglet.defaultFormatValue(constantValue)));
					}
					nodes.add(prototype);
					//
					createFullDescriptionBody(element, nodes, false, true);
					//
					createBlockTagsFor(element, nodes, Location.FIELD, CssStyles.DETAIL_BOX_TAG);
					return nodes;
				});
	}

	/** Generate details of the properties of the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generatePropertiesDetails(Element parent, TypeElement typeElement) {
		final Iterable<? extends ExecutableElement> source = Iterables.filter(typeElement.getEnclosedElements(), ExecutableElement.class);
		final NoType voidType = getEnvironment().getTypeUtils().getNoType(TypeKind.VOID);
		final Iterable<? extends ExecutableElement> properties = Iterables.filter(source,
				it -> {
					if (getEnvironment().isIncluded(it) && !getElementUtils().isStatic(it)
							&& it.getParameters().size() == 0 && it.getReturnType() != null
							&& !voidType.equals(it.getReturnType()) && isPropertyGetterName(it.getSimpleName().toString())) {
						return true;
					}
					return false;
				});
		//
		final Set<String> declaredSetters = new TreeSet<>();
		for (final javax.lang.model.element.Element element : getTypeHierarchy().getDeclaredElements(
				typeElement, true, getEnvironment(), it1 -> {
			if (it1 instanceof ExecutableElement) {
				final ExecutableElement ee = (ExecutableElement) it1;
				if (!getElementUtils().isStatic(ee) && ee.getParameters().size() == 1
						&& (ee.getReturnType() == null || voidType.equals(ee.getReturnType()))
						&& isPropertySetterName(ee.getSimpleName().toString())) {
					return true;
				}
				}
			return false;
		})) {
			final String name = setterName2property(element.getSimpleName().toString());
			assert name != null;
			declaredSetters.add(name);
		}
		//
		createDetailBox(Messages.AbstractTypeDocumentationGenerator_50, ID_PROPERTY_DETAILS, parent, properties,
				getElementUtils().getExecutableElementComparator(),
				null, 
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String propertyName = getterName2property(element.getSimpleName().toString());
					nodes.add(new TextNode(propertyName));
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final Element prototype = getHtmlFactory().createPreTag(null, null);
					final String modifierStr = getElementUtils().getVisibilityModifiersString(element, false);
					if (!Strings.isNullOrEmpty(modifierStr)) {
						getHtmlFactory().keyword(prototype, modifierStr);
						getHtmlFactory().createSecableSpace(prototype);
					}
					final String propertyName = getterName2property(element.getSimpleName().toString());
					if (declaredSetters.contains(propertyName)) {
						getHtmlFactory().keyword(prototype, getSARLGrammarKeywordAccess().getWriteableVarKeyword());
					} else {
						getHtmlFactory().keyword(prototype, getSARLGrammarKeywordAccess().getValKeyword());
					}
					getHtmlFactory().createSecableSpace(prototype);
					prototype.appendText(propertyName);
					getHtmlFactory().createSecableSpace(prototype);
					prototype.appendText(getSARLGrammarKeywordAccess().getColonKeyword());
					getHtmlFactory().createSecableSpace(prototype);
					final TypeMirror type = element.getReturnType();
					if (type != null) {
						final List<Node> typeNodes = getHtmlFactory().createTypeLink(type, true, null, this);
						prototype.appendChildren(typeNodes);
					} else {
						prototype.appendChild(new TextNode(element.toString()));
					}
					nodes.add(prototype);
					//
					createFullDescriptionBody(element, nodes, false, true);
					//
					final List<Node> aliasedLabel = getHtmlFactory().getExecutablePrototype(element, this);
					final List<Node> aliasedNodes = getHtmlFactory().createExecutableLink(element, aliasedLabel, null, this);
					if (!aliasedNodes.isEmpty()) {
						final Element paragraph = getHtmlFactory().createParagraphTag(null, null);
						paragraph.appendText(Messages.AbstractTypeDocumentationGenerator_51);
						paragraph.appendChildren(aliasedNodes);
						nodes.add(paragraph);
					}
					//
					createBlockTagsFor(element, nodes, Location.METHOD, CssStyles.DETAIL_BOX_TAG);
					return nodes;
				});
	}

	/** Generate details of the enum constants defined in the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateEnumConstantsDetails(Element parent, TypeElement typeElement) {
		if (typeElement.getKind() != ElementKind.ENUM) {
			return;
		}
		final Iterable<? extends VariableElement> source = Iterables.filter(typeElement.getEnclosedElements(), VariableElement.class);
		final Iterable<? extends VariableElement> enumConstants = Iterables.filter(source, it -> it.getKind() == ElementKind.ENUM_CONSTANT);
		createDetailBox(Messages.AbstractTypeDocumentationGenerator_49, ID_ENUM_CONSTANT_DETAILS, parent, enumConstants,
				getElementUtils().getVariableElementComparator(),
				element -> getHtmlFactory().toVariableAnchor(element), 
				element -> {
					final List<Node> nodes = new ArrayList<>();
					nodes.add(new TextNode(element.getSimpleName().toString()));
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final Element prototype0 = getHtmlFactory().createPreTag(null, null);
					nodes.add(prototype0);
					final Element prototype1 = getHtmlFactory().createPreTag(prototype0, null);
					getHtmlFactory().keyword(prototype1, getSARLGrammarKeywordAccess().getStaticStaticKeyword());
					getHtmlFactory().createSecableSpace(prototype1);
					getHtmlFactory().keyword(prototype1, getSARLGrammarKeywordAccess().getValKeyword());
					getHtmlFactory().createSecableSpace(prototype1);
					prototype1.appendText(element.getSimpleName().toString());
					getHtmlFactory().createSecableSpace(prototype1);
					prototype1.appendText(getSARLGrammarKeywordAccess().getColonKeyword());
					getHtmlFactory().createSecableSpace(prototype1);
					prototype1.appendText(typeElement.getSimpleName().toString());
					//
					createFullDescriptionBody(element, nodes, false, true);
					createBlockTagsFor(element, nodes, Location.FIELD, CssStyles.DETAIL_BOX_TAG);
					return nodes;
				});
	}

	/** Generate the member summary defined in the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateMemberSummary(Element parent, TypeElement typeElement) {
		generatePropertiesSummary(parent, typeElement);
		generateNestedClassesSummary(parent, typeElement);
		generateEnumConstantsSummary(parent, typeElement);
		generateFieldsSummary(parent, typeElement);
		generateConstructorsSummary(parent, typeElement);
		generateActionsSummary(parent, typeElement);
		generateEventHandlersSummary(parent, typeElement);
		generateCapacityUsesSummary(parent, typeElement);
	}

	/** Generate list of uses capacities.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateCapacityUsesSummary(Element parent, TypeElement typeElement) {
		if (!getElementUtils().isCapacityUser(typeElement)) {
			return;
		}
		final Iterable<VariableElement> source = Iterables.filter(typeElement.getEnclosedElements(), VariableElement.class);
		final Iterable<VariableElement> capacityUsers = Iterables.filter(source,
				it -> it.getKind() == ElementKind.FIELD && !getElementUtils().isStatic(it)
				&& it.getAnnotation(ImportedCapacityFeature.class) != null);
		final Iterable<AnnotationMirror> capacityUses = Iterables.transform(capacityUsers, it -> {
			final Iterable<? extends AnnotationMirror> mirrors = Iterables.filter(it.getAnnotationMirrors(), it0 -> {
				final DeclaredType dt = it0.getAnnotationType();
				final String qn = getElementUtils().getFullIdentifier(dt.asElement());
				return ImportedCapacityFeature.class.getName().equals(qn);
			});
			return mirrors.iterator().next();
		});
		final Set<TypeElement> capacityElements = new TreeSet<>(getElementUtils().getTypeElementComparator());
		for (final AnnotationMirror annotation : capacityUses) {
			if (annotation.getElementValues().size() == 1) {
				new SimpleAnnotationValueVisitor9<Void,Void>() {
					@Override
					public Void visitType(TypeMirror t, Void p) {
						final TypeElement te = getElementUtils().asTypeElement(t, getEnvironment().getTypeUtils());
						capacityElements.add(te);
						return null;
					}
					@Override
					protected Void defaultAction(Object o, Void p) {
						return null;
					}
				}.visit(annotation.getElementValues().values().iterator().next());
			}
		}
		//
		createSummaryBox1(Messages.AbstractTypeDocumentationGenerator_59, Messages.AbstractTypeDocumentationGenerator_60,
				Messages.AbstractTypeDocumentationGenerator_61, ID_CAPACITY_USE_SUMMARY, parent, capacityElements,
				getElementUtils().getTypeElementComparator(),
				element -> getHtmlFactory().createTypeLink(element, false, null, this));
	}

	/** Generate list of the event handlers on the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateEventHandlersSummary(Element parent, TypeElement typeElement) {
		if (!getElementUtils().isEventHandlerContainer(typeElement)) {
			return;
		}
		final Iterable<ExecutableElement> source = Iterables.filter(typeElement.getEnclosedElements(), ExecutableElement.class);
		final Iterable<ExecutableElement> eventHandlers = Iterables.filter(source,
				it -> it.getKind() == ElementKind.METHOD && !getElementUtils().isStatic(it)
				&& it.getAnnotation(PerceptGuardEvaluator.class) != null);
		//
		createSummaryBox1(Messages.AbstractTypeDocumentationGenerator_56, Messages.AbstractTypeDocumentationGenerator_55,
				Messages.AbstractTypeDocumentationGenerator_57, ID_EVENT_HANDLER_SUMMARY, parent, eventHandlers,
				getElementUtils().getExecutableElementComparator(),
				element -> {
					final List<Node> label = new ArrayList<>();
					label.add(new TextNode(getSARLGrammarKeywordAccess().getOnKeyword()));
					label.add(getHtmlFactory().createSecableSpace(null));
					final TypeMirror eventType = element.getParameters().get(0).asType();
					final TypeElement eventTypeElement = getElementUtils().asTypeElement(eventType, getEnvironment().getTypeUtils());
					label.add(new TextNode(eventTypeElement.getSimpleName().toString()));
					final String anchor = getHtmlFactory().toEventHandlerAnchor(element);
					List<Node> elementLink = label;
					try {
						elementLink = getHtmlFactory().createLink((Path) null, anchor, label, null);
					} catch (Throwable ex) {
						//
					}
					final Element emphLink = getHtmlFactory().createSpanTag(null, CssStyles.SUMMARY_BOX_ID);
					emphLink.appendChildren(elementLink);
					final List<Node> nodes = new ArrayList<>();
					nodes.add(emphLink);
					createFirstSentence(element, nodes, true, false);
					createShortDeprecationMessage(element, nodes, true);
					return nodes;
				},
				() -> {
					final SortedMap<String, List<? extends Node>> nodes = new TreeMap<>();
					for (final javax.lang.model.element.Element element : getTypeHierarchy().getInheritedElements(
							typeElement, true, false, true, true, getEnvironment(), it1 -> {
						if (it1 instanceof ExecutableElement && it1.getKind() == ElementKind.METHOD && !getElementUtils().isStatic(it1)
								&& it1.getAnnotation(PerceptGuardEvaluator.class) != null) {
							return true;
						}
						return false;
					})) {
						final javax.lang.model.element.Element enclosing = element.getEnclosingElement();
						final TypeMirror enclosingType = enclosing.asType();
						final String anchor = getHtmlFactory().toEventHandlerAnchor((ExecutableElement) element);
						final TypeElement enclosingTypeElement = getElementUtils().asTypeElement(enclosingType, getEnvironment().getTypeUtils());
						final List<Node> label = new ArrayList<>();
						label.add(new TextNode(enclosingTypeElement.getSimpleName().toString()));
						label.add(new TextNode("/")); //$NON-NLS-1$
						label.add(new TextNode(getSARLGrammarKeywordAccess().getOnKeyword()));
						label.add(getHtmlFactory().createSecableSpace(null));
						final TypeMirror eventType = ((ExecutableElement) element).getParameters().get(0).asType();
						final TypeElement eventTypeElement = getElementUtils().asTypeElement(eventType, getEnvironment().getTypeUtils());
						label.add(new TextNode(eventTypeElement.getSimpleName().toString()));
						List<Node> elementLink = label;
						try {
							elementLink = getHtmlFactory().createTypeLink(enclosingType, anchor, label, null, this);
						} catch (Throwable ex) {
							//
						}
						nodes.putIfAbsent(eventTypeElement.getSimpleName().toString(), elementLink);
					}
					final List<Node> list = new ArrayList<>();
					boolean first = true;
					for (List<? extends Node> entity : nodes.values()) {
						if (first) {
							first = false;
						} else {
							list.add(new TextNode(getSARLGrammarKeywordAccess().getCommaKeyword()));
							list.add(getHtmlFactory().createSecableSpace(null));
						}
						list.addAll(entity);
					}
					return list;
				});
	}

	/** Generate list of the actions on the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateActionsSummary(Element parent, TypeElement typeElement) {
		final Iterable<ExecutableElement> source = Iterables.filter(typeElement.getEnclosedElements(), ExecutableElement.class);
		final Iterable<ExecutableElement> allActions = Iterables.filter(source,
				it -> it.getKind() == ElementKind.METHOD && getEnvironment().isIncluded(it));
		final Iterable<ExecutableElement> staticActions = Iterables.filter(allActions, it -> getElementUtils().isStatic(it));
		final Iterable<ExecutableElement> concreteActions = Iterables.filter(allActions, it -> !getElementUtils().isStatic(it) && !getElementUtils().isAbstract(it));
		final Iterable<ExecutableElement> abstractActions = Iterables.filter(allActions, it -> !getElementUtils().isStatic(it) && getElementUtils().isAbstract(it));
		final Map<String, Iterable<? extends ExecutableElement>> actions = new LinkedHashMap<>();
		// All methods
		actions.put(Messages.AbstractTypeDocumentationGenerator_39, allActions);
		// Static methods
		actions.put(Messages.AbstractTypeDocumentationGenerator_42, staticActions);
		// Abstract methods
		actions.put(Messages.AbstractTypeDocumentationGenerator_48, abstractActions);
		// Concrete methods
		actions.put(Messages.AbstractTypeDocumentationGenerator_43, concreteActions);
		createSummaryBox2(
				Messages.AbstractTypeDocumentationGenerator_38, Messages.AbstractTypeDocumentationGenerator_40,
				Messages.AbstractTypeDocumentationGenerator_41, ID_ACTION_SUMMARY, parent, actions,
				getElementUtils().getExecutableElementComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String modifierStr = getElementUtils().getModifiersString(element, false, true, false);
					if (!Strings.isNullOrEmpty(modifierStr)) {
						nodes.add(new TextNode(modifierStr));
						nodes.add(getHtmlFactory().createUnsecableSpace(null));
					}
					final TypeMirror rtype = element.getReturnType();
					final List<? extends Node> type = getHtmlFactory().createTypeLink(rtype, true, null, this);
					if (type != null) {
						nodes.addAll(type);
					}
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String methodName = element.getSimpleName().toString();
					final List<Node> methodPrototype = getHtmlFactory().getExecutablePrototype(element, methodName, this);
					final List<? extends Node> elementLink = getHtmlFactory().createExecutableLink(element, methodPrototype, null, this);
					if (elementLink != null) {
						final Element emphLink = getHtmlFactory().createSpanTag(null, CssStyles.SUMMARY_BOX_ID);
						emphLink.appendChildren(elementLink);
						nodes.add(emphLink);
					}
					createFirstSentence(element, nodes, true, false);
					createShortDeprecationMessage(element, nodes, true);
					return nodes;
				},
				() -> {
					final SortedMap<String, List<? extends Node>> nodes = new TreeMap<>();
					for (final javax.lang.model.element.Element element : getTypeHierarchy().getInheritedElements(
							typeElement, true, getEnvironment(), it1 -> {
						if (it1 instanceof ExecutableElement && it1.getKind() == ElementKind.METHOD) {
							return true;
						}
						return false;
					})) {
						final ExecutableElement ee = (ExecutableElement) element;
						final String methodName = element.getSimpleName().toString();
						final List<Node> methodPrototype = getHtmlFactory().getExecutablePrototype(ee, methodName, this);
						nodes.putIfAbsent(methodPrototype.toString(), getHtmlFactory().createExecutableLink(ee, methodPrototype, null, this));
					}
					final List<Node> list = new ArrayList<>();
					for (List<? extends Node> entity : nodes.values()) {
						list.addAll(entity);
					}
					return list;
				});
	}

	/** Generate list of the constructors defined in the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateConstructorsSummary(Element parent, TypeElement typeElement) {
		if (typeElement.getKind() != ElementKind.CLASS) {
			return;
		}
		final Iterable<? extends ExecutableElement> source = Iterables.filter(typeElement.getEnclosedElements(), ExecutableElement.class);
		final Iterable<? extends ExecutableElement> constructors = Iterables.filter(source,
				it -> it.getKind() == ElementKind.CONSTRUCTOR && getEnvironment().isIncluded(it));
		final String constructorName = getSARLGrammarKeywordAccess().getNewKeyword();
		createSummaryBox1(Messages.AbstractTypeDocumentationGenerator_35, Messages.AbstractTypeDocumentationGenerator_36,
				Messages.AbstractTypeDocumentationGenerator_37, ID_CONSTRUCTOR_SUMMARY, parent, constructors,
				getElementUtils().getExecutableElementComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final List<Node> constructorPrototype = getHtmlFactory().getExecutablePrototype(element, constructorName, this);
					final List<Node> elementLink = getHtmlFactory().createExecutableLink(element, constructorPrototype, null, this);
					if (elementLink != null) {
						final Element emphLink = getHtmlFactory().createSpanTag(null, CssStyles.SUMMARY_BOX_ID);
						emphLink.appendChildren(elementLink);
						nodes.add(emphLink);
					}
					createFirstSentence(element, nodes, true, false);
					createShortDeprecationMessage(element, nodes, true);
					return nodes;
				});
	}

	/** Generate the list of nested classes defined in the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateNestedClassesSummary(Element parent, TypeElement typeElement) {
		final Iterable<? extends TypeElement> source = Iterables.filter(typeElement.getEnclosedElements(), TypeElement.class);
		final Iterable<? extends TypeElement> nestedTypes = Iterables.filter(source, it -> getEnvironment().isIncluded(it));
		createSummaryBox2(Messages.AbstractTypeDocumentationGenerator_31, Messages.AbstractTypeDocumentationGenerator_32,
				Messages.AbstractTypeDocumentationGenerator_33, Messages.AbstractTypeDocumentationGenerator_34,
				ID_NESTED_CLASS_SUMMARY, parent, nestedTypes,
				getElementUtils().getTypeElementComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String modifierStr = getElementUtils().getVisibilityModifiersString(element, false);
					if (!Strings.isNullOrEmpty(modifierStr)) {
						nodes.add(new TextNode(modifierStr));
					}
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String typeName = getElementUtils().getInnerTypeQualifiedName(element);
					final List<? extends Node> elementLink = getHtmlFactory().createTypeLink(element, typeName, null, this);
					if (elementLink != null) {
						final Element emphLink = getHtmlFactory().createSpanTag(null, CssStyles.SUMMARY_BOX_ID);
						emphLink.appendChildren(elementLink);
						nodes.add(emphLink);
					}
					createFirstSentence(element, nodes, true, false);
					createShortDeprecationMessage(element, nodes, true);
					return nodes;
				},
				null);
	}

	/** Generate list of the enum constants defined in the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateEnumConstantsSummary(Element parent, TypeElement typeElement) {
		if (typeElement.getKind() != ElementKind.ENUM) {
			return;
		}
		final Iterable<? extends VariableElement> source = Iterables.filter(typeElement.getEnclosedElements(), VariableElement.class);
		final Iterable<? extends VariableElement> enumConstants = Iterables.filter(source, it -> it.getKind() == ElementKind.ENUM_CONSTANT);
		createSummaryBox1(Messages.AbstractTypeDocumentationGenerator_23, Messages.AbstractTypeDocumentationGenerator_24,
				Messages.AbstractTypeDocumentationGenerator_25, ID_ENUM_CONSTANT_SUMMARY, parent, enumConstants,
				getElementUtils().getVariableElementComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String constantName = element.getSimpleName().toString();
					final List<? extends Node> elementLink = getHtmlFactory().createVariableLink(element, constantName, null, this);
					if (elementLink != null) {
						final Element emphLink = getHtmlFactory().createSpanTag(null, CssStyles.SUMMARY_BOX_ID);
						emphLink.appendChildren(elementLink);
						nodes.add(emphLink);
					}
					createFirstSentence(element, nodes, true, false);
					createShortDeprecationMessage(element, nodes, true);
					return nodes;
				});
	}

	/** Generate list of the fields on the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateFieldsSummary(Element parent, TypeElement typeElement) {
		final Iterable<VariableElement> source = Iterables.filter(typeElement.getEnclosedElements(), VariableElement.class);
		final Iterable<VariableElement> allFields = Iterables.filter(source, it -> it.getKind() == ElementKind.FIELD && getEnvironment().isIncluded(it));
		final Iterable<VariableElement> staticFields = Iterables.filter(allFields, it -> !getElementUtils().isFinal(it) && getElementUtils().isStatic(it));
		final Iterable<VariableElement> concreteFields = Iterables.filter(allFields, it -> !getElementUtils().isFinal(it) && !getElementUtils().isStatic(it));
		final Iterable<VariableElement> staticValues = Iterables.filter(allFields, it -> getElementUtils().isFinal(it) && getElementUtils().isStatic(it));
		final Iterable<VariableElement> concreteValues = Iterables.filter(allFields, it -> getElementUtils().isFinal(it) && !getElementUtils().isStatic(it));
		final Map<String, Iterable<? extends VariableElement>> fields = new LinkedHashMap<>();
		// All fields
		fields.put(Messages.AbstractTypeDocumentationGenerator_28, allFields);
		// Static fields
		fields.put(Messages.AbstractTypeDocumentationGenerator_44, staticFields);
		// Concrete fields
		fields.put(Messages.AbstractTypeDocumentationGenerator_45, concreteFields);
		// Static values
		fields.put(Messages.AbstractTypeDocumentationGenerator_46, staticValues);
		// Concrete values
		fields.put(Messages.AbstractTypeDocumentationGenerator_47, concreteValues);
		createSummaryBox2(Messages.AbstractTypeDocumentationGenerator_27,
				Messages.AbstractTypeDocumentationGenerator_29, Messages.AbstractTypeDocumentationGenerator_30,
				ID_FIELD_SUMMARY, parent, fields,
				getElementUtils().getVariableElementComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String modifierStr = getElementUtils().getModifiersString(element, false, false, true);
					if (!Strings.isNullOrEmpty(modifierStr)) {
						nodes.add(new TextNode(modifierStr));
						nodes.add(getHtmlFactory().createUnsecableSpace(null));
					}
					final TypeMirror rtype = element.asType();
					final List<? extends Node> type = getHtmlFactory().createTypeLink(rtype, true, null, this);
					if (type != null) {
						nodes.addAll(type);
					}
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String fieldName = element.getSimpleName().toString();
					final List<? extends Node> elementLink = getHtmlFactory().createVariableLink(element, fieldName, null, this);
					if (elementLink != null) {
						final Element emphLink = getHtmlFactory().createSpanTag(null, CssStyles.SUMMARY_BOX_ID);
						emphLink.appendChildren(elementLink);
						nodes.add(emphLink);
					}
					createFirstSentence(element, nodes, true, false);
					createShortDeprecationMessage(element, nodes, true);
					return nodes;
				},
				() -> {
					final SortedMap<String, List<? extends Node>> nodes = new TreeMap<>();
					for (final javax.lang.model.element.Element element : getTypeHierarchy().getInheritedElements(
							typeElement, true, getEnvironment(), it1 -> {
						if (it1 instanceof VariableElement && it1.getKind() == ElementKind.FIELD) {
							return true;
						}
						return false;
					})) {
						final String name = element.getSimpleName().toString();
						nodes.putIfAbsent(name, getHtmlFactory().createVariableLink((VariableElement) element, name, null, this));
					}
					final List<Node> list = new ArrayList<>();
					for (List<? extends Node> entity : nodes.values()) {
						list.addAll(entity);
					}
					return list;
				});
	}

	/** Generate list of the properties on the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generatePropertiesSummary(Element parent, TypeElement typeElement) {
		final Iterable<? extends ExecutableElement> source = Iterables.filter(typeElement.getEnclosedElements(), ExecutableElement.class);
		final NoType voidType = getEnvironment().getTypeUtils().getNoType(TypeKind.VOID);
		final Iterable<? extends ExecutableElement> properties = Iterables.filter(source,
				it -> {
					if (getEnvironment().isIncluded(it) && !getElementUtils().isStatic(it)
							&& it.getParameters().size() == 0 && it.getReturnType() != null
							&& !voidType.equals(it.getReturnType()) && isPropertyGetterName(it.getSimpleName().toString())) {
						return true;
					}
					return false;
				});
		createSummaryBox2(
				Messages.AbstractTypeDocumentationGenerator_19, Messages.AbstractTypeDocumentationGenerator_20,
				Messages.AbstractTypeDocumentationGenerator_21, Messages.AbstractTypeDocumentationGenerator_22,
				ID_PROPERTY_SUMMARY, parent, properties,
				getElementUtils().getExecutableElementComparator(),
				element -> {
					final List<Node> nodes = new ArrayList<>();
					
					final String modifierStr = getElementUtils().getVisibilityModifiersString(element, false);
					if (!Strings.isNullOrEmpty(modifierStr)) {
						nodes.add(new TextNode(modifierStr));
						nodes.add(getHtmlFactory().createUnsecableSpace(null));
					}
					final TypeMirror rtype = element.getReturnType();
					final List<? extends Node> type = getHtmlFactory().createTypeLink(rtype, true, null, this);
					if (type != null) {
						nodes.addAll(type);
					}
					return nodes;
				},
				element -> {
					final List<Node> nodes = new ArrayList<>();
					final String propertyName = getterName2property(element.getSimpleName().toString());
					final List<? extends Node> elementLink = getHtmlFactory().createExecutableLink(element, propertyName, null, this);
					if (elementLink != null) {
						final Element emphLink = getHtmlFactory().createSpanTag(null, CssStyles.SUMMARY_BOX_ID);
						emphLink.appendChildren(elementLink);
						nodes.add(emphLink);
					}
					createFirstSentence(element, nodes, true, false);
					createShortDeprecationMessage(element, nodes, true);
					return nodes;
				},
				() -> {
					final SortedMap<String, List<? extends Node>> nodes = new TreeMap<>();
					for (final javax.lang.model.element.Element element : getTypeHierarchy().getInheritedElements(
							typeElement, true, getEnvironment(), it1 -> {
						if (it1 instanceof ExecutableElement) {
							final ExecutableElement ee = (ExecutableElement) it1;
							if (!getElementUtils().isStatic(ee) && ee.getParameters().size() == 0 && ee.getReturnType() != null
									&& !voidType.equals(ee.getReturnType()) && isPropertyGetterName(ee.getSimpleName().toString())) {
								return true;
							}
 						}
						return false;
					})) {
						final String name = getterName2property(element.getSimpleName().toString());
						nodes.putIfAbsent(name, getHtmlFactory().createExecutableLink((ExecutableElement) element, name, null, this));
					}
					final List<Node> list = new ArrayList<>();
					for (List<? extends Node> entity : nodes.values()) {
						list.addAll(entity);
					}
					return list;
				});
	}

	/** Generate the information box on the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateTypeInfo(Element parent, TypeElement typeElement) {
		final Element divTag = getHtmlFactory().createDivTag(parent, CssStyles.TYPE_INFO_BOX);
		generateTypeParameterInfo(divTag, typeElement);
		generateSuperInterfacesInfo(divTag, typeElement);
		generateSubTypeInfo(divTag, typeElement);
		generateInterfaceUsageInfo(divTag, typeElement);
		generateNestedTypeInfo(divTag, typeElement);
		generateFunctionalInterfaceInfo(divTag, typeElement);
		generateDeprecationInfo(divTag, typeElement);
		generateTypeSignature(divTag, typeElement);
		generateTypeDescription(divTag, typeElement);
		generateTypeTagInfo(divTag, typeElement);
	}

	/** Generate the documentation tags for a given type.
	 *
	 * @param parent is the container element.
	 * @param typeElement is the element for which the deprecation info must be generated.
	 */
	protected void generateTypeTagInfo(Element parent, TypeElement typeElement) {
		final Element dlTag = getHtmlFactory().createDlTag(parent, CssStyles.TYPE_TAG_INFO);
		createTagInfo(dlTag, typeElement, Location.TYPE, CssStyles.TYPE_TAG_INFO);
	}

	/** Generate the detailed description of the given type.
	 *
	 * @param parent is the container element.
	 * @param typeElement is the element for which the deprecation info must be generated.
	 */
	protected void generateTypeDescription(Element parent, TypeElement typeElement) {
		final Element descriptionTag = getHtmlFactory().createDivTag(parent, CssStyles.TYPE_DESCRIPTION);
		final DocCommentTree commentTree = getEnvironment().getDocTrees().getDocCommentTree(typeElement);
		if (commentTree != null) {
			final List<? extends DocTree> body = commentTree.getFullBody();
			if (!body.isEmpty()) {
				final Element textDiv = getHtmlFactory().createDivTag(descriptionTag, CssStyles.TYPE_DESCRIPTION_MAIN);
				final CommentTextMemory memory = getHtmlFactory().createCommentTextMemory(textDiv, typeElement, this);
				for (final DocTree comment : body) {
					getHtmlFactory().createCommentText(memory, comment, CssStyles.TYPE_DESCRIPTION_MAIN);
				}
			}
		}
	}

	/** Generate the annotation information for the given type.
	 *
	 * @param parent is the container element.
	 * @param typeElement is the element for which the deprecation info must be generated.
	 */
	protected void generateDeprecationInfo(Element parent, TypeElement typeElement) {
		final javax.lang.model.element.Element deprecatedElement = getElementUtils().getFirstEnclosingDeprecatedElement(typeElement);
		if (deprecatedElement != null) {
			final List<? extends DocTree> deprs = getDocUtils().getBlockTags(deprecatedElement, DocTree.Kind.DEPRECATED, getEnvironment());
			if (!deprs.isEmpty()) {
				final Element dlTag = getHtmlFactory().createDlTag(parent, CssStyles.DEPRECATION_INFO);
				final Element dtTag = getHtmlFactory().createDtTag(dlTag, CssStyles.DEPRECATION_INFO);
				final boolean isForRemoval = getElementUtils().isDeprecatedForRemoval(deprecatedElement);
				final String since = getElementUtils().getDeprecatedSince(deprecatedElement);
				if (Strings.isNullOrEmpty(since)) {
					if (isForRemoval) {
						dtTag.appendText(Messages.AbstractTypeDocumentationGenerator_16);
					} else {
						dtTag.appendText(Messages.AbstractTypeDocumentationGenerator_17);
					}
				} else if (isForRemoval) {
					dtTag.appendText(MessageFormat.format(Messages.AbstractTypeDocumentationGenerator_62, since));
				} else {
					dtTag.appendText(MessageFormat.format(Messages.AbstractTypeDocumentationGenerator_63, since));
				}
				final Element ddTag = getHtmlFactory().createDdTag(dlTag, CssStyles.DEPRECATION_INFO);
				//
				final CommentTextMemory memory = getHtmlFactory().createCommentTextMemory(ddTag, deprecatedElement, this);
				for (final DocTree comment : deprs) {
					final List<? extends DocTree> text = getDocUtils().getCommentForDeprecatedTag(comment);
					if (!text.isEmpty()) {
						for (final DocTree tree : text) {
							getHtmlFactory().createCommentText(memory, tree, CssStyles.DEPRECATION_INFO);
						}
					}
				}
			}
		}
	}

	/** Generate the annotation information for the given type.
	 *
	 * @param parent the containing tag.
	 * @param typeElement the type to analyze.
	 */
	protected void generateAnnotationInfo(Element parent, TypeElement typeElement) {
		addAnnotationInfo(parent, 0, typeElement, typeElement.getAnnotationMirrors(), true);
	}

	private boolean addAnnotationInfo(Element containerTag, int indent, TypeElement element, List<? extends AnnotationMirror> descList, boolean lineBreak) {
		final List<Element> annotations = getHtmlFactory().getAnnotationsFor(indent, descList, lineBreak,
				CssStyles.TYPE_SIGNATURE_ANNOTATION_INFO, CssStyles.TYPE_SIGNATURE_ANNOTATION_INFO_VALUE, this);

		if (annotations.isEmpty()) {
			return false;
		}
		boolean first = true;
		for (final Element annotation : annotations) {
			containerTag.appendChild(annotation);
			if (first) {
				first = false;
			} else if (!lineBreak) {
				containerTag.appendText(" "); //$NON-NLS-1$
			}
		}
		return true;
	}

	/** Generate the signature of the type. 
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateTypeSignature(Element parent, TypeElement typeElement) {
		final Element preTag = getHtmlFactory().createDivTag(parent, CssStyles.TYPE_SIGNATURE);

		// Annotations
		generateAnnotationInfo(preTag, typeElement);

		// Modifiers
		final String modifiers = getElementUtils().getModifiersString(typeElement, true, true, false);
		preTag.appendText(modifiers);

		// Basename
		final Element typeNameElement = getHtmlFactory().createSpanTag(preTag, CssStyles.TYPE_SIGNATURE_TYPE_NAME);
		typeNameElement.appendText(typeElement.getSimpleName().toString());

		// Generic types
		if (!typeElement.getTypeParameters().isEmpty()) {
			typeNameElement.appendText(getSARLGrammarKeywordAccess().getLessThanSignKeyword());
			boolean first = true;
			for (final TypeParameterElement parameter : typeElement.getTypeParameters()) {
				if (first) {
					first = false;
				} else {
					typeNameElement.appendText(getSARLGrammarKeywordAccess().getCommaKeyword());
					typeNameElement.appendText(" "); //$NON-NLS-1$
				}
				typeNameElement.appendText(parameter.toString());
			}
			typeNameElement.appendText(getSARLGrammarKeywordAccess().getGreaterThanSignKeyword());
		}

		// Extends
		if (typeElement.getKind() != ElementKind.INTERFACE) {
			final TypeMirror superclass = getElementUtils().getFirstVisibleSuperType(typeElement, false, getEnvironment());
			if (superclass != null && superclass.getKind() != TypeKind.NONE) {
				preTag.appendChild(getHtmlFactory().createNewLineTag());
				preTag.appendText(getSARLGrammarKeywordAccess().getExtendsKeyword());
				preTag.appendText(" "); //$NON-NLS-1$
				preTag.appendChildren(getHtmlFactory().createTypeLink(superclass, true, CssStyles.TYPE_SIGNATURE, this));
			}
		}

		// Implements
		final List<? extends TypeMirror> interfaces = typeElement.getInterfaces();
		if (!interfaces.isEmpty()) {
			boolean first = true;
			for (final TypeMirror type : interfaces) {
				final TypeElement type0 = getElementUtils().asTypeElement(type, getEnvironment().getTypeUtils());
				if (getElementUtils().isPublic(type0) || getElementUtils().isLinkable(type0, getEnvironment())) {
					if (first) {
						first = false;
						preTag.appendChild(getHtmlFactory().createNewLineTag());
						if (type0.getKind() == ElementKind.INTERFACE) {
							preTag.appendText(getSARLGrammarKeywordAccess().getImplementsKeyword());
						} else {
							preTag.appendText(getSARLGrammarKeywordAccess().getExtendsKeyword());
						}
					} else {
						preTag.appendText(getSARLGrammarKeywordAccess().getCommaKeyword());
					}
					preTag.appendText(" "); //$NON-NLS-1$
					preTag.appendChildren(getHtmlFactory().createTypeLink(type, true, CssStyles.TYPE_SIGNATURE, this));
				}
			}
		}
	}

	/** Generate the box that contains the message related to the fact that the type is a functional interface. 
	 *
	 * @param bodyTag the container.
	 * @param typeElement the type element.
	 */
	protected void generateFunctionalInterfaceInfo(Element bodyTag, TypeElement typeElement) {
		if (typeElement.getKind() == ElementKind.INTERFACE && getEnvironment().getElementUtils().isFunctionalInterface(typeElement)) {
			final Element dlTag = getHtmlFactory().createDlTag(bodyTag, CssStyles.NESTED_TYPE_INFO);
			final Element dtTag = getHtmlFactory().createDtTag(dlTag, CssStyles.NESTED_TYPE_INFO);
			dtTag.appendText(Messages.AbstractTypeDocumentationGenerator_14);
			final Element ddTag = getHtmlFactory().createDdTag(dlTag, CssStyles.NESTED_TYPE_INFO);
			ddTag.appendText(Messages.AbstractTypeDocumentationGenerator_15);
		}
	}

	/** Generate the information box if the type is nested.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateNestedTypeInfo(Element parent, TypeElement typeElement) {
		final javax.lang.model.element.Element outerClass = typeElement.getEnclosingElement();
		if (outerClass != null) {
			new SimpleElementVisitor9<Void, Void>() {
				@Override
				public Void visitType(TypeElement currentElement, Void parameter) {
					final Element dlTag = getHtmlFactory().createDlTag(parent, CssStyles.NESTED_TYPE_INFO);
					final Element dtTag = getHtmlFactory().createDtTag(dlTag, CssStyles.NESTED_TYPE_INFO);
					if (currentElement.getKind() == ElementKind.INTERFACE) {
						dtTag.appendText(Messages.AbstractTypeDocumentationGenerator_13);
					} else {
						dtTag.appendText(Messages.AbstractTypeDocumentationGenerator_12);
					}
					final Element ddTag = getHtmlFactory().createDdTag(dlTag, CssStyles.NESTED_TYPE_INFO);
					ddTag.appendChildren(getHtmlFactory().createTypeLink(currentElement, true,
							CssStyles.NESTED_TYPE_INFO, AbstractTypeDocumentationGenerator.this));
					return null;
				}
			}.visit(outerClass);
		}
	}

	/** Generate the list of the types that are implementing the type if it is an interface.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateInterfaceUsageInfo(Element parent, TypeElement typeElement) {
		if (typeElement.getKind() == ElementKind.INTERFACE) {
			final Set<TypeElement> impl = getTypeHierarchy().getImplementingClasses(typeElement);
			if (!impl.isEmpty()) {
				final Element dlTag = getHtmlFactory().createDlTag(parent, CssStyles.IMPLEMENTING_CLASS_LIST);
				final Element dtTag = getHtmlFactory().createDtTag(dlTag, CssStyles.IMPLEMENTING_CLASS_LIST);
				dtTag.appendText(Messages.AbstractTypeDocumentationGenerator_11);
				final Element ddTag = getHtmlFactory().createDdTag(dlTag, CssStyles.IMPLEMENTING_CLASS_LIST);
				boolean first = true;
				for (final TypeElement type : impl) {
					if (first) {
						first = false;
					} else {
						ddTag.appendText(Messages.AbstractTypeDocumentationGenerator_8);
					}
					ddTag.appendChildren(getHtmlFactory().createTypeLink(type, true, CssStyles.IMPLEMENTING_CLASS_LIST, this));
				}
			}		
		}
	}

	/** Generate the list of the types that are extending the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateSubTypeInfo(Element parent, TypeElement typeElement) {
		final SortedSet<? extends TypeElement> directSubTypes = getTypeHierarchy().getDirectSubTypes(typeElement);
		if (!directSubTypes.isEmpty()) {
			final Element dlTag = getHtmlFactory().createDlTag(parent, CssStyles.DIRECT_SUBTYPE_LIST);
			final Element dtTag = getHtmlFactory().createDtTag(dlTag, CssStyles.DIRECT_SUBTYPE_LIST);
			if (typeElement.getKind() == ElementKind.INTERFACE) {
				dtTag.appendText(Messages.AbstractTypeDocumentationGenerator_9);
			} else {
				dtTag.appendText(Messages.AbstractTypeDocumentationGenerator_10);
			}
			final Element ddTag = getHtmlFactory().createDdTag(dlTag, CssStyles.DIRECT_SUBTYPE_LIST);
			boolean first = true;
			for (final TypeElement type : directSubTypes) {
				if (first) {
					first = false;
				} else {
					ddTag.appendText(Messages.AbstractTypeDocumentationGenerator_8);
				}
				ddTag.appendChildren(getHtmlFactory().createTypeLink(type, true, CssStyles.DIRECT_SUBTYPE_LIST, this));
			}
		}
	}

	/** Generate the list of the super interfaces.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateSuperInterfacesInfo(Element parent, TypeElement typeElement) {
		final SortedSet<? extends TypeMirror> interfaces = getElementUtils().getAllInterfaces(typeElement, getEnvironment());
		if (!interfaces.isEmpty()) {
			final Element dlTag = getHtmlFactory().createDlTag(parent, CssStyles.SUPER_INTERFACE_LIST);
			final Element dtTag = getHtmlFactory().createDtTag(dlTag, CssStyles.SUPER_INTERFACE_LIST);
			dtTag.appendText(Messages.AbstractTypeDocumentationGenerator_7);
			final Element ddTag = getHtmlFactory().createDdTag(dlTag, CssStyles.SUPER_INTERFACE_LIST);
			boolean first = true;
			for (final TypeMirror type : interfaces) {
				if (first) {
					first = false;
				} else {
					ddTag.appendText(Messages.AbstractTypeDocumentationGenerator_8);
				}
				ddTag.appendChildren(getHtmlFactory().createTypeLink(type, true, CssStyles.SUPER_INTERFACE_LIST, this));
			}
		}
	}

	/** Generate the information on the generic type parameters of the given types.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateTypeParameterInfo(Element parent, TypeElement typeElement) {
		if (!typeElement.getTypeParameters().isEmpty()) {
			final Element dlTag = getHtmlFactory().createDlTag(parent, CssStyles.TYPE_PARAMETER_LIST);
			final Element dtTag = getHtmlFactory().createDtTag(dlTag, CssStyles.TYPE_PARAMETER_LIST);
			dtTag.appendText(Messages.AbstractTypeDocumentationGenerator_4);
			//
			for (final TypeParameterElement parameter : typeElement.getTypeParameters()) {
				final Element ddTag = getHtmlFactory().createDdTag(dlTag, CssStyles.TYPE_PARAMETER_LIST);
				final List<? extends DocTree> comment = getDocUtils().getTypeParameterComment(typeElement, parameter.getSimpleName().toString(), getEnvironment());
				ddTag.appendText(MessageFormat.format(Messages.AbstractTypeDocumentationGenerator_5, parameter.getSimpleName().toString()));
				final CommentTextMemory memory = getHtmlFactory().createCommentTextMemory(ddTag, typeElement, this);
				for (final DocTree text : comment) {
					getHtmlFactory().createCommentText(memory, text, CssStyles.TYPE_PARAMETER_LIST);
				}
			}
		}
	}

	/** Generate the inheritance hierarchy for the given type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateTypeTree(Element parent, TypeElement typeElement) {
		final Element divTag = getHtmlFactory().createDivTag(parent, CssStyles.INHERITANCE_TREE);
		getHtmlFactory().createTypeInheritanceTree(divTag, typeElement.asType(),
				CssStyles.INHERITANCE_TREE_TREE, CssStyles.INHERITANCE_TREE_TYPE, this);
	}

	/** Generate the message that is related to the private API.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generatePrivateApiMessage(Element parent, TypeElement typeElement) {
		final Element messageDiv = getHtmlFactory().createDivTag(parent, CssStyles.HEADER_PRIVATEAPI_MESSAGE);
		messageDiv.appendText(MessageFormat.format(Messages.AbstractTypeDocumentationGenerator_18, typeElement.getSimpleName().toString()));
	}

	/** Generate the introduction of the type.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateTypeIntroduction(Element parent, TypeElement typeElement) {
		final ModuleElement module = getEnvironment().getElementUtils().getModuleOf(typeElement);
		if (!module.isUnnamed()) {
			final Element moduleNameDiv = getHtmlFactory().createDivTag(parent, CssStyles.HEADER_MODULE_NAME);
			moduleNameDiv.appendText(Messages.AbstractTypeDocumentationGenerator_2);
			getHtmlFactory().createUnsecableSpace(moduleNameDiv);
			final List<? extends Node> moduleLink = getHtmlFactory().createModuleLink(module, module.getQualifiedName().toString(),
					CssStyles.HEADER_MODULE_NAME, this);
			moduleNameDiv.appendChildren(moduleLink);
		}
		final PackageElement packageElement = getEnvironment().getElementUtils().getPackageOf(typeElement);
		if (!packageElement.isUnnamed()) {
			final Element packageNameDiv = getHtmlFactory().createDivTag(parent, CssStyles.HEADER_PACKAGE_NAME);
			final List<? extends Node> packageLink = getHtmlFactory().createPackageLink(packageElement, packageElement.getQualifiedName().toString(),
					CssStyles.HEADER_PACKAGE_NAME, this);
			packageNameDiv.appendChildren(packageLink);
		}
		final Element nameDiv = getHtmlFactory().createDivTag(parent, CssStyles.HEADER_TYPE_NAME);
		final String name;
		if (!typeElement.getTypeParameters().isEmpty()) {
			final StringBuilder params = new StringBuilder();
			for (final TypeParameterElement parameter : typeElement.getTypeParameters()) {
				if (params.length() > 0) {
					params.append(", "); //$NON-NLS-1$
				}
				params.append(parameter.getSimpleName().toString());
			}
			name = MessageFormat.format(Messages.AbstractTypeDocumentationGenerator_6,
					typeElement.getSimpleName().toString(),
					params.toString());
		} else {
			name = typeElement.getSimpleName().toString();
		}
		nameDiv.appendText(getTypeHeaderText(name.toString()));
	}	
	
	/** Generate the visible header in the body.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateBodyHeader(Element parent, TypeElement typeElement) {
		Element divTag = getHtmlFactory().createDivTag(parent, CssStyles.HEADER);
		generateHeaderNavigationBar(divTag, typeElement);
		if (getElementUtils().isPrivateAPI(typeElement)) {
			generatePrivateApiMessage(divTag, typeElement);
		}
	}

	/** Generate the navigation bar in the header.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateHeaderNavigationBar(Element parent, TypeElement typeElement) {
		getNavigation().createNavigationBar(parent);
	}

	/** Generate the navigation bar in the footer.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateFooterNavigationBar(Element parent, TypeElement typeElement) {
		getNavigation().createNavigationBar(parent);
	}

	/** Generate the visible footer in the body.
	 *
	 * @param parent the container.
	 * @param typeElement the type element.
	 */
	protected void generateBodyFooter(Element parent, TypeElement typeElement) {
		Element divTag = getHtmlFactory().createDivTag(parent, CssStyles.FOOTER);
		generateFooterNavigationBar(divTag, typeElement);
		createCopyrightBox(divTag);
	}

	/** Replies the header text for the type. It is usually the type of the type followed by the simple name.
	 *
	 * @param simpleName is the simple name.
	 * @return the header text.
	 */
	protected abstract String getTypeHeaderText(String simpleName);

}
