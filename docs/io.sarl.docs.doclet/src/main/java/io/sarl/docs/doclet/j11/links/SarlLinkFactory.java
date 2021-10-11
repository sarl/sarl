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

package io.sarl.docs.doclet.j11.links;

import java.util.List;

import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.type.ArrayType;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.type.TypeVariable;
import javax.lang.model.type.WildcardType;
import javax.lang.model.util.SimpleTypeVisitor9;

import jdk.javadoc.internal.doclets.formats.html.HtmlDocletWriter;
import jdk.javadoc.internal.doclets.formats.html.LinkFactoryImpl;
import jdk.javadoc.internal.doclets.formats.html.LinkInfoImpl;
import jdk.javadoc.internal.doclets.formats.html.markup.FixedStringContent;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTree;
import jdk.javadoc.internal.doclets.formats.html.markup.RawHtml;
import jdk.javadoc.internal.doclets.formats.html.markup.StringContent;
import jdk.javadoc.internal.doclets.toolkit.Content;
import jdk.javadoc.internal.doclets.toolkit.util.links.LinkInfo;
import org.eclipse.xtext.util.PolymorphicDispatcher;

import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Link factory for SARL.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Replace the Java syntax for variadic arguments {@code ...} by its SARL equivalent</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlLinkFactory extends LinkFactoryImpl {

	private final SARLGrammarKeywordAccess sarlKeywords;

	private final PolymorphicDispatcher<Void> linkLabelFixer = PolymorphicDispatcher.createForSingleTarget("_fixLabel", 2, 2, this);

	/** Constructor.
	 *
	 * @param writer the parent writer.
	 * @param access accessor to the SARL keywords.
	 */
	public SarlLinkFactory(HtmlDocletWriter writer, SARLGrammarKeywordAccess access) {
		super(writer);
		this.sarlKeywords = access;
	}

	@Override
	public Content getLink(LinkInfo linkInfo) {
		fixLabel((LinkInfoImpl) linkInfo);
		if (linkInfo.type != null) {
			SimpleTypeVisitor9<Content, LinkInfo> linkVisitor = new Visitor(linkInfo);
			return linkVisitor.visit(linkInfo.type, linkInfo);
		} else if (linkInfo.typeElement != null) {
			Content link = newContent();
			link.addContent(getClassLink(linkInfo));
			if (linkInfo.includeTypeAsSepLink) {
				link.addContent(getTypeParameterLinks(linkInfo, false));
			}
			return link;
		} else {
			return null;
		}
	}
	
	private void fixLabel(LinkInfoImpl linkInfo) {
		// This function replace the Java "..." variadic operand by its SARL equivalent.
		if (linkInfo.label != null) {
			this.linkLabelFixer.invoke(linkInfo.label, linkInfo);
		}
	}

	private String fixLabelString(Object source) {
		return source.toString().replace("...)", this.sarlKeywords.getWildcardAsteriskKeyword() + this.sarlKeywords.getRightParenthesisKeyword());
	}

	/** Fix the label of a string content.
	 *
	 * @param content the content
	 * @param link the link to fix.
	 */
	protected void _fixLabel(StringContent content, LinkInfoImpl link) {
		link.label(fixLabelString(content));
	}

	/** Fix the label of a string content.
	 *
	 * @param content the content
	 * @param link the link to fix.
	 */
	protected void _fixLabel(FixedStringContent content, LinkInfoImpl link) {
		link.label(new FixedStringContent(fixLabelString(content)));
	}

	/** Fix the label of a string content.
	 *
	 * @param content the content
	 * @param link the link to fix.
	 */
	protected void _fixLabel(RawHtml content, LinkInfoImpl link) {
		link.label(new RawHtml(fixLabelString(content)));
	}

	/** Fix the label of a string content.
	 *
	 * @param content the content
	 * @param link the link to fix.
	 */
	protected void _fixLabel(HtmlTree content, LinkInfoImpl link) {
		link.label(new RawHtml(fixLabelString(content)));
	}

	/** Visitor of elements.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.11
	 */
	private class Visitor extends SimpleTypeVisitor9<Content, LinkInfo> {

		final TypeMirror componentType;

		Content link = newContent();

		int currentDepth = 0;

		/** Constructor.
		 *
		 * @param linkInfo the information about the link.
		 */
		Visitor(LinkInfo linkInfo) {
			this.componentType = SarlLinkFactory.this.utils.getComponentType(linkInfo.type);
		}
		
		@Override
		protected Content defaultAction(TypeMirror type, LinkInfo linkInfo) {
			// handles primitives, no types and error types
			this.link.addContent(SarlLinkFactory.this.utils.getTypeName(type, false));
			return this.link;
		}

		@Override
		public Content visitArray(ArrayType type, LinkInfo linkInfo) {
			// keep track of the dimension depth and replace the last dimension
			// specifier with vararags, when the stack is fully unwound.
			++ this.currentDepth;
			linkInfo.type = type.getComponentType();
			visit(linkInfo.type, linkInfo);
			-- this.currentDepth;
			if (SarlLinkFactory.this.utils.isAnnotated(type)) {
				linkInfo.type = type;
				this.link.addContent(" ");
				this.link.addContent(getTypeAnnotationLinks(linkInfo));
			}
			// use vararg if required
			if (linkInfo.isVarArg && currentDepth == 0) {
				this.link.addContent(SarlLinkFactory.this.sarlKeywords.getWildcardAsteriskKeyword());
			} else {
				this.link.addContent(SarlLinkFactory.this.sarlKeywords.getLeftSquareBracketKeyword());
				this.link.addContent(SarlLinkFactory.this.sarlKeywords.getRightSquareBracketKeyword());
			}
			return link;
		}

		@Override
		public Content visitWildcard(WildcardType type, LinkInfo linkInfo) {
			linkInfo.isTypeBound = true;
			this.link.addContent(SarlLinkFactory.this.sarlKeywords.getQuestionMarkKeyword());
			TypeMirror extendsBound = type.getExtendsBound();
			if (extendsBound != null) {
				this.link.addContent(" ");
				this.link.addContent(SarlLinkFactory.this.sarlKeywords.getExtendsKeyword());
				this.link.addContent(" ");
				setBoundsLinkInfo(linkInfo, extendsBound);
				this.link.addContent(getLink(linkInfo));
			}
			TypeMirror superBound = type.getSuperBound();
			if (superBound != null) {
				this.link.addContent(" ");
				this.link.addContent(SarlLinkFactory.this.sarlKeywords.getSuperKeyword());
				this.link.addContent(" ");
				setBoundsLinkInfo(linkInfo, superBound);
				this.link.addContent(getLink(linkInfo));
			}
			return this.link;
		}

		@Override
		public Content visitTypeVariable(TypeVariable type, LinkInfo linkInfo) {
			this.link.addContent(getTypeAnnotationLinks(linkInfo));
			linkInfo.isTypeBound = true;
			final TypeVariable typevariable = (utils.isArrayType(type))
					? (TypeVariable) componentType : type;
					final Element owner = typevariable.asElement().getEnclosingElement();
					if ((!linkInfo.excludeTypeParameterLinks) && SarlLinkFactory.this.utils.isTypeElement(owner)) {
						linkInfo.typeElement = (TypeElement) owner;
						Content label = newContent();
						label.addContent(SarlLinkFactory.this.utils.getTypeName(type, false));
						linkInfo.label = label;
						this.link.addContent(getClassLink(linkInfo));
					} else {
						// No need to link method type parameters.
						this.link.addContent(SarlLinkFactory.this.utils.getTypeName(typevariable, false));
					}

					if (!linkInfo.excludeTypeBounds) {
						linkInfo.excludeTypeBounds = true;
						TypeParameterElement tpe = ((TypeParameterElement) typevariable.asElement());
						boolean more = false;
						List<? extends TypeMirror> bounds = SarlLinkFactory.this.utils.getBounds(tpe);
						for (TypeMirror bound : bounds) {
							// we get everything as extends java.lang.Object we suppress
							// all of them except those that have multiple extends
							if (bounds.size() == 1 &&
									bound.equals(SarlLinkFactory.this.utils.getObjectType()) &&
									!SarlLinkFactory.this.utils.isAnnotated(bound)) {
								continue;
							}
							this.link.addContent(" ");
							if (more) {
								this.link.addContent(SarlLinkFactory.this.sarlKeywords.getAmpersandKeyword());
							} else {
								this.link.addContent(SarlLinkFactory.this.sarlKeywords.getExtendsKeyword());
							}
							this.link.addContent(" ");
							setBoundsLinkInfo(linkInfo, bound);
							this.link.addContent(getLink(linkInfo));
							more = true;
						}
					}
					return this.link;
		}

		@Override
		public Content visitDeclared(DeclaredType type, LinkInfo linkInfo) {
			if (linkInfo.isTypeBound && linkInfo.excludeTypeBoundsLinks) {
				// Since we are excluding type parameter links, we should not
				// be linking to the type bound.
				this.link.addContent(SarlLinkFactory.this.utils.getTypeName(type, false));
				this.link.addContent(getTypeParameterLinks(linkInfo));
				return this.link;
			}
			this.link = newContent();
			this.link.addContent(getTypeAnnotationLinks(linkInfo));
			linkInfo.typeElement = SarlLinkFactory.this.utils.asTypeElement(type);
			this.link.addContent(getClassLink(linkInfo));
			if (linkInfo.includeTypeAsSepLink) {
				this.link.addContent(getTypeParameterLinks(linkInfo, false));
			}
			return this.link;
		}

	    private void setBoundsLinkInfo(LinkInfo linkInfo, TypeMirror bound) {
	        linkInfo.typeElement = null;
	        linkInfo.label = null;
	        linkInfo.type = bound;
	    }

	}

	
}
