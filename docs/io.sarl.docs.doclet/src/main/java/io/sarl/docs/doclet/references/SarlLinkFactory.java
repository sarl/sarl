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

package io.sarl.docs.doclet.references;

import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.Doc;
import com.sun.javadoc.ParameterizedType;
import com.sun.javadoc.Type;
import com.sun.javadoc.WildcardType;
import com.sun.tools.doclets.formats.html.HtmlDocletWriter;
import com.sun.tools.doclets.formats.html.LinkFactoryImpl;
import com.sun.tools.doclets.formats.html.LinkInfoImpl;
import com.sun.tools.doclets.internal.toolkit.Content;
import com.sun.tools.doclets.internal.toolkit.util.links.LinkFactory;
import com.sun.tools.doclets.internal.toolkit.util.links.LinkInfo;

import io.sarl.docs.doclet.utils.Reflect;
import io.sarl.docs.doclet.utils.SARLFeatureAccess;
import io.sarl.docs.doclet.utils.Utils;

/** Link factory for the SARL doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SarlLinkFactory extends LinkFactoryImpl {

	/** Constructor.
	 *
	 * @param writer the writer.
	 */
	public SarlLinkFactory(HtmlDocletWriter writer) {
		super(writer);
	}

	/** Build the link for the primitive.
	 *
	 * @param link the link.
	 * @param linkInfo the information on the link.
	 * @param type the type.
	 */
	@SuppressWarnings("static-method")
	protected void getLinkForPrimitive(Content link, LinkInfo linkInfo, Type type) {
		link.addContent(type.typeName());
	}

	/** Build the link for the annotation type.
	 *
	 * @param link the link.
	 * @param linkInfo the information on the link.
	 * @param type the type.
	 */
	protected void getLinkForAnnotationType(Content link, LinkInfo linkInfo, Type type) {
		link.addContent(getTypeAnnotationLinks(linkInfo));
		linkInfo.type = type.asAnnotatedType().underlyingType();
		link.addContent(getLink(linkInfo));
	}

	/** Build the link for the wildcard.
	 *
	 * @param link the link.
	 * @param linkInfo the information on the link.
	 * @param type the type.
	 */
	protected void getLinkForWildcard(Content link, LinkInfo linkInfo, Type type) {
		linkInfo.isTypeBound = true;
		link.addContent("?"); //$NON-NLS-1$
		final WildcardType wildcardType = type.asWildcardType();
		final Type[] extendsBounds = wildcardType.extendsBounds();
		final SARLFeatureAccess kw = Utils.getKeywords();
		for (int i = 0; i < extendsBounds.length; i++) {
			link.addContent(i > 0 ? kw.getCommaKeyword() + " " //$NON-NLS-1$
					: " " + kw.getExtendsKeyword() + " "); //$NON-NLS-1$ //$NON-NLS-2$
			setBoundsLinkInfo(linkInfo, extendsBounds[i]);
			link.addContent(getLink(linkInfo));
		}
		final Type[] superBounds = wildcardType.superBounds();
		for (int i = 0; i < superBounds.length; i++) {
			link.addContent(i > 0 ? kw.getCommaKeyword() + " " //$NON-NLS-1$
					: " " + kw.getSuperKeyword() + " "); //$NON-NLS-1$ //$NON-NLS-2$
			setBoundsLinkInfo(linkInfo, superBounds[i]);
			link.addContent(getLink(linkInfo));
		}
	}

	/** Build the link for the type variable.
	 *
	 * @param link the link.
	 * @param linkInfo the information on the link.
	 * @param type the type.
	 */
	protected void getLinkForTypeVariable(Content link, LinkInfo linkInfo, Type type) {
		link.addContent(getTypeAnnotationLinks(linkInfo));
		linkInfo.isTypeBound = true;
		final Doc owner = type.asTypeVariable().owner();
		if ((!linkInfo.excludeTypeParameterLinks) && owner instanceof ClassDoc) {
			linkInfo.classDoc = (ClassDoc) owner;
			final Content label = newContent();
			label.addContent(type.typeName());
			linkInfo.label = label;
			link.addContent(getClassLink(linkInfo));
		} else {
			link.addContent(type.typeName());
		}
		final Type[] bounds = type.asTypeVariable().bounds();
		if (!linkInfo.excludeTypeBounds) {
			linkInfo.excludeTypeBounds = true;
			final SARLFeatureAccess kw = Utils.getKeywords();
			for (int i = 0; i < bounds.length; ++i) {
				link.addContent(i > 0 ? " " + kw.getAmpersandKeyword() + " " //$NON-NLS-1$ //$NON-NLS-2$
						: " " + kw.getExtendsKeyword() + " "); //$NON-NLS-1$ //$NON-NLS-2$
				setBoundsLinkInfo(linkInfo, bounds[i]);
				link.addContent(getLink(linkInfo));
			}
		}
	}

	/** Change the bounds into the link info.
	 *
	 * @param linkInfo the link info.
	 * @param bound the new bounds.
	 */
	protected void setBoundsLinkInfo(LinkInfo linkInfo, Type bound) {
		Reflect.callProc(this, LinkFactory.class, "setBoundsLinkInfo", //$NON-NLS-1$
				new Class<?>[]{LinkInfo.class, Type.class},
				linkInfo, bound);
    }

	/** Build the link for the class.
	 *
	 * @param link the link.
	 * @param linkInfo the information on the link.
	 * @param type the type.
	 * @return the content.
	 */
	protected Content getLinkForClass(Content link, LinkInfo linkInfo, Type type) {
		if (linkInfo.isTypeBound && linkInfo.excludeTypeBoundsLinks) {
			//Since we are excluding type parameter links, we should not
			//be linking to the type bound.
			link.addContent(type.typeName());
			link.addContent(getTypeParameterLinks(linkInfo));
			return null;
		}
		linkInfo.classDoc = type.asClassDoc();
		final Content nlink = newContent();
		nlink.addContent(getClassLink(linkInfo));
		if (linkInfo.includeTypeAsSepLink) {
			nlink.addContent(getTypeParameterLinks(linkInfo, false));
		}
		return nlink;
	}

	@Override
	@SuppressWarnings("checkstyle:cyclomaticcomplexity")
	public Content getLink(LinkInfo linkInfo) {
		updateLinkLabel(linkInfo);
		if (linkInfo.type != null) {
			Type type = linkInfo.type;
			Content link = newContent();
			if (type.isPrimitive()) {
				getLinkForPrimitive(link, linkInfo, type);
			} else if (type.asAnnotatedType() != null && type.dimension().length() == 0) {
				getLinkForAnnotationType(link, linkInfo, type);
				return link;
			} else if (type.asWildcardType() != null) {
				getLinkForWildcard(link, linkInfo, type);
			} else if (type.asTypeVariable() != null) {
				getLinkForTypeVariable(link, linkInfo, type);
			} else if (type.asClassDoc() != null) {
				final Content ntype = getLinkForClass(link, linkInfo, type);
				if (ntype == null) {
					return link;
				}
				link = ntype;
			}

			final SARLFeatureAccess keywords = Utils.getKeywords();
			if (linkInfo.isVarArg) {
				if (type.dimension().length() > 2) {
					//Javadoc returns var args as array.
					//Strip out the first [] from the var arg.
					link.addContent(type.dimension().substring(2));
				}
				link.addContent(keywords.getWildcardAsteriskKeyword());
			} else {
				while (type != null && type.dimension().length() > 0) {
					if (type.asAnnotatedType() != null) {
						linkInfo.type = type;
						link.addContent(" "); //$NON-NLS-1$
						link.addContent(getTypeAnnotationLinks(linkInfo));
						link.addContent(keywords.getSquareBracketKeywords());
						type = type.asAnnotatedType().underlyingType().getElementType();
					} else {
						link.addContent(keywords.getSquareBracketKeywords());
						type = type.getElementType();
					}
				}
				linkInfo.type = type;
				final Content newLink = newContent();
				newLink.addContent(getTypeAnnotationLinks(linkInfo));
				newLink.addContent(link);
				link = newLink;
			}
			return link;
		} else if (linkInfo.classDoc != null) {
			final Content link = newContent();
			link.addContent(getClassLink(linkInfo));
			if (linkInfo.includeTypeAsSepLink) {
				link.addContent(getTypeParameterLinks(linkInfo, false));
			}
			return link;
		} else {
			return null;
		}
	}

	/** Update the label of the given link with the SARL notation for lambdas.
	 *
	 * @param linkInfo the link information to update.
	 */
	protected void updateLinkLabel(LinkInfo linkInfo) {
		if (linkInfo.type != null && linkInfo instanceof LinkInfoImpl) {
			final LinkInfoImpl impl = (LinkInfoImpl) linkInfo;
			final ClassDoc classdoc = linkInfo.type.asClassDoc();
			if (classdoc != null) {
				final SARLFeatureAccess kw = Utils.getKeywords();
				final String name = classdoc.qualifiedName();
				if (isPrefix(name, kw.getProceduresName())) {
					linkInfo.label = createProcedureLambdaLabel(impl);
				} else if (isPrefix(name, kw.getFunctionsName())) {
					linkInfo.label = createFunctionLambdaLabel(impl);
				}
			}
		}
	}

	private static boolean isPrefix(String name, String prefix) {
		return name.startsWith(prefix + ".") || name.startsWith(prefix + "$");  //$NON-NLS-1$//$NON-NLS-2$
	}

	/** Create the label for a procedure lambda.
	 *
	 * @param linkInfo the type.
	 * @return the label.
	 */
	protected Content createProcedureLambdaLabel(LinkInfoImpl linkInfo) {
		final ParameterizedType type = linkInfo.type.asParameterizedType();
		if (type != null) {
			final Type[] arguments = type.typeArguments();
			if (arguments != null && arguments.length > 0) {
				return createLambdaLabel(linkInfo, arguments, arguments.length);
			}
		}
		return linkInfo.label;
	}

	/** Create the label for a function lambda.
	 *
	 * @param linkInfo the type.
	 * @return the label.
	 */
	protected Content createFunctionLambdaLabel(LinkInfoImpl linkInfo) {
		final ParameterizedType type = linkInfo.type.asParameterizedType();
		if (type != null) {
			final Type[] arguments = type.typeArguments();
			if (arguments != null && arguments.length > 0) {
				return createLambdaLabel(linkInfo, arguments, arguments.length - 1);
			}
		}
		return linkInfo.label;
	}

	private Content createLambdaLabel(LinkInfoImpl linkInfo, Type[] arguments, int nParams) {
		final SARLFeatureAccess keywords = Utils.getKeywords();
		final Content content = newContent();
		content.addContent(keywords.getLeftParenthesisKeyword());
		boolean first = true;
		int i = 0;
		for (final Type parameterType : arguments) {
			if (first) {
				first = false;
			} else {
				content.addContent(","); //$NON-NLS-1$
			}
			content.addContent(getLink(new LinkInfoImpl(
					linkInfo.configuration, linkInfo.context,
					boundType(parameterType))));
			++i;
			if (i >= nParams) {
				break;
			}
		}
		content.addContent(keywords.getRightParenthesisKeyword());
		content.addContent(keywords.getEqualsSignGreaterThanSignKeyword());
		if (nParams >= arguments.length) {
			content.addContent(keywords.getVoidKeyword());
		} else {
			content.addContent(getLink(new LinkInfoImpl(
					linkInfo.configuration, linkInfo.context,
					boundType(arguments[arguments.length - 1]))));
		}
		return content;
	}

	private static Type boundType(Type parameterType) {
		final Type ptype;
		if (parameterType.isPrimitive()) {
			ptype = parameterType;
		} else {
			final WildcardType wcType = parameterType.asWildcardType();
			if (wcType != null) {
				ptype = findFirstType(wcType.superBounds(), wcType.extendsBounds(), parameterType);
			} else {
				ptype = parameterType;
			}
		}
		return ptype;
	}

	private static Type findFirstType(Type[] v1, Type[] v2, Type v3) {
		if (v1 != null && v1.length > 0) {
			return v1[0];
		}
		if (v2 != null && v2.length > 0) {
			return v2[0];
		}
		return v3;
	}

}
