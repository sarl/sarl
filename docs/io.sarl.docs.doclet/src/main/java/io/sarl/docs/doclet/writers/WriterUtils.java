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

package io.sarl.docs.doclet.writers;

import com.sun.javadoc.ExecutableMemberDoc;
import com.sun.javadoc.Parameter;
import com.sun.tools.doclets.formats.html.ConfigurationImpl;
import com.sun.tools.doclets.formats.html.LinkInfoImpl;
import com.sun.tools.doclets.formats.html.SubWriterHolderWriter;
import com.sun.tools.doclets.internal.toolkit.Content;

import io.sarl.docs.doclet.SarlConfiguration;
import io.sarl.docs.doclet.proxy.ProxyInstaller;
import io.sarl.docs.doclet.utils.Utils;

/** Utilities for writers.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public final class WriterUtils {

	private WriterUtils() {
		//
	}

	/** Add a parameter to the given executable member.
	 *
	 * @param member the executable member.
	 * @param param the parameter.
	 * @param isVarArg indicates if the parameter is variadic.
	 * @param htmlTree the output.
	 * @param configuration the configuration.
	 * @param writer the background writer.
	 */
	public static void addFormalParameter(ExecutableMemberDoc member, Parameter param, boolean isVarArg, Content htmlTree,
			SarlConfiguration configuration, SubWriterHolderWriter writer) {
		final ProxyInstaller proxyInstaller = configuration.getProxyInstaller();
		final ExecutableMemberDoc omember = proxyInstaller.unwrap(member);
		final Parameter oparam = proxyInstaller.unwrap(param);
		final String defaultValue = Utils.getParameterDefaultValue(omember, oparam, configuration);
		final boolean addDefaultValueBrackets = Utils.isNullOrEmpty(defaultValue)
				&& Utils.isDefaultValuedParameter(oparam, configuration);
		if (addDefaultValueBrackets) {
			htmlTree.addContent("["); //$NON-NLS-1$
		}
		if (oparam.name().length() > 0) {
			htmlTree.addContent(oparam.name());
		}
		htmlTree.addContent(writer.getSpace());
		htmlTree.addContent(Utils.getKeywords().getColonKeyword());
		htmlTree.addContent(" "); //$NON-NLS-1$
		if (oparam.type() != null) {
			final Content link = writer.getLink(new LinkInfoImpl(
					configuration, LinkInfoImpl.Kind.EXECUTABLE_MEMBER_PARAM,
					oparam.type()).varargs(isVarArg));
			htmlTree.addContent(link);
		}
		if (addDefaultValueBrackets) {
			htmlTree.addContent("]"); //$NON-NLS-1$
		} else if (!Utils.isNullOrEmpty(defaultValue)) {
			htmlTree.addContent(" "); //$NON-NLS-1$
			htmlTree.addContent(Utils.getKeywords().getEqualsSignKeyword());
			htmlTree.addContent(writer.getSpace());
			htmlTree.addContent(defaultValue);
		}
	}

	/** Add a type parameter to the given executable member.
	 *
	 * @param typeParameters the type parameters.
	 * @param htmlTree the output.
	 * @param configuration the configuration.
	 * @param writer the background writer.
	 */
	public static void addTypeParameters(Content typeParameters, Content htmlTree,
			ConfigurationImpl configuration, SubWriterHolderWriter writer) {
		if (!typeParameters.isEmpty()) {
			htmlTree.addContent(" "); //$NON-NLS-1$
			htmlTree.addContent(Utils.keyword(Utils.getKeywords().getWithKeyword()));
			htmlTree.addContent(" "); //$NON-NLS-1$
			htmlTree.addContent(typeParameters);
			htmlTree.addContent(" "); //$NON-NLS-1$
		}
	}

	/** Add a type parameter to the given executable member.
	 *
	 * @param member the member to update.
	 * @param htmlTree the output.
	 * @param configuration the configuration.
	 * @param writer the background writer.
	 */
	public static void addAnnotations(ExecutableMemberDoc member, Content htmlTree,
			ConfigurationImpl configuration, SubWriterHolderWriter writer) {
		writer.addAnnotationInfo(member, htmlTree);
	}

}
