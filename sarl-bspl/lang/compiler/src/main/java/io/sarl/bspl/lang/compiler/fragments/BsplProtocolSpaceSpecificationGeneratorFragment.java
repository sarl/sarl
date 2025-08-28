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
 */

package io.sarl.bspl.lang.compiler.fragments;

import java.io.IOException;

import com.google.inject.Singleton;

import io.sarl.bspl.lang.bspl.BsplProtocol;
import io.sarl.bspl.lang.compiler.IProtocolNames;
import io.sarl.bspl.lang.compiler.generic.ISarlTargetGeneratorContext;

/** The generator of the space specification for a BSPL protocol.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
public class BsplProtocolSpaceSpecificationGeneratorFragment {

	/** Run the pre-stage actions for the BSPL protocol space specification.
	 *
	 * @param protocol the protocol.
	 * @param context the generation context.
	 * @throws IOException if the generated file cannot be created.
	 */
	public void preGenerate(BsplProtocol protocol, ISarlTargetGeneratorContext<IProtocolNames> context) throws IOException {
		//
	}

	/** Generate the SARL code that is specific to BSPL protocol space specification.
	 *
	 * @param protocol the protocol.
	 * @param context the generation context.
	 * @throws IOException if the generated file cannot be created.
	 */
	@SuppressWarnings("static-method")
	public void generate(BsplProtocol protocol, ISarlTargetGeneratorContext<IProtocolNames> context) throws IOException {
		final var names = context.getNameProvider();
		final var specificationName = names.getProtocolSpaceSpecificationName(context.getEnclosingTypeName());
		final var specificationPackageName = names.getProtocolAdapterPackageName(context.getPackage(), context.getEnclosingTypeName());
		final var roleEnumeration = names.getProtocolRoleEnumeration(context.getPackage(), protocol);

		final var importManager = context.newImportManager(specificationPackageName, specificationName);
		final var content = context.newAppendableContent(importManager);

		if (protocol.isPackageVisibility()) {
			content.append("package "); //$NON-NLS-1$
		} else {
			content.append("public "); //$NON-NLS-1$
		}
		
		content.append("class ").append(specificationName) //$NON-NLS-1$
			.append(" extends ").append(names.getProtocolSpaceSpecificationGenericInterface()) //$NON-NLS-1$
			.append(" {").increaseIndentation() //$NON-NLS-1$
			.newLine().append("override getRoles : ").append(names.getProtocolRoleGenericInterface().arrayType()).append(" {").increaseIndentation() //$NON-NLS-1$ //$NON-NLS-2$
			.newLine().append(roleEnumeration).append(".values") //$NON-NLS-1$
			.decreaseIndentation().newLine().append("}") //$NON-NLS-1$
			.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$

		context.createSarlFile(context.getSource(), specificationPackageName, specificationName, importManager, content);
	}

}
