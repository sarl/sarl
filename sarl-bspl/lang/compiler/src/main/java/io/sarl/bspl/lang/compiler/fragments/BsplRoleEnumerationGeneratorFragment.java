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
import java.util.List;

import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;

import com.google.inject.Singleton;

import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;
import io.sarl.bspl.api.protocol.impl.ProtocolSpace;
import io.sarl.bspl.lang.bspl.BsplProtocolRole;
import io.sarl.bspl.lang.compiler.IProtocolNames;
import io.sarl.bspl.lang.compiler.generic.ISarlTargetGeneratorContext;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Skill;

/** The generator of the BSPL role enumeration.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
public class BsplRoleEnumerationGeneratorFragment {

	/** Run the pre-stage actions for the BSPL role enumeration.
	 *
	 * @param roles the roles in the BSPL protocol.
	 * @param context the generation context.
	 * @throws IOException if the generated file cannot be created.
	 */
	public void preGenerate(List<BsplProtocolRole> roles, ISarlTargetGeneratorContext<IProtocolNames> context) throws IOException {
		//
	}

	/** Generate the SARL code that is specific to BSPL roles.
	 *
	 * @param roles the roles in the BSPL protocol.
	 * @param context the generation context.
	 * @throws IOException if the generated file cannot be created.
	 */
	public void generate(List<BsplProtocolRole> roles, ISarlTargetGeneratorContext<IProtocolNames> context) throws IOException {
		generateEnumeration(context, (content, names) -> {
			for (var i = 0; i < roles.size(); ++i) {
				generate(roles.get(i), i == (roles.size() - 1), names, context, content);
			}
		});
	}

	private static void generateEnumeration(ISarlTargetGeneratorContext<IProtocolNames> context, Procedure2<ITreeAppendable, IProtocolNames> generator) throws IOException {
		final var names = context.getNameProvider();
		final var enumerationName = names.getProtocolEnumerationName(context.getEnclosingTypeName());

		final var importManager = context.newImportManager(context.getPackage(), enumerationName);
		final var content = context.newAppendableContent(importManager);

		content.append("public enum ").append(enumerationName) //$NON-NLS-1$
			.append(" implements ").append(names.getProtocoRoleGenericInterface()) //$NON-NLS-1$
			.append(" {").increaseIndentation(); //$NON-NLS-1$

		if (generator != null) {
			generator.apply(content, names);
		}

		content.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$

		context.createJavaFile(context.getPackage(), enumerationName, importManager, content);
	}

	/** Generate the SARL code that is specific to BSPL role.
	 *
	 * @param role the role in the BSPL protocol.
	 * @param lastRole indicates if the given role is the last one in the list of roles.
	 * @param names the tools for accessing to the naming convention for the protocols.
	 * @param context the generation context.
	 * @param content the generated content.
	 */
	@SuppressWarnings("static-method")
	protected void generate(BsplProtocolRole role, boolean lastRole, IProtocolNames names,
			ISarlTargetGeneratorContext<IProtocolNames> context, ITreeAppendable content) {
		content.newLine().append(role.getName()).append(" {").increaseIndentation().newLine(); //$NON-NLS-1$

		content
			.append("public ").append(Class.class).append("<? extends ").append(ProtocolCapacity.class) //$NON-NLS-1$ //$NON-NLS-2$
			.append("> getProtocolCapacity() {").increaseIndentation().newLine() //$NON-NLS-1$
			.append("return ").append(names.getProtocolCapacity(context.getPackage(), context.getEnclosingTypeName(), role)) //$NON-NLS-1$
			.append(".class;").decreaseIndentation().newLine().append("}").newLine(); //$NON-NLS-1$ //$NON-NLS-2$

		content
			.append("public ").append(Skill.class).append(" getProtocolSkill(") //$NON-NLS-1$ //$NON-NLS-2$
			.append(ProtocolSpace.class).append(" space) {").increaseIndentation().newLine() //$NON-NLS-1$
			.append("return new ").append(names.getProtocolSkill(context.getPackage(), context.getEnclosingTypeName(), role)) //$NON-NLS-1$
			.append("(space);").decreaseIndentation().newLine().append("}").newLine(); //$NON-NLS-1$ //$NON-NLS-2$

		content
			.append("public ").append(Behavior.class).append(" getProtocolBehavior(") //$NON-NLS-1$ //$NON-NLS-2$
			.append(Agent.class).append(" ag) {").increaseIndentation().newLine() //$NON-NLS-1$
			.append("return new ").append(names.getProtocolBehavior(context.getPackage(), context.getEnclosingTypeName(), role)) //$NON-NLS-1$
			.append("(ag);").decreaseIndentation().newLine().append("}").newLine(); //$NON-NLS-1$ //$NON-NLS-2$

		final String minCard;
		if (role.getMin() != null && role.getMin().intValue() > 0) {
			minCard = role.getMin().toString();
		} else {
			minCard = Integer.toString(0);
		}
		content
			.append("public int  getMinCardinality() {").increaseIndentation().newLine() //$NON-NLS-1$
			.append("return ").append(minCard) //$NON-NLS-1$
			.append(";").decreaseIndentation().newLine().append("}").newLine(); //$NON-NLS-1$ //$NON-NLS-2$

		content
			.append("public int  getMaxCardinality() {").increaseIndentation().newLine() //$NON-NLS-1$
			.append("return "); //$NON-NLS-1$
		if (role.getMax() != null && role.getMax().intValue() >= 0) {
			content.append(role.getMax().toString());
		} else {
			content.append(Integer.class).append(".MAX_VALUE"); //$NON-NLS-1$
		}
		content
			.append(";").decreaseIndentation().newLine().append("}"); //$NON-NLS-1$ //$NON-NLS-2$

		content.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$
		if (lastRole) {
			content.append(";"); //$NON-NLS-1$
		} else {
			content.append(","); //$NON-NLS-1$
		}
	}

}
