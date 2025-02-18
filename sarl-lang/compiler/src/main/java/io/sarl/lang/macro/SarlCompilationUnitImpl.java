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

package io.sarl.lang.macro;

import org.eclipse.xtend.core.macro.declaration.CompilationUnitImpl;
import org.eclipse.xtend.core.macro.declaration.XtendTypeDeclarationImpl;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlSkill;

/** Implementation of a SARL compilation unit.
 *
 * <p>This processor ensures that SARL type declarations are supported by the compilation unit.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class SarlCompilationUnitImpl extends CompilationUnitImpl {

	@Override
	public XtendTypeDeclarationImpl<? extends XtendTypeDeclaration> toXtendTypeDeclaration(XtendTypeDeclaration delegate) {
		if (delegate instanceof SarlAgent cvalue) {
			final var declaration = new SarlAgentDeclarationImpl();
			declaration.setDelegate(cvalue);
			declaration.setCompilationUnit(this);
			return declaration;
		}
		if (delegate instanceof SarlBehavior cvalue) {
			final var declaration = new SarlBehaviorDeclarationImpl();
			declaration.setDelegate(cvalue);
			declaration.setCompilationUnit(this);
			return declaration;
		}
		if (delegate instanceof SarlSkill cvalue) {
			final var declaration = new SarlSkillDeclarationImpl();
			declaration.setDelegate(cvalue);
			declaration.setCompilationUnit(this);
			return declaration;
		}
		return super.toXtendTypeDeclaration(delegate);
	}

	/** SARL agent declaration for macro system.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	protected static class SarlAgentDeclarationImpl extends XtendTypeDeclarationImpl<SarlAgent> {
		//
	}

	/** SARL behavior declaration for macro system.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	protected static class SarlBehaviorDeclarationImpl extends XtendTypeDeclarationImpl<SarlBehavior> {
		//
	}

	/** SARL skill declaration for macro system.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	protected static class SarlSkillDeclarationImpl extends XtendTypeDeclarationImpl<SarlSkill> {
		//
	}

}
