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

package io.sarl.bspl.lang.compiler;

import org.eclipse.xtext.generator.IFileSystemAccess;

import io.sarl.bspl.lang.sarl_bspl.BsplProtocol;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocolRole;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocolSpecification;

/** The generator from SARL to the default target language and an extra target language.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SarlBsplGenerator extends AbstractSarlTargetGenerator {

	/** Generate the SARL code from a BSPL specification.
	 *
	 * @param specification the BSPL specification.
	 * @param fsa the accessor to the file system for creating the SARL elements.
	 */
	protected void _generate(BsplProtocolSpecification specification, IFileSystemAccess fsa) {
		for (final var member : specification.getBsplProtocols()) {
			doGenerate(member, fsa);
		}
	}

	/** Generate the SARL code from a BSPL protocol.
	 *
	 * @param role the BSPL protocol.
	 * @param fsa the accessor to the file system for creating the SARL elements.
	 */
	protected void _generate(BsplProtocol protocol, IFileSystemAccess fsa) {
		for (final var member : protocol.getMembers()) {
			doGenerate(member, fsa);
		}
	}

	/** Generate the SARL code from a BSPL role
	 *
	 * @param role the BSPL role.
	 * @param fsa the accessor to the file system for creating the SARL elements.
	 */
	protected void _generate(BsplProtocolRole role, IFileSystemAccess fsa) {
		
	}

}
