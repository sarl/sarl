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

import java.io.IOException;

import com.google.inject.Inject;

import io.sarl.bspl.lang.compiler.fragments.BsplProtocolCapacityGeneratorFragment;
import io.sarl.bspl.lang.compiler.fragments.BsplRoleEnumerationGeneratorFragment;
import io.sarl.bspl.lang.compiler.generic.AbstractSarlTargetGenerator;
import io.sarl.bspl.lang.compiler.generic.ISarlTargetGeneratorContext;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocol;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocolSpecification;

/** The generator from SARL to the default target language and an extra target language.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SarlBsplGenerator extends AbstractSarlTargetGenerator<IProtocolNames> {

	@Inject
	private BsplRoleEnumerationGeneratorFragment roleEnumerationFragment; 

	@Inject
	private BsplProtocolCapacityGeneratorFragment protocolCapacityFragment;
	
	/** Generate the SARL code from a BSPL specification.
	 *
	 * @param specification the BSPL specification.
	 * @param context the generation context.
	 */
	protected void _generate(BsplProtocolSpecification specification, ISarlTargetGeneratorContext<IProtocolNames> context) {
		final var packageName = specification.getPackage();
		final var names = context.getInjector().getInstance(IProtocolNames.class);
		final var packagedContext = context.withNameProvider(names).withPackage(packageName);
		for (final var member : specification.getBsplProtocols()) {
			doGenerate(member, packagedContext);
		}
	}

	/** Generate the SARL code from a BSPL protocol.
	 *
	 * @param protocol the BSPL protocol.
	 * @param preStage indicates if the generation is for the pre-stage {@code true} or the final stage.
	 * @param context the generation context.
	 * @throws IOException if a file cannot be generated.
	 */
	protected void _generate(BsplProtocol protocol, ISarlTargetGeneratorContext<IProtocolNames> context) throws IOException {
		final var protocolName = protocol.getName();
		final var packagedNamedContext = context.withTypeName(protocolName);

		if (context.isPreStage()) {
			this.roleEnumerationFragment.preGenerate(protocol.getRoles(), packagedNamedContext);
			this.protocolCapacityFragment.preGenerate(protocol.getRoles(), packagedNamedContext);
		} else {
			this.roleEnumerationFragment.generate(protocol.getRoles(), packagedNamedContext);
			this.protocolCapacityFragment.generate(protocol.getMessages(), protocol.getRoles(), packagedNamedContext);
		}
	}

}
