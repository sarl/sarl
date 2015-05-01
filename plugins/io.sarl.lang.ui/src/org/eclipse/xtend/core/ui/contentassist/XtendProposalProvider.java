/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

package org.eclipse.xtend.core.ui.contentassist;

/** This class is defined for solving a bug in the Xtext/Xtend generators.
 *
 * See {@link "https://www.eclipse.org/forums/index.php?t=msg&th=303392&goto=812958&#msg_812958"}.
 *
 * FIXME: Remove this class when the MWE2 generator fragment will no more extends the "xtend.core.ui" class.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class XtendProposalProvider extends org.eclipse.xtend.ide.contentassist.XtendProposalProvider {

	//

}
