/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.ui.refactoring.rename;

import com.google.inject.Inject;
import com.google.inject.Provider;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.ui.refactoring.participant.JvmMemberRenameStrategy;
import org.eclipse.xtext.ui.refactoring.IRenameStrategy;
import org.eclipse.xtext.ui.refactoring.ui.IRenameElementContext;

/** Provider of rename strategies in the SARL context.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLRenameStrategyProvider extends JvmMemberRenameStrategy.Provider {

	@Inject
	private Provider<SARLPackageRenameParticipant.Strategy> guicePackageStartegyProvider;

	@Override
	public IRenameStrategy get(EObject targetEObject, IRenameElementContext renameElementContext)
			throws NoSuchStrategyException {
		if (renameElementContext instanceof SARLPackageRenameParticipant.Context) {
			final SARLPackageRenameParticipant.Strategy strategy = this.guicePackageStartegyProvider.get();
			if (strategy.initialize(targetEObject, renameElementContext)) {
				return strategy;
			}
		}
		return super.get(targetEObject, renameElementContext);
	}

}
