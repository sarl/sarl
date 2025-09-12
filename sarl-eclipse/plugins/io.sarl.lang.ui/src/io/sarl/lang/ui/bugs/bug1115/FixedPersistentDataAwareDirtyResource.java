/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.ui.bugs.bug1115;

import org.eclipse.emf.common.util.URI;
import org.eclipse.xtext.builder.impl.PersistentDataAwareDirtyResource;

/** Dirty resource.
 *
 * <p>This class provides a fix for Issue #1115: Eclipse error on the editor state.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1115"
 */
@SuppressWarnings("restriction")
public class FixedPersistentDataAwareDirtyResource extends PersistentDataAwareDirtyResource {

	@Override
	public synchronized URI getURI() {
		return super.getURI();
	}

}
