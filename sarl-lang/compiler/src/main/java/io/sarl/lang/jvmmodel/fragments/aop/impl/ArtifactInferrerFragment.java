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

package io.sarl.lang.jvmmodel.fragments.aop.impl;

import org.eclipse.xtext.common.types.JvmGenericType;

import com.google.common.base.Strings;

import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.IArtifactInferrerFragment;
import io.sarl.lang.sarl.SarlArtifact;

/** Fragment for inferred the artifacts to the JVM model.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class ArtifactInferrerFragment extends AbstractJvmModelInferrerFragment implements IArtifactInferrerFragment {

	@Override
	public void transform(SarlArtifact source, JvmGenericType inferredJvmType,
			IBaseJvmModelInferrer baseInferrer) {
		// Issue #356: do not generate if the space has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
	}

}
