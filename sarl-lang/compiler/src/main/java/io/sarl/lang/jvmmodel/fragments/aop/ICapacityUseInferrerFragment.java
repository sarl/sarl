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

package io.sarl.lang.jvmmodel.fragments.aop;

import org.eclipse.xtext.common.types.JvmGenericType;

import com.google.inject.ImplementedBy;

import io.sarl.lang.jvmmodel.fragments.ISingleStageInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.aop.impl.CapacityUseInferrerFragment;
import io.sarl.lang.sarl.SarlCapacityUses;

/** Fragment for inferred the capacity uses to the JVM model.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@ImplementedBy(CapacityUseInferrerFragment.class)
public interface ICapacityUseInferrerFragment extends ISingleStageInferrerFragment<SarlCapacityUses, JvmGenericType> {

	@Override
	default Class<SarlCapacityUses> getSupportedType() {
		return SarlCapacityUses.class;
	}

}
