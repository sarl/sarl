/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.jvmmodel.fragments.bspl;

import com.google.inject.ImplementedBy;

import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.fragments.JvmGenericTypeFactory;
import io.sarl.lang.jvmmodel.fragments.JvmGenericTypeProvider;
import io.sarl.lang.jvmmodel.fragments.bspl.impl.ProtocolInferrerFragment;
import io.sarl.lang.sarl.SarlProtocol;

/** Fragment for inferred the protocols to the JVM model.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @author $Author: mbaldoni$
 * @author $Author: cbaroglio$
 * @author $Author: rmicalizio$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@ImplementedBy(ProtocolInferrerFragment.class)
public interface IProtocolInferrerFragment {

	/** Transform the source protocol to its equivalent JVM elements during the second step of
	 * the creation process of multiple output files.
	 *
	 * @param source the source protocol.
	 * @param inferredJvmTypes the provider of the generated JVM types.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @see #transform(SarlProtocol, JvmGenericTypeFactory, IBaseJvmModelInferrer)
	 */
	void transform(SarlProtocol source, JvmGenericTypeProvider inferredJvmTypes, IBaseJvmModelInferrer baseInferrer);

	/** Transform the source protocol to its equivalent JVM elements during the first step of
	 * the creation process of multiple output files.
	 *
	 * @param source the source protocol.
	 * @param inferredJvmTypes the provider of the generated JVM types.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @see #transform(SarlProtocol, JvmGenericTypeProvider, IBaseJvmModelInferrer)
	 */
	void prepareTransform(SarlProtocol source, JvmGenericTypeFactory inferredJvmTypes, IBaseJvmModelInferrer baseInferrer);

}
