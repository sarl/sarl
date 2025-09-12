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

package io.sarl.lang.jvmmodel.fragments.oop;

import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtext.common.types.JvmGenericType;

import com.google.inject.ImplementedBy;

import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.fragments.ISingleStageInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.oop.impl.ActionInferrerFragment;

/** Fragment for inferred the actions (functions) to the JVM model.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@ImplementedBy(ActionInferrerFragment.class)
public interface IActionInferrerFragment extends ISingleStageInferrerFragment<XtendFunction, JvmGenericType> {

	@Override
	default Class<XtendFunction> getSupportedType() {
		return XtendFunction.class;
	}

	/** Transform the source action to its equivalent JVM elements.
	 *
	 * @param source the source action.
	 * @param container the receiver of the JVM elements.
	 * @param allowDispatch indicates if dispatch function is allowed in the context.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 */
	void transform(XtendFunction source, JvmGenericType container, boolean allowDispatch,
			IBaseJvmModelInferrer baseInferrer);

	@Override
	default void transform(XtendFunction source, JvmGenericType container, IBaseJvmModelInferrer baseInferrer) {
		transform(source, container, true, baseInferrer);
	}

}
