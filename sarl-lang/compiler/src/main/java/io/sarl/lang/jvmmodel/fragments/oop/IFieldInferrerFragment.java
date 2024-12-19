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

package io.sarl.lang.jvmmodel.fragments.oop;

import java.util.function.BiConsumer;

import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtext.common.types.JvmGenericType;

import com.google.inject.ImplementedBy;

import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.fragments.oop.impl.FieldInferrerFragment;

/** Fragment for inferred the fields to the JVM model.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@ImplementedBy(FieldInferrerFragment.class)
public interface IFieldInferrerFragment {

	/** Transform the source field to its equivalent JVM elements.
	 *
	 * @param source the source field.
	 * @param container the receiver of the JVM elements.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param inheritedTransformer the transformer that is inherited from the standard Xtext/Xtend API.
	 */
	void transform(XtendField source, JvmGenericType container,
			IBaseJvmModelInferrer baseInferrer,
			BiConsumer<XtendField, JvmGenericType> inheritedTransformer);
}
