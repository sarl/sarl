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

package io.sarl.lang.jvmmodel.fragments.oop;

import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtext.common.types.JvmGenericType;

import com.google.inject.ImplementedBy;

import io.sarl.lang.jvmmodel.fragments.IInheritedStageInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.oop.impl.FieldInferrerFragment;

/** Fragment for inferred the fields to the JVM model.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.15
 */
@ImplementedBy(FieldInferrerFragment.class)
public interface IFieldInferrerFragment extends IInheritedStageInferrerFragment<XtendField, JvmGenericType> {

	@Override
	default Class<XtendField> getSupportedType() {
		return XtendField.class;
	}

}
