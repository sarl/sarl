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

package io.sarl.lang.jvmmodel.fragments.oop.impl;

import java.util.function.BiConsumer;

import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;

import com.google.common.base.Strings;

import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerFragment;
import io.sarl.lang.jvmmodel.fragments.oop.IFieldInferrerFragment;

/** Fragment for inferred the fields to the JVM model.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class FieldInferrerFragment extends AbstractJvmModelInferrerFragment implements IFieldInferrerFragment {

	@Override
	public void transform(XtendField source, JvmGenericType container,
			IBaseJvmModelInferrer baseInferrer,
			BiConsumer<XtendField, JvmGenericType> inheritedTransformer) {
		inheritedTransformer.accept(source, container);
		// Override the visibility
		final var field = (JvmField) this.sarlAssociations.getPrimaryJvmElement(source);
		if (field != null) {
			setVisibility(field, source);
	
			final var context = baseInferrer.getContext(container);
			if (context != null) {
				final var name = source.getName();
				if (!Strings.isNullOrEmpty(name)) {
					context.incrementSerial(name.hashCode());
				}
				final var type = source.getType();
				if (type != null) {
					context.incrementSerial(type.getIdentifier().hashCode());
				}
				context.setInjectable(field);
			}
		}
	}

}
