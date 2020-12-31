/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.lang.macro;

import com.google.inject.Inject;
import org.eclipse.xtend.core.macro.ProcessorInstanceForJvmTypeProvider;
import org.eclipse.xtend.lib.annotations.AccessorsProcessor;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import io.sarl.lang.annotations.SarlAccessorsProcessor;

/** Processor for the {@code @Accessors} active annotations.
 *
 * <p>This processor ensures that the visibility of the generated functions is not higher
 * than the visility allowed into the containing type.
 *
 * <p>TODO: The macro API should use the injection mechanism into the Xtend library.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class SarlProcessorInstanceForJvmTypeProvider extends ProcessorInstanceForJvmTypeProvider {

	@Inject
	private CommonTypeComputationServices services;

	@Override
	public Object getProcessorInstance(JvmType type) {
		return super.getProcessorInstance(filterActiveProcessorType(type, this.services));
	}

	/** Filter the type in order to create the correct processor.
	 *
	 * @param type the type of the processor specified into the {@code @Active} annotation.
	 * @param services the type services of the framework.
	 * @return the real type of the processor to instance.
	 */
	public static JvmType filterActiveProcessorType(JvmType type, CommonTypeComputationServices services) {
		if (AccessorsProcessor.class.getName().equals(type.getQualifiedName())) {
			final JvmType filteredType = services.getTypeReferences().findDeclaredType(SarlAccessorsProcessor.class, type);
			if (filteredType != null) {
				return filteredType;
			}
		}
		return type;
	}

}
