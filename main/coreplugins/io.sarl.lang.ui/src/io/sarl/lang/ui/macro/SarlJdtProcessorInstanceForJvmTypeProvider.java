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

package io.sarl.lang.ui.macro;

import com.google.inject.Inject;
import org.eclipse.xtend.ide.macro.JdtBasedProcessorProvider;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import io.sarl.lang.macro.SarlProcessorInstanceForJvmTypeProvider;

/** Processor for the {@code @Accessors} active annotations.
 *
 * <p>This processor ensures that the visibility of the generated functions is not higher
 * than the visility allowed into the containing type.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class SarlJdtProcessorInstanceForJvmTypeProvider extends JdtBasedProcessorProvider {

	@Inject
	private CommonTypeComputationServices services;

	@Override
	public Object getProcessorInstance(JvmType type) {
		final JvmType filteredType = SarlProcessorInstanceForJvmTypeProvider.filterActiveProcessorType(type, this.services);
		return super.getProcessorInstance(filteredType);
	}

}
