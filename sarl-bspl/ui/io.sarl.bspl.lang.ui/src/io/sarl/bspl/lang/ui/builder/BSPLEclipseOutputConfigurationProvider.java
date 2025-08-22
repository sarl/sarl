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

package io.sarl.bspl.lang.ui.builder;

import com.google.inject.Inject;
import jakarta.inject.Singleton;
import org.eclipse.xtext.builder.EclipseOutputConfigurationProvider;
import org.eclipse.xtext.generator.IOutputConfigurationProvider;

import io.sarl.apputils.uiextensions.outputconfig.IContextualOutputConfigurationProvider3;

/** Provider of output configuration for the BSPL Eclipse environment.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
public class BSPLEclipseOutputConfigurationProvider extends EclipseOutputConfigurationProvider implements IContextualOutputConfigurationProvider3 {

	/** Constructor.
	 *
	 * @param delegate the provider to delegate to.
	 */
	@Inject
	public BSPLEclipseOutputConfigurationProvider(IOutputConfigurationProvider delegate) {
		super(delegate);
	}

}
