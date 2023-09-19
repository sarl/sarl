/*
 * $Id$
 * This file is a part of the Arakhne Foundation Classes, http://www.arakhne.org/afc
 *
 * Copyright (c) 2000-2012 Stephane GALLAND.
 * Copyright (c) 2005-10, Multiagent Team, Laboratoire Systemes et Transports,
 *                        Universite de Technologie de Belfort-Montbeliard.
 * Copyright (c) 2013-2022 The original authors, and other authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.arakhne.afc.bootique.applicationdata2.modules;

import io.bootique.BQCoreModuleExtender;
import io.bootique.di.Binder;

import org.arakhne.afc.bootique.applicationdata2.annotations.ApplicationDescription2;
import org.arakhne.afc.bootique.applicationdata2.annotations.DefaultApplicationName;

/** Module for the compiler application metadata version 2.
 *
 * <p>This file is copied from the Bootique's original file in order to change the visibility.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 18.0
 */
public final class BQCoreModuleExtender2 extends BQCoreModuleExtender {

	/** Constructor.
	 *
	 * @param binder the binder.
	 */
	protected BQCoreModuleExtender2(Binder binder) {
		super(binder);
	}

	/**
	 * Binds an optional application description used in help messages, etc.
	 *
	 * @param description optional application description used in help messages, etc.
	 * @return this extender instance.
	 */
	@Override
	public BQCoreModuleExtender setApplicationDescription(String description) {
		super.setApplicationDescription(description);
		this.binder.bind(String.class, ApplicationDescription2.class).toInstance(description);
		return this;
	}

	/**
	 * Binds an optional application name used in help messages, etc.
	 *
	 * @param description optional application name used in help messages, etc.
	 * @return this extender instance.
	 */
	public BQCoreModuleExtender setApplicationName(String description) {
		this.binder.bind(String.class, DefaultApplicationName.class).toInstance(description);
		return this;
	}

}
