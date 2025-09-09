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

package io.sarl.eclipse.runtime;

import org.eclipse.jdt.launching.PropertyChangeEvent;

import io.sarl.apputils.eclipseextensions.sreprovider.ISREInstall;

/**
 * Default implementation of {@code ISREInstallChangedAdapter}.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
public class SREInstallChangedAdapter implements ISREInstallChangedListener {

	/** Construct the adapter.
	 */
	public SREInstallChangedAdapter() {
		//
	}

	@Override
	public void defaultSREInstallChanged(ISREInstall previous, ISREInstall current) {
		//
	}

	@Override
	public void sreChanged(PropertyChangeEvent event) {
		//
	}

	@Override
	public void sreAdded(ISREInstall sre) {
		//
	}

	@Override
	public void sreRemoved(ISREInstall sre) {
		//
	}

}
