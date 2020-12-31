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

package io.sarl.eclipse.slf4j;

import org.slf4j.ILoggerFactory;
import org.slf4j.IMarkerFactory;
import org.slf4j.helpers.BasicMDCAdapter;
import org.slf4j.helpers.BasicMarkerFactory;
import org.slf4j.spi.MDCAdapter;
import org.slf4j.spi.SLF4JServiceProvider;

/**
 * Provider of a SLF4J that is linked to the M2E plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 * @deprecated Remove when SLF4J is properly configured for Eclipse.
 */
@Deprecated
public class Slf4jEclipseLoggerServiceProvider implements SLF4JServiceProvider {

	/**
	 * Declare the version of the SLF4J API this implementation is compiled against.
	 * The value of this field is modified with each major release.
	 */
	private static final String REQUESTED_API_VERSION = "1.8.99"; //$NON-NLS-1$

	private ILoggerFactory loggerFactory;

	private MDCAdapter mdc;

	private IMarkerFactory markerFactory;

	@Override
	public ILoggerFactory getLoggerFactory() {
		return this.loggerFactory;
	}

	@Override
	public IMarkerFactory getMarkerFactory() {
		return this.markerFactory;
	}

	@Override
	public synchronized MDCAdapter getMDCAdapter() {
		return this.mdc;
	}

	@Override
	public String getRequesteApiVersion() {
		return REQUESTED_API_VERSION;
	}

	@Override
	public void initialize() {
		this.loggerFactory = new Slf4jEclipseLoggerFactory();
		this.markerFactory = new BasicMarkerFactory();
		this.mdc = new BasicMDCAdapter();
	}

}
